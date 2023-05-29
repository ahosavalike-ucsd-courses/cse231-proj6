use std::collections::HashSet;
use std::env;

static mut HEAP_START: *const u64 = std::ptr::null();
static HEAP_META_SIZE: usize = 3;

const TRUE_VAL: i64 = 7;
const FALSE_VAL: i64 = 3;
const NIL_VAL: i64 = 1;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, buffer: *mut u64) -> i64;
}

fn deep_equal_recurse(l: i64, r: i64, seen: &mut HashSet<(i64, i64)>) -> bool {
    // If not list, early exit
    if l & 3 != 1 || r & 3 != 1 || l == NIL_VAL || r == NIL_VAL {
        return l == r;
    }

    if seen.contains(&(l, r)) {
        return true;
    }
    seen.insert((l, r));

    let la = (l - 1) as *const i64;
    let ra = (r - 1) as *const i64;
    let lc = unsafe { *la.add(1) } as isize;
    let rc = unsafe { *ra.add(1) } as isize;
    // Check length
    if lc != rc {
        return false;
    }
    for i in 0..lc {
        let ln = unsafe { *la.offset(i + 2) };
        let rn = unsafe { *ra.offset(i + 2) };
        if !deep_equal_recurse(ln, rn, seen) {
            return false;
        }
    }
    return true;
}

#[export_name = "\x01snek_deep_equal"]
fn deep_equal(l: i64, r: i64) -> i64 {
    if deep_equal_recurse(l, r, &mut HashSet::new()) {
        TRUE_VAL
    } else {
        FALSE_VAL
    }
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // print error message according to writeup
    eprintln!(
        "an error ocurred {errcode}: {}",
        match errcode {
            1 => "invalid representation",
            i if i >= 20 && i <= 29 => "invalid argument",
            i if i >= 30 && i <= 39 => "overflow",
            i if i == 40 => "index out of range",
            _ => "",
        }
    );
    std::process::exit(1);
}

fn parse_input(input: &str) -> i64 {
    // parse the input string into internal value representation
    match input {
        "true" => TRUE_VAL,
        "false" | "" => FALSE_VAL,
        _ => (input.parse::<i64>().expect("Invalid") as i64) << 1,
    }
}

fn snek_str(val: i64, seen: &mut HashSet<i64>) -> String {
    if val == TRUE_VAL {
        "true".to_string()
    } else if val == FALSE_VAL {
        "false".to_string()
    } else if val % 2 == 0 {
        format!("{}", val >> 1)
    } else if val == NIL_VAL {
        "nil".to_string()
    } else if val & 1 == 1 {
        if seen.contains(&val) {
            return "(list <cyclic>)".to_string();
        }
        seen.insert(val);
        let addr = (val - 1) as *const i64;
        let count = unsafe { *addr.add(1) } as usize;
        let mut v: Vec<i64> = vec![0; count];
        for i in 0..count {
            v[i] = unsafe { *addr.add(i + 2) };
        }
        let result = format!(
            "(list {})",
            v.iter()
                .map(|x| snek_str(*x, seen))
                .collect::<Vec<String>>()
                .join(" ")
        );
        seen.remove(&val);
        return result;
    } else {
        format!("Unknown value: {}", val)
    }
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn print_result(result: i64) -> i64 {
    println!("{}", snek_str(result, &mut HashSet::new()));
    return result;
}

#[export_name = "\x01snek_try_gc"]
pub unsafe fn snek_try_gc(count: isize, curr_rbp: *const u64, curr_rsp: *const u64) {
    let heap_end = *HEAP_START.add(1) as *const u64;
    let new_heap_ptr = snek_gc(curr_rbp, curr_rsp);
    if heap_end.offset_from(new_heap_ptr) < count {
        eprintln!("out of memory");
        std::process::exit(5)
    }
    *(HEAP_START as *mut u64) = new_heap_ptr as u64;
}

#[export_name = "\x01snek_gc"]
pub unsafe fn snek_gc(curr_rbp: *const u64, curr_rsp: *const u64) -> *const u64 {
    let heap_ptr = *HEAP_START as *const u64;
    let stack_base = *HEAP_START.add(2) as *const u64;
    let stack_refs = root_set(stack_base, curr_rbp, curr_rsp);
    // 1. Mark
    for (_, heap_addr) in &stack_refs {
        mark(*heap_addr as *mut u64);
    }
    // 2. Forward headers
    forward_headers(heap_ptr);
    // 3. Forward stack and heap internal references
    for (stack_ptr, heap_addr) in stack_refs {
        *(stack_ptr as *mut u64) = *heap_addr | 1;
        forward_heap_refs(heap_addr as *mut u64);
    }
    // 4. Compact and return
    compact(heap_ptr)
}

// This function traverses the stack frames to gather the root set
unsafe fn root_set(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> Vec<(*const u64, *const u64)> {
    let mut curr_rbp = curr_rbp;
    let mut stack_ptr = curr_rsp;
    let mut set = vec![];
    while stack_base.offset_from(stack_ptr) > 0 {
        while curr_rbp.offset_from(stack_ptr) > 0 {
            match *stack_ptr {
                x if x & 3 == 1 && x != 1 => {
                    set.push((stack_ptr, (x - 1) as *const u64));
                }
                _ => (),
            }
            stack_ptr = stack_ptr.add(1);
        }
        curr_rbp = *stack_ptr as *const u64;
        // Skip rbp and ret ptr
        stack_ptr = stack_ptr.add(2);
    }
    set
}

// This function marks all references to heap given a starting heap value
unsafe fn mark(ele: *mut u64) {
    if *ele == 1 {
        return;
    }
    // Mark
    *ele = 1;
    // Iterate over data structure
    for i in 0..*ele.add(1) as usize {
        let entry = ele.add(i + 2) as *mut u64;
        if *entry & 3 == 1 && *entry != 1 {
            // Mark any references
            let next = (*entry - 1) as *mut u64;
            mark(next);
        }
    }
}

unsafe fn forward_headers(heap_ptr: *const u64) {
    let mut from = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    let mut to = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    while heap_ptr.offset_from(from) > 0 {
        // If from is marked as live
        if *from == 1 {
            // Fill src GC word with dst address
            *from = to as u64;
            to = to.add(*from.add(1) as usize + 2);
        }
        from = from.add(*from.add(1) as usize + 2);
    }
}

unsafe fn forward_heap_refs(heap_addr: *mut u64) {
    if *heap_addr & 1 != 0 {
        return;
    }
    *heap_addr |= 1;
    for i in 0..*heap_addr.add(1) as usize {
        let entry = heap_addr.add(i + 2) as *mut u64;
        if *entry & 3 == 1 && *entry != 1 {
            let old_entry = (*entry - 1) as *mut u64;
            *entry = *old_entry | 1;
            forward_heap_refs(old_entry);
        }
    }
}

unsafe fn compact(heap_ptr: *const u64) -> *const u64 {
    let mut ptr = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    let mut r15 = HEAP_START.add(HEAP_META_SIZE);
    while heap_ptr.offset_from(ptr) > 0 {
        // +2 to include metadata
        let len = *ptr.add(1) as usize + 2;
        // If GC word is zero, garbage, skip
        if *ptr == 0 {
            ptr = ptr.add(len);
            continue;
        }
        // ptr will always be tagged (through forward_heap_refs)
        // to be safe tag and remove
        let dst = ((*ptr | 1) - 1) as *mut u64;
        // No need to copy if dst is same as src
        if ptr.offset_from(dst) != 0 {
            for i in 1..len {
                *dst.add(i) = *ptr.add(i);
            }
        }
        // Clear GC word
        *dst = 0;
        ptr = ptr.add(len);
        r15 = dst.add(len);
    }
    r15
}

#[export_name = "\x01snek_print_heap"]
unsafe fn print_heap(heap_ptr: Option<*const u64>) {
    let heap_ptr = if heap_ptr.is_some() {
        heap_ptr.unwrap()
    } else {
        *HEAP_START as *const u64
    };
    println!("Heap: ");
    let mut ptr = HEAP_START;
    println!("{ptr:p}: {:#x} {:#x} {:#x}", *ptr, *ptr.add(1), *ptr.add(2));

    ptr = ptr.add(3);
    let mut next = ptr.add(*ptr.add(1) as usize + 2);
    print!("{ptr:p}: ");
    while heap_ptr.offset_from(ptr) > 0 {
        print!("{:#x} ", *ptr);
        ptr = ptr.add(1);

        if next.offset_from(ptr) <= 0 {
            println!("");
            next = ptr.add(*ptr.add(1) as usize + 2);
            print!("{ptr:p}: ");
        }
    }
    println!("End heap");
}

/// A helper function that can called with the `(snek-printstack)` snek function. It prints the stack
/// See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_print_stack"]
pub unsafe fn snek_print_stack(curr_rsp: *const u64) {
    let stack_base = *HEAP_START.add(2) as *const u64;
    let mut ptr = curr_rsp;
    println!("-----------------------------------------");
    while ptr <= stack_base {
        let val = *ptr;
        println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.add(1);
    }
    println!("-----------------------------------------");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() >= 2 { &args[1] } else { "false" };
    let input = parse_input(&input);
    let heap_size = if args.len() >= 3 {
        args[2].parse::<usize>().expect("heap size should be usize")
    } else {
        16384
    };
    let heap_len = HEAP_META_SIZE + heap_size;
    let mut heap = vec![0; heap_len];
    // Placeholder for offset
    heap[0] = unsafe { heap.as_mut_ptr().add(HEAP_META_SIZE) } as u64;
    // Placeholder for end of heap
    heap[1] = unsafe { heap.as_mut_ptr().add(heap_len) } as u64;
    // Placeholder for Rsp base
    heap[2] = 0;

    unsafe { HEAP_START = heap.as_ptr() };

    let i: i64 = unsafe { our_code_starts_here(input, HEAP_START as *mut u64) };

    print_result(i);
}
