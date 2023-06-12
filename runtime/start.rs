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
            i if i == 40 => "index out of bounds",
            i if i == 50 => "divide by zero",
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
        if !seen.insert(val) {
            return "[...]".to_string();
        }
        let addr = (val - 1) as *const i64;
        let count = unsafe { *addr.add(1) } as usize;
        let mut v: Vec<i64> = vec![0; count];
        for i in 0..count {
            v[i] = unsafe { *addr.add(i + 2) };
        }
        let result = format!(
            "[{}]",
            v.iter()
                .map(|x| snek_str(*x, seen))
                .collect::<Vec<String>>()
                .join(", ")
        );
        seen.remove(&val);
        return result;
    } else {
        format!("unknown value: {}", val)
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

enum StackIterType {
    ValIter,
    #[allow(dead_code)]
    RetPtrIter,
    EntryIter,
}

#[derive(Clone)]
struct Stack {
    init_rbp: *const u64,
    rsp_top: *const u64,
    rsp_base: *const u64,
}

impl Stack {
    unsafe fn new(init_rbp: *const u64, rsp_top: *const u64, rsp_base: *const u64) -> Stack {
        Stack {
            init_rbp,
            rsp_top,
            rsp_base,
        }
    }

    unsafe fn iter(&self) -> StackIter {
        StackIter::new(self, StackIterType::EntryIter)
    }
    unsafe fn iter_val(&self) -> StackIter {
        StackIter::new(self, StackIterType::ValIter)
    }
    #[allow(dead_code)]
    unsafe fn iter_retptr(&self) -> StackIter {
        StackIter::new(self, StackIterType::RetPtrIter)
    }
}

struct StackIter {
    stack: Stack,

    curr_rbp: *const u64,
    stack_ptr: *const u64,
    curr_rsp: *const u64,
    si: u64,
    iter_type: StackIterType,
}

impl StackIter {
    unsafe fn new(stack: &Stack, iter_type: StackIterType) -> StackIter {
        StackIter {
            stack: stack.clone(),

            curr_rbp: stack.init_rbp,
            stack_ptr: stack
                .rsp_top
                .add(if let StackIterType::ValIter = iter_type {
                    1
                } else {
                    0
                }),
            curr_rsp: stack.rsp_top,
            si: *stack.rsp_top,
            iter_type,
        }
    }

    unsafe fn next_val(&mut self) -> Option<(*const u64, u64)> {
        if self.stack_ptr.offset_from(self.curr_rsp) >= self.si as isize
        // || self.stack_ptr.offset_from(self.curr_rbp) <= 0
        {
            // Skip rbp and ret ptr
            self.curr_rsp = self.curr_rbp.add(2);
            // Skip si
            self.stack_ptr = self.curr_rbp.add(3);
            self.curr_rbp = *self.curr_rbp as *const u64;

            // Consume si
            self.si = *self.stack_ptr;
        }

        if self.stack.rsp_base.offset_from(self.stack_ptr) <= 0 {
            return None;
        }

        let val = (self.stack_ptr, *self.stack_ptr);
        self.stack_ptr = self.stack_ptr.add(1);
        Some(val)
    }

    #[allow(dead_code)]
    unsafe fn next_retptr(&mut self) -> Option<(*const u64, u64)> {
        // Ignore the final return pointer to main
        if self.curr_rbp.offset_from(self.stack.rsp_base) >= -1 {
            None
        } else {
            let ret_ptr = self.curr_rbp.add(1);
            let val = (ret_ptr, *ret_ptr);
            self.curr_rbp = *self.curr_rbp as *const u64;
            Some(val)
        }
    }

    unsafe fn next_entry(&mut self) -> Option<(*const u64, u64)> {
        if self.stack_ptr.offset_from(self.stack.rsp_base) > 0 {
            None
        } else {
            let val = (self.stack_ptr, *self.stack_ptr);
            self.stack_ptr = self.stack_ptr.add(1);
            Some(val)
        }
    }
}

impl Iterator for StackIter {
    type Item = (*const u64, u64);

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            match self.iter_type {
                StackIterType::ValIter => self.next_val(),
                StackIterType::RetPtrIter => self.next_retptr(),
                StackIterType::EntryIter => self.next_entry(),
            }
        }
    }
}

// This function traverses the stack frames to gather the root set
unsafe fn root_set(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> Vec<(*const u64, *const u64)> {
    let mut set = vec![];
    let stack = Stack::new(curr_rbp, curr_rsp, stack_base);
    let upper = *HEAP_START as *const u64;
    for (stack_ptr, val) in stack.iter_val() {
        if val & 3 == 1 && val != 1 {
            let ptr = val as *const u64;

            if ptr.offset_from(HEAP_START) > 0 && upper.offset_from(ptr) > 0 {
                set.push((stack_ptr, (val - 1) as *const u64));
            }
        }
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
    // Modify the heap pointer metadata
    *(HEAP_START as *mut u64) = r15 as u64;
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
    let stack = Stack::new(curr_rsp, curr_rsp, stack_base);
    println!("-----------------------------------------");
    for (ptr, val) in stack.iter() {
        println!("{ptr:?}: {val:#0x}");
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
