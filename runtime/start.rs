use std::collections::HashSet;
use std::env;

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
    let lc = unsafe { *la } as isize;
    let rc = unsafe { *ra } as isize;
    // Check length
    if lc != rc {
        return false;
    }
    for i in 1..=lc {
        let ln = unsafe { *la.offset(i) };
        let rn = unsafe { *ra.offset(i) };
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
        let count = unsafe { *addr } as usize;
        let mut v: Vec<i64> = vec![0; count];
        for i in 1..=count {
            v[i - 1] = unsafe { *addr.offset(i as isize) };
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

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);
    let mut heap = vec![0; 16384];
    // Placeholder for offset
    heap[0] = unsafe { heap.as_mut_ptr().offset(2) } as u64;
    // Placeholder for Rsp base
    heap[1] = 0;

    let i: i64 = unsafe { our_code_starts_here(input, heap.as_mut_ptr()) };
    
    print_result(i);
}
