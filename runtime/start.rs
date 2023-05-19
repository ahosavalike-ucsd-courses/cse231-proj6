use std::collections::HashSet;
use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, buffer: *mut u64) -> i64;
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

fn parse_input(input: &str) -> u64 {
    // parse the input string into internal value representation
    match input {
        "true" => 7,
        "false" | "" => 3,
        _ => (input.parse::<i64>().expect("Invalid") as u64) << 1,
    }
}

fn snek_str(val: i64, seen: &mut HashSet<i64>) -> String {
    if val == 7 {
        "true".to_string()
    } else if val == 3 {
        "false".to_string()
    } else if val % 2 == 0 {
        format!("{}", val >> 1)
    } else if val == 1 {
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
    let mut mem = vec![0; 16384];

    let i: i64 = unsafe { our_code_starts_here(input, mem.as_mut_ptr()) };
    print_result(i);
}
