use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // print error message according to writeup
    eprintln!("an error ocurred {errcode}: {}", match errcode {
        1 => "invalid representation",
        i if i >= 20 && i <= 29  => "invalid argument",
        i if i >= 30 && i <= 39 => "overflow",
        _ => "",
    });
    std::process::exit(1);
}

fn parse_input(input: &str) -> u64 {
    // parse the input string into internal value representation
    match input {
        "true" => 3,
        "false" | "" => 1,
        _ => (input.parse::<i64>().expect("Invalid") as u64) << 1,
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);

    let i: u64 = unsafe { our_code_starts_here(input) };
    if i % 2 == 0 {
        println!("{}", i as i64 / 2);
    } else if i == 1 {
        println!("false");
    } else if i == 3 {
        println!("true");
    } else {
        snek_error(1);
    }
}
