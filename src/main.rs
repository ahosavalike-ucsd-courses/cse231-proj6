use sexp::*;
use std::env;
use std::fs::File;
use std::io::prelude::*;

mod parser;
use parser::*;
mod compiler;
use compiler::*;
mod repl;
use repl::*;
mod structs;
use structs::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args[1] == "-i" {
        return Ok(repl(None));
    }

    let in_name = if args[1] == "-e" { &args[2] } else { &args[1] };

    let out_name = &args[2];

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let expr =
        parse_top_level(&parse(&format!("(\n{in_contents}\n)").to_string()).expect("Invalid"));

    if args[1] == "-e" {
        return Ok(repl(Some((
            &expr,
            if args.len() > 3 { &args[3] } else { "false" },
        ))));
    }

    let result = compile_expr_with_unknown_input(&expr, &Context::new(None));

    let asm_program = format!(
        "section .text
extern snek_error
extern snek_print
global our_code_starts_here
our_code_starts_here:
{}
ret
",
        instrs_to_string(&result)
    );

    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;
    // repl(Some(&result));

    Ok(())
}
