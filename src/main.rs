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
mod repl_helper;
mod structs;
use structs::*;
mod optimize;
use optimize::*;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args[1] == "-i" {
        return Ok(repl(None, if args.len() > 2 {
            Some(args[2].parse::<usize>().expect("heap size should be usize"))
        } else {
            None
        }));
    }

    let in_name = if args[1] == "-e" { &args[2] } else { &args[1] };

    let mut in_file = File::open(in_name)?;
    let mut in_contents = String::new();
    in_file.read_to_string(&mut in_contents)?;

    let (funcs, expr) =
        parse_top_level(&parse(&format!("(\n{in_contents}\n)").to_string()).expect("Invalid"));

    if args[1] == "-e" {
        return Ok(repl(
            Some((
                &funcs,
                &expr,
                if args.len() > 3 { &args[3] } else { "false" },
            )),
            if args.len() > 4 {
                Some(args[4].parse::<usize>().expect("heap size should be usize"))
            } else {
                None
            },
        ));
    }

    let com = &mut ContextMut::new();
    let funcs = funcs.iter().map(optimize).collect();
    let funcs = compile_func_defns(&funcs, com);
    let expr = optimize(&expr);
    let result = compile_expr_aligned(&expr, None, Some(com), None);

    let asm_program = format!(
        "section .text
extern snek_error
extern snek_print
extern snek_deep_equal
extern snek_try_gc
extern snek_minor_gc
extern snek_major_gc
extern snek_write_barrier
global our_code_starts_here
snek_error_stub:
 mov rsp, [r15 + 16] ; 3nd word in heap
 pop rbp  ; Restore Rbp
 push rbp ; Align stack
 call snek_error
 ret

; Function Definitions
{}

; Main code
our_code_starts_here:
 mov r15, rsi   ; 2nd argument is available heap space address
{}
ret
",
        instrs_to_string(&funcs),
        instrs_to_string(&result)
    );

    let out_name = &args[2];
    let mut out_file = File::create(out_name)?;
    out_file.write_all(asm_program.as_bytes())?;

    Ok(())
}
