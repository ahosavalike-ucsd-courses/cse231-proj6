use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::hashmap;
use im::HashMap;
use sexp::*;
use std::io::Write;
use std::mem;
use std::panic;

use crate::compiler::*;
use crate::parser::*;
use crate::structs::*;

fn snek_error_exit(errcode: i64) {
    // print error message according to writeup
    eprintln!(
        "an error ocurred {errcode}: {}",
        match errcode {
            1 => "invalid representation",
            i if i >= 20 && i <= 29 => "invalid argument",
            i if i >= 30 && i <= 39 => "overflow",
            _ => "",
        }
    );
    std::process::exit(1);
}

fn snek_error_print(errcode: i64) {
    // print error message according to writeup
    eprintln!(
        "an error ocurred {errcode}: {}",
        match errcode {
            1 => "invalid representation",
            i if i >= 20 && i <= 29 => "invalid argument",
            i if i >= 30 && i <= 39 => "overflow",
            _ => "",
        }
    );
}

fn add_interface_calls(
    ops: &mut dynasmrt::x64::Assembler,
    lbls: &mut HashMap<Label, DynamicLabel>,
    exit: bool,
) {
    let snek_error_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_error")), snek_error_lbl);
    dynasm!(ops; .arch x64; =>snek_error_lbl);
    if exit {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_exit as _);
    } else {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_print as _);
    }
    dynasm!(ops; .arch x64; call rax; ret);
}

fn parse_input(input: &str) -> (i64, bool) {
    // parse the input string into internal value representation
    match input {
        "true" => (3, true),
        "false" | "" => (1, true),
        _ => (input.parse::<i64>().expect("Invalid") << 1, false),
    }
}

fn print_result(result: i64) {
    if result % 2 == 0 {
        println!("{}", result as i64 / 2);
    } else if result == 1 {
        println!("false");
    } else if result == 3 {
        println!("true");
    } else {
        println!("Unknown format: {result}")
    }
}

fn eval(instrs: &Vec<Instr>, exit: bool) -> i64 {
    let mut ops = dynasmrt::x64::Assembler::new().unwrap();
    let mut labels: im::HashMap<Label, dynasmrt::DynamicLabel> = hashmap! {};
    add_interface_calls(&mut ops, &mut labels, exit);

    let start = ops.offset();
    instrs_to_asm(&instrs, &mut ops, &mut labels);
    dynasm!(ops; .arch x64; ret);
    let buf = ops.finalize().unwrap();
    let jitted_fn: extern "C" fn() -> i64 = unsafe { mem::transmute(buf.ptr(start)) };
    jitted_fn()
}

pub fn repl(eval_input: Option<(&Expr, &str)>) {
    // Initial heap size low to see reallocations
    let mut heap: Vec<u64> = vec![0; 1];

    let mut co = Context::new(Some(heap.as_mut_ptr()));
    let mut com = ContextMut::new();

    // Eval
    if let Some((eval_in, input)) = eval_input {
        let mut instrs: Vec<Instr> = vec![];
        // Setup input
        let (input, is_bool) = parse_input(input);
        co.env.insert(
            "input".to_string(),
            VarEnv {
                offset: co.si,
                is_bool: Some(is_bool),
                in_heap: false,
            },
        );
        instrs.push(Instr::Mov(MovArgs::ToMem(
            MemRef {
                reg: Reg::Rsp,
                offset: co.si,
            },
            Arg64::Imm64(input),
        )));
        co.si += 1;
        instrs.extend(compile_expr(eval_in, &co, &mut com));
        return print_result(eval(&instrs, true));
    }

    // REPL
    let mut line = String::new();
    loop {
        println!("{co:?}");
        // println!("{com:?}");
        line.clear();
        print!("> ");
        _ = std::io::stdout().flush();
        _ = std::io::stdin().read_line(&mut line);

        // Check empty input (Ctrl+D)
        if line == "" {
            break;
        }

        line = line.trim().to_string();

        // Check empty input
        if line == "" {
            continue;
        }

        // Add top level list
        if line.starts_with("let")
            || line.starts_with("define")
            || line.starts_with("add1")
            || line.starts_with("sub1")
            || line.starts_with("isnum")
            || line.starts_with("isbool")
            || line.starts_with("+")
            || line.starts_with("*")
            || line.starts_with("-")
            || line.starts_with("set!")
            || line.starts_with("loop")
            || line.starts_with("if")
            || line.starts_with("block")
        {
            line = format!("({line})");
        }

        // Parse and Compile, check for panic
        let res = panic::catch_unwind(|| {
            let com_discard = &mut com.clone();
            let expr = parse_expr(match &parse(&line) {
                Ok(r) => r,
                Err(e) => {
                    panic!("Error parsing input: {e}")
                }
            });
            if let Expr::Define(x, e) = expr {
                (
                    Some(x),
                    compile_expr(&e, &co, com_discard),
                    com_discard.result_is_bool,
                )
            } else {
                (
                    None,
                    compile_expr(&expr, &co, com_discard),
                    com_discard.result_is_bool,
                )
            }
        });

        // Eval with dynasm
        if let Ok((var, mut instrs, is_bool)) = res {
            // Check if this was a defined variable
            if let Some(x) = &var {
                // Update even if present, type might have changed
                co.env.insert(
                    x.to_string(),
                    VarEnv::new(
                        if !co.env.contains_key(x) {
                            // Increment heap index and extend heap if overflow
                            co.hi += 1;
                            -co.hi+1
                        } else {
                            co.env.get(x).unwrap().offset
                        },
                        is_bool,
                        true,
                    ),
                );

                // Move RAX to heap
                instrs.push(Instr::Mov(MovArgs::ToReg(
                    Reg::Rbx,
                    Arg64::Imm64(co.get_heap()),
                )));
                instrs.push(Instr::Mov(MovArgs::ToMem(
                    MemRef {
                        reg: Reg::Rbx,
                        offset: co.env.get(x).unwrap().offset,
                    },
                    Arg64::OReg(Reg::Rax),
                )));
            }

            for i in &instrs {println!("{i:?}");}

            print_result(eval(&instrs, false));
        };

        // Increase heap if needed
        if co.hi as usize >= heap.len() {
            heap.resize(2 * heap.len(), 0);
            co.heap = Some(heap.as_mut_ptr());
        }
    }
}
