use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi, DynasmLabelApi};
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

fn add_interface_calls(ops: &mut Assembler, lbls: &mut HashMap<Label, DynamicLabel>, exit: bool) {
    let snek_error_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_error_stub")), snek_error_lbl);
    dynasm!(ops; .arch x64; =>snek_error_lbl);
    if exit {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_exit as _);
    } else {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_print as _);
    }
    dynasm!(ops; .arch x64; call rax; ret);

    let snek_print_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_print")), snek_print_lbl);
    dynasm!(ops; .arch x64; =>snek_print_lbl);
    dynasm!(ops; .arch x64; mov rax, QWORD print_result as _);
    dynasm!(ops; .arch x64; call rax; ret);
}

fn parse_input(input: &str) -> (i64, Type) {
    // parse the input string into internal value representation
    match input {
        "true" => (3, Type::Bool),
        "false" | "" => (1, Type::Bool),
        x => {
            let x = x.parse::<i64>().expect("Invalid") << 1;
            if x & 1 == 0 { (x, Type::Int) } else { (x, Type::Pair(None)) }
        },
    }
}

fn print_result(result: u64) -> u64 {
    if result % 2 == 0 {
        println!("{}", result as i64 / 2);
    } else if result == 1 {
        println!("false");
    } else if result == 3 {
        println!("true");
    } else {
        println!("Unknown format: {result}")
    }
    result
}

fn eval(
    ops: &mut Assembler,
    labels: &mut HashMap<Label, DynamicLabel>,
    instrs: &Vec<Instr>,
) -> u64 {
    let start = ops.offset();
    instrs_to_asm(&instrs, ops, labels);
    dynasm!(ops; .arch x64; ret);

    if let Err(e) = ops.commit() {
        panic!("error committing ops {e}");
    }

    let jitted_fn: extern "C" fn() -> u64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(start)) }
    };
    jitted_fn()
}

pub fn repl(eval_input: Option<(&Expr, &str)>) {
    // Initial heap size low to see reallocations
    let mut heap: Vec<u64> = vec![0; 1];

    let mut co = Context::new(Some(heap.as_mut_ptr())).modify_si(1);
    let mut com = ContextMut::new();

    let mut ops = Assembler::new().unwrap();
    let mut labels: HashMap<Label, DynamicLabel> = hashmap! {};

    // Eval
    if let Some((eval_in, input)) = eval_input {
        add_interface_calls(&mut ops, &mut labels, true);
        let mut instrs: Vec<Instr> = vec![];
        // Setup input
        let (input, is_bool) = parse_input(input);
        instrs.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(input))));

        instrs.extend(compile_expr_aligned(
            eval_in,
            Some(&co),
            Some(&mut com),
            Some(is_bool),
        ));
        print_result(eval(&mut ops, &mut labels, &instrs));
        return;
    }

    // REPL
    add_interface_calls(&mut ops, &mut labels, false);
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
        let keywords = &vec![
            "add1", "sub1", "let", "isnum", "isbool", "if", "loop", "break", "set!", "block",
            "input", "print", "fun", "define", "+", "-", "*", "<", ">", ">=", "<=", "=",
        ];
        for k in keywords {
            if line.starts_with(k) {
                line = format!("({line})");
                break;
            }
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

            // TODO: Check for usage of input, if so ask for input and validate

            match &expr {
                Expr::Define(x, e) => CompileResponse::Define(
                    x.clone(),
                    compile_expr_aligned(&e, Some(&co), Some(com_discard), None),
                    com_discard.result_type,
                ),
                Expr::FnDefn(f, args, _) => CompileResponse::FnDefn(
                    f.clone(),
                    args.clone(),
                    depth(&expr),
                    compile_func_defns(&vec![expr], com_discard),
                ),
                _ => CompileResponse::Expr(compile_expr_aligned(
                    &expr,
                    Some(&co),
                    Some(com_discard),
                    None,
                )),
            }
        });

        // Eval with dynasm
        if let Ok(res) = res {
            let instrs = &mut match res {
                CompileResponse::Define(x, mut instrs, is_bool) => {
                    // Update even if present, type might have changed
                    co.env.insert(
                        x.to_string(),
                        VarEnv::new(
                            if !co.env.contains_key(&x) {
                                // Increment heap index and extend heap if overflow
                                co.hi += 1;
                                -co.hi + 1
                            } else {
                                co.env.get(&x).unwrap().offset
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
                            offset: co.env.get(&x).unwrap().offset,
                        },
                        Arg64::OReg(Reg::Rax),
                    )));
                    instrs
                }
                CompileResponse::FnDefn(f, a, depth, instrs) => {
                    com.fns.insert(f.clone(), FunEnv { argc: a.len() as i32, depth });
                    labels.insert(Label::new(Some(&format!("fun_{f}"))), ops.new_dynamic_label());
                    
                    // dynasm!(ops; .arch x64; => fun_lbl);
                    instrs_to_asm(&instrs, &mut ops, &mut labels);
                    if let Err(e) = ops.commit() {
                        println!("{e}");
                    }
                    // Do not run any code
                    for i in &*instrs {
                        println!("{i:?}");
                    }
                    continue;
                },
                CompileResponse::Expr(instrs) => instrs,
            };

            for i in &*instrs {
                println!("{i:?}");
            }

            print_result(eval(&mut ops, &mut labels, instrs));
        };

        // Increase heap if needed
        if co.hi as usize >= heap.len() {
            heap.resize(2 * heap.len(), 0);
            co.heap = Some(heap.as_mut_ptr());
        }
    }
}
