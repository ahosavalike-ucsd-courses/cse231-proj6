use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi};
use im::hashmap;
use im::HashMap;
use once_cell::sync::Lazy;
use sexp::*;
use std::io::Write;
use std::mem;
use std::panic;
use std::sync::{Mutex, MutexGuard};

use crate::compiler::*;
use crate::parser::*;
use crate::repl_helper::*;
use crate::structs::*;

static OPS: Lazy<Mutex<Assembler>> = Lazy::new(|| Mutex::new(Assembler::new().unwrap()));

fn eval(
    mut ops: MutexGuard<Assembler>,
    labels: &mut HashMap<Label, DynamicLabel>,
    instrs: &Vec<Instr>,
) -> i64 {
    let start = ops.offset();
    instrs_to_asm(&instrs, &mut ops, labels);
    dynasm!(ops; .arch x64; ret);

    ops.commit().unwrap();

    let jitted_fn: extern "C" fn() -> i64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(start)) }
    };
    // Drop before calling the function since it might try to acquire the lock as well.
    drop(ops);
    jitted_fn()
}

pub fn repl(eval_input: Option<(&Vec<Expr>, &Expr, &str)>) {
    // Initial define stack size low to see reallocations
    let mut define_stack: Vec<u64> = vec![0; 1];
    let mut heap: Vec<u64> = vec![0; 16384];

    // Placeholder for offset
    heap[0] = unsafe { heap.as_mut_ptr().offset(2) } as u64;
    // Placeholder for Rsp base
    heap[1] = 0;

    let mut co = Context::new(Some(define_stack.as_mut_ptr())).modify_si(1);
    let mut com = ContextMut::new();
    com.curr_heap_ptr = heap.as_mut_ptr() as i64;

    let mut labels: HashMap<Label, DynamicLabel> = hashmap! {};

    let mut input = (FALSE_VAL, Some(Type::Bool));

    // Eval
    if let Some((eval_fns, eval_in, input_str)) = eval_input {
        let mut ops = OPS.lock().unwrap();
        add_interface_calls(&mut ops, &mut labels, true);
        let mut instrs: Vec<Instr> = vec![Instr::Mov(MovArgs::ToReg(
            Reg::R15,
            Arg64::Imm64(heap.as_mut_ptr() as i64),
        ))];

        // Setup functions
        com.fns
            .extend(eval_fns.iter().fold(hashmap! {}, |mut acc, f| {
                if let Expr::FnDefn(n, v, _) = f {
                    if acc.get(n).is_some() {
                        panic!("function redefined")
                    }
                    acc.insert(
                        n.to_string(),
                        FunEnv::new(v.len() as i32, depth_aligned(f, 0)),
                    );
                    return acc;
                }
                // Should not happen, since we are catching it in parse
                panic!("Invalid: cannot compile anything other than function definitions here")
            }));
        labels.extend(com.fns.iter().fold(hashmap! {}, |mut acc, (f, _)| {
            acc.insert(
                Label::new(Some(&format!("fun_{f}"))),
                ops.new_dynamic_label(),
            );
            acc
        }));
        instrs_to_asm(
            &compile_func_defns(eval_fns, &mut com),
            &mut ops,
            &mut labels,
        );
        ops.commit().unwrap();

        // Setup input
        input = parse_input(input_str);
        instrs.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(input.0))));

        instrs.extend(compile_expr_aligned(
            eval_in,
            Some(&co),
            Some(&mut com),
            input.1,
        ));
        print_result(eval(ops, &mut labels, &mut instrs));
        return;
    }

    // REPL
    add_interface_calls(&mut OPS.lock().unwrap(), &mut labels, false);
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
            "print", "fun", "define", "nil", "list", "index", "+", "-", "*", "<", ">", ">=", "<=",
            "=", "==",
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
                    compile_expr_aligned(&e, Some(&co), Some(com_discard), input.1),
                    com_discard.result_type,
                ),
                Expr::FnDefn(f, args, _) => CompileResponse::FnDefn(
                    f.clone(),
                    args.clone(),
                    depth_aligned(&expr, 0),
                    compile_func_defns(&vec![expr], com_discard),
                ),
                _ => CompileResponse::Expr(compile_expr_aligned(
                    &expr,
                    Some(&co),
                    Some(com_discard),
                    input.1,
                )),
            }
        });

        // Eval with dynasm
        let mut ops = OPS.lock().unwrap();
        if let Ok(res) = res {
            let instrs = &mut match res {
                CompileResponse::Define(x, mut instrs, vtype) if x == "input" => {
                    input.1 = vtype;

                    // Move RAX to input.0
                    instrs.push(Instr::Mov(MovArgs::ToReg(
                        Reg::Rbx,
                        Arg64::Imm64(&input.0 as *const i64 as i64),
                    )));
                    instrs.push(Instr::Mov(MovArgs::ToMem(
                        MemRef {
                            reg: Reg::Rbx,
                            offset: 0,
                        },
                        Arg64::OReg(Reg::Rax),
                    )));
                    instrs
                }
                CompileResponse::Define(x, mut instrs, vtype) => {
                    // Update even if present, type might have changed
                    co.env.insert(
                        x.to_string(),
                        VarEnv::new(
                            if !co.env.contains_key(&x) {
                                // Increment heap index and extend heap if overflow
                                co.dsi += 1;
                                -co.dsi + 1
                            } else {
                                co.env.get(&x).unwrap().offset
                            },
                            vtype,
                            true,
                        ),
                    );

                    // Move RAX to define stack
                    instrs.push(Instr::Mov(MovArgs::ToReg(
                        Reg::Rbx,
                        Arg64::Imm64(co.get_define_stack()),
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
                    com.fns.insert(
                        f.clone(),
                        FunEnv {
                            argc: a.len() as i32,
                            depth,
                        },
                    );

                    instrs_to_asm(&instrs, &mut ops, &mut labels);
                    ops.commit().unwrap();
                    // Do not run any code
                    for i in &*instrs {
                        println!("{i:?}");
                    }
                    continue;
                }
                CompileResponse::Expr(instrs) => instrs,
            };

            // Set input to false for now
            instrs.insert(
                0,
                Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(input.0))),
            );
            // Add heap reference
            instrs.insert(
                0,
                Instr::Mov(MovArgs::ToReg(Reg::R15, Arg64::Imm64(com.curr_heap_ptr))),
            );

            for i in &*instrs {
                println!("{i:?}");
            }
            print_result(eval(ops, &mut labels, instrs));
        };

        // Increase define stack if needed
        if co.dsi as usize >= define_stack.len() {
            define_stack.resize(2 * define_stack.len(), 0);
            co.define_stack = Some(define_stack.as_mut_ptr());
        }
    }
}
