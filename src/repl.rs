use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::hashmap;
use im::HashMap;
use im::HashSet;
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
            i if i == 40 => "index out of range",
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
            i if i == 40 => "index out of range",
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
        // TODO: Make it gracefully handle being called from inside functions.
        // Rsp will be incorrect after this returns
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_print as _);
    }
    dynasm!(ops;
        .arch x64;
        mov rsp, r14 ;
        pop r14;
        call rax;
        ret
    );

    let snek_print_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_print")), snek_print_lbl);
    dynasm!(ops; .arch x64; =>snek_print_lbl; mov rax, QWORD print_result as _; call rax; ret);

    let snek_deep_equal_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_deep_equal")), snek_deep_equal_lbl);
    dynasm!(ops; .arch x64; =>snek_deep_equal_lbl; mov rax, QWORD deep_equal as _; call rax; ret);
}

fn parse_input(input: &str) -> (i64, Type) {
    // parse the input string into internal value representation
    match input {
        "true" => (TRUE_VAL, Type::Bool),
        "false" | "" => (FALSE_VAL, Type::Bool),
        x => {
            let x = x.parse::<i64>().expect("Invalid") << 1;
            if x & 1 == 0 {
                (x, Type::Int)
            } else {
                (x, Type::List)
            }
        }
    }
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

fn deep_equal(l: i64, r: i64) -> i64 {
    if deep_equal_recurse(l, r, &mut HashSet::new()) {
        TRUE_VAL
    } else {
        FALSE_VAL
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
    } else if val & 3 == 1 {
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

fn print_result(result: i64) -> i64 {
    println!("{}", snek_str(result, &mut HashSet::new()));
    return result;
}

fn eval(
    ops: &mut Assembler,
    labels: &mut HashMap<Label, DynamicLabel>,
    instrs: &Vec<Instr>,
) -> i64 {
    let start = ops.offset();
    instrs_to_asm(&instrs, ops, labels);
    dynasm!(ops; .arch x64; ret);

    if let Err(e) = ops.commit() {
        panic!("error committing ops {e}");
    }

    let jitted_fn: extern "C" fn() -> i64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(start)) }
    };
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

    let mut ops = Assembler::new().unwrap();
    let mut labels: HashMap<Label, DynamicLabel> = hashmap! {};

    // Eval
    if let Some((eval_fns, eval_in, input)) = eval_input {
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
        if let Err(e) = ops.commit() {
            println!("{e}");
        }

        // Setup input
        let (input, is_bool) = parse_input(input);
        instrs.push(Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(input))));

        instrs.extend(compile_expr_aligned(
            eval_in,
            Some(&co),
            Some(&mut com),
            Some(is_bool),
        ));
        print_result(eval(&mut ops, &mut labels, &mut instrs));
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
            "input", "print", "fun", "define", "nil", "list", "index", "+", "-", "*", "<", ">",
            ">=", "<=", "=", "==",
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
                    depth_aligned(&expr, 0),
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
                                co.dsi += 1;
                                -co.dsi + 1
                            } else {
                                co.env.get(&x).unwrap().offset
                            },
                            is_bool,
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
                    if let Err(e) = ops.commit() {
                        println!("{e}");
                    }
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
                Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(FALSE_VAL))),
            );
            // Add heap reference
            instrs.insert(
                0,
                Instr::Mov(MovArgs::ToReg(Reg::R15, Arg64::Imm64(com.curr_heap_ptr))),
            );

            for i in &*instrs {
                println!("{i:?}");
            }
            print_result(eval(&mut ops, &mut labels, instrs));
        };

        // Increase define stack if needed
        if co.dsi as usize >= define_stack.len() {
            define_stack.resize(2 * define_stack.len(), 0);
            co.define_stack = Some(define_stack.as_mut_ptr());
        }
    }
}
