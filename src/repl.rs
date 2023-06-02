use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi, DynasmLabelApi};
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
static COM: Lazy<Mutex<ContextMut>> = Lazy::new(|| Mutex::new(ContextMut::new()));
static FUNCTIONS: Lazy<Mutex<HashMap<u64, FunDefEnv>>> = Lazy::new(|| Mutex::new(hashmap! {}));
static LABELS: Lazy<Mutex<HashMap<Label, DynamicLabel>>> = Lazy::new(|| Mutex::new(hashmap! {}));
static FUNCTION_INDEX: Mutex<u64> = Mutex::new(0);
pub static mut HEAP_START: *const u64 = std::ptr::null();
pub static HEAP_META_SIZE: usize = 3;

// Compiles the initial stub for the function
fn function_compile_initial(
    ops: &mut Assembler,
    com: &mut ContextMut,
    labels: &mut HashMap<Label, DynamicLabel>,
    f: String,
    argc: i32,
    defn: Expr,
) {
    let depth = depth_aligned(&defn, 0);
    com.fns.insert(f.clone(), FunEnv { argc: argc, depth });

    let fi;
    // Get unique index
    {
        let mut gfi = FUNCTION_INDEX.lock().unwrap();
        fi = gfi.clone();
        *gfi += 1;
    }

    // Generate and store the dynamic labels for all three functions
    let stub = ops.new_dynamic_label();
    let fast = ops.new_dynamic_label();
    let slow = ops.new_dynamic_label();
    labels.insert(Label::new(Some(&format!("fun_{f}"))), stub);
    labels.insert(Label::new(Some(&format!("fnf_{f}"))), fast);
    labels.insert(Label::new(Some(&format!("fns_{f}"))), slow);
    FUNCTIONS
        .lock()
        .unwrap()
        .insert(fi, FunDefEnv::new(f, ops.offset(), defn, depth, argc));

    // Set up call into Rust for dynamic compile
    dynasm!(ops
        ; .arch x64
        ; => stub
        ; push rbp
        ; sub rsp, depth * 8
        ; mov rax, QWORD function_compile_runtime as _
        ; mov rdi, QWORD fi as i64
        ; mov rsi, rsp
        ; call rax
        ; add rsp, depth * 8
        ; pop rbp
        // since fast is still not compiled, first time will have to go through the modified stub
        ; jmp =>stub
        // should not happen!
        ; ret
    );
}

// Compiles the slow and fast versions of the function
extern "C" fn function_compile_runtime(fi: u64, stack: u64) {
    let f = FUNCTIONS.lock().unwrap().get(&fi).unwrap().clone();
    let mut arg_types = Vec::with_capacity(f.argc as usize);

    // Go through stack to get the arg types
    let stack = stack as *const i64;
    for i in 0..f.argc {
        let arg = unsafe { *stack.offset(i as isize + 1) }; // Ignore the top word of stack
        arg_types.push(if arg == FALSE_VAL || arg == TRUE_VAL {
            Type::Bool
        } else if arg & 1 == 0 {
            Type::Int
        } else if arg & 3 == 1 {
            Type::List
        } else {
            panic!("Unknown type");
        });
    }

    // Assemble the functions
    asm_repl_func_defn(
        &mut OPS.lock().unwrap(),
        &mut COM.lock().unwrap(),
        &mut LABELS.lock().unwrap(),
        &f,
        &arg_types,
    );
}

fn eval(
    mut ops: MutexGuard<Assembler>,
    com: MutexGuard<ContextMut>,
    labels: MutexGuard<HashMap<Label, DynamicLabel>>,
    instrs: &Vec<Instr>,
) -> i64 {
    let start = ops.offset();
    instrs_to_asm(&instrs, &mut ops, &labels);
    dynasm!(ops; .arch x64; ret);

    ops.commit().unwrap();

    let jitted_fn: extern "C" fn() -> i64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(start)) }
    };
    // Drop before calling the function since it might try to acquire the lock as well.
    drop(ops);
    drop(com);
    drop(labels);
    jitted_fn()
}

pub fn repl(eval_input: Option<(&Vec<Expr>, &Expr, &str)>, heap_size: Option<usize>) {
    // Initial define stack size low to see reallocations
    let mut define_stack: Vec<u64> = vec![0; 1];

    let heap_size = heap_size.unwrap_or(16384);
    let heap_len = HEAP_META_SIZE + heap_size;
    let mut heap = vec![0; heap_len];
    // Placeholder for offset
    heap[0] = unsafe { heap.as_mut_ptr().add(HEAP_META_SIZE) } as u64;
    // Placeholder for end of heap
    heap[1] = unsafe { heap.as_mut_ptr().add(heap_len) } as u64;
    // Placeholder for Rsp base
    heap[2] = 0;

    unsafe { HEAP_START = heap.as_ptr() };

    // 1 word for stack usage, 1 word for input
    let mut co = Context::new(Some(define_stack.as_mut_ptr())).modify_si(2);

    let mut input = (FALSE_VAL, Some(Type::Bool));

    // Eval
    if let Some((eval_fns, eval_in, input_str)) = eval_input {
        let mut ops = OPS.lock().unwrap();
        let mut com = COM.lock().unwrap();
        let mut labels = LABELS.lock().unwrap();

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
        for expr in eval_fns {
            if let Expr::FnDefn(f, args, _) = expr {
                function_compile_initial(
                    &mut ops,
                    &mut com,
                    &mut labels,
                    f.clone(),
                    args.len() as i32,
                    expr.clone(),
                );
            } else {
                panic!("Not a function");
            }
        }

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
        print_result(eval(ops, com, labels, &mut instrs));
        return;
    }

    // REPL
    add_interface_calls(&mut OPS.lock().unwrap(), &mut LABELS.lock().unwrap(), false);
    let mut line = String::new();
    loop {
        // println!("{co:?}");
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
            "add1", "sub1", "let", "isnum", "isbool", "islist", "if", "loop", "break", "set!",
            "block", "print", "fun", "define", "nil", "list", "index", "slist", "len", "+", "-",
            "*", "/", "<", ">", ">=", "<=", "=", "==",
        ];
        for k in keywords {
            if line.starts_with(k) {
                line = format!("({line})");
                break;
            }
        }

        let mut com = COM.lock().unwrap();
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
                Expr::FnDefn(f, args, _) => {
                    CompileResponse::FnDefn(f.clone(), expr.clone(), args.len() as i32)
                }
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
        let mut labels = LABELS.lock().unwrap();
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
                CompileResponse::FnDefn(f, defn, argc) => {
                    function_compile_initial(&mut ops, &mut com, &mut labels, f, argc, defn);
                    ops.commit().unwrap();
                    continue;
                }
                CompileResponse::Expr(instrs) => instrs,
            };

            // Set input from define for now
            instrs.insert(
                0,
                Instr::Mov(MovArgs::ToReg(Reg::Rdi, Arg64::Imm64(input.0))),
            );
            // Add heap reference
            instrs.insert(
                0,
                Instr::Mov(MovArgs::ToReg(
                    Reg::R15,
                    Arg64::Imm64(unsafe { HEAP_START } as i64),
                )),
            );

            print_result(eval(ops, com, labels, instrs));
        };

        // Increase define stack if needed
        if co.dsi as usize >= define_stack.len() {
            define_stack.resize(2 * define_stack.len(), 0);
            co.define_stack = Some(define_stack.as_mut_ptr());
        }
    }
}
