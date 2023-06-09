use crate::structs::*;
use Arg64::*;
use Instr::*;
use MovArgs::*;
use Reg::*;
use Type::*;

use dynasmrt::DynamicLabel;

use im::{HashSet, hashset};
use im::{hashmap, HashMap};

pub const TRUE_VAL: i64 = 7;
pub const FALSE_VAL: i64 = 3;
pub const NIL_VAL: i64 = 1;

pub const TRUE: Arg64 = Imm(TRUE_VAL as i32);
pub const FALSE: Arg64 = Imm(FALSE_VAL as i32);
pub const NIL: Arg64 = Imm(NIL_VAL as i32);

fn nonovars(e: &Expr, acc: &HashSet<String>) -> HashSet<String> {
    let svm = |e| nonovars(e, acc);
    let svf = |a: HashSet<String>, e: &Expr| a.union(nonovars(e, acc));
    match e {
        Expr::Set(x, _) => acc.update(x.clone()),
        Expr::List(es) => es.iter().fold(acc.clone(), svf),
        Expr::UnOp(_, e) | Expr::Loop(e) | Expr::Break(e) | Expr::FnDefn(_, _, e) => svm(e),
        Expr::BinOp(_, e1, e2) | Expr::SizedList(e1, e2) => svf(svf(acc.clone(), e1), e2),
        Expr::If(e1, e2, e3) | Expr::SetLst(e1, e2, e3) => svf(svf(svf(acc.clone(), e1), e2), e3),
        Expr::Let(es, e) => svf(es.iter().map(|(_, e)| e).fold(acc.clone(), svf), e),
        Expr::Block(es) | Expr::FnCall(_, es) => es.iter().fold(acc.clone(), svf),
        _ => acc.clone(),
    }
}

fn depth(e: &Expr) -> i32 {
    match e {
        Expr::Nil => 0,
        Expr::Num(_) => 0,
        Expr::Var(_) => 0,
        Expr::Boolean(_) => 0,
        Expr::UnOp(_, e) => depth(e),
        // Right to left evaluation order
        Expr::BinOp(_, e1, e2) => depth(e1).max(depth(e2) + 1),
        Expr::Let(bindings, e) => bindings
            .iter()
            .enumerate()
            .map(|(i, (_, e))| i as i32 + depth(e))
            .max()
            .unwrap_or(0)
            .max(bindings.len() as i32 + depth(e)),
        Expr::List(es) => es
            .iter()
            .enumerate()
            .map(|(i, e)| i as i32 + depth(e))
            .max()
            .unwrap_or(0)
            .max(es.len() as i32),
        Expr::SizedList(c, v) => depth(c).max(depth(v) + 1).max(2),
        Expr::If(cond, then, other) => depth(cond).max(depth(then)).max(depth(other)),
        Expr::Loop(e) => depth(e),
        Expr::Block(es) => es.iter().map(|expr| depth(expr)).max().unwrap_or(0),
        Expr::Break(e) => depth(e),
        Expr::Set(_, e) => depth(e),
        Expr::SetLst(lst, idx, val) => depth(lst).max(1 + depth(idx)).max(2 + depth(val)),
        Expr::Define(_, e) => depth(e),
        Expr::FnDefn(_, v, b) => depth_aligned(b, v.len() as i32 + 1), // 1 extra for storing co.si
        Expr::FnCall(_, args) => args
            .iter()
            .enumerate()
            .map(|(i, e)| i as i32 + depth(e))
            .max()
            .unwrap_or(0)
            .max(args.len() as i32),
    }
}

pub fn depth_aligned(e: &Expr, extra: i32) -> i32 {
    // Aligned 16byte depth
    let d = depth(e) + extra;
    if d % 2 == 0 {
        d
    } else {
        d + 1
    }
}

pub fn compile_func_defns(fns: &Vec<Expr>, com: &mut ContextMut) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = vec![];

    // Preprocess all function definitions
    com.fns.extend(fns.iter().fold(hashmap! {}, |mut acc, f| {
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

    for f in fns {
        // No else block as we checked and paniced in preprocessing
        if let Expr::FnDefn(name, vars, body) = f {
            com.depth = com.fns.get_mut(name).unwrap().depth;
            // Separate context for each function definiton
            let mut co = Context::new(None)
                // 1 + to skip the top word
                .modify_si(1 + vars.len() as i32)
                // Function body is tail position
                .modify_tail(true);

            for (i, v) in vars.iter().enumerate() {
                let existing = co.env.get(v.as_str());
                if existing.is_some() && !existing.unwrap().defined {
                    panic!("duplicate parameter binding in definition");
                }
                // Top word is for co.si, so i+1
                co.env
                    .insert(v.to_string(), VarEnv::new(i as i32 + 1, None, false));
            }

            instrs.push(LabelI(Label::new(Some(&format!("fun_{name}")))));
            instrs.extend(vec![
                Push(Rbp),
                Mov(ToReg(Rbp, OReg(Rsp))),
                Sub(ToReg(Rsp, Imm(com.depth * 8))),
            ]);
            instrs.extend(compile_expr(body, &co, com));
            instrs.extend(vec![Add(ToReg(Rsp, Imm(com.depth * 8))), Pop(Rbp), Ret]);
        }
    }
    return instrs;
}

pub fn compile_expr_aligned(
    e: &Expr,
    co: Option<&Context>,
    com: Option<&mut ContextMut>,
    input: Option<Type>,
) -> Vec<Instr> {
    // Top level is not a tail position
    // Top word in stackframe is for si, next for input
    let mut co_ = &Context::new(None).modify_si(2);
    if let Some(x) = co {
        co_ = x;
    };
    let co = co_;
    let mut com_ = &mut ContextMut::new();
    if let Some(x) = com {
        com_ = x;
    }
    let com = com_;

    com.depth = depth_aligned(e, 2); // 1 extra for input, 1 extra for storing co.si

    let mut instrs: Vec<Instr> = vec![
        // Heap's second word saves the initial RSP, used to restore on runtime error
        Push(Rbp),
        Mov(ToMem(
            MemRef {
                reg: R15,
                offset: 2,
            },
            OReg(Rsp),
        )),
        Mov(ToReg(Rbp, OReg(Rsp))),
        Sub(ToReg(Rsp, Imm(com.depth * 8))),
        Mov(ToMem(
            MemRef {
                reg: Rsp,
                offset: 1,
            },
            OReg(Rdi),
        )),
    ];
    instrs.extend(compile_expr(
        e,
        &co.modify_env(
            co.env
                .update("input".to_string(), VarEnv::new(1, input, false)),
        ),
        com,
    ));
    instrs.extend(vec![Add(ToReg(Rsp, Imm(com.depth * 8))), Pop(Rbp)]);
    return instrs;
}

pub fn compile_expr(e: &Expr, co: &Context, com: &mut ContextMut) -> Vec<Instr> {
    let mut instrs: Vec<Instr> = vec![];
    let snek_error: Label = Label::new(Some("snek_error_stub"));

    match e {
        Expr::Nil => {
            instrs.push(Mov(co.src_to_target(NIL)));

            com.result_type = Some(List);
        }
        Expr::Num(n) => {
            let (i, overflow) = n.overflowing_mul(2);
            if overflow {
                panic!("Invalid");
            }

            if let Ok(n) = i32::try_from(i) {
                instrs.push(Mov(co.src_to_target(Imm(n))));
            } else {
                instrs.push(Mov(ToReg(Rax, Imm64(i))));
                co.rax_to_target(&mut instrs);
            }

            com.result_type = Some(Int);
        }
        Expr::Boolean(b) => {
            let res = match b {
                true => TRUE,
                false => FALSE,
            };
            instrs.push(Mov(co.src_to_target(res)));
            com.result_type = Some(Bool);
        }
        Expr::Var(x) => {
            // Get variable's VarEnv
            let venv = match if co.env.contains_key(x) {
                &co.env
            } else {
                &com.env
            }
            .get(x)
            {
                Some(o) => o,
                None => panic!("Unbound variable identifier {x}"),
            };

            let reg = if venv.defined {
                instrs.push(Mov(ToReg(Rbx, Imm64(co.get_define_stack()))));
                Rbx
            } else {
                Rsp
            };
            instrs.push(Mov(ToReg(
                Rax,
                Mem(MemRef {
                    reg,
                    offset: venv.offset,
                }),
            )));
            co.rax_to_target(&mut instrs);
            com.result_type = venv.vtype;
        }
        Expr::UnOp(op, subexpr) => {
            // UnOp operand cannot be a tail position
            let co = &co.modify_tail(false);
            instrs.extend(compile_expr(subexpr, &co.modify_target(None), com));

            match op {
                Op1::Add1 => {
                    // Check if Rax is number
                    match com.result_type {
                        None => {
                            instrs.push(Test(ToReg(Rax, Imm(1))));
                            instrs.push(Mov(ToReg(Rdi, Imm(20)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        }
                        Some(Int) => {}
                        _ => {
                            instrs.push(Mov(ToReg(Rdi, Imm(20)))); // invalid argument
                            instrs.push(JumpI(Jump::U(snek_error.clone())));
                            com.result_type = Some(Int);
                            return instrs;
                        }
                    }
                    instrs.push(Add(ToReg(Rax, Imm(2))));
                    // Check overflow
                    instrs.push(Mov(ToReg(Rdi, Imm(30))));
                    instrs.push(JumpI(Jump::O(snek_error)));
                    com.result_type = Some(Int);
                }
                Op1::Sub1 => {
                    // Check if Rax is number
                    match com.result_type {
                        None => {
                            instrs.push(Test(ToReg(Rax, Imm(1))));
                            instrs.push(Mov(ToReg(Rdi, Imm(20)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        }
                        Some(Int) => {}
                        _ => {
                            instrs.push(Mov(ToReg(Rdi, Imm(20)))); // invalid argument
                            instrs.push(JumpI(Jump::U(snek_error.clone())));
                            com.result_type = Some(Int);
                            return instrs;
                        }
                    }
                    instrs.push(Sub(ToReg(Rax, Imm(2))));
                    // Check overflow
                    instrs.push(Mov(ToReg(Rdi, Imm(30))));
                    instrs.push(JumpI(Jump::O(snek_error)));
                    com.result_type = Some(Int);
                }
                Op1::IsBool => {
                    match com.result_type {
                        Some(Bool) => {
                            instrs.push(Mov(ToReg(Rax, TRUE)));
                        }
                        Some(_) => {
                            instrs.push(Mov(ToReg(Rax, FALSE)));
                        }
                        None => {
                            instrs.push(And(ToReg(Rax, Imm(3))));
                            instrs.push(Cmp(ToReg(Rax, Imm(3))));
                            instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                            instrs.push(Mov(ToReg(Rbx, TRUE)));
                            instrs.push(CMovI(CMov::E(Rax, OReg(Rbx)))); // Set true if equal
                        }
                    }

                    com.result_type = Some(Bool);
                }
                Op1::IsList => {
                    match com.result_type {
                        Some(List) => {
                            instrs.push(Mov(ToReg(Rax, TRUE)));
                        }
                        Some(_) => {
                            instrs.push(Mov(ToReg(Rax, FALSE)));
                        }
                        None => {
                            instrs.push(And(ToReg(Rax, Imm(3))));
                            instrs.push(Cmp(ToReg(Rax, Imm(1))));
                            instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                            instrs.push(Mov(ToReg(Rbx, TRUE)));
                            instrs.push(CMovI(CMov::E(Rax, OReg(Rbx)))); // Set true if equal
                        }
                    }

                    com.result_type = Some(Bool);
                }
                Op1::IsNum => {
                    match com.result_type {
                        Some(Int) => {
                            instrs.push(Mov(ToReg(Rax, TRUE)));
                        }
                        Some(_) => {
                            instrs.push(Mov(ToReg(Rax, FALSE)));
                        }
                        None => {
                            instrs.push(And(ToReg(Rax, Imm(1))));
                            instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                            instrs.push(Mov(ToReg(Rbx, TRUE)));
                            instrs.push(CMovI(CMov::Z(Rax, OReg(Rbx)))); // Set true if zero
                        }
                    }

                    com.result_type = Some(Bool);
                }
                Op1::Print => {
                    instrs.push(Mov(ToReg(Rdi, OReg(Rax))));
                    instrs.push(Call(Label::new(Some("snek_print"))));
                }
                Op1::Len => {
                    let nil_len = com.label("nil_len");
                    com.index_used();
                    instrs.push(Mov(ToReg(Rbx, OReg(Rax))));
                    match com.result_type {
                        Some(List) => {}
                        Some(_) => {
                            instrs.push(Mov(ToReg(Rdi, Imm(23)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        }
                        _ => {
                            instrs.extend(vec![
                                And(ToReg(Rax, Imm(3))),
                                Cmp(ToReg(Rax, Imm(1))),
                                Mov(ToReg(Rdi, Imm(24))), // invalid argument
                                JumpI(Jump::NZ(snek_error.clone())),
                            ]);
                        }
                    }
                    instrs.extend(vec![
                        // Nil Check
                        Cmp(ToReg(Rbx, NIL)),
                        Mov(ToReg(Rax, Imm(0))),
                        JumpI(Jump::E(nil_len.clone())),
                        // Remove tag
                        Sub(ToReg(Rbx, Imm(1))),
                        // Get length
                        Mov(ToReg(
                            Rax,
                            Mem(MemRef {
                                reg: Rbx,
                                offset: 1,
                            }),
                        )),
                        Sal(Rax, 1),
                        LabelI(nil_len),
                    ]);
                    com.result_type = Some(Int);
                }
            }
            co.rax_to_target(&mut instrs);
        }
        Expr::BinOp(op, left, right) => {
            // BinOp operands cannot be a tail position
            let co = &co.modify_tail(false);

            instrs.extend(compile_expr(
                left,
                &co.modify_target(Some(MemRef {
                    reg: Rsp,
                    offset: co.si,
                })),
                com,
            ));
            let ltype = com.result_type;

            instrs.extend(compile_expr(
                right,
                &co.modify_target(None).modify_si(co.si + 1),
                com,
            ));
            let rtype = com.result_type;
            instrs.push(Mov(ToReg(Rbx, OReg(Rax))));
            instrs.push(Mov(ToReg(
                Rax,
                Mem(MemRef {
                    reg: Rsp,
                    offset: co.si,
                }),
            )));

            match op {
                Op2::Index => {
                    // Ltype/Rax should be list
                    match ltype {
                        None => {
                            instrs.extend(vec![
                                Push(Rax),               // Save Rax to test
                                And(ToReg(Rax, Imm(3))), // Check last two bits to be 01
                                Cmp(ToReg(Rax, Imm(1))),
                                Mov(ToReg(Rdi, Imm(26))),
                                Pop(Rax),
                                JumpI(Jump::NE(snek_error.clone())),
                                Cmp(ToReg(Rax, NIL)),
                                Mov(ToReg(Rdi, Imm(40))),
                                JumpI(Jump::E(snek_error.clone())),
                            ]);
                        }
                        Some(List) => instrs.extend(vec![
                            Cmp(ToReg(Rax, NIL)),
                            Mov(ToReg(Rdi, Imm(25))),
                            JumpI(Jump::Z(snek_error.clone())),
                        ]),
                        _ => {
                            instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                            com.result_type = None;
                            return instrs;
                        }
                    }
                    // Rtype/Rbx should be number
                    match rtype {
                        None => {
                            instrs.push(Test(ToReg(Rbx, Imm(1))));
                            instrs.push(Mov(ToReg(Rdi, Imm(25)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        }
                        Some(Int) => {}
                        _ => {
                            instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                            instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                            com.result_type = None;
                            return instrs;
                        }
                    }
                    // 1 <= Index(snek) <= length(int) after adding 1
                    instrs.extend(vec![
                        // Remove tag from address
                        Sub(ToReg(Rax, Imm(1))),
                        // Convert index from snek to 1 based number
                        Sar(Rbx, 1),
                        Add(ToReg(Rbx, Imm(1))),
                        // Error code index out of bounds
                        Mov(ToReg(Rdi, Imm(40))),
                        // Test upper bound
                        Cmp(ToReg(
                            Rbx,
                            Mem(MemRef {
                                reg: Rax,
                                offset: 1, // Length is at offset 1
                            }),
                        )),
                        JumpI(Jump::G(snek_error.clone())),
                        // Test lower bound
                        Cmp(ToReg(Rbx, Imm(0))),
                        JumpI(Jump::LE(snek_error.clone())),
                        // Ignore the length word
                        Add(ToReg(Rbx, Imm(1))),
                        // Add offset to Rax, multiply 8 = shift 3
                        Sal(Rbx, 3),
                        Add(ToReg(Rax, OReg(Rbx))),
                        Mov(ToReg(
                            Rax,
                            Mem(MemRef {
                                reg: Rax,
                                offset: 0,
                            }),
                        )),
                    ]);
                    com.result_type = None;
                }
                Op2::Equal => {
                    let needs_check = ltype.is_none() || rtype.is_none();
                    if ltype.is_some() && rtype.is_some() && ltype != rtype {
                        instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                        instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        com.result_type = Some(Bool);
                        return instrs;
                    }
                    // Check equality with sub instead of cmp
                    instrs.push(Sub(ToReg(Rax, OReg(Rbx))));
                    if needs_check {
                        instrs.push(Push(Rax)); // Push to stack for checking type later
                    }
                    instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                    instrs.push(Mov(ToReg(Rbx, TRUE)));
                    instrs.push(CMovI(CMov::E(Rax, OReg(Rbx))));

                    if needs_check {
                        // Check if both were of the same type
                        instrs.push(Pop(Rbx));
                        instrs.push(Test(ToReg(Rbx, Imm(1))));
                        instrs.push(Mov(ToReg(Rdi, Imm(22)))); // invalid argument
                        instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                    }
                    com.result_type = Some(Bool);
                }
                Op2::DeepEqual => {
                    // Early exit
                    if ltype.is_some() && rtype.is_some() && ltype != rtype {
                        instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                        instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                        com.result_type = Some(Bool);
                        return instrs;
                    }
                    instrs.extend(vec![
                        // Arguments to rust helper, Rdi later
                        Mov(ToReg(Rsi, OReg(Rax))),
                        // Check for same type and early return
                        Sub(ToReg(Rax, OReg(Rbx))),
                        Test(ToReg(Rax, Imm(3))),
                        Mov(ToReg(Rdi, Imm(22))), // invalid argument
                        JumpI(Jump::NZ(snek_error.clone())),
                        Mov(ToReg(Rdi, OReg(Rbx))),
                        Call(Label::new(Some("snek_deep_equal"))),
                    ]);
                }
                _ => {
                    // Check if Rax and mem is a number
                    if match ltype {
                        Some(Int) | None => false,
                        _ => true,
                    } || match rtype {
                        Some(Int) | None => false,
                        _ => true,
                    } {
                        instrs.push(Mov(ToReg(Rdi, Imm(23)))); // invalid argument
                        instrs.push(JumpI(Jump::U(snek_error.clone())));
                        com.result_type = Some(
                            if let Op2::Plus | Op2::Minus | Op2::Times | Op2::Divide = op {
                                Int
                            } else {
                                Bool
                            },
                        );
                        return instrs;
                    }

                    if ltype.is_none() {
                        instrs.push(Test(ToReg(Rax, Imm(1))));
                        instrs.push(Mov(ToReg(Rdi, Imm(24)))); // invalid argument
                        instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                    }
                    if rtype.is_none() {
                        instrs.push(Test(ToReg(Rbx, Imm(1))));
                        instrs.push(Mov(ToReg(Rdi, Imm(25)))); // invalid argument
                        instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                    }

                    if let Op2::Plus | Op2::Minus | Op2::Times | Op2::Divide = op {
                        match op {
                            Op2::Plus => instrs.push(Add(ToReg(Rax, OReg(Rbx)))),
                            Op2::Minus => instrs.push(Sub(ToReg(Rax, OReg(Rbx)))),
                            Op2::Times => {
                                instrs.push(Sar(Rax, 1));
                                instrs.push(Mul(Rax, OReg(Rbx)));
                            }
                            Op2::Divide => {
                                instrs.extend(vec![
                                    Cmp(ToReg(Rbx, Imm(0))),
                                    Mov(ToReg(Rdi, Imm(50))), // Divide by zero
                                    JumpI(Jump::E(snek_error.clone())),
                                    Mov(ToReg(Rdx, Imm(0))),
                                    Div(OReg(Rbx)),
                                    Sal(Rax, 1),
                                ]);
                            }
                            _ => panic!("should not happen"),
                        }
                        instrs.push(Mov(ToReg(Rdi, Imm(32)))); // overflow
                        instrs.push(JumpI(Jump::O(snek_error)));
                        com.result_type = Some(Int);
                    } else {
                        instrs.push(Cmp(ToReg(Rax, OReg(Rbx))));
                        instrs.push(Mov(ToReg(Rax, FALSE))); // Set false
                        instrs.push(Mov(ToReg(Rbx, TRUE)));
                        match op {
                            Op2::Greater => instrs.push(CMovI(CMov::G(Rax, OReg(Rbx)))),
                            Op2::GreaterEqual => instrs.push(CMovI(CMov::GE(Rax, OReg(Rbx)))),
                            Op2::Less => instrs.push(CMovI(CMov::L(Rax, OReg(Rbx)))),
                            Op2::LessEqual => instrs.push(CMovI(CMov::LE(Rax, OReg(Rbx)))),
                            _ => panic!("should not happen"),
                        }
                        com.result_type = Some(Bool);
                    }
                }
            }
            co.rax_to_target(&mut instrs);
        }
        Expr::Let(bindings, e) => {
            let mut new_env = co.env.clone();
            let mut track_dup = HashSet::new();

            for (i, (x, b)) in bindings.iter().enumerate() {
                let si_ = co.si + i as i32;
                if track_dup.contains(x) {
                    panic!("Duplicate binding")
                }
                instrs.extend(compile_expr(
                    b,
                    &co.modify(
                        Some(si_),
                        Some(new_env.clone()),
                        None,
                        Some(Some(MemRef {
                            reg: Rsp,
                            offset: si_,
                        })),
                        // Let binding is not a tail position
                        Some(false),
                    ),
                    com,
                ));
                track_dup.insert(x);
                new_env.insert(x.to_string(), VarEnv::new(si_, com.result_type, false));
            }

            instrs.extend(compile_expr(
                e,
                &co.modify(
                    Some(co.si + bindings.len() as i32),
                    Some(new_env),
                    None,
                    None,
                    None,
                ),
                com,
            ));
        }
        Expr::If(c, t, e) => {
            // Else and endif have same label index
            let else_label = com.label("else");
            let end_if_label = com.label("end_if");
            com.index_used();

            // Use Rax
            instrs.extend(compile_expr(
                c,
                // If condition is not a tail position
                &co.modify_target(None).modify_tail(false),
                com,
            ));
            // If
            if match com.result_type {
                None | Some(Type::Bool) => true,
                _ => false,
            } {
                instrs.push(Cmp(ToReg(Rax, FALSE)));
                instrs.push(JumpI(Jump::E(else_label.clone())));
                // Then
                instrs.extend(compile_expr(t, co, com));
                instrs.push(JumpI(Jump::U(end_if_label.clone())));
                // Else
                instrs.push(LabelI(else_label));
                instrs.extend(compile_expr(e, co, com));

                instrs.push(LabelI(end_if_label));
            } else {
                // Then
                instrs.extend(compile_expr(t, co, com));
            }
            com.result_type = None;
        }
        Expr::Set(x, e) => {
            // Set expression is not a tail position
            let co = &co.modify_tail(false);
            instrs.extend(compile_expr(e, &co.modify_target(None), com));

            let venv = if co.env.contains_key(x) {
                &co.env
            } else if com.env.contains_key(x) {
                let old_var = com.env.get(x).unwrap();
                com.env.insert(
                    x.to_string(),
                    VarEnv {
                        offset: old_var.offset,
                        vtype: com.result_type,
                        defined: old_var.defined,
                    },
                );
                &com.env
            } else {
                panic!("Unbound variable identifier {x}")
            }
            .get(x)
            .unwrap();

            let reg = if venv.defined {
                instrs.push(Mov(ToReg(Rbx, Imm64(co.get_define_stack()))));
                Rbx
            } else {
                Rsp
            };

            instrs.push(Mov(ToMem(
                MemRef {
                    reg,
                    offset: venv.offset,
                },
                OReg(Rax),
            )));
            co.rax_to_target(&mut instrs)
        }
        Expr::SetLst(lst, idx, val) => {
            // Set expression is not a tail position
            let co = &co.modify_tail(false);
            let co_child = &co.modify_target(None);

            // Compile list to Rax
            instrs.extend(compile_expr(lst, co_child, com));
            // Copy to stack
            instrs.push(Mov(ToMem(
                MemRef {
                    reg: Rsp,
                    offset: co.si,
                },
                OReg(Rax),
            )));

            // Type check lst
            match com.result_type {
                None => {
                    instrs.extend(vec![
                        Cmp(ToReg(Rax, NIL)),
                        Mov(ToReg(Rdi, Imm(40))),
                        JumpI(Jump::E(snek_error.clone())),
                        And(ToReg(Rax, Imm(3))), // Check last two bits to be 01
                        Cmp(ToReg(Rax, Imm(1))),
                        Mov(ToReg(Rdi, Imm(26))),
                        JumpI(Jump::NE(snek_error.clone())),
                    ]);
                }
                Some(List) => instrs.extend(vec![
                    Cmp(ToReg(Rax, NIL)),
                    Mov(ToReg(Rdi, Imm(25))),
                    JumpI(Jump::Z(snek_error.clone())),
                ]),
                _ => {
                    instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                    instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                    com.result_type = None;
                    return instrs;
                }
            }

            // Compile index
            instrs.extend(compile_expr(idx, &co_child.modify_si(co.si + 1), com));
            // Type check idx
            match com.result_type {
                None => {
                    instrs.push(Test(ToReg(Rax, Imm(1))));
                    instrs.push(Mov(ToReg(Rdi, Imm(25)))); // invalid argument
                    instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                }
                Some(Int) => {}
                _ => {
                    instrs.push(Mov(ToReg(Rdi, Imm(21)))); // invalid argument
                    instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                    com.result_type = None;
                    return instrs;
                }
            }
            // Save index
            instrs.push(Mov(ToMem(
                MemRef {
                    reg: Rsp,
                    offset: co.si + 1,
                },
                OReg(Rax),
            )));
            // Compile value
            instrs.extend(compile_expr(val, &co_child.modify_si(co.si + 2), com));
            instrs.extend(vec![
                // Get heap address
                Mov(ToReg(
                    Rbx,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si,
                    }),
                )),
                // Get index
                Mov(ToReg(
                    Rdi,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si + 1,
                    }),
                )),
                // Save value to stack
                Mov(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: co.si + 1,
                    },
                    OReg(Rax),
                )),
                // Bring index to Rax
                Mov(ToReg(Rax, OReg(Rdi))),
                // Remove tag from address
                Sub(ToReg(Rbx, Imm(1))),
                // Convert index from snek to 1 based number
                Sar(Rax, 1),
                Add(ToReg(Rax, Imm(1))),
                // 1 <= Index(snek) <= length(int) test after increment
                // Error code index out of bounds
                Mov(ToReg(Rdi, Imm(40))),
                // Test upper bound
                Cmp(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: Rbx,
                        offset: 1, // Length are offset 1
                    }),
                )),
                JumpI(Jump::G(snek_error.clone())),
                // Test lower bound
                Cmp(ToReg(Rax, Imm(0))),
                JumpI(Jump::LE(snek_error.clone())),
                // Ignore the length word
                Add(ToReg(Rax, Imm(1))),
                // Add offset to Rbx, multiply 8 = shift 3
                Sal(Rax, 3),
                Add(ToReg(Rbx, OReg(Rax))),
                // Copy value
                Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si + 1,
                    }),
                )),
                Mov(ToMem(
                    MemRef {
                        reg: Rbx,
                        offset: 0,
                    },
                    OReg(Rax),
                )),
                // Set result
                Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si,
                    }),
                )),
            ]);
            co.rax_to_target(&mut instrs);
            com.result_type = Some(List);
        }
        Expr::Block(es) => {
            let mut co = co.clone();
            let nonovars = nonovars(e, &hashset![]);
            for (x,v) in co.env.iter_mut() {
                if nonovars.contains(x) {
                    v.vtype = None;
                }
            }
            for (x,v) in com.env.iter_mut() {
                if nonovars.contains(x) {
                    v.vtype = None;
                }
            }
            // Only last expression in the block can be a tail position
            let co_rax = &co.modify_target(None).modify_tail(false);

            // Only last instruction needs to be put into target
            for (i, e) in es.into_iter().enumerate() {
                instrs.extend(compile_expr(
                    e,
                    if i + 1 == es.len() {
                        // Last expression
                        &co
                    } else {
                        co_rax
                    },
                    com,
                ));
            }
        }
        Expr::Loop(e) => {
            let mut co = co.clone();
            let nonovars = nonovars(e, &hashset![]);
            for (x,v) in co.env.iter_mut() {
                if nonovars.contains(x) {
                    v.vtype = None;
                }
            }
            for (x,v) in com.env.iter_mut() {
                if nonovars.contains(x) {
                    v.vtype = None;
                }
            }
            // Begin and end label have same label index
            let begin_loop = com.label("begin_loop");
            let end_loop = com.label("end_loop");
            com.index_used();
            instrs.push(LabelI(begin_loop.clone()));
            // Work with Rax, move to target at the end
            instrs.extend(compile_expr(
                e,
                &com.new_ce_label(&co.modify_target(None), end_loop.clone()),
                com,
            ));
            instrs.push(JumpI(Jump::U(begin_loop)));
            instrs.push(LabelI(end_loop));
            co.rax_to_target(&mut instrs);
        }
        Expr::Break(e) => {
            // TODO: Optimize this further?
            let co = &co.modify_tail(false);
            if co.label.name == "" {
                panic!("dangling break");
            } else {
                instrs.extend(compile_expr(e, co, com));
                // Jump to end_loop
                instrs.push(JumpI(Jump::U(co.label.clone())));
            }
        }
        Expr::FnCall(name, args) => {
            if name == "gc" {
                instrs.extend(vec![
                    Mov(ToReg(Rdi, OReg(Rbp))),
                    Mov(ToReg(Rsi, OReg(Rsp))),
                    // Set the top of stack frame to current usage of stack
                    Mov(ToMem(
                        MemRef {
                            reg: Rsp,
                            offset: 0,
                        },
                        Imm(co.si),
                    )),
                    Call(Label::new(Some("snek_gc"))),
                    Mov(co.src_to_target(Imm(0))),
                ]);
                com.result_type = Some(Int);
                return instrs;
            }
            let fenv = com
                .fns
                .get(name)
                .expect(&format!("Invalid: undefined function {name}"))
                .clone();
            if fenv.argc != args.len() as i32 {
                panic!("Invalid: mismatched argument count");
            }

            for (i, arg) in args.iter().enumerate() {
                // Result in main's stack
                instrs.extend(compile_expr(
                    arg,
                    &co.modify(
                        Some(co.si + i as i32),
                        None,
                        None,
                        Some(Some(MemRef {
                            reg: Rsp,
                            offset: co.si + i as i32,
                        })),
                        // Arguments to function calls are not tail positions
                        Some(false),
                    ),
                    com,
                ));
            }

            if co.tail {
                // Do tail call if co.tail is true
                // Move result from current function's stack to the current function's arguments
                // Diff is to 1st argument pos, not top of stack (off by 1 for co.si storage)
                let diff = com.depth - fenv.depth + 1;
                // No need to copy if already at the right place
                if co.si != diff {
                    // Copy top to bottom or bottom to top depending on diff and co.si
                    let rng = if co.si > diff {
                        Box::new(0..args.len() as i32) as Box<dyn Iterator<Item = i32>>
                    } else {
                        Box::new((0..args.len() as i32).rev())
                    };
                    for i in rng {
                        instrs.push(Mov(ToReg(
                            Rax,
                            Mem(MemRef {
                                reg: Rsp,
                                offset: co.si + i,
                            }),
                        )));
                        instrs.push(Mov(ToMem(
                            MemRef {
                                reg: Rsp,
                                offset: diff + i,
                            },
                            OReg(Rax),
                        )));
                    }
                }
                // Bring RSP to ret ptr
                instrs.push(Add(ToReg(Rsp, Imm(com.depth * 8))));
                instrs.push(Pop(Rbp));
                instrs.push(JumpI(Jump::U(Label::new(Some(&format!("fun_{name}"))))))
                // Already in tail position, no need to move to target
            } else {
                // Move result from current function's stack to the callee's stack layout
                for i in 0..args.len() as i32 {
                    instrs.push(Mov(ToReg(
                        Rax,
                        Mem(MemRef {
                            reg: Rsp,
                            offset: co.si + i,
                        }),
                    )));
                    instrs.push(Mov(ToMem(
                        MemRef {
                            reg: Rsp,
                            offset: -(fenv.depth + 2) + i + 1, // + 1 because top word is for stack usage
                        },
                        OReg(Rax),
                    )));
                }
                // Set the top of stack frame to current usage of stack
                instrs.push(Mov(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: 0,
                    },
                    Imm(co.si + args.len() as i32),
                )));
                instrs.push(Call(Label::new(Some(&format!("fun_{name}")))));
                co.rax_to_target(&mut instrs);
            }
            com.result_type = None;
        }
        Expr::List(es) => {
            for (i, e) in es.iter().enumerate() {
                // Result in main's stack
                instrs.extend(compile_expr(
                    e,
                    &co.modify(
                        Some(co.si + i as i32),
                        None,
                        None,
                        Some(Some(MemRef {
                            reg: Rsp,
                            offset: co.si + i as i32,
                        })),
                        // List items are not tail positions
                        Some(false),
                    ),
                    com,
                ));
            }

            let alloc_succ = com.label("alloc_succ");
            com.index_used();

            instrs.extend(vec![
                // Get heap head
                Mov(ToReg(
                    Rbx,
                    Mem(MemRef {
                        reg: R15,
                        offset: 0,
                    }),
                )),
                // Check if space exists to allocate
                Mov(ToReg(Rax, OReg(Rbx))),
                Add(ToReg(Rax, Imm((es.len() + 2) as i32 * 8))), // 2 word metadata
                Cmp(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: R15,
                        offset: 1,
                    }),
                )),
                JumpI(Jump::LE(alloc_succ.clone())),
                // Insufficient mem, Call GC
                Mov(ToReg(Rdi, Imm(es.len() as i32 + 2))),
                Mov(ToReg(Rsi, OReg(Rbp))),
                Mov(ToReg(Rdx, OReg(Rsp))),
                // Set stack usage
                Mov(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: 0,
                    },
                    Imm(co.si + es.len() as i32),
                )),
                Call(Label::new(Some("snek_try_gc"))),
                // Continue if success
                LabelI(alloc_succ),
                // Get heap head again
                Mov(ToReg(
                    Rbx,
                    Mem(MemRef {
                        reg: R15,
                        offset: 0,
                    }),
                )),
                // Clear GC word
                Mov(ToMem(
                    MemRef {
                        reg: Rbx,
                        offset: 0,
                    },
                    Imm(0),
                )),
                // Length as the second value
                Mov(ToMem(
                    MemRef {
                        reg: Rbx,
                        offset: 1,
                    },
                    Imm64(es.len() as i64),
                )),
            ]);

            for i in 0..es.len() as i32 {
                instrs.push(Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si + i,
                    }),
                )));
                instrs.push(Mov(ToMem(
                    MemRef {
                        reg: Rbx,
                        offset: i + 2, // 1st two words are metadata
                    },
                    OReg(Rax),
                )));
            }
            // Set target to address and tag with 1
            instrs.push(Mov(co.src_to_target(OReg(Rbx))));
            instrs.push(Add(co.src_to_target(Imm(1))));
            // Move heap offset, two words extra for GC and length
            instrs.push(Add(ToMem(
                MemRef {
                    reg: R15,
                    offset: 0,
                },
                Imm(8 * (2 + es.len() as i32)),
            )));
            com.result_type = Some(List);
        }
        Expr::SizedList(c, v) => {
            // Count
            instrs.extend(compile_expr(c, &co.modify_target(None), com));
            match com.result_type {
                Some(Int) => {}
                Some(_) => {
                    instrs.push(Mov(ToReg(Rdi, Imm(23)))); // invalid argument
                    instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                }
                _ => {
                    instrs.push(Test(ToReg(Rax, Imm(1))));
                    instrs.push(Mov(ToReg(Rdi, Imm(24)))); // invalid argument
                    instrs.push(JumpI(Jump::NZ(snek_error.clone())));
                }
            }
            // Copy to stack after converting to numeric form
            instrs.push(Sar(Rax, 1));
            instrs.push(Mov(ToMem(
                MemRef {
                    reg: Rsp,
                    offset: co.si,
                },
                OReg(Rax),
            )));

            let index_valid = com.label("index_valid");
            let index_zero = com.label("index_zero");
            instrs.extend(vec![
                Cmp(ToReg(Rax, Imm(0))),
                Mov(ToReg(Rdi, Imm(40))),
                JumpI(Jump::G(index_valid.clone())),
                Mov(ToReg(Rbx, NIL)),
                CMovI(CMov::E(Rax, OReg(Rbx))),
                JumpI(Jump::E(index_zero.clone())),
                Mov(ToReg(Rdi, Imm(40))),
                JumpI(Jump::U(snek_error.clone())),
                LabelI(index_valid),
            ]);

            // Value
            instrs.extend(compile_expr(
                v,
                &co.modify_target(Some(MemRef {
                    reg: Rsp,
                    offset: co.si + 1,
                }))
                .modify_si(co.si + 1),
                com,
            ));

            let alloc_succ = com.label("alloc_succ");
            let fill_list = com.label("fill_list");
            com.index_used();
            instrs.extend(vec![
                // Check heap availability
                Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si,
                    }),
                )),
                Mov(ToReg(Rdi, OReg(Rax))), // GC arg, need to add metadata length
                Add(ToReg(Rax, Imm(2))),    // Metadata length
                Sal(Rax, 3),                // * 8
                Add(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: R15,
                        offset: 0,
                    }),
                )),
                Cmp(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: R15,
                        offset: 1,
                    }),
                )),
                Mov(ToReg(Rbx, OReg(Rdi))),
                JumpI(Jump::LE(alloc_succ.clone())),
                // Call GC
                Add(ToReg(Rdi, Imm(2))), // Add metadata length
                Mov(ToReg(Rsi, OReg(Rbp))),
                Mov(ToReg(Rdx, OReg(Rsp))),
                // co.si and co.si+1 are used
                Mov(ToMem(
                    MemRef {
                        reg: Rsp,
                        offset: 0,
                    },
                    Imm(co.si + 2),
                )),
                Call(Label::new(Some("snek_try_gc"))),
                // Continue
                LabelI(alloc_succ),
                // Rbx has count, Rax has base of heap, Rdi has the value to fill
                Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: R15,
                        offset: 0,
                    }),
                )),
                Mov(ToReg(
                    Rdi,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si + 1,
                    }),
                )),
                // Clear GC word
                Mov(ToMem(
                    MemRef {
                        reg: Rax,
                        offset: 0,
                    },
                    Imm(0),
                )),
                // Set length
                Mov(ToMem(
                    MemRef {
                        reg: Rax,
                        offset: 1,
                    },
                    OReg(Rbx),
                )),
                Add(ToReg(Rax, Imm(16))),
                // Fill list
                LabelI(fill_list.clone()),
                Mov(ToMem(
                    MemRef {
                        reg: Rax,
                        offset: 0,
                    },
                    OReg(Rdi),
                )),
                Add(ToReg(Rax, Imm(8))),
                Sub(ToReg(Rbx, Imm(1))),
                JumpI(Jump::NZ(fill_list)),
                // Tag address to return
                Mov(ToReg(
                    Rax,
                    Mem(MemRef {
                        reg: R15,
                        offset: 0,
                    }),
                )),
                Add(ToReg(Rax, Imm(1))),
                Mov(ToReg(
                    Rbx,
                    Mem(MemRef {
                        reg: Rsp,
                        offset: co.si,
                    }),
                )),
                // Increment heap pointer
                Add(ToReg(Rbx, Imm(2))),
                Sal(Rbx, 3),
                Add(ToMem(
                    MemRef {
                        reg: R15,
                        offset: 0,
                    },
                    OReg(Rbx),
                )),
                LabelI(index_zero),
            ]);
            co.rax_to_target(&mut instrs);
            com.result_type = Some(List);
        }
        Expr::Define(_, _) => panic!("define cannot be compiled"),
        Expr::FnDefn(_, _, _) => panic!("Invalid: fn defn cannot be compiled here"),
    }
    return instrs;
}

pub fn instrs_to_string(instrs: &Vec<Instr>) -> String {
    instrs
        .iter()
        .map(|i| {
            if matches!(i, LabelI(_)) {
                format!("{i}")
            } else {
                format!(" {i}")
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn instrs_to_asm(
    cmds: &Vec<Instr>,
    ops: &mut dynasmrt::x64::Assembler,
    lbls: &HashMap<Label, DynamicLabel>,
) {
    let lbls = &mut lbls.clone();
    cmds.iter().for_each(|c| {
        if let LabelI(l) = c {
            if lbls.get(&l).is_none() {
                lbls.insert(l.clone(), ops.new_dynamic_label());
            }
        }
    });
    cmds.iter().for_each(|c| c.asm(ops, lbls))
}
