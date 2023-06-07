use crate::structs::*;
use Expr::*;

pub fn optimize(e: &Expr) -> Expr {
    let mut olde = e.clone();
    let mut newe = optimize_recurse(&olde);
    while newe != olde {
        olde = newe;
        newe = optimize_recurse(&olde);
    }
    newe.clone()
}

fn optimize_recurse(e: &Expr) -> Expr {
    let orm = |e| optimize_recurse(e);
    let copy = e.clone();
    match e {
        Nil => copy,
        Num(n) => {
            if n.overflowing_mul(2).1 {
                panic!("Invalid")
            } else {
                copy
            }
        }
        Var(_) => copy, // Track statically if not set!?
        Boolean(_) => copy,
        List(es) => {
            if es.len() != 0 {
                List(es.iter().map(orm).collect())
            } else {
                Nil
            }
        }
        UnOp(op, e) => match orm(e) {
            Num(n) => match op {
                // Test overflow
                Op1::Add1 => overflow_unop(Num(n + 1), copy),
                Op1::Sub1 => overflow_unop(Num(n - 1), copy),
                Op1::IsNum => Boolean(true),
                Op1::IsBool | Op1::IsList => Boolean(false),
                _ => copy,
            },
            Boolean(_) => match op {
                Op1::IsBool => Boolean(true),
                Op1::IsNum | Op1::IsList => Boolean(false),
                _ => copy,
            },
            List(es) => match op {
                Op1::Len => Num(es.len() as i64),
                Op1::IsList => Boolean(true),
                Op1::IsNum | Op1::IsBool => Boolean(false),
                _ => copy,
            },
            Nil => match op {
                Op1::Len => Num(0),
                Op1::IsList => Boolean(true),
                Op1::IsNum | Op1::IsBool => Boolean(false),
                _ => copy,
            },
            _ => copy,
        },
        BinOp(op, e1, e2) => {
            let e1 = orm(e1);
            let e2 = orm(e2);

            // both are nums
            if let Num(n1) = e1 {
                if let Num(n2) = e2 {
                    match op {
                        // Test overflow
                        Op2::Plus | Op2::Minus | Op2::Times => overflow_binop(op, n1, n2, copy),
                        Op2::Divide => {
                            if n2 != 0 {
                                Num(n1 / n2)
                            } else {
                                copy
                            }
                        }

                        Op2::Equal => Boolean(n1 == n2),
                        Op2::DeepEqual => Boolean(n1 == n2),
                        Op2::Greater => Boolean(n1 > n2),
                        Op2::GreaterEqual => Boolean(n1 >= n2),
                        Op2::Less => Boolean(n1 < n2),
                        Op2::LessEqual => Boolean(n1 <= n2),
                        _ => copy,
                    }
                } else {
                    copy
                }
            } else {
                copy
            }
        }
        Let(bindings, e) => Let(
            bindings
                .iter()
                .map(|(x, e)| (x.clone(), orm(e)))
                .collect(),
            Box::new(orm(e)),
        ),
        SizedList(c, v) => {
            if let Num(c) = orm(c) {
                match orm(v) {
                    Num(n) => List(vec![Num(n); c as usize]),
                    Boolean(b) => List(vec![Boolean(b); c as usize]),
                    Nil => List(vec![Nil; c as usize]),
                    _ => copy,
                }
            } else {
                copy
            }
        }
        If(cond, then, other) => match orm(cond) {
            Boolean(b) => {
                if b {
                    orm(then)
                } else {
                    orm(other)
                }
            }
            Nil | Num(_) | List(_) => orm(then),
            _ => copy,
        },
        Loop(e) => Loop(Box::new(orm(e))),
        Block(es) => Block(es.iter().map(orm).collect()),
        Break(e) => Break(Box::new(orm(e))),
        Set(x, e) => Set(x.clone(), Box::new(orm(e))),
        SetLst(lst, idx, val) => SetLst(
            Box::new(orm(lst)),
            Box::new(orm(idx)),
            Box::new(orm(val)),
        ),
        Define(_, _) => copy,
        FnDefn(f, a, b) => FnDefn(f.clone(), a.clone(), Box::new(orm(b))),
        FnCall(f, args) => FnCall(f.clone(), args.iter().map(orm).collect()),
    }
}

fn overflow_unop(e: Expr, default: Expr) -> Expr {
    if let Num(n) = &e {
        if n.overflowing_mul(2).1 {
            default
        } else {
            e
        }
    } else {
        default
    }
}

fn overflow_binop(op: &Op2, n1: i64, n2: i64, default: Expr) -> Expr {
    fn always_true(_: i64, _: i64) -> (i64, bool) {
        (0, true)
    }
    let (n, o) = match op {
        Op2::Plus => i64::overflowing_add,
        Op2::Minus => i64::overflowing_sub,
        Op2::Times => i64::overflowing_mul,
        _ => always_true,
    }(n1, n2);
    if o {
        default
    } else if n.overflowing_mul(2).1 {
        default
    } else {
        Num(n)
    }
}
