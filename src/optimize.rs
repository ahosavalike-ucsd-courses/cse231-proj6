use crate::structs::*;
use im::{hashmap, hashset, HashMap, HashSet};
use Expr::*;

pub fn optimize(e: &Expr) -> Expr {
    let nonovars = set_vars(e, &hashset![]);
    let mut olde = e.clone();
    let mut newe = optimize_recurse(&olde, &hashmap! {}, &nonovars);
    while newe != olde {
        olde = newe;
        newe = optimize_recurse(&olde, &hashmap! {}, &nonovars);
    }
    newe.clone()
}

fn set_vars(e: &Expr, acc: &HashSet<String>) -> HashSet<String> {
    let svm = |e| set_vars(e, acc);
    let svf = |a: HashSet<String>, e: &Expr| a.union(set_vars(e, acc));
    match e {
        Set(x, _) => acc.update(x.clone()),
        List(es) => es.iter().fold(acc.clone(), svf),
        UnOp(_, e) | Loop(e) | Break(e) | FnDefn(_, _, e) => svm(e),
        BinOp(_, e1, e2) | SizedList(e1, e2) => svf(svf(acc.clone(), e1), e2),
        If(e1, e2, e3) | SetLst(e1, e2, e3) => svf(svf(svf(acc.clone(), e1), e2), e3),
        Let(es, e) => svf(es.iter().map(|(_, e)| e).fold(acc.clone(), svf), e),
        Block(es) | FnCall(_, es) => es.iter().fold(acc.clone(), svf),
        _ => acc.clone(),
    }
}

fn optimize_recurse(e: &Expr, letmap: &HashMap<String, Expr>, nonovars: &HashSet<String>) -> Expr {
    let orm = |e| optimize_recurse(e, letmap, nonovars);
    match e {
        Nil => e.clone(),
        Num(n) => {
            if n.overflowing_mul(2).1 {
                panic!("Invalid")
            } else {
                e.clone()
            }
        }
        Var(x) => letmap.get(x).unwrap_or(&e.clone()).clone(),
        Boolean(_) => e.clone(),
        List(es) => {
            if es.len() != 0 {
                List(es.iter().map(orm).collect())
            } else {
                Nil
            }
        }
        UnOp(op, e) => {
            let e = orm(e);
            let copy = UnOp(op.clone(), Box::new(e.clone()));
            match e {
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
            }
        }
        BinOp(op, e1, e2) => {
            let e1 = orm(e1);
            let e2 = orm(e2);

            let copy = BinOp(op.clone(), Box::new(e1.clone()), Box::new(e2.clone()));
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
        Let(bindings, e) => {
            let mut new_letmap = letmap.clone();
            let mut bs = vec![];
            let mut seen = hashset![];
            for (x, xe) in bindings {
                if seen.contains(x) {
                    return Let(
                        bindings.clone(),
                        Box::new(optimize_recurse(e, letmap, nonovars)),
                    );
                }
                seen.insert(x.clone());
                let be = optimize_recurse(xe, &new_letmap, nonovars);
                if let Num(_) | Boolean(_) | Nil = &be {
                    if !nonovars.contains(x) {
                        new_letmap.insert(x.clone(), be);
                    } else {
                        bs.push((x.clone(), be));
                    }
                } else {
                    bs.push((x.clone(), be));
                }
            }
            if bs.len() == 0 {
                optimize_recurse(e, &new_letmap, nonovars)
            } else {
                Let(bs, Box::new(optimize_recurse(e, &new_letmap, nonovars)))
            }
        }
        SizedList(c, v) => {
            let c = orm(c);
            let v = orm(v);
            let copy = SizedList(Box::new(c.clone()), Box::new(v.clone()));
            if let Num(c) = c {
                match v {
                    Num(n) => List(vec![Num(n); c as usize]),
                    Boolean(b) => List(vec![Boolean(b); c as usize]),
                    Nil => List(vec![Nil; c as usize]),
                    _ => copy,
                }
            } else {
                copy
            }
        }
        If(cond, then, other) => {
            let cond = orm(cond);
            let then = orm(then);
            let other = orm(other);
            let copy = If(
                Box::new(cond.clone()),
                Box::new(then.clone()),
                Box::new(other.clone()),
            );
            match cond {
                Boolean(b) => {
                    if b {
                        then
                    } else {
                        other
                    }
                }
                Nil | Num(_) | List(_) => then,
                _ => copy,
            }
        }
        Loop(e) => Loop(Box::new(orm(e))),
        Block(es) => Block(es.iter().map(orm).collect()),
        Break(e) => Break(Box::new(orm(e))),
        Set(x, e) => Set(x.clone(), Box::new(orm(e))),
        SetLst(lst, idx, val) => SetLst(Box::new(orm(lst)), Box::new(orm(idx)), Box::new(orm(val))),
        Define(_, _) => e.clone(),
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
    if o || n.overflowing_mul(2).1 {
        default
    } else {
        Num(n)
    }
}
