use crate::structs::*;
use sexp::Atom::*;
use sexp::*;

pub fn parse_top_level(s: &Sexp) -> (Vec<Expr>, Expr) {
    if let Sexp::List(v) = s {
        if let [fns @ .., expr] = &v[..] {
            return (
                fns.iter()
                    .map(parse_expr)
                    .map(|f| {
                        if let Expr::FnDefn(_, _, _) = f {
                            f
                        } else {
                            panic!("Invalid: only function definitions allowed here")
                        }
                    })
                    .collect(),
                parse_expr(expr),
            );
        }
    }
    panic!("Invalid")
}

pub fn parse_expr(s: &Sexp) -> Expr {
    let keywords = &vec![
        "add1", "sub1", "let", "isnum", "isbool", "islist", "if", "loop", "break", "set!", "block",
        "input", "print", "fun", "define", "nil", "list", "index", "slist", "len", "+", "-", "*",
        "/", "<", ">", ">=", "<=", "=", "==",
    ];
    match s {
        // Num
        Sexp::Atom(I(n)) => Expr::Num(i64::try_from(*n).expect("Invalid")),
        // Boolean/Nil/Variable
        Sexp::Atom(S(x)) => match x.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "nil" => Expr::Nil,
            _ => Expr::Var(String::from(x)),
        },
        // List of tokens
        Sexp::List(vec) => match &vec[..] {
            // Func calls
            [Sexp::Atom(S(f)), args @ ..] if !keywords.contains(&f.as_str()) => {
                Expr::FnCall(f.to_string(), args.iter().map(parse_expr).collect())
            }
            // Block
            [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                if es.len() == 0 {
                    panic!("Invalid")
                }
                Expr::Block(es.into_iter().map(parse_expr).collect())
            }
            // List
            [Sexp::Atom(S(op)), es @ ..] if op == "list" => {
                if es.len() == 0 {
                    Expr::Nil
                } else {
                    Expr::List(es.into_iter().map(parse_expr).collect())
                }
            }
            // Define
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), e] if op == "define" => {
                Expr::Define(x.to_string(), Box::new(parse_expr(e)))
            }
            // Function defn
            [Sexp::Atom(S(op)), Sexp::List(v), b] if op == "fun" => {
                if let [Sexp::Atom(S(name)), vars @ ..] = &v[..] {
                    Expr::FnDefn(
                        name.to_string(),
                        vars.iter()
                            .map(|v| {
                                if let Sexp::Atom(S(vn)) = v {
                                    if keywords.contains(&vn.as_str()) || vn == "gc" {
                                        panic!("keyword cannot be used")
                                    }
                                    vn.to_string()
                                } else {
                                    panic!("Invalid")
                                }
                            })
                            .collect(),
                        Box::new(parse_expr(b)),
                    )
                } else {
                    panic!("Invalid");
                }
            }
            [Sexp::Atom(S(op)), e] => match op.as_str() {
                // Loop
                "loop" => Expr::Loop(Box::new(parse_expr(e))),
                // Break
                "break" => Expr::Break(Box::new(parse_expr(e))),
                // Unary Operators
                _ => Expr::UnOp(
                    match op.as_str() {
                        "add1" => Op1::Add1,
                        "sub1" => Op1::Sub1,
                        "isnum" => Op1::IsNum,
                        "isbool" => Op1::IsBool,
                        "islist" => Op1::IsList,
                        "print" => Op1::Print,
                        "len" => Op1::Len,
                        _ => panic!("Invalid"),
                    },
                    Box::new(parse_expr(e)),
                ),
            },
            // Set!
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), b] if op == "set!" => {
                if x == "input" {
                    panic!("Cannot set! on input");
                }
                Expr::Set(x.to_string(), Box::new(parse_expr(b)))
            }
            // Set! List
            [Sexp::Atom(S(op)), lst, idx, val] if op == "set!" => {
                let lst = parse_expr(lst);
                if let Expr::Var(x) = &lst {
                    if x == "input" {
                        panic!("Cannot set! on input");
                    }
                }
                Expr::SetLst(
                    Box::new(lst),
                    Box::new(parse_expr(idx)),
                    Box::new(parse_expr(val)),
                )
            }
            // Let
            [Sexp::Atom(S(op)), Sexp::List(bindings), e] if op == "let" => {
                if bindings.len() == 0 {
                    panic!("Invalid")
                }
                Expr::Let(
                    bindings
                        .into_iter()
                        .map(|binding| {
                            if let Sexp::List(bind) = binding {
                                if let [Sexp::Atom(S(x)), b] = &bind[..] {
                                    if keywords.contains(&x.as_str()) {
                                        panic!("keyword cannot be used")
                                    }
                                    return (x.to_string(), parse_expr(b));
                                }
                            }
                            panic!("Invalid")
                        })
                        .collect(),
                    Box::new(parse_expr(e)),
                )
            }
            // Sized list
            [Sexp::Atom(S(op)), l, r] if op == "slist" => {
                Expr::SizedList(Box::new(parse_expr(l)), Box::new(parse_expr(r)))
            }
            // Binary Operators
            [Sexp::Atom(S(op)), l, r] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "/" => Op2::Divide,
                    "=" => Op2::Equal,
                    "==" => Op2::DeepEqual,
                    ">" => Op2::Greater,
                    "<" => Op2::Less,
                    ">=" => Op2::GreaterEqual,
                    "<=" => Op2::LessEqual,
                    "index" => Op2::Index,
                    _ => panic!("Invalid {op}"),
                },
                Box::new(parse_expr(l)),
                Box::new(parse_expr(r)),
            ),
            // If
            [Sexp::Atom(S(op)), c, t, e] if op == "if" => Expr::If(
                Box::new(parse_expr(c)),
                Box::new(parse_expr(t)),
                Box::new(parse_expr(e)),
            ),
            _ => panic!("Invalid"),
        },
        _ => panic!("Invalid"),
    }
}
