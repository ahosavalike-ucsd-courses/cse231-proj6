use crate::structs::*;
use sexp::Atom::*;
use sexp::*;

pub fn parse_top_level(s: &Sexp) -> Expr {
    if let Sexp::List(v) = s {
        if let [_fns @ .., expr] = &v[..] {
            // TODO: parse each fns
            return parse_expr(expr);
        }
    }
    panic!("Invalid")
}

pub fn parse_expr(s: &Sexp) -> Expr {
    let keywords = &vec![
        "add1", "sub1", "let", "isnum", "isbool", "if", "loop", "break", "set!", "block", "input",
        "print",
    ];
    match s {
        // Num
        Sexp::Atom(I(n)) => Expr::Num(i64::try_from(*n).expect("Invalid")),
        // Boolean
        Sexp::Atom(S(x)) => match x.as_str() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            _ => Expr::Var(String::from(x)),
        },
        // List of tokens
        Sexp::List(vec) => match &vec[..] {
            // Block
            [Sexp::Atom(S(op)), es @ ..] if op == "block" => {
                if es.len() == 0 {
                    panic!("Invalid")
                }
                Expr::Block(es.into_iter().map(parse_expr).collect())
            }
            // Define
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), e] if op == "define" => {
                Expr::Define(x.to_string(), Box::new(parse_expr(e)))
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
                        "print" => Op1::Print,
                        _ => panic!("Invalid"),
                    },
                    Box::new(parse_expr(e)),
                ),
            },
            // Set!
            [Sexp::Atom(S(op)), Sexp::Atom(S(x)), b] if op == "set!" => {
                Expr::Set(x.to_string(), Box::new(parse_expr(b)))
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
            // Binary Operators
            [Sexp::Atom(S(op)), l, r] => Expr::BinOp(
                match op.as_str() {
                    "+" => Op2::Plus,
                    "-" => Op2::Minus,
                    "*" => Op2::Times,
                    "=" => Op2::Equal,
                    ">" => Op2::Greater,
                    "<" => Op2::Less,
                    ">=" => Op2::GreaterEqual,
                    "<=" => Op2::LessEqual,
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
