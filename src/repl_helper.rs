use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::HashMap;

use crate::compiler::*;
use crate::structs::*;
use im::HashSet;

pub fn snek_error_exit(errcode: i64) {
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

pub fn snek_error_print(errcode: i64) {
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

pub fn add_interface_calls(
    ops: &mut Assembler,
    lbls: &mut HashMap<Label, DynamicLabel>,
    exit: bool,
) {
    let snek_error_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_error_stub")), snek_error_lbl);
    dynasm!(ops; .arch x64; =>snek_error_lbl);
    if exit {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_exit as _);
    } else {
        dynasm!(ops; .arch x64; mov rax, QWORD snek_error_print as _);
    }
    dynasm!(ops;
        .arch x64;
        mov rsp, [r15 + 8];
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

pub fn parse_input(input: &str) -> (i64, Option<Type>) {
    // parse the input string into internal value representation
    match input {
        "true" => (TRUE_VAL, Some(Type::Bool)),
        "false" | "" => (FALSE_VAL, Some(Type::Bool)),
        _ => (
            (input.parse::<i64>().expect("Invalid") as i64) << 1,
            Some(Type::Int),
        ),
    }
}

pub fn deep_equal_recurse(l: i64, r: i64, seen: &mut HashSet<(i64, i64)>) -> bool {
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

pub fn deep_equal(l: i64, r: i64) -> i64 {
    if deep_equal_recurse(l, r, &mut HashSet::new()) {
        TRUE_VAL
    } else {
        FALSE_VAL
    }
}

pub fn snek_str(val: i64, seen: &mut HashSet<i64>) -> String {
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

pub fn print_result(result: i64) -> i64 {
    println!("{}", snek_str(result, &mut HashSet::new()));
    return result;
}

pub fn asm_repl_func_defn(
    ops: &mut Assembler,
    com: &mut ContextMut,
    lbls: &mut HashMap<Label, DynamicLabel>,
    f: &FunDefEnv,
    arg_types: &Vec<Type>,
) {
    let stub = ops.new_dynamic_label();
    lbls.insert(Label::new(Some(&format!("fun_{}", f.name))), stub);
    let fast = *lbls
        .get(&Label::new(Some(&format!("fnf_{}", f.name))))
        .unwrap();
    let slow = *lbls
        .get(&Label::new(Some(&format!("fns_{}", f.name))))
        .unwrap();

    // Compile new Stub
    dynasm!(ops; .arch x64; => stub);
    for (i, arg) in arg_types.iter().enumerate() {
        let i = (i as i32 - f.depth) * 8;
        match arg {
            Type::Int => {
                dynasm!(ops; .arch x64; test [rsp+i], 1; jnz =>slow);
            }
            Type::Bool => {
                dynasm!(ops
                    ; .arch x64
                    ; mov rax, [rsp+i]
                    ; and rax, 3
                    ; cmp rax, 3
                    ; jne =>slow
                );
            }
            Type::List => {
                dynasm!(ops; .arch x64; mov rax, [rsp+i]; and rax, 3; cmp rax, 1; jne =>slow);
            }
        }
    }
    dynasm!(ops; .arch x64; jmp =>fast);

    if let Expr::FnDefn(name, vars, body) = &f.defn {
        // Compile Slow
        let mut instrs = vec![];
        let mut co = Context::new(None)
            .modify_si(vars.len() as i32)
            // Function body is tail position
            .modify_tail(true);

        for (i, v) in vars.iter().enumerate() {
            let existing = co.env.get(v.as_str());
            if existing.is_some() && !existing.unwrap().defined {
                panic!("duplicate parameter binding in definition");
            }
            co.env
                .insert(v.to_string(), VarEnv::new(i as i32, None, false));
        }

        instrs.push(Instr::LabelI(Label::new(Some(&format!("fns_{name}")))));
        instrs.push(Instr::Sub(MovArgs::ToReg(
            Reg::Rsp,
            Arg64::Imm(f.depth * 8),
        )));
        com.depth = f.depth;
        instrs.extend(compile_expr(&body, &co, com));
        instrs.push(Instr::Add(MovArgs::ToReg(
            Reg::Rsp,
            Arg64::Imm(f.depth * 8),
        )));
        instrs.push(Instr::Ret);

        // Compile Fast
        let mut co = Context::new(None)
            .modify_si(vars.len() as i32)
            // Function body is tail position
            .modify_tail(true);

        for (i, v) in vars.iter().enumerate() {
            let existing = co.env.get(v.as_str());
            if existing.is_some() && !existing.unwrap().defined {
                panic!("duplicate parameter binding in definition");
            }
            co.env.insert(
                v.to_string(),
                VarEnv::new(i as i32, Some(arg_types[i]), false),
            );
        }

        instrs.push(Instr::LabelI(Label::new(Some(&format!("fnf_{name}")))));
        instrs.push(Instr::Sub(MovArgs::ToReg(
            Reg::Rsp,
            Arg64::Imm(f.depth * 8),
        )));
        com.depth = f.depth;
        instrs.extend(compile_expr(&body, &co, com));
        instrs.push(Instr::Add(MovArgs::ToReg(
            Reg::Rsp,
            Arg64::Imm(f.depth * 8),
        )));
        instrs.push(Instr::Ret);
        
        // Assemble both the fast and slow versions
        instrs_to_asm(&instrs, ops, lbls);
    } else {
        panic!("Compiling wrong thing here: {:?}", f.defn);
    }

    ops.commit().unwrap();

    // Alter original stub to jump into new stub
    ops.alter(|ops| {
        ops.goto(f.orig);
        dynasm!(ops; .arch x64; jmp =>stub);
    })
    .unwrap();
}
