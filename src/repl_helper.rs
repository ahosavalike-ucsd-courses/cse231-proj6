use dynasmrt::{dynasm, x64::Assembler, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::HashMap;

use crate::compiler::*;
use crate::structs::*;
use crate::repl::{HEAP_START, HEAP_META_SIZE};
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

unsafe fn snek_try_gc(count: isize, curr_rbp: *const u64, curr_rsp: *const u64) {
    let heap_end = *HEAP_START.add(1) as *const u64;
    let new_heap_ptr = snek_gc(curr_rbp, curr_rsp);
    if heap_end.offset_from(new_heap_ptr) < count {
        eprintln!("out of memory");
        std::process::exit(5)
    }
    *(HEAP_START.add(1) as *mut u64) = new_heap_ptr as u64;
}

unsafe fn snek_gc(curr_rbp: *const u64, curr_rsp: *const u64) -> *const u64 {
    let heap_ptr = *HEAP_START as *const u64;
    let stack_base = *HEAP_START.add(2) as *const u64;
    let stack_refs = root_set(stack_base, curr_rbp, curr_rsp);
    // 1. Mark
    for (_, heap_addr) in &stack_refs {
        mark(*heap_addr as *mut u64);
    }
    // 2. Forward headers
    forward_headers(heap_ptr);
    // 3. Forward stack and heap internal references
    for (stack_ptr, heap_addr) in stack_refs {
        *(stack_ptr as *mut u64) = *heap_addr | 1;
        forward_heap_refs(heap_addr as *mut u64);
    }
    // 4. Compact and return
    compact(heap_ptr)
}

// This function traverses the stack frames to gather the root set
unsafe fn root_set(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> Vec<(*const u64, *const u64)> {
    let mut curr_rbp = curr_rbp;
    let mut stack_ptr = curr_rsp;
    let mut set = vec![];
    while stack_base.offset_from(stack_ptr) > 0 {
        while curr_rbp.offset_from(stack_ptr) > 1 {
            match *stack_ptr {
                x if x & 3 == 1 && x != 1 => {
                    set.push((stack_ptr, (x - 1) as *const u64));
                }
                _ => (),
            }
            stack_ptr = stack_ptr.add(1);
        }
        curr_rbp = *stack_ptr.sub(1) as *const u64;
        // Skip rbp and ret ptr
        stack_ptr = stack_ptr.add(2);
    }
    set
}

// This function marks all references to heap given a starting heap value
unsafe fn mark(ele: *mut u64) {
    if *ele & 1 == 1 {
        return;
    }
    // Mark
    *ele = 1;
    // Iterate over data structure
    for i in 0..*ele.add(1) as usize {
        let entry = ele.add(i + 2) as *mut u64;
        if *entry & 3 == 1 && *entry != 1 {
            // Mark any references
            let next = (*entry - 1) as *mut u64;
            mark(next);
        }
    }
}

unsafe fn forward_headers(heap_ptr: *const u64) {
    let mut from = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    let mut to = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    while heap_ptr.offset_from(from) > 0 {
        // If from is marked as live
        if *from & 1 == 1 {
            // Fill src GC word with dst address
            *from = to as u64;
            to = to.add(*from.add(1) as usize + 2);
        }
        from = from.add(*from.add(1) as usize + 2);
    }
}

unsafe fn forward_heap_refs(heap_addr: *mut u64) {
    if *heap_addr & 1 != 0 {
        return;
    }
    *heap_addr |= 1;
    for i in 0..*heap_addr.add(1) as usize {
        let entry = heap_addr.add(i + 2) as *mut u64;
        if *entry & 3 == 1 && *entry != 1 {
            let old_entry = (*entry - 1) as *mut u64;
            *entry = *old_entry | 1;
            forward_heap_refs(old_entry);
        }
    }
}

unsafe fn compact(heap_ptr: *const u64) -> *const u64 {
    let mut ptr = HEAP_START.add(HEAP_META_SIZE) as *mut u64;
    let mut r15 = HEAP_START.add(HEAP_META_SIZE);
    while heap_ptr.offset_from(ptr) > 0 {
        // +2 to include metadata
        let len = *ptr.add(1) as usize + 2;
        // If GC word is zero, garbage, skip
        if *ptr == 0 {
            ptr = ptr.add(len);
            continue;
        }
        // ptr will always be tagged (through forward_heap_refs)
        // to be safe tag and remove
        let dst = ((*ptr | 1) - 1) as *mut u64;
        // No need to copy if dst is same as src
        if ptr.offset_from(dst) != 0 {
            for i in 1..len {
                *dst.add(i) = *ptr.add(i);
            }
        }
        // Clear GC word
        *dst = 0;
        ptr = ptr.add(len);
        r15 = dst.add(len);
    }
    r15
}

#[allow(dead_code)]
unsafe fn print_heap(heap_ptr: Option<*const u64>) {
    let heap_ptr = if heap_ptr.is_some() {
        heap_ptr.unwrap()
    } else {
        *HEAP_START as *const u64
    };
    println!("Heap: ");
    let mut ptr = HEAP_START;
    println!("{ptr:p}: {:#x} {:#x} {:#x}", *ptr, *ptr.add(1), *ptr.add(2));

    ptr = ptr.add(3);
    let mut next = ptr.add(*ptr.add(1) as usize + 2);
    print!("{ptr:p}: ");
    while heap_ptr.offset_from(ptr) > 0 {
        print!("{:#x} ", *ptr);
        ptr = ptr.add(1);

        if next.offset_from(ptr) <= 0 {
            println!("");
            next = ptr.add(*ptr.add(1) as usize + 2);
            print!("{ptr:p}: ");
        }
    }
    println!("End heap");
}

#[allow(dead_code)]
unsafe fn snek_print_stack(curr_rsp: *const u64) {
    let stack_base = *HEAP_START.add(2) as *const u64;
    let mut ptr = curr_rsp;
    println!("-----------------------------------------");
    while ptr <= stack_base {
        let val = *ptr;
        println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.add(1);
    }
    println!("-----------------------------------------");
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

    let snek_try_gc_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_try_gc")), snek_try_gc_lbl);
    dynasm!(ops; .arch x64; =>snek_try_gc_lbl; mov rax, QWORD snek_try_gc as _; call rax; ret);
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
    let lc = unsafe { *la.add(1) } as isize;
    let rc = unsafe { *ra.add(1) } as isize;
    // Check length
    if lc != rc {
        return false;
    }
    for i in 0..lc {
        let ln = unsafe { *la.offset(i + 2) };
        let rn = unsafe { *ra.offset(i + 2) };
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
    } else if val & 1 == 1 {
        if seen.contains(&val) {
            return "(list <cyclic>)".to_string();
        }
        seen.insert(val);
        let addr = (val - 1) as *const i64;
        let count = unsafe { *addr.add(1) } as usize;
        let mut v: Vec<i64> = vec![0; count];
        for i in 0..count {
            v[i] = unsafe { *addr.add(i + 2) };
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

        com.depth = f.depth;
        instrs.extend(vec![
            Instr::LabelI(Label::new(Some(&format!("fns_{name}")))),
            Instr::Mov(MovArgs::ToMem(
                MemRef {
                    reg: Reg::Rsp,
                    offset: -1,
                },
                Arg64::OReg(Reg::Rbp),
            )),
            Instr::Mov(MovArgs::ToReg(Reg::Rbp, Arg64::OReg(Reg::Rsp))),
            Instr::Sub(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
        ]);
        instrs.extend(compile_expr(&body, &co, com));
        instrs.extend(vec![
            Instr::Add(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
            Instr::Mov(MovArgs::ToReg(
                Reg::Rbp,
                Arg64::Mem(MemRef {
                    reg: Reg::Rsp,
                    offset: -1,
                }),
            )),
            Instr::Ret,
        ]);

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

        com.depth = f.depth;
        instrs.extend(vec![
            Instr::LabelI(Label::new(Some(&format!("fnf_{name}")))),
            Instr::Mov(MovArgs::ToMem(
                MemRef {
                    reg: Reg::Rsp,
                    offset: -1,
                },
                Arg64::OReg(Reg::Rbp),
            )),
            Instr::Mov(MovArgs::ToReg(Reg::Rbp, Arg64::OReg(Reg::Rsp))),
            Instr::Sub(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
        ]);
        instrs.extend(compile_expr(&body, &co, com));
        instrs.extend(vec![
            Instr::Add(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
            Instr::Mov(MovArgs::ToReg(
                Reg::Rbp,
                Arg64::Mem(MemRef {
                    reg: Reg::Rsp,
                    offset: -1,
                }),
            )),
            Instr::Ret,
        ]);

        
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
