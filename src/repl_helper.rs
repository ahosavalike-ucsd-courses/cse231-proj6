use dynasmrt::{dynasm, x64::Assembler, AssemblyOffset, DynamicLabel, DynasmApi, DynasmLabelApi};
use im::{HashMap, HashSet};
use std::mem;

use crate::compiler::*;
use crate::repl::{HEAP_META_SIZE, HEAP_START};
use crate::structs::*;

pub extern "C" fn snek_error_exit(errcode: i64) {
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

pub extern "C" fn snek_error_print(errcode: i64) {
    // print error message according to writeup
    eprintln!(
        "an error ocurred {errcode}: {}",
        match errcode {
            1 => "invalid representation",
            i if i >= 20 && i <= 29 => "invalid argument",
            i if i >= 30 && i <= 39 => "overflow",
            i if i == 40 => "index out of range",
            i if i == 50 => "divide by zero",
            _ => "",
        }
    );
}

unsafe extern "C" fn snek_try_gc(count: isize, curr_rbp: *const u64, curr_rsp: *const u64) {
    let heap_end = *HEAP_START.add(1) as *const u64;
    let new_heap_ptr = snek_gc(curr_rbp, curr_rsp);
    if heap_end.offset_from(new_heap_ptr) < count {
        eprintln!("out of memory");
        std::process::exit(5)
    }
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

enum StackIterType {
    ValIter,
    RetPtrIter,
    EntryIter,
}

#[derive(Clone)]
struct Stack {
    init_rbp: *const u64,
    rsp_top: *const u64,
    rsp_base: *const u64,
}

impl Stack {
    unsafe fn new(init_rbp: *const u64, rsp_top: *const u64, rsp_base: *const u64) -> Stack {
        Stack {
            init_rbp,
            rsp_top,
            rsp_base,
        }
    }

    unsafe fn iter(&self) -> StackIter {
        StackIter::new(self, StackIterType::EntryIter)
    }
    unsafe fn iter_val(&self) -> StackIter {
        StackIter::new(self, StackIterType::ValIter)
    }
    unsafe fn iter_retptr(&self) -> StackIter {
        StackIter::new(self, StackIterType::RetPtrIter)
    }
}

// impl Iterator for Stack {
//     type Item = (*const u64, u64);

//     fn next(&mut self) -> Option<Self::Item> {
//         unsafe { self.iter().next() }
//     }
// }

struct StackIter {
    stack: Stack,

    curr_rbp: *const u64,
    stack_ptr: *const u64,
    curr_rsp: *const u64,
    si: u64,
    iter_type: StackIterType,
}

impl StackIter {
    unsafe fn new(stack: &Stack, iter_type: StackIterType) -> StackIter {
        StackIter {
            stack: stack.clone(),

            curr_rbp: stack.init_rbp,
            stack_ptr: stack
                .rsp_top
                .add(if let StackIterType::ValIter = iter_type {
                    1
                } else {
                    0
                }),
            curr_rsp: stack.rsp_top,
            si: *stack.rsp_top,
            iter_type,
        }
    }

    unsafe fn next_val(&mut self) -> Option<(*const u64, u64)> {
        if self.stack_ptr.offset_from(self.curr_rsp) >= self.si as isize
        // || self.stack_ptr.offset_from(self.curr_rbp) <= 0
        {
            // Skip rbp and ret ptr
            self.curr_rsp = self.curr_rbp.add(2);
            // Skip si
            self.stack_ptr = self.curr_rbp.add(3);
            self.curr_rbp = *self.curr_rbp as *const u64;

            // Consume si
            self.si = *self.stack_ptr;
        }

        if self.stack.rsp_base.offset_from(self.stack_ptr) <= 0 {
            return None;
        }

        let val = (self.stack_ptr, *self.stack_ptr);
        self.stack_ptr = self.stack_ptr.add(1);
        Some(val)
    }

    unsafe fn next_retptr(&mut self) -> Option<(*const u64, u64)> {
        // Ignore the final return pointer to main
        if self.curr_rbp.offset_from(self.stack.rsp_base) >= -1 {
            None
        } else {
            let ret_ptr = self.curr_rbp.add(1);
            let val = (ret_ptr, *ret_ptr);
            self.curr_rbp = *self.curr_rbp as *const u64;
            Some(val)
        }
    }

    unsafe fn next_entry(&mut self) -> Option<(*const u64, u64)> {
        if self.stack_ptr.offset_from(self.stack.rsp_base) > 0 {
            None
        } else {
            let val = (self.stack_ptr, *self.stack_ptr);
            self.stack_ptr = self.stack_ptr.add(1);
            Some(val)
        }
    }
}

impl Iterator for StackIter {
    type Item = (*const u64, u64);

    fn next(&mut self) -> Option<Self::Item> {
        unsafe {
            match self.iter_type {
                StackIterType::ValIter => self.next_val(),
                StackIterType::RetPtrIter => self.next_retptr(),
                StackIterType::EntryIter => self.next_entry(),
            }
        }
    }
}

// This function traverses the stack frames to gather the root set
unsafe fn root_set(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> Vec<(*const u64, *const u64)> {
    let mut set = vec![];
    let stack = Stack::new(curr_rbp, curr_rsp, stack_base);
    let upper = *HEAP_START as *const u64;
    for (stack_ptr, val) in stack.iter_val() {
        if val & 3 == 1 && val != 1 {
            let ptr = val as *const u64;

            if ptr.offset_from(HEAP_START) > 0 && upper.offset_from(ptr) > 0 {
                set.push((stack_ptr, (val - 1) as *const u64));
            }
        }
    }
    set
}

// This function marks all references to heap given a starting heap value
unsafe fn mark(ele: *mut u64) {
    if *ele == 1 {
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
        if *from == 1 {
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
    // Modify the heap pointer metadata
    *(HEAP_START as *mut u64) = r15 as u64;
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
    let stack = Stack::new(curr_rsp, curr_rsp, stack_base);
    println!("-----------------------------------------");
    for (ptr, val) in stack.iter() {
        println!("{ptr:?}: {val:#0x}");
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
    dynasm!(ops
        ; .arch x64
        ; mov rsp, [r15 + 16]
        ; pop rbp
        ; jmp rax
        ; ret
    );

    let snek_print_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_print")), snek_print_lbl);
    dynasm!(ops
        ; .arch x64
        ; =>snek_print_lbl
        ; mov rax, QWORD print_result as _
        ; jmp rax
        ; ret
    );

    let snek_deep_equal_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_deep_equal")), snek_deep_equal_lbl);
    dynasm!(ops
        ; .arch x64
        ; =>snek_deep_equal_lbl
        ; mov rax, QWORD deep_equal as _
        ; jmp rax
        ; ret
    );

    let snek_try_gc_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_try_gc")), snek_try_gc_lbl);
    dynasm!(ops
        ; .arch x64
        ; =>snek_try_gc_lbl
        ; mov rax, QWORD snek_try_gc as _
        ; jmp rax
        ; ret
    );
    let snek_gc_lbl = ops.new_dynamic_label();
    lbls.insert(Label::new(Some("snek_gc")), snek_gc_lbl);
    dynasm!(ops
        ; .arch x64
        ; =>snek_gc_lbl
        ; mov rax, QWORD snek_gc as _
        ; jmp rax
        ; ret
    );
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

extern "C" fn deep_equal(l: i64, r: i64) -> i64 {
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
    rbp: *const u64,
    rsp: *const u64,
) {
    let old_ptr_start: i64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(AssemblyOffset(0))) }
    };
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
        // -1 to skip Rbp, + 1 to skip the top word
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

        com.depth = f.depth;
        instrs.extend(vec![
            Instr::LabelI(Label::new(Some(&format!("fns_{name}")))),
            Instr::Push(Reg::Rbp),
            Instr::Mov(MovArgs::ToReg(Reg::Rbp, Arg64::OReg(Reg::Rsp))),
            Instr::Sub(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
        ]);
        instrs.extend(compile_expr(&body, &co, com));
        instrs.extend(vec![
            Instr::Add(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
            Instr::Pop(Reg::Rbp),
            Instr::Ret,
        ]);

        // Compile Fast
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
            co.env.insert(
                v.to_string(),
                // Top word is for co.si, so i+1
                VarEnv::new(i as i32 + 1, Some(arg_types[i]), false),
            );
        }

        com.depth = f.depth;
        instrs.extend(vec![
            Instr::LabelI(Label::new(Some(&format!("fnf_{name}")))),
            Instr::Push(Reg::Rbp),
            Instr::Mov(MovArgs::ToReg(Reg::Rbp, Arg64::OReg(Reg::Rsp))),
            Instr::Sub(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
        ]);
        instrs.extend(compile_expr(&body, &co, com));
        instrs.extend(vec![
            Instr::Add(MovArgs::ToReg(Reg::Rsp, Arg64::Imm(com.depth * 8))),
            Instr::Pop(Reg::Rbp),
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

    // Fix the call stack if the ops was relocated
    let new_ptr_start: i64 = {
        let reader = ops.reader();
        let buf = reader.lock();
        unsafe { mem::transmute(buf.ptr(AssemblyOffset(0))) }
    };

    let diff = new_ptr_start - old_ptr_start;
    // Closure to fix the return pointers on the stack
    let fix = |old: *const u64| unsafe {
        if diff > 0 {
            *old + diff as u64
        } else {
            *old - (-diff) as u64
        }
    };
    if diff != 0 {
        unsafe {
            let base = *HEAP_START.add(2) as *const u64;

            // Modify rust function_compile_runtime's return
            let runtime_ret = rsp.sub(1);
            *(runtime_ret as *mut u64) = fix(runtime_ret);

            // Modify stack references
            let stack = Stack::new(rbp, rsp, base);
            for (ptr, _) in stack.iter_retptr() {
                *(ptr as *mut u64) = fix(ptr);
            }
        }
    }
}
