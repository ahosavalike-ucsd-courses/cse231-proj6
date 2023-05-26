use im::{hashmap, HashMap};
use std::fmt;
use std::fmt::Debug;

use dynasmrt::{dynasm, DynamicLabel, DynasmApi, DynasmLabelApi, AssemblyOffset};

// Parser
#[derive(Clone, Debug)]
pub enum Op1 {
    Add1,
    Sub1,
    IsNum,
    IsBool,
    Print,
}

#[derive(Clone, Debug)]
pub enum Op2 {
    Plus,
    Minus,
    Times,
    Equal,
    DeepEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Index,
}

#[derive(Clone, Debug)]
pub enum Expr {
    Nil,
    Num(i64),
    Boolean(bool),
    Var(String),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Define(String, Box<Expr>),
    UnOp(Op1, Box<Expr>),
    BinOp(Op2, Box<Expr>, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Loop(Box<Expr>),
    List(Vec<Expr>),
    Break(Box<Expr>),
    Set(String, Box<Expr>),
    SetLst(Box<Expr>, Box<Expr>, Box<Expr>),
    Block(Vec<Expr>),
    FnDefn(String, Vec<String>, Box<Expr>),
    FnCall(String, Vec<Expr>),
}

// Compiler
#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Label {
    pub name: String,
    pub index: i32,
}

impl Label {
    pub fn new(s: Option<&str>) -> Label {
        if let Some(s_) = s {
            // Fixed label
            Label {
                name: s_.to_string(),
                index: -1,
            }
        } else {
            // Mutable label
            Label {
                name: "".to_string(),
                index: 0,
            }
        }
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.index >= 0 {
            write!(f, "{}_{}", self.name, self.index)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

#[derive(Clone, Debug)]
pub struct VarEnv {
    pub offset: i32,
    pub vtype: Option<Type>,
    pub defined: bool,
}

impl VarEnv {
    pub fn new(offset: i32, vtype: Option<Type>, defined: bool) -> VarEnv {
        VarEnv {
            offset,
            vtype,
            defined,
        }
    }
}

#[derive(Clone, Debug)]
pub struct FunEnv {
    pub argc: i32,
    pub depth: i32,
}

impl FunEnv {
    pub fn new(argc: i32, depth: i32) -> FunEnv {
        FunEnv { argc, depth }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Type {
    Int,
    Bool,
    List,
}

#[derive(Clone, Debug)]
pub struct ContextMut {
    pub env: HashMap<String, VarEnv>,
    pub label_index: i32,
    pub result_type: Option<Type>,
    pub fns: HashMap<String, FunEnv>,
    pub depth: i32,
    pub curr_heap_ptr: i64,
}

impl ContextMut {
    pub fn new() -> ContextMut {
        ContextMut {
            env: hashmap! {},
            label_index: 0,
            result_type: None,
            fns: hashmap! {},
            depth: 0,
            curr_heap_ptr: 0,
        }
    }
    pub fn index_used(&mut self) {
        self.label_index += 1;
    }
    pub fn new_ce_label(&self, ce: &Context, l: Label) -> Context {
        ce.modify_label(l)
    }
    pub fn label(&self, s: &str) -> Label {
        Label {
            name: s.to_string(),
            index: self.label_index,
        }
    }
    pub fn update_from(&mut self, other: &ContextMut) {
        for (k, v) in other.env.iter() {
            self.env.insert(k.clone(), v.clone());
        }
        self.result_type = other.result_type;
        self.label_index = other.label_index;
    }
}

#[derive(Clone, Debug)]
pub struct Context {
    pub si: i32,
    pub define_stack: Option<*mut u64>,
    pub dsi: i32,
    pub env: HashMap<String, VarEnv>,
    pub label: Label,
    pub target: Option<MemRef>,
    pub tail: bool,
}

impl Context {
    pub fn new(define_stack: Option<*mut u64>) -> Context {
        Context {
            si: 0,
            define_stack,
            dsi: 0,
            env: hashmap! {},
            label: Label::new(None),
            target: None,
            tail: false,
        }
    }
    pub fn modify(
        &self,
        si: Option<i32>,
        env: Option<HashMap<String, VarEnv>>,
        label: Option<Label>,
        target: Option<Option<MemRef>>,
        tail: Option<bool>,
    ) -> Context {
        Context {
            si: si.unwrap_or(self.si),
            define_stack: self.define_stack,
            dsi: self.dsi,
            env: env.unwrap_or(self.env.clone()),
            label: label.unwrap_or(self.label.clone()),
            target: target.unwrap_or(self.target.clone()),
            tail: tail.unwrap_or(self.tail),
        }
    }
    pub fn modify_si(&self, si: i32) -> Context {
        let mut n = self.clone();
        n.si = si;
        n
    }
    pub fn modify_env(&self, env: HashMap<String, VarEnv>) -> Context {
        let mut n = self.clone();
        n.env = env;
        n
    }
    pub fn modify_label(&self, label: Label) -> Context {
        let mut n = self.clone();
        n.label = label;
        n
    }
    pub fn modify_target(&self, target: Option<MemRef>) -> Context {
        let mut n = self.clone();
        n.target = target;
        n
    }
    pub fn modify_tail(&self, tail: bool) -> Context {
        let mut n = self.clone();
        n.tail = tail;
        n
    }
    pub fn src_to_target(&self, src: Arg64) -> MovArgs {
        match &self.target {
            None => ToReg(Rax, src),
            Some(m) => ToMem(m.clone(), src),
        }
    }
    pub fn rax_to_target(&self, instrs: &mut Vec<Instr>) {
        match &self.target {
            None => (),
            Some(m) => instrs.push(Mov(ToMem(m.clone(), OReg(Rax)))),
        }
    }
    pub fn get_define_stack(&self) -> i64 {
        self.define_stack.expect("No heap found") as i64
    }
}

#[derive(Clone, Debug)]
#[allow(dead_code)]
pub enum Jump {
    U(Label),
    NE(Label),
    NZ(Label),
    E(Label),
    Z(Label),
    O(Label),
    G(Label),
    LE(Label),
}

impl fmt::Display for Jump {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Jump::U(x) => write!(f, "jmp {}", x),
            Jump::NE(x) | Jump::NZ(x) => write!(f, "jne {}", x),
            Jump::E(x) | Jump::Z(x) => write!(f, "je {}", x),
            Jump::O(x) => write!(f, "jo {}", x),
            Jump::G(x) => write!(f, "jg {}", x),
            Jump::LE(x) => write!(f, "jle {}", x),
        }
    }
}

impl Jump {
    fn asm(&self, ops: &mut dynasmrt::x64::Assembler, lbls: &mut HashMap<Label, DynamicLabel>) {
        match self {
            Jump::U(l) => dynasm!(ops; .arch x64; jmp =>*lbls.get(l).unwrap()),
            Jump::NE(l) | Jump::NZ(l) => dynasm!(ops; .arch x64; jne =>*lbls.get(l).unwrap()),
            Jump::E(l) | Jump::Z(l) => dynasm!(ops; .arch x64; je =>*lbls.get(l).unwrap()),
            Jump::O(l) => dynasm!(ops; .arch x64; jo =>*lbls.get(l).unwrap()),
            Jump::G(l) => dynasm!(ops; .arch x64; jg =>*lbls.get(l).unwrap()),
            Jump::LE(l) => dynasm!(ops; .arch x64; jle =>*lbls.get(l).unwrap()),
        }
    }
}

#[derive(Debug)]
pub enum CMov {
    E(Reg, Arg64),
    Z(Reg, Arg64),
    G(Reg, Arg64),
    GE(Reg, Arg64),
    L(Reg, Arg64),
    LE(Reg, Arg64),
}

impl fmt::Display for CMov {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CMov::E(r, a) => write!(f, "cmove {r}, {a}"),
            CMov::Z(r, a) => write!(f, "cmovz {r}, {a}"),
            CMov::G(r, a) => write!(f, "cmovg {r}, {a}"),
            CMov::GE(r, a) => write!(f, "cmovge {r}, {a}"),
            CMov::L(r, a) => write!(f, "cmovl {r}, {a}"),
            CMov::LE(r, a) => write!(f, "cmovle {r}, {a}"),
        }
    }
}

impl CMov {
    fn asm(&self, ops: &mut dynasmrt::x64::Assembler) {
        match self {
            CMov::E(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmove Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmove Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmove"),
                Imm64(_) => panic!("cannot use immediate with cmove"),
            },
            CMov::Z(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovz Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovz Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovz"),
                Imm64(_) => panic!("cannot use immediate with cmovz"),
            },
            CMov::G(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovg Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovg Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovg"),
                Imm64(_) => panic!("cannot use immediate with cmovg"),
            },
            CMov::GE(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovge Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovge Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovge"),
                Imm64(_) => panic!("cannot use immediate with cmovge"),
            },
            CMov::L(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovl Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovl Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovl"),
                Imm64(_) => panic!("cannot use immediate with cmovl"),
            },
            CMov::LE(r, a) => match a {
                OReg(o) => dynasm!(ops; .arch x64; cmovle Rq(r.asm()), Rq(o.asm())),
                Mem(m) => {
                    dynasm!(ops; .arch x64; cmovle Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                }
                Imm(_) => panic!("cannot use immediate with cmovle"),
                Imm64(_) => panic!("cannot use immediate with cmovle"),
            },
        }
    }
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub enum Reg {
    Rax,
    Rcx,
    Rbx,
    Rsp,
    Rbp,
    Rsi,
    Rdi,
    R14,
    R15,
}
use Reg::*;

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Rax => write!(f, "rax"),
            Rcx => write!(f, "rcx"),
            Rbx => write!(f, "rbx"),
            Rsp => write!(f, "rsp"),
            Rbp => write!(f, "rbp"),
            Rsi => write!(f, "rsi"),
            Rdi => write!(f, "rdi"),
            R14 => write!(f, "r14"),
            R15 => write!(f, "r15"),
        }
    }
}

impl Reg {
    fn asm(&self) -> u8 {
        // https://corsix.github.io/dynasm-doc/instructions.html#registers
        match self {
            Rax => 0,
            Rcx => 1,
            Rbx => 3,
            Rsp => 4,
            Rbp => 5,
            Rsi => 6,
            Rdi => 7,
            R14 => 14,
            R15 => 15,
        }
    }
}

#[derive(Clone, Debug)]
pub struct MemRef {
    pub reg: Reg,
    pub offset: i32,
}

impl fmt::Display for MemRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "QWORD [{}+{}]", self.reg, self.offset * 8)
    }
}

#[derive(Clone, Debug)]
pub enum Arg64 {
    OReg(Reg),
    Imm(i32),
    Imm64(i64),
    Mem(MemRef),
}
use Arg64::*;

impl fmt::Display for Arg64 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OReg(r) => write!(f, "{r}"),
            Imm(i) => write!(f, "{i}"),
            Imm64(i) => write!(f, "{i}"),
            Mem(m) => write!(f, "{m}"),
        }
    }
}

#[derive(Debug)]
pub enum MovArgs {
    ToReg(Reg, Arg64),
    ToMem(MemRef, Arg64),
}
use MovArgs::*;

impl fmt::Display for MovArgs {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ToMem(r, a) => write!(f, "{r}, {a}"),
            ToReg(m, r) => write!(f, "{m}, {r}"),
        }
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Instr {
    Mov(MovArgs),
    Cmp(MovArgs),
    Test(MovArgs),
    Add(MovArgs),
    Sub(MovArgs),
    CMovI(CMov),
    Push(Reg),
    Pop(Reg),
    And(MovArgs),
    Mul(Reg, Arg64),
    Xor(Reg, Arg64),
    Sar(Reg, i8),
    Sal(Reg, i8),
    LabelI(Label),
    JumpI(Jump),
    Call(Label),
    Ret,
}
use Instr::*;

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Mov(m) => write!(f, "mov {m}"),
            Cmp(m) => write!(f, "cmp {m}"),
            Test(m) => write!(f, "test {m}"),
            Add(m) => write!(f, "add {m}"),
            Sub(m) => write!(f, "sub {m}"),
            And(m) => write!(f, "and {m}"),
            CMovI(c) => write!(f, "{c}"),
            JumpI(j) => write!(f, "{j}"),
            LabelI(l) => write!(f, "{l}:"),
            Push(r) => write!(f, "push {r}"),
            Pop(r) => write!(f, "pop {r}"),
            Mul(r, a) => write!(f, "imul {r}, {a}"),
            Xor(r, a) => write!(f, "xor {r}, {a}"),
            Sar(r, i) => write!(f, "sar {r}, {i}"),
            Sal(r, i) => write!(f, "sal {r}, {i}"),
            Call(l) => write!(f, "call {l}"),
            Ret => write!(f, "ret"),
        }
    }
}

impl Instr {
    pub fn asm(&self, ops: &mut dynasmrt::x64::Assembler, lbls: &mut HashMap<Label, DynamicLabel>) {
        match self {
            Mov(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; mov Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; mov Rq(r.asm()), *n),
                    Imm64(n) => dynasm!(ops; .arch x64; mov Rq(r.asm()), QWORD *n),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; mov Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; mov [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; mov QWORD [Rq(m.reg.asm()) + m.offset*8], *n)
                    }
                    Imm64(n) => match i32::try_from(*n) {
                        Ok(n) => {
                            dynasm!(ops; .arch x64; mov QWORD [Rq(m.reg.asm()) + m.offset*8], n)
                        }
                        Err(_) => {
                            panic!("cannot mov qword to mem")
                            // dynasm!(ops; .arch x64; mov rax, QWORD *n; mov QWORD [Rq(m.reg.asm()) + m.offset*8], rax)
                        }
                    },
                    Mem(_) => panic!("cannot mov from memory to memory"),
                },
            },
            Add(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; add Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; add Rq(r.asm()), *n),
                    Imm64(_) => panic!("cannot add from imm64 to reg"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; add Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; add [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; add QWORD [Rq(m.reg.asm()) + m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot add from imm64 to memory"),
                    Mem(_) => panic!("cannot add from memory to memory"),
                },
            },
            Sub(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; sub Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; sub Rq(r.asm()), *n),
                    Imm64(_) => panic!("cannot sub from imm64 to reg"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; sub Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; sub [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; sub QWORD [Rq(m.reg.asm()) + m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot sub from imm64 to memory"),
                    Mem(_) => panic!("cannot sub from memory to memory"),
                },
            },
            And(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; and Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; and Rq(r.asm()), *n),
                    Imm64(_) => panic!("cannot add from imm64 to reg"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; and Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; and [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; and QWORD [Rq(m.reg.asm()) + m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot and from imm64 to memory"),
                    Mem(_) => panic!("cannot and from memory to memory"),
                },
            },
            Cmp(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; cmp Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; cmp Rq(r.asm()), *n),
                    Imm64(_) => panic!("cannot cmp with imm64"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; cmp Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; cmp [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; cmp QWORD [Rq(m.reg.asm()) + m.offset*8], *n)
                    }
                    Imm64(_) => panic!("cannot cmp with imm64"),
                    Mem(_) => panic!("cannot cmp from memory to memory"),
                },
            },
            Push(r) => dynasm!(ops; .arch x64; push Rq(r.asm())),
            Pop(r) => dynasm!(ops; .arch x64; pop Rq(r.asm())),
            Test(m) => match m {
                ToReg(r, a) => match a {
                    OReg(o) => dynasm!(ops; .arch x64; test Rq(r.asm()), Rq(o.asm())),
                    Imm(n) => dynasm!(ops; .arch x64; test Rq(r.asm()), DWORD *n),
                    Imm64(_) => panic!("cannot test with imm64"),
                    Mem(m) => {
                        dynasm!(ops; .arch x64; test Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8])
                    }
                },
                ToMem(m, a) => match a {
                    OReg(r) => {
                        dynasm!(ops; .arch x64; test [Rq(m.reg.asm()) + m.offset*8], Rq(r.asm()))
                    }
                    Imm(n) => {
                        dynasm!(ops; .arch x64; test QWORD [Rq(m.reg.asm()) + m.offset*8], DWORD *n)
                    }
                    Imm64(_) => panic!("cannot test with imm64"),
                    Mem(_) => panic!("cannot test from memory to memory"),
                },
            },
            CMovI(c) => c.asm(ops),
            Mul(r, a) => match a {
                OReg(or) => dynasm!(ops; .arch x64; imul Rq(r.asm()), Rq(or.asm())),
                Mem(m) => dynasm!(ops; .arch x64; imul Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8]),
                _ => panic!("mul does not support this operand"),
            },
            Xor(r, a) => match a {
                OReg(or) => dynasm!(ops; .arch x64; xor Rq(r.asm()), Rq(or.asm())),
                Mem(m) => dynasm!(ops; .arch x64; xor Rq(r.asm()), [Rq(m.reg.asm()) + m.offset*8]),
                Imm(n) => dynasm!(ops; .arch x64; xor Rq(r.asm()), DWORD *n),
                Imm64(_) => panic!("cannot add with imm64"),
            },
            Sar(r, i) => dynasm!(ops; .arch x64; sar Rq(r.asm()), *i),
            Sal(r, i) => dynasm!(ops; .arch x64; sal Rq(r.asm()), *i),
            LabelI(l) => dynasm!(ops; .arch x64; =>*lbls.get(l).unwrap()),
            JumpI(j) => j.asm(ops, lbls),
            Call(l) => dynasm!(ops; .arch x64; call =>*lbls.get(l).unwrap()),
            Ret => dynasm!(ops; .arch x64; ret),
        }
    }
}

// REPL structs

pub enum CompileResponse {
    Define(String, Vec<Instr>, Option<Type>),
    FnDefn(String, Expr, i32), // name, defn, argc
    Expr(Vec<Instr>),
}

#[derive(Clone, Debug)]
pub struct FunDefEnv {
    pub name: String,
    pub orig: AssemblyOffset,
    pub defn: Expr,
    pub depth: i32,
    pub argc: i32,
}

impl FunDefEnv {
    pub fn new(
        name: String,
        orig: AssemblyOffset,
        defn: Expr,
        depth: i32,
        argc: i32,
    ) -> FunDefEnv {
        FunDefEnv {
            name,
            orig,
            defn,
            depth,
            argc,
        }
    }
}
