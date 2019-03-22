
// This pass generates x86-64 assembly from IR.

#![allow(non_upper_case_globals)]

use crate::regalloc::*;
use crate::util::*;
use crate::*;
use std::collections::HashMap;

lazy_static! {
    static ref LABEL: Mutex<usize> = Mutex::new(0);
    static ref ESCAPED: Mutex<HashMap<char,char>> = Mutex::new(HashMap::new());
}

const argreg8: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const argreg32: [&'static str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
const argreg64: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

fn label() -> usize {
    *LABEL.lock().unwrap()
}

fn init_escaped() {
    let mut escaped = ESCAPED.lock().unwrap();
    if escaped.len() > 0 {
        return;
    }

    escaped.insert(char::from(8), 'b');  // \b
    escaped.insert(char::from(12), 'f'); // \f
    escaped.insert(char::from(10), 'n'); // \n
    escaped.insert(char::from(13), 'r'); // \r
    escaped.insert(char::from(9), 't');  // \t
    escaped.insert('\\', '\\');
    escaped.insert('\'', '\'');
    escaped.insert('\"', '"');
}

fn escaped(c: char) -> Option<char> {
    let mut ret = None;
    match ESCAPED.lock() {
        Ok(escaped) => {
            match escaped.get(&c) {
                Some(esc) => {
                    ret = Some(*esc);
                }
                None => { }
            }
        }
        Err(_) => { }
    }
    return ret;
}

fn inc_label() {
    let mut label = LABEL.lock().unwrap();
    *label += 1;
}

fn escape(s: & String) -> String {
    init_escaped();

    let mut buf = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if let Some(esc) = escaped(c) {
            buf.push('\\');
            buf.push(esc);
        } else if c.is_ascii_graphic() || c == ' ' {
            buf.push(c);
        } else {
            buf.push_str(&format!("\\{:03o}", u32::from(c)));
        }
    }

    //buf.push(char::from(0));
    return buf;
}

fn emit_cmp(ir: &IR, insn: &str) {
    let lhs = ir.lhs as usize;
    let rhs = ir.rhs as usize;

    println!("  cmp {}, {}", regs[lhs], regs[rhs]);
    println!("  {} {}", insn, regs8[lhs]);
    println!("  movzb {}, {}", regs[lhs], regs8[lhs]);
}

fn gen(fun: &IR) {
    let ret = format!(".Lend{}", label());
    inc_label();

    println!(".global {}", fun.name);
    println!("{}:", fun.name);
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, {}", roundup(fun.stacksize, 16));
    println!("  push r12");
    println!("  push r13");
    println!("  push r14");
    println!("  push r15");

    for i in 0..fun.ir.len() {
        let ir = &fun.ir[i as usize];
        let lhs = ir.lhs as usize;
        let rhs = ir.rhs as usize;

        match ir.op {
            IRType::IMM => {
                println!("  mov {}, {}", regs[lhs], rhs);
            }
            IRType::BPREL => {
                println!("  lea {}, [rbp-{}]", regs[lhs], rhs);
            }
            IRType::MOV => {
                println!("  mov {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::RETURN => {
                println!("  mov rax, {}", regs[lhs]);
                println!("  jmp {}", ret);
            }
            IRType::CALL => {
                for i in 0..ir.nargs {
                    println!("  mov {}, {}", argreg64[i], regs[ir.args[i] as usize]);
                }

                println!("  push r10");
                println!("  push r11");
                println!("  mov rax, 0");
                println!("  call {}", ir.name);
                println!("  pop r11");
                println!("  pop r10");

                println!("  mov {}, rax", regs[lhs]);
            }
            IRType::LABEL => {
                println!(".L{}:", lhs);
            }
            IRType::LABEL_ADDR => {
                println!("  lea {}, {}", regs[lhs], ir.name);
            }
            IRType::NEG => {
                println!("  neg {}", regs[lhs]);
            }
            IRType::EQ => {
                emit_cmp(ir, "sete")
            }
            IRType::NE => {
                emit_cmp(ir, "setne")
            }
            IRType::LT => {
                emit_cmp(ir, "setl")
            }
            IRType::LE => {
                emit_cmp(ir, "setle")
            }
            IRType::AND => {
                println!("  and {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::OR => {
                println!("  or {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::XOR => {
                println!("  xor {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::SHL => {
                println!("  mov cl, {}", regs8[rhs]);
                println!("  shl {}, cl", regs[lhs]);
            }
            IRType::SHR => {
                println!("  mov cl, {}", regs8[rhs]);
                println!("  shr {}, cl", regs[lhs]);
            }
            IRType::JMP => {
                println!("  jmp .L{}", lhs);
            }
            IRType::IF => {
                println!("  cmp {}, 0", regs[lhs]);
                println!("  jne .L{}", rhs);
            }
            IRType::UNLESS => {
                println!("  cmp {}, 0", regs[lhs]);
                println!("  je .L{}", rhs);
            }
            IRType::LOAD8 => {
                println!("  mov {}, [{}]", regs8[lhs], regs[rhs]);
                println!("  movzb {}, {}", regs[lhs], regs8[lhs]);
            }
            IRType::LOAD32 => {
                println!("  mov {}, [{}]", regs32[lhs], regs[rhs]);
            }
            IRType::LOAD64 => {
                println!("  mov {}, [{}]", regs[lhs], regs[rhs]);
            }
            IRType::STORE8 => {
                println!("  mov [{}], {}", regs[lhs], regs8[rhs]);

                // fall through in 9cc
                println!("  mov [{}], {}", regs[lhs], regs32[rhs]);
            }
            IRType::STORE32 => {
                println!("  mov [{}], {}", regs[lhs], regs32[rhs]);
            }
            IRType::STORE64 => {
                println!("  mov [{}], {}", regs[lhs], regs[rhs]);
            }
            IRType::STORE8_ARG => {
                println!("  mov [rbp-{}], {}", lhs, argreg8[rhs]);
            }
            IRType::STORE32_ARG => {
                println!("  mov [rbp-{}], {}", lhs, argreg32[rhs]);
            }
            IRType::STORE64_ARG => {
                println!("  mov [rbp-{}], {}", lhs, argreg64[rhs]);
            }
            IRType::ADD => {
                println!("  add {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::ADD_IMM => {
                println!("  add {}, {}", regs[lhs], rhs);
            }
            IRType::SUB => {
                println!("  sub {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::SUB_IMM => {
                println!("  sub {}, {}", regs[lhs], rhs);
            }
            IRType::MUL => {
                println!("  mov rax, {}", regs[rhs]);
                println!("  mul {}", regs[lhs]);
                println!("  mov {}, rax", regs[lhs]);
            }
            IRType::MUL_IMM => {
                println!("  mov rax, {}", rhs);
                println!("  mul {}", regs[lhs]);
                println!("  mov {}, rax", regs[lhs]);
            }
            IRType::DIV => {
                println!(" mov rax, {}", regs[lhs]);
                println!(" cqo");
                println!(" div {}", regs[rhs]);
                println!(" mov {}, rax", regs[lhs]);
            }
            IRType::MOD => {
                println!("  mov rax, {}", regs[lhs]);
                println!("  cqo");
                println!("  div {}", regs[rhs]);
                println!("  mov {}, rdx", regs[lhs]);
            }
            IRType::NOP => {}
            ref i => {
                panic!(format!("unknown operator {:?}", i));
            }
        }
    }
    println!("{}:", ret);
    println!("  pop r15");
    println!("  pop r14");
    println!("  pop r13");
    println!("  pop r12");
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}


pub fn gen_x86(globals: Vec<Var>, fns: &Vec<IR>) {
    println!(".intel_syntax noprefix");

    println!(".data");
    for var in globals.iter() {
        if var.is_extern {
            continue;
        }
        println!("{}:", var.name);
        println!("  .ascii \"{}\"", escape(&var.data));
    }

    println!(".text");
    for i in 0..fns.len() {
        gen(&fns[i]);
    }
}
