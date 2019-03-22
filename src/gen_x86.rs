
// This pass generates x86-64 assembly from IR.

#![allow(non_upper_case_globals)]

use crate::regalloc::*;
use crate::util::*;
use crate::*;
use std::collections::HashMap;

macro_rules! emit {
    ($fmt:expr) => {
        print!("\t");
        println!($fmt);
    };
    ($fmt:expr,$($x:tt)*) => {
        print!("\t");
        println!($fmt, $( $x )*);
    };
}

lazy_static! {
    static ref NLABEL: Mutex<usize> = Mutex::new(0);
    static ref ESCAPED: Mutex<HashMap<char,char>> = Mutex::new(HashMap::new());
}

fn bump_nlabel() -> usize {
    let mut nlabel = NLABEL.lock().unwrap();
    let ret = *nlabel;
    *nlabel += 1;
    return ret;
}


const argreg8: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const argreg32: [&'static str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
const argreg64: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

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

fn emit_cmp(insn: &str, ir: &IR) {
    let lhs = ir.lhs as usize;
    let rhs = ir.rhs as usize;

    emit!("cmp {}, {}", regs[lhs], regs[rhs]);
    emit!("{} {}", insn, regs8[lhs]);
    emit!("movzb {}, {}", regs[lhs], regs8[lhs]);
}

fn reg(r: usize, size: i32) -> &'static str {
    match size {
        1 => regs8[r],
        4 => regs32[r],
        8 => regs[r],
        _ => panic!(),
    }
}

fn argreg(r: usize, size: i32) -> &'static str {
    match size {
        1 => argreg8[r],
        4 => argreg32[r],
        8 => argreg64[r],
        _ => panic!(),
    }
}

fn gen(fun: &IR) {
    let ret = format!(".Lend{}", bump_nlabel());

    println!(".global {}", fun.name);
    println!("{}:", fun.name);
    emit!("push rbp");
    emit!("mov rbp, rsp");
    emit!("sub rsp, {}", roundup(fun.stacksize, 16));
    emit!("push r12");
    emit!("push r13");
    emit!("push r14");
    emit!("push r15");

    for i in 0..fun.ir.len() {
        let ir = &fun.ir[i as usize];
        let lhs = ir.lhs as usize;
        let rhs = ir.rhs as usize;

        match ir.op {
            IRType::IMM => {
                emit!("mov {}, {}", regs[lhs], rhs);
            }
            IRType::BPREL => {
                emit!("lea {}, [rbp-{}]", regs[lhs], rhs);
            }
            IRType::MOV => {
                emit!("mov {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::RETURN => {
                emit!("mov rax, {}", regs[lhs]);
                emit!("jmp {}", ret);
            }
            IRType::CALL => {
                for i in 0..ir.nargs {
                    emit!("mov {}, {}", argreg64[i], regs[ir.args[i] as usize]);
                }

                emit!("push r10");
                emit!("push r11");
                emit!("mov rax, 0");
                emit!("call {}", ir.name);
                emit!("pop r11");
                emit!("pop r10");

                emit!("mov {}, rax", regs[lhs]);
            }
            IRType::LABEL => {
                println!(".L{}:", lhs);
            }
            IRType::LABEL_ADDR => {
                emit!("lea {}, {}", regs[lhs], ir.name);
            }
            IRType::NEG => {
                emit!("neg {}", regs[lhs]);
            }
            IRType::EQ => {
                emit_cmp("sete", ir)
            }
            IRType::NE => {
                emit_cmp("setne", ir)
            }
            IRType::LT => {
                emit_cmp("setl", ir)
            }
            IRType::LE => {
                emit_cmp("setle", ir)
            }
            IRType::AND => {
                emit!("and {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::OR => {
                emit!("or {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::XOR => {
                emit!("xor {}, {}", regs[lhs], regs[rhs]);
            }
            IRType::SHL => {
                emit!("mov cl, {}", regs8[rhs]);
                emit!("shl {}, cl", regs[lhs]);
            }
            IRType::SHR => {
                emit!("mov cl, {}", regs8[rhs]);
                emit!("shr {}, cl", regs[lhs]);
            }
            IRType::JMP => {
                emit!("jmp .L{}", lhs);
            }
            IRType::IF => {
                emit!("cmp {}, 0", regs[lhs]);
                emit!("jne .L{}", rhs);
            }
            IRType::UNLESS => {
                emit!("cmp {}, 0", regs[lhs]);
                emit!("je .L{}", rhs);
            }
            IRType::LOAD => {
                emit!("mov {}, [{}]", reg(lhs, ir.size), regs[rhs]);
                if ir.size == 1 {
                    emit!("movzb {}, {}", regs[lhs], regs8[lhs]);
                }
            }
            IRType::STORE => {
                emit!("mov [{}], {}", regs[lhs], reg(rhs, ir.size));
            }
            IRType::STORE_ARG => {
                emit!("mov [rbp-{}], {}", lhs, argreg(rhs, ir.size));
            }
            IRType::ADD => {
                if ir.is_imm {
                    emit!("add {}, {}", regs[lhs], rhs);
                } else {
                    emit!("add {}, {}", regs[lhs], regs[rhs]);
                }
            }
            IRType::SUB => {
                if ir.is_imm {
                    emit!("sub {}, {}", regs[lhs], rhs);
                } else {
                    emit!("sub {}, {}", regs[lhs], regs[rhs]);
                }
            }
            IRType::MUL => loop {
                if !ir.is_imm {
                    emit!("mov rax, {}", regs[rhs]);
                    emit!("mul {}", regs[lhs]);
                    emit!("mov {}, rax", regs[lhs]);
                    break;
                }

                if rhs < 256 && rhs.count_ones() == 1 {
                    emit!("shl {}, {}", regs[lhs], rhs.trailing_zeros());
                    break;
                }

                emit!("mov rax, {}", rhs);
                emit!("mul {}", regs[lhs]);
                emit!("mov {}, rax", regs[lhs]);
                break;
            }
            IRType::DIV => {
                emit!("mov rax, {}", regs[lhs]);
                emit!("cqo");
                emit!("div {}", regs[rhs]);
                emit!("mov {}, rax", regs[lhs]);
            }
            IRType::MOD => {
                emit!("mov rax, {}", regs[lhs]);
                emit!("cqo");
                emit!("div {}", regs[rhs]);
                emit!("mov {}, rdx", regs[lhs]);
            }
            IRType::NOP => {}
            ref i => {
                panic!("unknown operator {:?}", i);
            }
        }
    }
    println!("{}:", ret);
    emit!("pop r15");
    emit!("pop r14");
    emit!("pop r13");
    emit!("pop r12");
    emit!("mov rsp, rbp");
    emit!("pop rbp");
    emit!("ret");
}

pub fn gen_x86(globals: Vec<Var>, fns: &Vec<IR>) {
    println!(".intel_syntax noprefix");

    println!(".data");
    for var in globals.iter() {
        if var.is_extern {
            continue;
        }
        println!("{}:", var.name);
        emit!(".ascii \"{}\"", escape(&var.data));
    }

    println!(".text");
    for i in 0..fns.len() {
        gen(&fns[i]);
    }
}
