
// This pass generates x86-64 assembly from IR.

#![allow(non_upper_case_globals)]

use crate::util::*;
use crate::*;
use std::cell::RefCell;
use std::collections::HashMap;

macro_rules! p {
    ($fmt:expr) => {
        println!($fmt);
    };
    ($fmt:expr,$($x:tt)*) => {
        println!($fmt, $( $x )*);
    };
}

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

thread_local! {
    static ESCAPED: RefCell<HashMap<char,char>> = RefCell::new(HashMap::new());
}

pub const regs: [&'static str; 7] = ["r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub const regs8: [&'static str; 7] = ["r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub const regs32: [&'static str; 7] = ["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];

pub fn num_regs() -> usize {
    return regs.len();
}

const argregs: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
const argregs8: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const argregs32: [&'static str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];

fn init_escaped() {
    ESCAPED.with(|e| {
        let mut escaped = e.borrow_mut();

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
    })
}

fn escaped(c: char) -> Option<char> {
    ESCAPED.with(|e| {
        let escaped = e.borrow();
        match escaped.get(&c) {
            Some(esc) => Some(*esc),
            None => None,
        }
    })
}

fn backslash_escape(s: & String) -> String {
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
        1 => argregs8[r],
        4 => argregs32[r],
        8 => argregs[r],
        _ => panic!(),
    }
}

fn emit_code(fun: &Function) {
    let ret = format!(".Lend{}", bump_nlabel());

    p!(".text");
    p!(".global {}", fun.name);
    p!("{}:", fun.name);
    emit!("push rbp");
    emit!("mov rbp, rsp");
    emit!("sub rsp, {}", roundup(fun.stacksize, 16));
    emit!("push r12");
    emit!("push r13");
    emit!("push r14");
    emit!("push r15");

    for i in 0..fun.ir.len() {
        let ir = &fun.ir[i as usize];
        let lhs = ir.lhs;
        let rhs = ir.rhs;

        match ir.op {
            IRType::IMM => {
                emit!("mov {}, {}", regs[lhs as usize], rhs);
            }
            IRType::BPREL => {
                emit!("lea {}, [rbp{}]", regs[lhs as usize], rhs);
            }
            IRType::MOV => {
                emit!("mov {}, {}", regs[lhs as usize], regs[rhs as usize]);
            }
            IRType::RETURN => {
                emit!("mov rax, {}", regs[lhs as usize]);
                emit!("jmp {}", ret);
            }
            IRType::CALL => {
                for i in 0..ir.nargs {
                    emit!("mov {}, {}", argregs[i], regs[ir.args[i] as usize]);
                }

                emit!("push r10");
                emit!("push r11");
                emit!("mov rax, 0");
                emit!("call {}", ir.name);
                emit!("pop r11");
                emit!("pop r10");

                emit!("mov {}, rax", regs[lhs as usize]);
            }
            IRType::LABEL => {
                p!(".L{}:", lhs);
            }
            IRType::LABEL_ADDR => {
                emit!("lea {}, {}", regs[lhs as usize], ir.name);
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
                emit!("and {}, {}", regs[lhs as usize], regs[rhs as usize]);
            }
            IRType::OR => {
                emit!("or {}, {}", regs[lhs as usize], regs[rhs as usize]);
            }
            IRType::XOR => {
                if ir.is_imm {
                    emit!("xor {}, {}", regs[lhs as usize], rhs);
                } else {
                    emit!("xor {}, {}", regs[lhs as usize], regs[rhs as usize]);
                }
            }
            IRType::SHL => {
                emit!("mov cl, {}", regs8[rhs as usize]);
                emit!("shl {}, cl", regs[lhs as usize]);
            }
            IRType::SHR => {
                emit!("mov cl, {}", regs8[rhs as usize]);
                emit!("shr {}, cl", regs[lhs as usize]);
            }
            IRType::JMP => {
                emit!("jmp .L{}", lhs);
            }
            IRType::IF => {
                emit!("cmp {}, 0", regs[lhs as usize]);
                emit!("jne .L{}", rhs);
            }
            IRType::UNLESS => {
                emit!("cmp {}, 0", regs[lhs as usize]);
                emit!("je .L{}", rhs);
            }
            IRType::LOAD => {
                emit!("mov {}, [{}]", reg(lhs as usize, ir.size), regs[rhs as usize]);
                if ir.size == 1 {
                    emit!("movzb {}, {}", regs[lhs as usize], regs8[lhs as usize]);
                }
            }
            IRType::STORE => {
                emit!("mov [{}], {}", regs[lhs as usize], reg(rhs as usize, ir.size));
            }
            IRType::STORE_ARG => {
                emit!("mov [rbp{}], {}", lhs, argreg(rhs as usize, ir.size));
            }
            IRType::ADD => {
                if ir.is_imm {
                    emit!("add {}, {}", regs[lhs as usize], rhs);
                } else {
                    emit!("add {}, {}", regs[lhs as usize], regs[rhs as usize]);
                }
            }
            IRType::SUB => {
                if ir.is_imm {
                    emit!("sub {}, {}", regs[lhs as usize], rhs);
                } else {
                    emit!("sub {}, {}", regs[lhs as usize], regs[rhs as usize]);
                }
            }
            IRType::MUL => loop {
                if !ir.is_imm {
                    emit!("mov rax, {}", regs[rhs as usize]);
                    emit!("imul {}", regs[lhs as usize]);
                    emit!("mov {}, rax", regs[lhs as usize]);
                    break;
                }

                if rhs < 256 && rhs.count_ones() == 1 {
                    emit!("shl {}, {}", regs[lhs as usize], rhs.trailing_zeros());
                    break;
                }

                emit!("mov rax, {}", rhs);
                emit!("imul {}", regs[lhs as usize]);
                emit!("mov {}, rax", regs[lhs as usize]);
                break;
            }
            IRType::DIV => {
                emit!("mov rax, {}", regs[lhs as usize]);
                emit!("cqo");
                emit!("idiv {}", regs[rhs as usize]);
                emit!("mov {}, rax", regs[lhs as usize]);
            }
            IRType::MOD => {
                emit!("mov rax, {}", regs[lhs as usize]);
                emit!("cqo");
                emit!("idiv {}", regs[rhs as usize]);
                emit!("mov {}, rdx", regs[lhs as usize]);
            }
            IRType::NOP => {}
            ref i => {
                panic!("unknown operator {:?}", i);
            }
        }
    }
    p!("{}:", ret);
    emit!("pop r15");
    emit!("pop r14");
    emit!("pop r13");
    emit!("pop r12");
    emit!("mov rsp, rbp");
    emit!("pop rbp");
    emit!("ret");
}

fn emit_data(var: & Var) {
    if var.data.is_some() {
        p!(".data");
        p!("{}:", var.name);
        emit!(".ascii \"{}\"", backslash_escape(&var.data.clone().unwrap()));
        return;
    }

    p!(".bss");
    p!("{}:", var.name);
    emit!(".zero {}", var.ty.len);
}

pub fn gen_x86(prog: &mut Program) {
    p!(".intel_syntax noprefix");

    for v in prog.gvars.iter() {
        emit_data(&v.borrow());
   }

    for f in prog.funcs.iter() {
        emit_code(f);
    }
}
