#![allow(non_upper_case_globals)]

use crate::regalloc::*;
use crate::*;

lazy_static! {
    static ref LABEL: Mutex<usize> = Mutex::new(0);
}

const argreg8: [&'static str; 6] = ["dil", "sil", "dl", "cl", "r8b", "r9b"];
const argreg32: [&'static str; 6] = ["edi", "esi", "edx", "ecx", "r8d", "r9d"];
const argreg64: [&'static str; 6] = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

fn label() -> usize {
    *LABEL.lock().unwrap()
}

fn inc_label() {
    let mut label = LABEL.lock().unwrap();
    *label += 1;
}

fn escape(s: & String) -> String {
    let mut buf = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
    //let char_bytes = s.as_bytes();
    //for i in 0..len {
        //let c = char::from(char_bytes[i]);
        if c == '\\' || c == '"'{
            buf.push('\\');
            buf.push(c);
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
    println!("  cmp {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
    println!("  {} {}", insn, regs8[ir.lhs as usize]);
    println!("  movzb {}, {}", regs[ir.lhs as usize], regs8[ir.lhs as usize]);
}

fn gen(fun: &IR) {
    let ret = format!(".Lend{}", label());
    inc_label();

    println!(".global {}", fun.name);
    println!("{}:", fun.name);
    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, {}", fun.stacksize);
    println!("  push r12");
    println!("  push r13");
    println!("  push r14");
    println!("  push r15");

    for i in 0..fun.ir.len() {
        let ir = &fun.ir[i as usize];
        match ir.op {
            IRType::IMM => {
                println!("  mov {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::SUB_IMM => {
                println!("  sub {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::MOV => {
                println!("  mov {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::RETURN => {
                println!("  mov rax, {}", regs[ir.lhs as usize]);
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

                println!("  mov {}, rax", regs[ir.lhs as usize]);
            }
            IRType::LABEL => {
                println!(".L{}:", ir.lhs);
            }
            IRType::LABEL_ADDR => {
                println!("  lea {}, {}", regs[ir.lhs as usize], ir.name);
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
            IRType::JMP => {
                println!("  jmp .L{}", ir.lhs);
            }
            IRType::IF => {
                println!("  cmp {}, 0", regs[ir.lhs as usize]);
                println!("  jne .L{}", ir.rhs);
            }
            IRType::UNLESS => {
                println!("  cmp {}, 0", regs[ir.lhs as usize]);
                println!("  je .L{}", ir.rhs);
            }
            IRType::LOAD8 => {
                println!("  mov {}, [{}]", regs8[ir.lhs as usize], regs[ir.rhs as usize]);
                println!("  movzb {}, {}", regs[ir.lhs as usize], regs8[ir.lhs as usize]);
            }
            IRType::LOAD32 => {
                println!(
                    "  mov {}, [{}]",
                    regs32[ir.lhs as usize], regs[ir.rhs as usize]
                );
            }
            IRType::LOAD64 => {
                println!(
                    "  mov {}, [{}]",
                    regs[ir.lhs as usize], regs[ir.rhs as usize]
                );
            }
            IRType::STORE8 => {
                println!(
                    "  mov [{}], {}",
                    regs[ir.lhs as usize], regs8[ir.rhs as usize]
                );

                // fall through in 9cc
                println!(
                    "  mov [{}], {}",
                    regs[ir.lhs as usize], regs32[ir.rhs as usize]
                );
            }
            IRType::STORE32 => {
                println!(
                    "  mov [{}], {}",
                    regs[ir.lhs as usize], regs32[ir.rhs as usize]
                );
            }
            IRType::STORE64 => {
                println!(
                    "  mov [{}], {}",
                    regs[ir.lhs as usize], regs[ir.rhs as usize]
                );
            }
            IRType::STORE8_ARG => {
                println!("  mov [rbp-{}], {}", ir.lhs, argreg8[ir.rhs as usize]);
            }
            IRType::STORE32_ARG => {
                println!("  mov [rbp-{}], {}", ir.lhs, argreg32[ir.rhs as usize]);
            }
            IRType::STORE64_ARG => {
                println!("  mov [rbp-{}], {}", ir.lhs, argreg64[ir.rhs as usize]);
            }
            IRType::ADD => {
                println!("  add {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::SUB => {
                println!("  sub {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::MUL => {
                println!("  mov rax, {}", regs[ir.rhs as usize]);
                println!("  mul {}", regs[ir.lhs as usize]);
                println!("  mov {}, rax", regs[ir.lhs as usize]);
            }
            IRType::DIV => {
                println!(" mov rax, {}", regs[ir.lhs as usize]);
                println!(" cqo");
                println!(" div {}", regs[ir.rhs as usize]);
                println!(" mov {}, rax", regs[ir.lhs as usize]);
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
