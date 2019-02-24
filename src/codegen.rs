use crate::regalloc::*;
use crate::*;

lazy_static! {
    static ref N: Mutex<usize> = Mutex::new(0);
}

pub fn gen_x86(irv: &Vec<IR>) {
    let ret = ".Lend";

    println!("  push rbp");
    println!("  mov rbp, rsp");

    let regs = REGS.lock().unwrap();
    for i in 0..irv.len() {
        let ir = &irv[i as usize];
        match ir.op {
            IRType::IMM => {
                println!("  mov {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::ADD_IMM => {
                println!("  add {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::MOV => {
                println!("  mov {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::RETURN => {
                println!("  mov rax, {}", regs[ir.lhs as usize]);
                println!("  jmp {}", ret);
            }
            IRType::CALL => {
                println!("  push rbx");
                println!("  push rbp");
                println!("  push rsp");
                println!("  push r12");
                println!("  push r13");
                println!("  push r14");
                println!("  push r15");

                let arg = vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for i in 0..arg.len() {
                    println!("  mov {}, {}", arg[i], regs[ir.args[i] as usize]);
                }

                println!("  mov rax, 0");
                println!("  call {}", ir.name);
                println!("  mov {}, rax", regs[ir.lhs as usize]);

                println!("  push r15");
                println!("  push r14");
                println!("  push r13");
                println!("  push r12");
                println!("  push rsp");
                println!("  push rbp");
                println!("  push rbx");
            }
            IRType::LABEL => {
                println!(".L{}:", ir.lhs);
            }
            IRType::JMP => {
                println!("  jmp .L{}", ir.lhs);
            }
            IRType::UNLESS => {
                println!("  cmp {}, 0", regs[ir.lhs as usize]);
                println!("  je .L{}", ir.rhs);
            }
            IRType::ALLOCA => {
                if ir.rhs != 0 {
                    println!("  sub rsp, {}", ir.rhs);
                }
                println!("  mov {}, rsp", regs[ir.lhs as usize]);
            }
            IRType::LOAD => {
                println!(
                    "  mov {}, [{}]",
                    regs[ir.lhs as usize], regs[ir.rhs as usize]
                );
            }
            IRType::STORE => {
                println!(
                    "  mov [{}], {}",
                    regs[ir.lhs as usize], regs[ir.rhs as usize]
                );
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
    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
}
