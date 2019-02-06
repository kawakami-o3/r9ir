use crate::*;
use crate::regalloc::*;

pub fn gen_x86(irv: &Vec<IR>) {
    let regs = REGS.lock().unwrap();
    for i in 0..irv.len() {
        let ir = &irv[i as usize];
        match ir.op {
            IRType::IMM => {
                println!("  mov {}, {}", regs[ir.lhs as usize], ir.rhs);
            }
            IRType::MOV => {
                println!("  mov {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::RETURN => {
                println!("  mov rax, {}", regs[ir.lhs as usize]);
                println!("  ret");
            }
            IRType::ADD => {
                println!("  add {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            IRType::SUB => {
                println!("  sub {}, {}", regs[ir.lhs as usize], regs[ir.rhs as usize]);
            }
            _ => {}
        }
    }
}
