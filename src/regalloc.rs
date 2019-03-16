// Register allocator.
//
// Before this pass, it is assumed that we have infinite number of
// registers. This pass maps them to a finite number of registers.
// We actually have only 7 registers.
//
// We allocate registers only within a single expression. In other
// words, there are no registers that live beyond semicolons.
// This design choice simplifies the implementation a lot, since
// practically we don't have to think about the case in which
// registers are exhausted and need to be spilled to memory.

#![allow(non_upper_case_globals)]

use crate::gen_ir::*;
use crate::irdump::*;
use crate::*;

lazy_static! {
    static ref USED: Mutex<Vec<bool>> = Mutex::new(Vec::new());
    static ref REG_MAP: Mutex<Vec<i32>> = Mutex::new(Vec::new());
}

pub const regs: [&'static str; 8] = ["rbp", "r10", "r11", "rbx", "r12", "r13", "r14", "r15"];
pub const regs8: [&'static str; 8] = ["rbl", "r10b", "r11b", "bl", "r12b", "r13b", "r14b", "r15b"];
pub const regs32: [&'static str; 8] = ["ebp", "r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"];

// Code generator

fn alloc(ir_reg: i32) -> i32 {
    let mut reg_map = REG_MAP.lock().unwrap();
    if reg_map[ir_reg as usize] != -1 {
        let r = reg_map[ir_reg as usize];
        assert!(USED.lock().unwrap()[r as usize], "");
        return r;
    }

    let mut used = USED.lock().unwrap();
    for i in 0..regs.len() {
        if used[i] {
            continue;
        }
        reg_map[ir_reg as usize] = i as i32;
        used[i] = true;
        return i as i32;
    }
    panic!("register exhausted");
}

fn visit(irv: &mut Vec<IR>) {
    // r0 is a reserved register that is always mapped to rbp.

    match (REG_MAP.lock(), USED.lock()) {
        (Ok(mut reg_map), Ok(mut used)) => {
            reg_map[0] = 0;
            used[0] = true;
        }
        _ => {
            panic!();
        }
    }

    for i in 0..irv.len() {
        let mut ir = &mut irv[i];

        match irinfo_get(&ir.op).unwrap().ty {
            IRInfoType::REG |
                IRInfoType::REG_IMM |
                IRInfoType::REG_LABEL |
                IRInfoType::LABEL_ADDR => {
                ir.lhs = alloc(ir.lhs);
            }
            IRInfoType::REG_REG => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            IRInfoType::CALL => {
                ir.lhs = alloc(ir.lhs);
                for i in 0..ir.nargs {
                    ir.args[i] = alloc(ir.args[i]);
                }
            }
            _ => {}
        }

        if ir.op == IRType::KILL {
            let mut used = USED.lock().unwrap();
            assert!(used[ir.lhs as usize]);
            used[ir.lhs as usize] = false;
            ir.op = IRType::NOP;
        }
    }
}

pub fn alloc_regs(fns: &mut Vec<IR>) {

    for i in 0..fns.len() {
        let mut fun = &mut fns[i];
        match (REG_MAP.lock(), USED.lock()) {
            (Ok(mut reg_map), Ok(mut used)) => {
                *reg_map = Vec::new();
                for _i in 0..fun.ir.len() {
                    reg_map.push(-1);
                }

                *used = Vec::new();
                for _i in 0..regs.len() {
                    used.push(false);
                }
            }
            _ => {
                panic!();
            }
        }

        visit(&mut fun.ir);
    }
}
