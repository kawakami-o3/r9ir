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
use std::cell::RefCell;

thread_local! {
    static USED: RefCell<Vec<bool>> = RefCell::new(Vec::new());
    static REG_MAP: RefCell<Vec<i32>> = RefCell::new(Vec::new());
}

const reg_map_size: usize = 8192;

fn init_used() {
    USED.with(|u| {
        let mut used = u.borrow_mut();
        *used = Vec::new();
        for _i in 0..num_regs() {
            used.push(false);
        }
    })
}

fn init_reg_map() {
    REG_MAP.with(|r| {
        let mut reg_map = r.borrow_mut();
        *reg_map = Vec::new();
        for _i in 0..reg_map_size {
            reg_map.push(-1);
        }
    })
}

fn used_get(i: i32) -> bool {
    USED.with(|u| {
        return u.borrow()[i as usize];
    })
}

fn used_set(i: i32, v: bool) {
    USED.with(|u| {
        u.borrow_mut()[i as usize] = v;
    })
}

fn reg_map_get(i: i32) -> i32 {
    REG_MAP.with(|r| {
        return r.borrow()[i as usize];
    })
}

fn reg_map_set(i: i32, v: i32) {
    REG_MAP.with(|r| {
        r.borrow_mut()[i as usize] = v;
    })
}

fn alloc(ir_reg: i32) -> i32 {
    if reg_map_size <= ir_reg as usize {
        panic!("program too big");
    }

    if reg_map_get(ir_reg) != -1 {
        let r = reg_map_get(ir_reg);
        assert!(used_get(r));
        return r;
    }

    for i in 0..num_regs() {
        if used_get(i as i32) {
            continue;
        }
        reg_map_set(ir_reg, i as i32);
        used_set(i as i32, true);
        return i as i32;
    }
    panic!("register exhausted");
}

fn visit(irv: &mut Vec<IR>) {
    for i in 0..irv.len() {
        let mut ir = &mut irv[i];

        match irinfo_get(&ir.op).unwrap().ty {
            IRInfoType::BINARY => {
                ir.lhs = alloc(ir.lhs);
                ir.rhs = alloc(ir.rhs);
            }
            IRInfoType::REG |
                IRInfoType::REG_IMM |
                IRInfoType::REG_LABEL |
                IRInfoType::LABEL_ADDR => {
                ir.lhs = alloc(ir.lhs);
            }
            IRInfoType::MEM |
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
            assert!(used_get(ir.lhs));
            used_set(ir.lhs, false);
            ir.op = IRType::NOP;
        }
    }
}

pub fn alloc_regs(prog: &mut Program) -> &mut Program {
    init_reg_map();
    init_used();
    for i in 0..prog.funcs.len() {
        let fun = &mut prog.funcs[i];
        visit(&mut fun.ir);
    }
    return prog;
}
