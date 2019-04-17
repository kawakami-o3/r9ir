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
use crate::*;
use std::cell::RefCell;
use std::rc::Rc;

thread_local! {
    static USED: RefCell<Vec<bool>> = RefCell::new(Vec::new());
    static REG_MAP: RefCell<Vec<i32>> = RefCell::new(Vec::new());
}

const reg_map_size: i32 = 8192;

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
    if reg_map_size <= ir_reg {
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

fn kill(ri: i32) {
    USED.with(|u| {
        let r = ri as usize;
        assert!(u.borrow()[r]);
        u.borrow_mut()[r] = false;
    })
}

fn visit(ir: Rc<RefCell<IR>>) {
    let r0 = ir.borrow().r0;
    if r0 > 0 {
        ir.borrow_mut().r0 = alloc(r0);
    }

    let r2 = ir.borrow().r2;
    if r2 > 0 {
        ir.borrow_mut().r2 = alloc(r2);
    }

    let op = ir.borrow().op.clone();
    if op == IRType::CALL {
        let nargs = ir.borrow().nargs;
        for i in 0..nargs {
            let arg = ir.borrow().args[i];
            ir.borrow_mut().args[i] = alloc(arg);
        }
    }

    for r in ir.borrow().kill.iter() {
        kill(reg_map_get(*r));
    }
}

pub fn alloc_regs(prog: &mut Program) -> &mut Program {
    init_reg_map();
    init_used();
    for i in 0..prog.funcs.len() {
        let fun = &prog.funcs[i];
        for bb in fun.borrow().bbs.iter() {
            for ir in bb.borrow().ir.iter() {
                visit(ir.clone());
            }
        }
    }
    return prog;
}
