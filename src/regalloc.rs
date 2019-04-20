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
}

fn init_used() {
    USED.with(|u| {
        let mut used = u.borrow_mut();
        *used = Vec::new();
        for _i in 0..num_regs() {
            used.push(false);
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

fn alloc(r: Option<Rc<RefCell<Reg>>>) {
    if r.is_none() {
        return;
    }
    let rr = r.clone().unwrap();
    if rr.borrow().rn != -1 {
        return;
    }

    for i in 0..num_regs() {
        if used_get(i as i32) {
            continue;
        }
        used_set(i as i32, true);
        rr.borrow_mut().rn = i as i32;
        return;
    }
    panic!("register exhausted");
}

fn visit(ir: Rc<RefCell<IR>>) {
    alloc(ir.borrow().r0.clone());
    alloc(ir.borrow().r2.clone());

    let op = ir.borrow().op.clone();
    if op == IRType::CALL {
        let nargs = ir.borrow().nargs;
        for i in 0..nargs {
            let arg = ir.borrow().args[i].clone();
            alloc(Some(arg));
        }
    }

    for r in ir.borrow().kill.iter() {
        assert!(r.borrow().rn != -1);
        used_set(r.borrow().rn, false);
    }
}

pub fn alloc_regs(prog: &mut Program) -> &mut Program {
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
