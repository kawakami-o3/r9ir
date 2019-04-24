// Optimization pass. In this pass, we promote all non-address-taken
// integer variables to register values. As a result, we may have more
// register values than the number of the physical registers, but
// that's fine. Regalloc will spill them out to memory.

// Rewrite
//
//  BPREL r1, <offset>
//  STORE r1, r2
//  LOAD  r3, r1
//
// to
//
//  NOP
//  r4 = r2
//  r3 = r4

use crate::gen_ir::*;
use crate::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

fn opt(ir: & Rc<RefCell<IR>>) {
    let op = ir.borrow().op.clone();
    if op == IRType::BPREL {
        let var = ir.borrow().var.clone().unwrap();
        let address_taken = var.borrow().address_taken;
        let ty = var.borrow().ty.clone();
        if address_taken || ty.ty != CType::INT {
            return;
        }

        let promoted = var.borrow().promoted.clone();
        if promoted.is_none() {
            var.borrow_mut().promoted = Some(new_reg());
        }

        ir.borrow_mut().op = IRType::NOP;
        let r0 = ir.borrow().r0.clone().unwrap();
        r0.borrow_mut().promoted = var.borrow().promoted.clone();
        return;
    }

    if op == IRType::LOAD {
        let r2 = ir.borrow().r2.clone().unwrap();
        if r2.borrow().promoted.is_none() {
            return;
        }
        ir.borrow_mut().op = IRType::MOV;
        ir.borrow_mut().r2 = r2.borrow().promoted.clone();
        return;
    }

    if op == IRType::STORE {
        let r1 = ir.borrow().r1.clone().unwrap();
        if r1.borrow().promoted.is_none() {
            return;
        }
        ir.borrow_mut().op = IRType::MOV;
        ir.borrow_mut().r0 = r1.borrow().promoted.clone();
        ir.borrow_mut().r1 = None;
        return;
    }
}

pub fn optimize(prog: &mut Program) {
    for fun in prog.funcs.iter() {
        let bbs = fun.borrow().bbs.clone();
        for bb in bbs.iter() {
            let irs = bb.borrow().ir.clone();
            for ir in irs.iter() {
                opt(ir);
            }
        }
    }
}
