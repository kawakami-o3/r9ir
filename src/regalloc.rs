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

//fn used() -> Vec<bool> {
//    USED.with(|u| {
//        return u.borrow().clone();
//    })
//}

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

// Rewrite `A = B op C` to `A = B; A = A op C`.
fn three_to_two(bb: Rc<RefCell<BB>>) {
    let mut v = Vec::new();

    let irs = bb.borrow().ir.clone();
    for ir in irs.iter() {
        let r0 = ir.borrow().r0.clone();
        let r1 = ir.borrow().r1.clone();
        if r0.is_none() || r1.is_none() {
            v.push(ir.clone());
            continue;
        }
 
        assert!(r0.clone().unwrap() != r1.clone().unwrap());

        let mut ir2 = alloc_ir();
        ir2.op = IRType::MOV;
        ir2.r0 = r0.clone();
        ir2.r2 = r1;
        v.push(Rc::new(RefCell::new(ir2)));

        ir.borrow_mut().r1 = r0;
        v.push(ir.clone());
    }
    bb.borrow_mut().ir = v;
}

fn mark(ir: Rc<RefCell<IR>>, r: Option<Rc<RefCell<Reg>>>) {
    if r.is_none() {
        return;
    }
    let rr = r.clone().unwrap();
    if rr.borrow().marked {
        return;
    }
    rr.borrow_mut().marked = true;
    ir.borrow_mut().kill.push(r.clone().unwrap());
}

fn mark_last_use(ir: Rc<RefCell<IR>>) {
    let r0 = ir.borrow().r0.clone();
    let r1 = ir.borrow().r1.clone();
    let r2 = ir.borrow().r2.clone();
    let bbarg = ir.borrow().bbarg.clone();
    mark(ir.clone(), r0);
    mark(ir.clone(), r1);
    mark(ir.clone(), r2);
    mark(ir.clone(), bbarg);

    let op = ir.borrow().op.clone();
    if op == IRType::CALL {
        let nargs = ir.borrow().nargs;
        for i in 0..nargs {
            let arg = ir.borrow().args[i].clone();
            mark(ir.clone(), Some(arg));
        }
    }
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

fn regalloc(ir: Rc<RefCell<IR>>) {
    alloc(ir.borrow().r0.clone());
    alloc(ir.borrow().r1.clone());
    alloc(ir.borrow().r2.clone());
    alloc(ir.borrow().bbarg.clone());

    let op = ir.borrow().op.clone();
    if op == IRType::CALL {
        let nargs = ir.borrow().nargs;
        for i in 0..nargs {
            let arg = ir.borrow().args[i].clone();
            alloc(Some(arg));
        }
    }

    if ir.borrow().kill.len() == 0 {
        return;
    }

    for r in ir.borrow().kill.iter() {
        assert!(r.borrow().rn != -1);
        used_set(r.borrow().rn, false);
    }
}

pub fn alloc_regs(prog: &mut Program) -> &mut Program {
    init_used();
    for fun in prog.funcs.iter() {
        for bb in fun.borrow().bbs.iter() {
            three_to_two(bb.clone());
        }
    }

    for fun in prog.funcs.iter().rev() {
        for bb in fun.borrow().bbs.iter().rev() {
            for ir in bb.borrow().ir.iter().rev() {
                mark_last_use(ir.clone());
            }
        }
    }

    for fun in prog.funcs.iter() {
        for bb in fun.borrow().bbs.iter() {
            alloc(bb.borrow().param.clone());

            for ir in bb.borrow().ir.iter() {
                regalloc(ir.clone());
            }
        }
    }
    return prog;
}
