
use crate::gen_ir::*;
use crate::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

fn add_edges(bb: Rc<RefCell<BB>>) {
    if bb.borrow().succ.len() > 0 {
        return;
    }
    assert!(bb.borrow().ir.len() > 0);

    let ir_length = bb.borrow().ir.len();
    let ir = bb.borrow().ir[ir_length - 1].clone();

    let bb1 = ir.borrow().bb1.clone();
    if bb1.is_some() {
        let tmp = bb1.clone().unwrap();
        bb.borrow_mut().succ.push(tmp.clone());
        tmp.borrow_mut().pred.push(bb.clone());
        add_edges(tmp);
    }

    let bb2 = ir.borrow().bb2.clone();
    if bb2.is_some() {
        let tmp = bb2.clone().unwrap();
        bb.borrow_mut().succ.push(tmp.clone());
        tmp.borrow_mut().pred.push(bb.clone());
        add_edges(tmp);
    }
}

fn set_def_regs(bb: & Rc<RefCell<BB>>) {
    let param = bb.borrow().param.clone();
    if param.is_some() {
        let def_regs = bb.borrow().def_regs.clone();
        if def_regs.contains(&param.clone().unwrap()) {
            // do nothing
        } else {
            bb.borrow_mut().def_regs.push(param.clone().unwrap());
        }
    }

    let irs = bb.borrow().ir.clone();
    for ir in irs.iter() {
        let r0 = ir.borrow().r0.clone();
        if r0.is_some() {
            let def_regs = bb.borrow().def_regs.clone();
            if def_regs.contains(&r0.clone().unwrap()) {
                continue;
            } else {
                bb.borrow_mut().def_regs.push(r0.clone().unwrap());
            }
        }
    }
}

fn propagate(bb: & Rc<RefCell<BB>>, r: Option<Rc<RefCell<Reg>>>) {
    if r.is_none() {
        return;
    }
    let def_regs = bb.borrow().def_regs.clone();
    if def_regs.contains(&r.clone().unwrap()) {
        return;
    }
    
    let in_regs = bb.borrow().in_regs.clone();
    if in_regs.contains(&r.clone().unwrap()) {
        return;
    } else {
        bb.borrow_mut().in_regs.push(r.clone().unwrap());
    }

    let preds = bb.borrow().pred.clone();
    for pred  in preds.iter() {
        if pred.borrow().out_regs.contains(&r.clone().unwrap()) {
            continue;
        } else {
            propagate(pred, r.clone());
        }
    }
}

fn visit(bb: & Rc<RefCell<BB>>, ir: & Rc<RefCell<IR>>) {
    propagate(bb, ir.borrow().r1.clone());
    propagate(bb, ir.borrow().r2.clone());
    propagate(bb, ir.borrow().bbarg.clone());

    let op = ir.borrow().op.clone();
    if op == IRType::CALL {
        let nargs = ir.borrow().nargs;
        for i in 0..nargs {
            propagate(bb, Some(ir.borrow().args[i].clone()));
        }
    }
}

pub fn liveness(prog: &mut Program) -> &mut Program {
    for fun in prog.funcs.iter() {
        let bbs = fun.borrow().bbs.clone();
        add_edges(bbs[0].clone());

        for bb in bbs.iter() {
            set_def_regs(bb);

            let irs = bb.borrow().ir.clone();
            for ir in irs.iter() {
                visit(bb, ir);
            }
        }
    }
    return prog;
}
