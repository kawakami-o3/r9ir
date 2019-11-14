
use crate::gen_ir::*;
use crate::parse::*;
use crate::util::*;
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
        vec_union(bb.borrow().def_regs.clone(), &param.clone().unwrap());
    }

    let irs = bb.borrow().ir.clone();
    for ir in irs.iter() {
        let r0 = ir.borrow().r0.clone();
        if r0.is_some() {
            vec_union(bb.borrow().def_regs.clone(), &r0.clone().unwrap());
        }
    }
}

fn propagate(bb: & Rc<RefCell<BB>>, r: Option<Rc<RefCell<Reg>>>) {
    if r.is_none() {
        return;
    }
    let def_regs = bb.borrow().def_regs.clone();
    if def_regs.borrow().contains(&r.clone().unwrap()) {
        return;
    }
   
    if !vec_union(bb.borrow().in_regs.clone(), &r.clone().unwrap()) {
        return;
    }

    let preds = bb.borrow().pred.clone();
    for pred  in preds.iter() {
        if vec_union(pred.borrow().out_regs.clone(), &r.clone().unwrap()) {
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

pub fn liveness(prog: &mut Program) {
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

        // Incoming registers of the entry BB correspond to
        // uninitialized variables in a program.
        // Add dummy definitions to make later analysis easy.
        let ent = bbs[0].clone();
        let in_regs = ent.borrow().in_regs.clone();
        for r in in_regs.borrow().iter() {
            let mut ir = alloc_ir();
            ir.op = IRType::MOV;
            ir.r0 = Some(r.clone());
            ir.imm = 0;
            ent.borrow_mut().ir.push(Rc::new(RefCell::new(ir)));
            let def_regs = ent.borrow_mut().def_regs.clone();
            def_regs.borrow_mut().push(r.clone());
        }
        ent.borrow_mut().in_regs = Rc::new(RefCell::new(Vec::new()));
    }
}
