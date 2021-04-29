// Linear scan register allocator
//
// Before this pass, it is assumed that we have infinite number of
// registers. This pass maps them to finite number of registers.
// Here is the algorithm
//
// First, we find the definition and the last use for each register.
// A register is considered "live" in the range. At the definition of
// some register R, if all physical registers are already allocated,
// one of them (including R itself) needs to be spilled to the stack.
// As long as one register is spilled, the algorithm is logically
// correct. As a heuristic, we spill a register whose last use is
// furthest.
//
// We then insert load and store instructions for spilled registesr.
// The last register (num_regs-1'th register) is reserved for that
// purpose.

#![allow(non_upper_case_globals)]

use crate::gen_ir::*;
use crate::parse::*;
use crate::util::*;
use crate::*;
use std::cell::RefCell;
use std::rc::Rc;

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

fn set_last_use(r: Option<Rc<RefCell<Reg>>>, ic: i32) {
    match r {
        Some(reg) => {
            if reg.borrow().last_use < ic {
                reg.borrow_mut().last_use = ic;
            }
        }
        None => {}
    }
}

fn collect_regs(fun: &Rc<RefCell<Function>>) -> Vec<Rc<RefCell<Reg>>> {
    let mut v = Vec::new();
    let mut ic = 1; // instruction counter

    for bb in fun.borrow().bbs.iter() {
        if bb.borrow().param.is_some() {
            let param = bb.borrow().param.clone().unwrap();
            param.borrow_mut().def = ic;
            v.push(param);
        }

        for ir in bb.borrow().ir.iter() {
            let r0 = ir.borrow().r0.clone();

            if r0.is_some() {
                let rr0 = r0.clone().unwrap();
                if rr0.borrow().def == -1 {
                    rr0.borrow_mut().def = ic;
                    v.push(rr0);
                }
            }

            let r1 = ir.borrow().r1.clone();
            set_last_use(r1, ic);
            let r2 = ir.borrow().r2.clone();
            set_last_use(r2, ic);
            let bbarg = ir.borrow().bbarg.clone();
            set_last_use(bbarg, ic);

            let op = ir.borrow().op.clone();
            if op == IRType::CALL {
                let nargs = ir.borrow().nargs;
                for i in 0..nargs {
                    let arg = ir.borrow().args[i].clone();
                    set_last_use(Some(arg), ic);
                }
            }

            ic += 1;
        }

        let out_regs = bb.borrow().out_regs.clone();
        for r in out_regs.borrow().iter() {
            set_last_use(Some(r.clone()), ic);
        }
    }

    return v;
}

fn choose_to_spill(used: &Vec<Option<Rc<RefCell<Reg>>>>) -> usize {
    let mut k = 0;
    for i in 1..num_regs() {
        let uk = used[k].clone().unwrap();
        let ui = used[i].clone().unwrap();
        if uk.borrow().last_use < ui.borrow().last_use {
            k = i;
        }
    }
    return k;
}

// Allocate registers.
fn scan(regs: &Vec<Rc<RefCell<Reg>>>) {
    let mut used: Vec<Option<Rc<RefCell<Reg>>>> = vec![None; num_regs()];

    for r in regs.iter() {
        let mut found = false;
        for i in 0..num_regs() {
            if used[i].is_some() {
                let u = used[i].clone().unwrap();
                if r.borrow().def < u.borrow().last_use {
                    continue;
                }
            }

            r.borrow_mut().rn = i as i32;
            used[i] = Some(r.clone());
            found = true;
            break;
        }

        if found {
            continue;
        }

        used[num_regs() - 1] = Some(r.clone());
        let k = choose_to_spill(&used);

        r.borrow_mut().rn = k as i32;
        let uk = used[k].clone().unwrap();
        uk.borrow_mut().rn = (num_regs() - 1) as i32;
        uk.borrow_mut().spill = true;
        used[k] = Some(r.clone());
    }
}

fn spill_store(v: &mut Vec<Rc<RefCell<IR>>>, ir: &Rc<RefCell<IR>>) {
    let r = ir.borrow().r0.clone();
    if r.is_none() {
        return;
    }
    let rr = r.clone().unwrap();
    if !rr.borrow().spill {
        return;
    }

    let mut ir2 = alloc_ir();
    ir2.op = IRType::STORE_SPILL;
    ir2.r1 = r.clone();
    ir2.var = rr.borrow().var.clone();
    v.push(Rc::new(RefCell::new(ir2)));
}

fn spill_load(v: &mut Vec<Rc<RefCell<IR>>>, _ir: &Rc<RefCell<IR>>, r: Option<Rc<RefCell<Reg>>>) {
    if r.is_none() {
        return;
    }
    let rr = r.clone().unwrap();
    if !rr.borrow().spill {
        return;
    }

    let mut ir2 = alloc_ir();
    ir2.op = IRType::LOAD_SPILL;
    ir2.r0 = r.clone();
    ir2.var = rr.borrow().var.clone();
    v.push(Rc::new(RefCell::new(ir2)));
}

fn emit_spill_code(bb: &Rc<RefCell<BB>>) {
    let mut v: Vec<Rc<RefCell<IR>>> = Vec::new();

    let irs = bb.borrow().ir.clone();
    for ir in irs.iter() {
        let r1 = ir.borrow().r1.clone();
        spill_load(&mut v, ir, r1);
        let r2 = ir.borrow().r2.clone();
        spill_load(&mut v, ir, r2);
        let bbarg = ir.borrow().bbarg.clone();
        spill_load(&mut v, ir, bbarg);
        v.push(ir.clone());
        spill_store(&mut v, ir);
    }

    bb.borrow_mut().ir = v;
}

pub fn alloc_regs(prog: &mut Program) {
    for fun in prog.funcs.iter() {
        // Convert SSA to x86-ish two-address form.
        for bb in fun.borrow().bbs.iter() {
            three_to_two(bb.clone());
        }

        // Allocate registers and decide which registers to spill.
        let regs = collect_regs(fun);
        scan(&regs);

        // Reserve a stack area for spilled registers.
        for r in regs.iter() {
            if !r.borrow().spill {
                continue;
            }

            let mut var = alloc_var();
            var.ty = ptr_to(Rc::new(RefCell::new(int_ty())));
            var.is_local = true;
            var.name = "spill".to_string();

            let v = Rc::new(RefCell::new(var));
            r.borrow_mut().var = Some(v.clone());
            fun.borrow_mut().lvars.push(v);
        }

        // Convert accesses to spilled registers to loads and stores
        for bb in fun.borrow().bbs.iter() {
            emit_spill_code(bb);
        }
    }
}
