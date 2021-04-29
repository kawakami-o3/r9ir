#![allow(non_camel_case_types)]

use crate::gen_ir::*;
use crate::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

fn regno(r: Option<Rc<RefCell<Reg>>>) -> i32 {
    if r.is_none() {
        return 0;
    }
    let rr = r.unwrap().clone();
    let rn = rr.borrow().rn;
    if rn != -1 {
        return rr.borrow().rn;
    }
    return rr.borrow().vn;
}

fn tostr_call(ir: &IR) -> String {
    let mut s = String::new();
    s.push_str(&format!("r{} = {}(", regno(ir.r0.clone()), ir.name));
    for i in 0..ir.nargs {
        if i != 0 {
            s.push_str(", ");
        }
        s.push_str(&format!("r{}", regno(Some(ir.args[i].clone()))));
    }
    s.push_str(")");
    return s;
}

pub fn tostr(ir: &IR) -> String {
    let r0 = regno(ir.r0.clone());
    let r1 = regno(ir.r1.clone());
    let r2 = regno(ir.r2.clone());

    match ir.op {
        IRType::ADD => format!("r{} = r{} + r{}", r0, r1, r2),
        IRType::CALL => tostr_call(ir),
        IRType::DIV => format!("r{} = r{} / r{}", r0, r1, r2),
        IRType::IMM => format!("r{} = {}", r0, ir.imm),
        IRType::JMP => {
            let bb1 = ir.bb1.clone().unwrap();
            let bbarg = ir.bbarg.clone();
            if bbarg.is_some() {
                return format!("JMP .L{} (r{})", bb1.borrow().label, regno(bbarg));
            }
            return format!("JMP .L{}", bb1.borrow().label);
        }
        IRType::LABEL_ADDR => format!("r{} = .L{}", r0, ir.label),
        IRType::EQ => format!("r{} = r{} == r{}", r0, r1, r2),
        IRType::NE => format!("r{} = r{} != r{}", r0, r1, r2),
        IRType::LE => format!("r{} = r{} <= r{}", r0, r1, r2),
        IRType::LT => format!("r{} = r{} < r{}", r0, r1, r2),
        IRType::AND => format!("r{} = r{} & r{}", r0, r1, r2),
        IRType::OR => format!("r{} = r{} | r{}", r0, r1, r2),
        IRType::XOR => format!("r{} = r{} ^ r{}", r0, r1, r2),
        IRType::SHL => format!("r{} = r{} << r{}", r0, r1, r2),
        IRType::SHR => format!("r{} = r{} >> r{}", r0, r1, r2),
        IRType::LOAD => format!("LOAD{} r{}, r{}", ir.size, r0, r2),
        IRType::LOAD_SPILL => format!("LOAD_SPILL r{}, {}", r0, ir.imm),
        IRType::MOD => format!("r{} = r{} % r{}", r0, r1, r2),
        IRType::MOV => format!("r{} = r{}", r0, r2),
        IRType::MUL => format!("r{} = r{} * r{}", r0, r1, r2),
        IRType::NOP => "NOP".to_string(),
        IRType::RETURN => format!("RET r{}", r2),
        IRType::STORE => format!("STORE{} r{}, r{}", ir.size, r1, r2),
        IRType::STORE_ARG => {
            let var = ir.var.clone().unwrap();
            let name = var.borrow().name.clone();
            let offset = var.borrow().offset;
            format!("STORE_ARG{} {} {} ({})", ir.size, ir.imm, name, offset)
        }
        IRType::STORE_SPILL => format!("STORE_SPILL r{}, {}", r1, ir.imm),
        IRType::SUB => format!("r{} = r{} - r{}", r0, r1, r2),
        IRType::BPREL => {
            let var = ir.var.clone().unwrap();
            let name = var.borrow().name.clone();
            let offset = var.borrow().offset;
            format!("BPREL r{} {} ({})", r0, name, offset)
        }
        IRType::BR => {
            let bb1 = ir.bb1.clone().unwrap();
            let bb2 = ir.bb2.clone().unwrap();
            return format!(
                "BR r{} .L{} .L{}",
                r2,
                bb1.borrow().label,
                bb2.borrow().label
            );
        }
    }
}

fn print_rel(name: &str, v: Vec<Rc<RefCell<BB>>>) {
    if v.len() == 0 {
        return;
    }
    eprint!(" {}=", name);
    for i in 0..v.len() {
        let bb = &v[i];
        if i > 0 {
            eprint!(",");
        }
        eprint!(".L{}", bb.borrow().label);
    }
}

fn print_regs(name: &str, v: Vec<Rc<RefCell<Reg>>>) {
    if v.len() == 0 {
        return;
    }
    eprint!(" {}=", name);
    for i in 0..v.len() {
        let r = &v[i];
        if i > 0 {
            eprint!(",");
        }
        eprint!("r{}", regno(Some(r.clone())));
    }
}

fn print_bb(bb: Rc<RefCell<BB>>) {
    let label = bb.borrow().label.clone();
    let param = bb.borrow().param.clone();
    if param.is_some() {
        eprint!(".L{}({})", label, regno(param));
    } else {
        eprint!(".L{}", label);
    }

    print_rel("pred", bb.borrow().pred.clone());
    print_rel("succ", bb.borrow().succ.clone());
    let def_regs = bb.borrow().def_regs.clone();
    let in_regs = bb.borrow().in_regs.clone();
    let out_regs = bb.borrow().out_regs.clone();
    print_regs("defs", def_regs.borrow().clone());
    print_regs("in", in_regs.borrow().clone());
    print_regs("out", out_regs.borrow().clone());
    eprintln!();
}

pub fn dump_ir(irv: Vec<Rc<RefCell<Function>>>) {
    for fun in irv.iter() {
        eprintln!("{}:", fun.borrow().name);

        for bb in fun.borrow().bbs.iter() {
            print_bb(bb.clone());

            for i in bb.borrow().ir.iter() {
                eprintln!("\t{}", tostr(&i.borrow()).to_string());
            }
        }
    }
}
