#![allow(dead_code, non_camel_case_types)]

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

fn tostr_call(ir: & IR) -> String {
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

pub fn tostr(ir: & IR) -> String {
    let r0 = regno(ir.r0.clone());
    let r1 = regno(ir.r1.clone());
    let r2 = regno(ir.r2.clone());

    match ir.op {
        IRType::ADD => format!("r{} = r{} + r{}", r0, r1, r2),
        IRType::CALL => tostr_call(ir),
        IRType::DIV => format!("r{} = r{} / r{}", r0, r1, r2),
        IRType::IMM => format!("r{} = r{}", r0, ir.imm),
        IRType::JMP => {
            let bbarg = ir.bbarg.clone();
            if bbarg.is_some() {
                return format!("JMP .L{} (r{})", ir.bb1.borrow().label, regno(bbarg))
            }
            format!("JMP .L{}", ir.bb1.borrow().label)
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
        IRType::RETURN => format!("RET r{}", r0),
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
            format!("BPREL r{} {} {}", r0, name, offset)
        }
        IRType::BR => format!("BR r{} .L{} .L{}", r0, ir.bb1.borrow().label, ir.bb2.borrow().label),
        _ => {
            panic!("unknown op: {:?}", ir.op);
        }
    }
}

pub fn dump_ir(irv: Vec<Rc<RefCell<Function>>>) {
    for fun in irv.iter() {
        eprintln!("{}:", fun.borrow().name);

        for bb in fun.borrow().bbs.iter() {
            let param = bb.borrow().param.clone();
            if param.is_some() {
                eprintln!(".L{}(r{}):", bb.borrow().label, regno(param));
            } else {
                eprintln!(".L{}:", bb.borrow().label);
            }

            for i in bb.borrow().ir.iter() {
                eprintln!("\t{}", tostr(&i.borrow()).to_string());
            }
        }
    }
}

