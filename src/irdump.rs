#![allow(dead_code, non_camel_case_types)]

use crate::gen_ir::*;
use crate::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

fn tostr_call(ir: & IR) -> String {
    let mut s = String::new();
    s.push_str(&format!("r{} = {}(", ir.lhs, ir.name));
    for i in 0..ir.nargs {
        if i != 0 {
            s.push_str(", ");
        }
        s.push_str(&format!("r{}", ir.args[i]));
    }
    s.push_str(")");
    return s;
}

pub fn tostr(ir: & IR) -> String {
    let lhs = ir.lhs;
    let rhs = ir.rhs;

    match ir.op {
        IRType::ADD => format!("ADD r{}, r{}", lhs, rhs),
        IRType::CALL => tostr_call(ir),
        IRType::DIV => format!("DIV r{}, r{}", lhs, rhs),
        IRType::IMM => format!("r{} = r{}", lhs, ir.imm),
        IRType::JMP => format!("JMP .L{}", ir.bb1.borrow().label),
        IRType::LABEL_ADDR => format!("r{} = .L{}", lhs, ir.label),
        IRType::EQ => format!("EQ r{}, r{}", lhs, rhs),
        IRType::NE => format!("NE r{}, r{}", lhs, rhs),
        IRType::LE => format!("LE r{}, r{}", lhs, rhs),
        IRType::LT => format!("LT r{}, r{}", lhs, rhs),
        IRType::AND => format!("AND r{}, r{}", lhs, rhs),
        IRType::OR => format!("OR r{}, r{}", lhs, rhs),
        IRType::XOR => format!("XOR r{}, r{}", lhs, rhs),
        IRType::SHL => format!("SHL r{}, r{}", lhs, rhs),
        IRType::SHR => format!("SHR r{}, r{}", lhs, rhs),
        IRType::LOAD => format!("LOAD{} r{}, r{}", ir.size, lhs, rhs),
        IRType::MOD => format!("MOD r{}, r{}", lhs, rhs),
        IRType::MUL => format!("MUL r{}, r{}", lhs, rhs),
        IRType::NOP => "NOP".to_string(),
        IRType::RETURN => format!("RET r{}", lhs),
        IRType::STORE => format!("STORE{} r{}, r{}", ir.size, lhs, rhs),
        IRType::STORE_ARG => format!("STORE_ARG{} {}, {}", ir.size, ir.imm, ir.imm2),
        IRType::SUB => format!("SUB r{}, r{}", lhs, rhs),
        IRType::BPREL => format!("BPREL r{}, {}", lhs, ir.imm),
        IRType::BR => format!("BR r{}, .L{}, .L{}", lhs, ir.bb1.borrow().label, ir.bb2.borrow().label),
        _ => {
            panic!("unknown op");
        }
    }
}

pub fn dump_ir(irv: Vec<Rc<RefCell<Function>>>) {
    for fun in irv.iter() {
        eprintln!("{}:", fun.borrow().name);

        for bb in fun.borrow().bbs.iter() {
            eprintln!(".L{}:", bb.borrow().label);

            for i in bb.borrow().ir.iter() {
                eprintln!("\t{}", tostr(&i.borrow()).to_string());
            }
        }
    }
}

