#![allow(dead_code, non_camel_case_types)]

use crate::gen_ir::*;
use crate::parse::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    pub static IRINFO: RefCell<HashMap<IRType, IRInfo>> = RefCell::new(HashMap::new());
}

#[derive(Clone, Debug)]
pub enum IRInfoType {
    NOARG,
    BINARY,
    REG,
    IMM,
    MEM,
    JMP,
    LABEL,
    LABEL_ADDR,
    REG_REG,
    REG_IMM,
    STORE_ARG,
    REG_LABEL,
    CALL,
    NULL,
}

#[derive(Clone, Debug)]
pub struct IRInfo {
    pub name: &'static str,
    pub ty: IRInfoType,
}

fn init_irinfo() {
    IRINFO.with(|i| {
        let mut irinfo = i.borrow_mut();

        if irinfo.len() > 0 {
            return;
        }

        irinfo.insert(IRType::ADD, IRInfo { name: "ADD", ty: IRInfoType::BINARY });
        irinfo.insert(IRType::CALL, IRInfo { name: "CALL", ty: IRInfoType::CALL });
        irinfo.insert(IRType::DIV, IRInfo { name: "DIV", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::IMM, IRInfo { name: "IMM", ty: IRInfoType::REG_IMM });
        irinfo.insert(IRType::JMP, IRInfo { name: "JMP", ty: IRInfoType::JMP });
        irinfo.insert(IRType::KILL, IRInfo { name: "KILL", ty: IRInfoType::REG });
        irinfo.insert(IRType::LABEL, IRInfo { name: "", ty: IRInfoType::LABEL });
        irinfo.insert(IRType::LABEL_ADDR, IRInfo { name: "LABEL_ADDR", ty: IRInfoType::LABEL_ADDR });
        irinfo.insert(IRType::EQ, IRInfo { name: "EQ", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::NE, IRInfo { name: "NE", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::LE, IRInfo { name: "LE", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::LT, IRInfo { name: "LT", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::AND, IRInfo { name: "AND", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::OR, IRInfo { name: "OR", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::XOR, IRInfo { name: "XOR", ty: IRInfoType::BINARY });
        irinfo.insert(IRType::SHL, IRInfo { name: "SHL", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::SHR, IRInfo { name: "SHR", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::LOAD, IRInfo { name: "LOAD", ty: IRInfoType::MEM });
        irinfo.insert(IRType::MOD, IRInfo { name: "MOD", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::MOV, IRInfo { name: "MOV", ty: IRInfoType::REG_REG });
        irinfo.insert(IRType::MUL, IRInfo { name: "MUL", ty: IRInfoType::BINARY });
        irinfo.insert(IRType::NOP, IRInfo { name: "NOP", ty: IRInfoType::NOARG });
        irinfo.insert(IRType::RETURN, IRInfo { name: "RET", ty: IRInfoType::REG });
        irinfo.insert(IRType::STORE, IRInfo { name: "STORE", ty: IRInfoType::MEM });
        irinfo.insert(IRType::STORE_ARG, IRInfo { name: "STORE_ARG", ty: IRInfoType::STORE_ARG });
        irinfo.insert(IRType::SUB, IRInfo { name: "SUB", ty: IRInfoType::BINARY });
        irinfo.insert(IRType::BPREL, IRInfo { name: "BPREL", ty: IRInfoType::REG_IMM });
        irinfo.insert(IRType::IF, IRInfo { name: "IF", ty: IRInfoType::REG_LABEL });
        irinfo.insert(IRType::UNLESS, IRInfo { name: "UNLESS", ty: IRInfoType::REG_LABEL });
        irinfo.insert(IRType::NULL, IRInfo { name: "", ty: IRInfoType::NULL });
    })
}

pub fn irinfo_get(ty: & IRType) -> Option<IRInfo> {
    init_irinfo();

    IRINFO.with(|irinfo| {
        match irinfo.borrow().get(ty) {
            Some(info) => Some(info.clone()),
            None => None,
        }
    })
}

pub fn tostr(ir: IR) -> String {
    IRINFO.with(|i| {
        let irinfo = i.borrow();

        let info = irinfo.get(&ir.op).unwrap();

        match info.ty {
            IRInfoType::BINARY => if ir.is_imm {
                format!("  {} r{}, {}", info.name, ir.lhs, ir.rhs)
            }else {
                format!("  {} r{}, r{}", info.name, ir.lhs, ir.rhs)
            },
            IRInfoType::LABEL => format!(".L{}:", ir.lhs),
            IRInfoType::LABEL_ADDR => format!("  {} r{}, {}", info.name, ir.lhs, ir.name),
            IRInfoType::IMM => format!("  {} {}", ir.name, ir.lhs),
            IRInfoType::REG => format!("  {} r{}", info.name, ir.lhs),
            IRInfoType::JMP => format!("  {} .L{}", info.name, ir.lhs),
            IRInfoType::REG_REG => format!("  {} r{}, r{}", info.name, ir.lhs, ir.rhs),
            IRInfoType::MEM => format!("  {}{} r{}, r{}", info.name, ir.size, ir.lhs, ir.rhs),
            IRInfoType::REG_IMM => format!("  {} r{}, {}", info.name, ir.lhs, ir.rhs),
            IRInfoType::STORE_ARG => format!("  {}{} {}, {}", info.name, ir.size, ir.lhs, ir.rhs),
            IRInfoType::REG_LABEL => format!("  {} r{}, .L{}", info.name, ir.lhs, ir.rhs),
            IRInfoType::CALL => {
                let mut s = String::new();
                s.push_str(&format!("  r{} = {}(", ir.lhs, ir.name));
                let args = ir.args.iter().map(|a| format!("r{}", a).to_string()).collect::<Vec<String>>();
                s.push_str(&args.join(", "));
                s.push_str(")");
                s
            }
            IRInfoType::NOARG => format!("  {}", info.name),
            _ => {
                panic!("unknown ir {:?}", info.ty);
            }
        }
    })
}

pub fn dump_ir(irv: Vec<Function>) {
    init_irinfo();

    for i in 0..irv.len() {
        let fun = &irv[i];
        eprintln!("{}():", fun.name);
        for j in 0..fun.ir.len() {
            eprintln!("{}", tostr(fun.ir[j].clone()));
        }
    }
}

