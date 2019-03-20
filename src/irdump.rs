#![allow(dead_code, non_camel_case_types)]

use crate::gen_ir::*;
use std::collections::HashMap;
use std::sync::Mutex;

lazy_static! {
    pub static ref IRINFO: Mutex<HashMap<IRType, IRInfo>> = Mutex::new(HashMap::new());
}

#[derive(Clone, Debug)]
pub enum IRInfoType {
    NOARG,
    REG,
    IMM,
    JMP,
    LABEL,
    LABEL_ADDR,
    REG_REG,
    REG_IMM,
    IMM_IMM,
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
    let mut irinfo = IRINFO.lock().unwrap();
    if irinfo.len() > 0 {
        return;
    }

    irinfo.insert(IRType::ADD, IRInfo {
        name: "ADD",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::CALL, IRInfo {
        name: "CALL",
        ty: IRInfoType::CALL
    });
    irinfo.insert(IRType::DIV, IRInfo {
        name: "DIV",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::IMM, IRInfo {
        name: "IMM",
        ty: IRInfoType::REG_IMM
    });
    irinfo.insert(IRType::JMP, IRInfo {
        name: "JMP",
        ty: IRInfoType::JMP
    });
    irinfo.insert(IRType::KILL, IRInfo {
        name: "KILL",
        ty: IRInfoType::REG
    });
    irinfo.insert(IRType::LABEL, IRInfo {
        name: "",
        ty: IRInfoType::LABEL
    });
    irinfo.insert(IRType::LABEL_ADDR, IRInfo {
        name: "LABEL_ADDR",
        ty: IRInfoType::LABEL_ADDR
    });
    irinfo.insert(IRType::EQ, IRInfo {
        name: "EQ",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::NE, IRInfo {
        name: "NE",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::LT, IRInfo {
        name: "LT",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::AND, IRInfo {
        name: "AND",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::OR, IRInfo {
        name: "OR",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::XOR, IRInfo {
        name: "XOR",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::LOAD8, IRInfo {
        name: "LOAD8",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::LOAD32, IRInfo {
        name: "LOAD32",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::LOAD64, IRInfo {
        name: "LOAD64",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::MOV, IRInfo {
        name: "MOV",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::MUL, IRInfo {
        name: "MUL",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::NOP, IRInfo {
        name: "NOP",
        ty: IRInfoType::NOARG
    });
    irinfo.insert(IRType::RETURN, IRInfo {
        name: "RET",
        ty: IRInfoType::REG
    });
    irinfo.insert(IRType::STORE8, IRInfo {
        name: "STORE8",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::STORE32, IRInfo {
        name: "STORE32",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::STORE64, IRInfo {
        name: "STORE64",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::STORE8_ARG, IRInfo {
        name: "STORE8_ARG",
        ty: IRInfoType::IMM_IMM
    });
    irinfo.insert(IRType::STORE32_ARG, IRInfo {
        name: "STORE32_ARG",
        ty: IRInfoType::IMM_IMM
    });
    irinfo.insert(IRType::STORE64_ARG, IRInfo {
        name: "STORE64_ARG",
        ty: IRInfoType::IMM_IMM
    });
    irinfo.insert(IRType::SUB, IRInfo {
        name: "SUB",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::BPREL, IRInfo {
        name: "BPREL",
        ty: IRInfoType::REG_IMM
    });
    irinfo.insert(IRType::IF, IRInfo {
        name: "IF",
        ty: IRInfoType::REG_LABEL
    });
    irinfo.insert(IRType::UNLESS, IRInfo {
        name: "UNLESS",
        ty: IRInfoType::REG_LABEL
    });
    irinfo.insert(IRType::NULL, IRInfo {
        name: "",
        ty: IRInfoType::NULL
    });
}

pub fn irinfo_get(ty: & IRType) -> Option<IRInfo> {
    init_irinfo();

    let mut ret = None;
    match IRINFO.lock() {
        Ok(irinfo) => {
            match irinfo.get(ty) {
                Some(info) => {
                    ret = Some(info.clone());
                }
                None => { }
            }
        }
        Err(_) => { }
    }
    return ret;
}

pub fn tostr(ir: IR) -> String {
    let irinfo = IRINFO.lock().unwrap();
    let info = irinfo.get(&ir.op).unwrap();

    return match info.ty {
        IRInfoType::LABEL => format!(".L{}:", ir.lhs),
        IRInfoType::LABEL_ADDR => format!("  {} r{}, {}", info.name, ir.lhs, ir.name),
        IRInfoType::IMM => format!("  {} {}", ir.name, ir.lhs),
        IRInfoType::REG => format!("  {} r{}", info.name, ir.lhs),
        IRInfoType::JMP => format!("  {} .L{}", info.name, ir.lhs),
        IRInfoType::REG_REG => format!("  {} r{}, r{}", info.name, ir.lhs, ir.rhs),
        IRInfoType::REG_IMM => format!("  {} r{}, {}", info.name, ir.lhs, ir.rhs),
        IRInfoType::IMM_IMM => format!("  {} {}, {}", info.name, ir.lhs, ir.rhs),
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
    };
}

pub fn dump_ir(irv: Vec<IR>) {
    init_irinfo();

    for i in 0..irv.len() {
        let fun = &irv[i];
        eprintln!("{}():", fun.name);
        for j in 0..fun.ir.len() {
            eprintln!("{}", tostr(fun.ir[j].clone()));
        }
    }
}

