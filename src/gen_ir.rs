// Intermediate representation
#![allow(dead_code, non_camel_case_types)]

use crate::parse::*;
use std::collections::HashMap;
use std::sync::Mutex;

fn to_ir_type(node_type: &NodeType) -> IRType {
    match node_type {
        NodeType::ADD => IRType::ADD,
        NodeType::SUB => IRType::SUB,
        NodeType::MUL => IRType::MUL,
        NodeType::DIV => IRType::DIV,
        _ => {
            panic!(format!("unknown NodeType {:?}", node_type));
        }
    }
}

lazy_static! {
    static ref CODE: Mutex<Vec<IR>> = Mutex::new(Vec::new());

    static ref VARS: Mutex<HashMap<String, i32>> = Mutex::new(HashMap::new());

    static ref REGNO: Mutex<i32> = Mutex::new(1);
    static ref STACKSIZE: Mutex<i32> = Mutex::new(0);
    static ref LABEL: Mutex<i32> = Mutex::new(0);

    // Compile AST to intermediate code that has infinite number of registers.
    // Base pointer is always assigned to r0.

    pub static ref IRINFO: Mutex<HashMap<IRType, IRInfo>> = Mutex::new(HashMap::new());
}


fn init_irinfo() {
    let mut irinfo = IRINFO.lock().unwrap();

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
        name: "MOV",
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
    irinfo.insert(IRType::LT, IRInfo {
        name: "LT",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::LOAD, IRInfo {
        name: "LOAD",
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
    irinfo.insert(IRType::SAVE_ARGS, IRInfo {
        name: "SAVE_ARGS",
        ty: IRInfoType::IMM
    });
    irinfo.insert(IRType::STORE, IRInfo {
        name: "STORE",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::SUB, IRInfo {
        name: "SUB",
        ty: IRInfoType::REG_REG
    });
    irinfo.insert(IRType::SUB_IMM, IRInfo {
        name: "SUB",
        ty: IRInfoType::REG_IMM
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

fn label() -> i32 {
    *LABEL.lock().unwrap()
}

fn inc_label() {
    let mut label = LABEL.lock().unwrap();
    *label += 1;
}

fn regno() -> i32 {
    *REGNO.lock().unwrap()
}

fn inc_regno() {
    let mut regno = REGNO.lock().unwrap();
    *regno += 1;
}

fn stacksize() -> i32 {
    *STACKSIZE.lock().unwrap()
}

fn add_stacksize(i: i32) {
    let mut stacksize = STACKSIZE.lock().unwrap();
    *stacksize += i;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IRType {
    IMM,
    SUB_IMM,
    MOV,
    RETURN,
    CALL,
    LABEL,
    LT,
    JMP,
    UNLESS,
    ALLOCA,
    LOAD,
    STORE,
    KILL,
    SAVE_ARGS,
    ADD,
    SUB,
    MUL,
    DIV,
    NOP,
    NULL,
}

#[derive(Clone, Debug)]
pub struct IR {
    pub op: IRType,
    pub lhs: i32,
    pub rhs: i32,

    pub name: String,
    pub nargs: usize,
    pub args: [i32; 6],

    // Function
    pub stacksize: i32,
    pub ir: Vec<IR>,
}

#[derive(Clone, Debug)]
pub enum IRInfoType {
    NOARG,
    REG,
    IMM,
    JMP,
    LABEL,
    REG_REG,
    REG_IMM,
    REG_LABEL,
    CALL,
    NULL,
}

#[derive(Clone, Debug)]
pub struct IRInfo {
    pub name: &'static str,
    pub ty: IRInfoType,
}

fn tostr(ir: IR) -> String {
    let irinfo = IRINFO.lock().unwrap();
    let info = irinfo.get(&ir.op).unwrap();

    return match info.ty {
        IRInfoType::LABEL => format!(".L{}:", ir.lhs),
        IRInfoType::IMM => format!("  {} {}", ir.name, ir.lhs),
        IRInfoType::REG => format!("  {} r{}", info.name, ir.lhs),
        IRInfoType::REG_REG => format!("  {} r{}, r{}", info.name, ir.lhs, ir.rhs),
        IRInfoType::REG_IMM => format!("  {} r{}, {}", info.name, ir.lhs, ir.rhs),
        IRInfoType::REG_LABEL => format!("  {} r{}, .L{}", info.name, ir.lhs, ir.rhs),
        IRInfoType::CALL => {
            let mut s = String::new();
            s.push_str(&format!("  r{} = {}(", ir.lhs, ir.name));
            for i in ir.args.iter() {
                s.push_str(&format!(", r{}", i));
            }
            s.push_str(")");
            s
        }
        IRInfoType::NOARG => format!("  {}", info.name),
        _ => {
            panic!("unknown ir");
        }
    };
}

fn dump_ir(irv: Vec<IR>) {
    for i in 0..irv.len() {
        let fun = &irv[i];
        eprintln!("{}():", fun.name);
        for j in 0..fun.ir.len() {
            eprintln!("{}", tostr(fun.ir[j].clone()));
        }
    }
}

fn alloc_ir() -> IR {
    return IR {
        op: IRType::NOP,
        lhs: 0,
        rhs: 0,
        
        name: String::new(),
        nargs: 0,
        args: [0; 6],

        stacksize: 0,
        ir: Vec::new(),
    };
}
fn add(op: IRType, lhs: i32, rhs: i32) -> usize {
    let mut ir = alloc_ir();
    ir.op = op;
    ir.lhs = lhs;
    ir.rhs = rhs;
        
    match CODE.lock() {
        Ok(mut code) => {
            (*code).push(ir);
            return code.len() - 1;
        }
        Err(_) => {
            panic!();
        }
    }
}

fn gen_lval(node: Node) -> i32 {
    if node.ty != NodeType::IDENT {
        panic!("not an lvaue");
    }

    let mut vars = VARS.lock().unwrap();
    if None == vars.get(&node.name) {
        add_stacksize(8);
        (*vars).insert(node.name.clone(), stacksize());
    }

    let r = regno();
    inc_regno();

    let off = *vars.get(&node.name).unwrap();

    add(IRType::MOV, r, 0);
    add(IRType::SUB_IMM, r, off);
    return r;
}

fn gen_binop(ty: IRType, lhs: Node, rhs: Node) -> i32 {
    let r1 = gen_expr(lhs);
    let r2 = gen_expr(rhs);
    add(ty, r1, r2);
    add(IRType::KILL, r2, -1);
    return r1;
}

fn gen_expr(node: Node) -> i32 {
    match node.ty {
        NodeType::NUM => {
            let r = regno();
            inc_regno();
            add(IRType::IMM, r, node.val);
            return r;
        }

        NodeType::LOGAND => {
            let x = label();
            inc_label();

            let r1 = gen_expr(*node.lhs.unwrap());
            add(IRType::UNLESS, r1, x);
            let r2 = gen_expr(*node.rhs.unwrap());
            add(IRType::MOV, r1, r2);
            add(IRType::KILL, r2, -1);
            add(IRType::UNLESS, r1, x);
            add(IRType::IMM, r1, 1);
            add(IRType::LABEL, x, -1);
            return r1;
        }

        NodeType::LOGOR => {
            let x = label();
            inc_label();
            let y = label();
            inc_label();

            let r1 = gen_expr(*node.lhs.unwrap());
            add(IRType::UNLESS, r1, x);
            add(IRType::IMM, r1, 1);
            add(IRType::JMP, y, -1);
            add(IRType::LABEL, x, -1);

            let r2 = gen_expr(*node.rhs.unwrap());
            add(IRType::MOV, r1, r2);
            add(IRType::KILL, r2, -1);
            add(IRType::UNLESS, r1, y);
            add(IRType::IMM, r1, 1);
            add(IRType::LABEL, y, -1);
            return r1
        }

        NodeType::IDENT => {
            let r = gen_lval(node);
            add(IRType::LOAD, r, r);
            return r;
        }

        NodeType::CALL => {
            let mut args = Vec::new();
            for a in node.args.iter() {
                args.push(gen_expr(a.clone()));
            }

            let r = regno();
            inc_regno();

            let ir_idx = add(IRType::CALL, r, -1);

            let (nargs, args) = match CODE.lock() {
                Ok(mut code) => {
                    code[ir_idx].name = node.name.clone();
                    code[ir_idx].nargs = node.args.len();

                    for i in 0..code[ir_idx].nargs {
                        code[ir_idx].args[i] = args[i];
                    }

                    (code[ir_idx].nargs, code[ir_idx].args)
                }
                Err(_) => {
                    panic!();
                }
            };

            for i in 0..nargs {
                add(IRType::KILL, args[i], -1);
            }
            return r;
        }

        NodeType::EQ => {
            let rhs = gen_expr(*node.rhs.unwrap());
            let lhs = gen_lval(*node.lhs.unwrap());
            add(IRType::STORE, lhs, rhs);
            add(IRType::KILL, rhs, -1);
            return lhs;
        }
        NodeType::ADD => {
            return gen_binop(IRType::ADD, *node.lhs.unwrap(), *node.rhs.unwrap());
        }
        NodeType::SUB => {
            return gen_binop(IRType::SUB, *node.lhs.unwrap(), *node.rhs.unwrap());
        }
        NodeType::MUL => {
            return gen_binop(IRType::MUL, *node.lhs.unwrap(), *node.rhs.unwrap());
        }
        NodeType::DIV => {
            return gen_binop(IRType::DIV, *node.lhs.unwrap(), *node.rhs.unwrap());
        }
        NodeType::LT => {
            return gen_binop(IRType::LT, *node.lhs.unwrap(), *node.rhs.unwrap());
        }
        _ => {
            panic!("unknown AST type");
        }
    }
}

fn gen_stmt(node: Node) {
    match node.ty {
        NodeType::IF => {
            let r = gen_expr(*node.cond.unwrap());
            let mut label = LABEL.lock().unwrap();
            let x = *label;
            *label += 1;

            add(IRType::UNLESS, r, x);
            add(IRType::KILL, r, -1);

            gen_stmt(*node.then.unwrap());

            if node.els.is_none() {
                add(IRType::LABEL, x, -1);
                return;
            }

            let y = *label;
            *label += 1;
            add(IRType::JMP, y, -1);
            add(IRType::LABEL, x, -1);
            gen_stmt(*node.els.unwrap());
            add(IRType::LABEL, y, -1);
        }
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::RETURN, r, -1);
            add(IRType::KILL, r, -1);
        }
        NodeType::EXPR_STMT => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::KILL, r, -1);
        }
        NodeType::COMP_STMT => {
            for n in node.stmts {
                gen_stmt(n);
            }
        }
        _ => {
            panic!(format!("unknown node: {:?}", node.ty));
        }
    }
}

fn gen_args(nodes: Vec<Node>) {
    if nodes.len() == 0 {
        return;
    }

    add(IRType::SAVE_ARGS, nodes.len() as i32, -1);
    for i in 0..nodes.len() {
        let node = &nodes[i];
        if node.ty != NodeType::IDENT {
            panic!("bad parameter");
        }

        add_stacksize(8);
        match VARS.lock() {
            Ok(mut vars) => {
                (*vars).insert(node.name.clone(), stacksize());
            }
            Err(_) => {
                panic!();
            }
        }
    }
}

pub fn gen_ir(nodes: Vec<Node>) -> Vec<IR> {
    let mut v = Vec::new();

    init_irinfo();

    for i in 0..nodes.len() {
        let node = nodes[i].clone();
        assert!(node.ty == NodeType::FUNC, "");

        *CODE.lock().unwrap() = Vec::new();

        gen_args(node.args);
        gen_stmt(*node.body.unwrap());

        let mut fun = alloc_ir();
        fun.name = node.name.clone();
        fun.stacksize = stacksize();
        fun.ir = CODE.lock().unwrap().clone();

        v.push(fun);
    }

    return v;
}
