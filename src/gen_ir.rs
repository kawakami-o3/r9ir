// Intermediate representation
#![allow(dead_code, non_camel_case_types)]

use crate::parse::*;
use crate::sema::*;
use crate::util::size_of;
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

    static ref NREG: Mutex<i32> = Mutex::new(1);
    static ref NLABEL: Mutex<i32> = Mutex::new(0);

    static ref RETURN_LABEL: Mutex<i32> = Mutex::new(0);
    static ref RETURN_REG: Mutex<i32> = Mutex::new(0);

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
    irinfo.insert(IRType::SUB_IMM, IRInfo {
        name: "SUB",
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

fn nlabel() -> i32 {
    *NLABEL.lock().unwrap()
}

fn bump_nlabel() -> i32 {
    let mut nlabel = NLABEL.lock().unwrap();
    let ret = *nlabel;
    *nlabel += 1;
    return ret;
}

fn set_nlabel(i: i32) {
    let mut nlabel = NLABEL.lock().unwrap();
    *nlabel = i;
}

fn nreg() -> i32 {
    *NREG.lock().unwrap()
}

fn bump_nreg() -> i32 {
    let mut nreg = NREG.lock().unwrap();
    let ret = *nreg;
    *nreg += 1;
    return ret;
}

fn set_nreg(i: i32) {
    let mut nreg = NREG.lock().unwrap();
    *nreg = i;
}

fn return_label() -> i32 {
    *RETURN_LABEL.lock().unwrap()
}

fn set_return_label(i: i32) {
    let mut return_label = RETURN_LABEL.lock().unwrap();
    *return_label = i;
}

fn return_reg() -> i32 {
    *RETURN_REG.lock().unwrap()
}

fn set_return_reg(i: i32) {
    let mut return_reg = RETURN_REG.lock().unwrap();
    *return_reg = i;
}

fn init_code() {
    let mut code = CODE.lock().unwrap();
    *code = Vec::new();
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IRType {
    IMM,
    SUB_IMM,
    MOV,
    RETURN,
    CALL,
    LABEL,
    LABEL_ADDR,
    EQ,
    NE,
    LT,
    JMP,
    IF,
    UNLESS,
    LOAD8,
    LOAD32,
    LOAD64,
    STORE8,
    STORE32,
    STORE64,
    STORE8_ARG,
    STORE32_ARG,
    STORE64_ARG,
    KILL,
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

    // Function struct fields in 9cc
    pub stacksize: i32,
    pub ir: Vec<IR>,
    pub globals: Vec<Var>,
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

fn tostr(ir: IR) -> String {
    let irinfo = IRINFO.lock().unwrap();
    let info = irinfo.get(&ir.op).unwrap();

    return match info.ty {
        IRInfoType::LABEL => format!(".L{}:", ir.lhs),
        IRInfoType::LABEL_ADDR => format!(" {} r{}, {}", info.name, ir.lhs, ir.name),
        IRInfoType::IMM => format!("  {} {}", ir.name, ir.lhs),
        IRInfoType::REG => format!("  {} r{}", info.name, ir.lhs),
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
        globals: Vec::new(),
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

fn kill(r: i32) {
    add(IRType::KILL, r, -1);
}

fn label(x: i32) {
    add(IRType::LABEL, x, -1);
}

fn gen_lval(node: Node) -> i32 {
    if node.op == NodeType::DEREF {
        return gen_expr(*node.expr.unwrap());
    }

    if node.op == NodeType::LVAR {
        let r = bump_nreg();
        add(IRType::MOV, r, 0);
        add(IRType::SUB_IMM, r, node.offset);
        return r;
    }

    assert!(node.op == NodeType::GVAR);
    let r = bump_nreg();
    let ir_idx = add(IRType::LABEL_ADDR, r, -1);
    match CODE.lock() {
        Ok(mut code) => {
            code[ir_idx].name = node.name.clone();
        }
        Err(_) => {
            panic!();
        }
    }
    return r;
}

fn gen_binop(ty: IRType, lhs: Node, rhs: Node) -> i32 {
    let r1 = gen_expr(lhs);
    let r2 = gen_expr(rhs);
    add(ty, r1, r2);
    kill(r2);
    return r1;
}

fn gen_expr(node: Node) -> i32 {
    match node.op {
        NodeType::NUM => {
            let r = bump_nreg();
            add(IRType::IMM, r, node.val);
            return r;
        }

        NodeType::EQ => {
            let lhs = gen_expr(*node.lhs.unwrap());
            let rhs = gen_expr(*node.rhs.unwrap());
            add(IRType::EQ, lhs, rhs);
            kill(rhs);
            return lhs;
        }

        NodeType::NE => {
            let lhs = gen_expr(*node.lhs.unwrap());
            let rhs = gen_expr(*node.rhs.unwrap());
            add(IRType::NE, lhs, rhs);
            kill(rhs);
            return lhs;
        }

        NodeType::LOGAND => {
            let x = bump_nlabel();

            let r1 = gen_expr(*node.lhs.unwrap());
            add(IRType::UNLESS, r1, x);
            let r2 = gen_expr(*node.rhs.unwrap());
            add(IRType::MOV, r1, r2);
            kill(r2);
            add(IRType::UNLESS, r1, x);
            add(IRType::IMM, r1, 1);
            label(x);
            return r1;
        }

        NodeType::LOGOR => {
            let x = bump_nlabel();
            let y = bump_nlabel();

            let r1 = gen_expr(*node.lhs.unwrap());
            add(IRType::UNLESS, r1, x);
            add(IRType::IMM, r1, 1);
            add(IRType::JMP, y, -1);
            label(x);

            let r2 = gen_expr(*node.rhs.unwrap());
            add(IRType::MOV, r1, r2);
            kill(r2);
            add(IRType::UNLESS, r1, y);
            add(IRType::IMM, r1, 1);
            label(y);
            return r1
        }

        NodeType::GVAR | NodeType::LVAR => {
            let r = gen_lval(node.clone());
            if node.ty.ty == CType::CHAR {
                add(IRType::LOAD8, r, r);
            } else if node.ty.ty == CType::INT {
                add(IRType::LOAD32, r, r);
            } else {
                add(IRType::LOAD64, r, r);
            }
            return r;
        }

        NodeType::CALL => {
            let mut args = Vec::new();
            for a in node.args.iter() {
                args.push(gen_expr(a.clone()));
            }

            let r = bump_nreg();

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
                kill(args[i]);
            }
            return r;
        }

        NodeType::ADDR => {
            return gen_lval(*node.expr.unwrap());
        }

        NodeType::DEREF => {
            let r = gen_expr(*node.clone().expr.unwrap());
            if node.clone().expr.unwrap().ty.ptr_of.unwrap().ty == CType::CHAR {
                add(IRType::LOAD8, r, r);
            } else if node.clone().expr.unwrap().ty.ptr_of.unwrap().ty == CType::INT {
                add(IRType::LOAD32, r, r);
            } else {
                add(IRType::LOAD64, r, r);
            }
            return r;
        }

        NodeType::STMT_EXPR => {
            let orig_label = return_label();
            let orig_reg = return_reg();
            set_return_label(bump_nlabel());
            let r = bump_nreg();
            set_return_reg(r);

            gen_stmt(*node.stmt.clone().unwrap());
            label(return_label());

            set_return_label(orig_label);
            set_return_reg(orig_reg);
            return r;
        }

        NodeType::EQL => {
            let rhs = gen_expr(*node.clone().rhs.unwrap());
            let lhs = gen_lval(*node.clone().lhs.unwrap());
            if node.lhs.clone().unwrap().ty.ty == CType::CHAR {
                add(IRType::STORE8, lhs, rhs);
            } else if node.lhs.clone().unwrap().ty.ty == CType::PTR {
                add(IRType::STORE64, lhs, rhs);
            } else {
                add(IRType::STORE32, lhs, rhs);
            }
            kill(rhs);
            return lhs;
        }
        NodeType::ADD | NodeType::SUB => {
            let insn = match node.op {
                NodeType::ADD => IRType::ADD,
                NodeType::SUB => IRType::SUB,
                _ => {
                    panic!();
                }
            };
            match node.lhs {
                Some(ref lhs) => {
                    if lhs.ty.ty != CType::PTR {
                        return gen_binop(insn, *lhs.clone(), *node.rhs.unwrap());
                    }
                }
                None => {}
            }

            let rhs = gen_expr(*node.rhs.unwrap());
            let r = bump_nreg();
            match node.lhs {
                Some(ref lhs) => {
                    add(IRType::IMM, r, size_of(& lhs.clone().ty.ptr_of.unwrap()));
                }
                None => {}
            }
            add(IRType::MUL, rhs, r);
            kill(r);

            let lhs = gen_expr(*node.lhs.unwrap());
            add(insn, lhs, rhs);
            kill(rhs);

            return lhs;
            //return gen_binop(IRType::SUB, *node.lhs.unwrap(), *node.rhs.unwrap());
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
            panic!("unknown AST type {:?}", node);
        }
    }
}

fn gen_stmt(node: Node) {
    if node.op == NodeType::VARDEF {
        if node.init.is_none() {
            return;
        }

        let rhs = gen_expr(*node.init.unwrap());
        let lhs = bump_nreg();
        add(IRType::MOV, lhs, 0);
        add(IRType::SUB_IMM, lhs, node.offset);
        if node.ty.ty == CType::CHAR {
            add(IRType::STORE8, lhs, rhs);
        } else if node.ty.ty == CType::INT {
            add(IRType::STORE32, lhs, rhs);
        } else {
            add(IRType::STORE64, lhs, rhs);
        }
        kill(lhs);
        kill(rhs);
        return;
    }

    match node.op {
        NodeType::IF => {
            let cond = *node.cond.unwrap();
            let then = *node.then.unwrap();
            if node.els.is_some() {
                let x = bump_nlabel();
                let y = bump_nlabel();
                let r = gen_expr(cond.clone());
                add(IRType::UNLESS, r, x);
                kill(r);
                gen_stmt(then.clone());
                add(IRType::JMP, y, -1);
                label(x);
                gen_stmt(*node.els.unwrap());
                label(y);
                return;
            }

            let x = bump_nlabel();
            let r = gen_expr(cond);

            add(IRType::UNLESS, r, x);
            kill(r);
            gen_stmt(then);
            label(x);
        }
        NodeType::FOR => {
            let x = bump_nlabel();
            let y = bump_nlabel();

            gen_stmt(*node.init.unwrap());
            label(x);
            let r = gen_expr(*node.cond.unwrap());
            add(IRType::UNLESS, r, y);
            kill(r);
            gen_stmt(*node.body.unwrap());
            kill(gen_expr(*node.inc.unwrap()));
            add(IRType::JMP, x, -1);
            label(y);
        }
        NodeType::DO_WHILE => {
            let x = bump_nlabel();
            label(x);
            gen_stmt(*node.body.unwrap());
            let r = gen_expr(*node.cond.unwrap());
            add(IRType::IF, r, x);
            kill(r);
        }
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());

            // Statement expression (GNU extension)
            if return_label() != 0 {
                add(IRType::MOV, return_reg(), r);
                kill(r);
                add(IRType::JMP, return_label(), -1);
                return;
            }

            add(IRType::RETURN, r, -1);
            kill(r);
        }
        NodeType::EXPR_STMT => {
            let r = gen_expr(*node.expr.unwrap());
            kill(r);
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

pub fn gen_ir(nodes: Vec<Node>) -> Vec<IR> {
    let mut v = Vec::new();
    set_nlabel(1);

    init_irinfo();

    for i in 0..nodes.len() {
        let node = nodes[i].clone();

        if node.op == NodeType::VARDEF {
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_code();
        set_nreg(1);

        for i in 0..node.args.len() {
            let arg = &node.args[i];

            if arg.ty.ty == CType::CHAR {
                add(IRType::STORE8_ARG, arg.offset, i as i32);
            } else if arg.ty.ty == CType::INT {
                add(IRType::STORE32_ARG, arg.offset, i as i32);
            } else {
                add(IRType::STORE64_ARG, arg.offset, i as i32);
            }
        }
        gen_stmt(*node.body.unwrap());

        let mut fun = alloc_ir();
        fun.name = node.name.clone();
        fun.stacksize = node.stacksize;
        fun.ir = CODE.lock().unwrap().clone();
        fun.globals = node.globals;

        v.push(fun);
    }

    return v;
}
