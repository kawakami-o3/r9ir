// Intermediate representation

// 9cc's code generation is two-pass. In the first pass, abstract
// syntax trees are compiled to IR (intermediate representation).
//
// IR resembles the real x86-64 instruction set, but it has infinite
// number of registers. We don't try too hard to reuse registers in
// this pass. Instead, we "kill" registers to mark them as dead when
// we are done with them and use new registers.
//
// Such infinite number of registers are mapped to a finite registers
// in a later pass.

#![allow(dead_code, non_camel_case_types)]

use crate::parse::*;
use crate::sema::*;
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
    static ref NLABEL: Mutex<i32> = Mutex::new(1);

    static ref RETURN_LABEL: Mutex<i32> = Mutex::new(0);
    static ref RETURN_REG: Mutex<i32> = Mutex::new(0);
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
    BPREL,
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

fn choose_insn(node: Node, op8: IRType, op32: IRType, op64: IRType) -> IRType {
    match node.ty.size {
        1 => op8,
        4 => op32,
        8 => op64,
        _ => panic!(),
    }
}

fn load_insn(node: Node) -> IRType {
    return choose_insn(node, IRType::LOAD8, IRType::LOAD32, IRType::LOAD64);
}

fn store_insn(node: Node) -> IRType {
    return choose_insn(node, IRType::STORE8, IRType::STORE32, IRType::STORE64);
}

fn store_arg_insn(node: Node) -> IRType {
    return choose_insn(node, IRType::STORE8_ARG, IRType::STORE32_ARG, IRType::STORE64_ARG);
}

// In C, all expressions that can be written on the left-hand side of
// the '=' operator must have an address in memory. In other words, if
// you can apply the '&' operator to take an address of some
// expression E, you can assign E to a new value.
//
// Other expressions, such as `1+2`, cannot be written on the lhs of
// '=', since they are just temporary values that don't have an address.
//
// The stuff that can be written on the lhs of '=' is called lvalue.
// Other values are called rvalue. An lvalue is essentially an address.
//
// When lvalues appear on the rvalue context, they are converted to
// rvalues by loading their values from their addresses. You can think
// '&' as an operator that suppresses such automatic lvalue-to-rvalue
// conversion.
//
// This function evaluates a given node as an lvalue.
fn gen_lval(node: Node) -> i32 {
    if node.op == NodeType::DEREF {
        return gen_expr(*node.expr.unwrap());
    }

    if node.op == NodeType::DOT {
        let r1 = gen_lval(*node.expr.unwrap());
        let r2 = bump_nreg();
        add(IRType::IMM, r2, node.offset);
        add(IRType::ADD, r1, r2);
        kill(r2);
        return r1;
    }

    if node.op == NodeType::LVAR {
        let r = bump_nreg();
        add(IRType::BPREL, r, node.offset);
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

fn gen_binop(ty: IRType, node: Node) -> i32 {
    let r1 = gen_expr(*node.lhs.unwrap());
    let r2 = gen_expr(*node.rhs.unwrap());
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
            return gen_binop(IRType::EQ, node);
        }

        NodeType::NE => {
            return gen_binop(IRType::NE, node);
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

        NodeType::GVAR |
            NodeType::LVAR |
            NodeType::DOT => {
            let r = gen_lval(node.clone());
            add(load_insn(node), r, r);
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
            add(load_insn(node), r, r);
            return r;
        }

        NodeType::STMT_EXPR => {
            let orig_label = return_label();
            let orig_reg = return_reg();
            set_return_label(bump_nlabel());
            let r = bump_nreg();
            set_return_reg(r);

            gen_stmt(*node.body.clone().unwrap());
            label(return_label());

            set_return_label(orig_label);
            set_return_reg(orig_reg);
            return r;
        }

        NodeType::EQL => {
            let rhs = gen_expr(*node.clone().rhs.unwrap());
            let lhs = gen_lval(*node.clone().lhs.unwrap());
            add(store_insn(node), lhs, rhs);
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
                        return gen_binop(insn, node.clone());
                    }
                }
                None => {}
            }

            let rhs = gen_expr(*node.rhs.unwrap());
            let r = bump_nreg();
            match node.lhs {
                Some(ref lhs) => {
                    add(IRType::IMM, r, lhs.ty.ptr_to.clone().unwrap().size);
                }
                None => {}
            }
            add(IRType::MUL, rhs, r);
            kill(r);

            let lhs = gen_expr(*node.lhs.unwrap());
            add(insn, lhs, rhs);
            kill(rhs);

            return lhs;
        }
        NodeType::MUL => {
            return gen_binop(IRType::MUL, node);
        }
        NodeType::DIV => {
            return gen_binop(IRType::DIV, node);
        }
        NodeType::LT => {
            return gen_binop(IRType::LT, node);
        }
        NodeType::EXCLAM => {
            let lhs = gen_expr(*node.expr.unwrap());
            let rhs = bump_nreg();
            add(IRType::IMM, rhs, 0);
            add(IRType::EQ, lhs, rhs);
            kill(rhs);
            return lhs;
        }
        _ => {
            panic!("unknown AST type {:?}", node);
        }
    }
}

fn gen_stmt(node: Node) {
    if node.op == NodeType::NULL {
        return;
    }

    if node.op == NodeType::VARDEF {
        if node.init.is_none() {
            return;
        }

        let rhs = gen_expr(*node.init.clone().unwrap());
        let lhs = bump_nreg();
        add(IRType::BPREL, lhs, node.offset);
        add(store_insn(node), lhs, rhs);
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
            gen_stmt(*node.inc.unwrap());
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

    for i in 0..nodes.len() {
        let node = nodes[i].clone();

        if node.op == NodeType::VARDEF {
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_code();

        for i in 0..node.args.len() {
            let arg = &node.args[i];
            add(store_arg_insn(arg.clone()), arg.offset, i as i32);
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
