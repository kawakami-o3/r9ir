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

#![allow(non_camel_case_types)]

use crate::parse::*;
use crate::util::*;
use std::cell::RefCell;

thread_local! {
    static CODE: RefCell<Vec<IR>> = RefCell::new(Vec::new());
    static NREG: RefCell<i32> = RefCell::new(1);
    static BREAK_LABEL: RefCell<i32> = RefCell::new(0);
}

fn bump_nreg() -> i32 {
    NREG.with(|v| {
        let ret = *v.borrow();
        *v.borrow_mut() += 1;
        return ret;
    })
}

fn init_code() {
    CODE.with(|code| {
        *code.borrow_mut() = Vec::new();
    })
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
    LE,
    LT,
    AND,
    OR,
    XOR,
    SHL,
    SHR,
    MOD,
    JMP,
    IF,
    UNLESS,
    LOAD,
    STORE,
    STORE_ARG,
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

    // Load/store size in bytes
    pub size: i32,

    // For binary operator. If true, rhs is an immediate.
    pub is_imm: bool,

    // Function call
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

        size: 0,

        is_imm: false,

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

    return CODE.with(|code| {
        (*code.borrow_mut()).push(ir);
        return code.borrow().len() - 1;
    });
}

fn add_imm(op: IRType, lhs: i32, rhs: i32) -> usize {
    let ir_idx = add(op, lhs, rhs);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.is_imm = true;
    });

    return ir_idx;
}

fn kill(r: i32) {
    add(IRType::KILL, r, -1);
}

fn label(x: i32) {
    add(IRType::LABEL, x, -1);
}

fn jmp(x: i32) {
    add(IRType::JMP, x, -1);
}

fn load(node: & Node, dst: i32, src: i32) {
    let ir_idx = add(IRType::LOAD, dst, src);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.size = node.ty.borrow().size;
    });
}

fn store(node: & Node, dst: i32, src: i32) {
    let ir_idx = add(IRType::STORE, dst, src);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.size = node.ty.borrow().size;
    });
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
        let r = gen_lval(*node.expr.unwrap());
        add_imm(IRType::ADD, r, node.ty.borrow().offset);
        return r;
    }

    assert!(node.op == NodeType::VARREF);
    let var = node.var.unwrap();

    let r = bump_nreg();
    if var.borrow().is_local {
        add(IRType::BPREL, r, var.borrow().offset);
    } else {
        let ir_idx = add(IRType::LABEL_ADDR, r, -1);
        CODE.with(|c| {
            let code = &mut *c.borrow_mut();
            code[ir_idx].name = var.borrow().name.clone();
        });
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

fn get_inc_scale(node: & Node) -> i32 {
    if node.ty.borrow().ty == CType::PTR {
        let tmp = node.ty.borrow().clone().ptr_to.unwrap();
        return tmp.borrow().size;
    }
    return 1;
}

fn gen_pre_inc(node: & Node, num: i32) -> i32 {
    let addr = gen_lval(*node.expr.clone().unwrap());
    let val = bump_nreg();
    load(&node, val, addr);
    add_imm(IRType::ADD, val, num * get_inc_scale(&node));
    store(&node, addr, val);
    kill(addr);
    return val;
}

fn gen_post_inc(node: & Node, num: i32) -> i32 {
    let val = gen_pre_inc(&node, num);
    add_imm(IRType::SUB, val, num * get_inc_scale(&node));
    return val;
}

fn to_assign_op(op: NodeType) -> IRType {
    match op {
        NodeType::MUL_EQ => IRType::MUL,
        NodeType::DIV_EQ => IRType::DIV,
        NodeType::MOD_EQ => IRType::MOD,
        NodeType::ADD_EQ => IRType::ADD,
        NodeType::SUB_EQ => IRType::SUB,
        NodeType::SHL_EQ => IRType::SHL,
        NodeType::SHR_EQ => IRType::SHR,
        NodeType::AND_EQ => IRType::AND,
        NodeType::XOR_EQ => IRType::XOR,
        NodeType::OR_EQ => IRType::OR,
        _ => {
            panic!();
        }
    }
}

fn gen_assign_op(node: Node) -> i32 {
    let src = gen_expr(*node.rhs.clone().unwrap());
    let dst = gen_lval(*node.lhs.clone().unwrap());
    let val = bump_nreg();

    load(&node, val, dst);
    add(to_assign_op(node.op.clone()), val, src);
    kill(src);
    store(&node, dst, val);
    kill(dst);
    return val;
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
            let x = bump_nlabel() as i32;

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
            let x = bump_nlabel() as i32;
            let y = bump_nlabel() as i32;

            let r1 = gen_expr(*node.lhs.unwrap());
            add(IRType::UNLESS, r1, x);
            add(IRType::IMM, r1, 1);
            jmp(y);
            label(x);

            let r2 = gen_expr(*node.rhs.unwrap());
            add(IRType::MOV, r1, r2);
            kill(r2);
            add(IRType::UNLESS, r1, y);
            add(IRType::IMM, r1, 1);
            label(y);
            return r1
        }

        NodeType::VARREF |
            NodeType::DOT => {
            let r = gen_lval(node.clone());
            load(&node, r, r);
            return r;
        }

        NodeType::CALL => {
            let mut args = Vec::new();
            for a in node.args.iter() {
                args.push(gen_expr(a.clone()));
            }

            let r = bump_nreg();

            let ir_idx = add(IRType::CALL, r, -1);

            let (nargs, args) = CODE.with(|c| {
                let code = &mut *c.borrow_mut();
                code[ir_idx].name = node.name.clone();
                code[ir_idx].nargs = node.args.len();

                for i in 0..code[ir_idx].nargs {
                    code[ir_idx].args[i] = args[i];
                }

                (code[ir_idx].nargs, code[ir_idx].args)
            });

            for i in 0..nargs {
                kill(args[i]);
            }
            return r;
        }

        NodeType::ADDR => {
            return gen_lval(*node.expr.unwrap());
        }

        NodeType::DEREF => {
            let r = gen_expr(*node.expr.clone().unwrap());
            load(&node, r, r);
            return r;
        }

        NodeType::CAST => {
            let r = gen_expr(*node.expr.clone().unwrap());
            if node.ty.borrow().ty != CType::BOOL {
                return r;
            }
            let r2 = bump_nreg();
            add(IRType::IMM, r2, 0);
            add(IRType::NE, r, r2);
            kill(r2);
            return r;
        }

        NodeType::STMT_EXPR => {
            gen_stmt(*node.body.unwrap());
            return gen_expr(*node.expr.unwrap());
        }

        NodeType::MUL_EQ |
            NodeType::DIV_EQ |
            NodeType::MOD_EQ |
            NodeType::ADD_EQ |
            NodeType::SUB_EQ |
            NodeType::SHL_EQ |
            NodeType::SHR_EQ |
            NodeType::AND_EQ |
            NodeType::XOR_EQ |
            NodeType::OR_EQ => {
                return gen_assign_op(node);
        }
        NodeType::EQL => {
            let rhs = gen_expr(*node.clone().rhs.unwrap());
            let lhs = gen_lval(*node.clone().lhs.unwrap());
            store(&node, lhs, rhs);
            kill(lhs);
            return rhs;
        }
        NodeType::ADD => { return gen_binop(IRType::ADD, node); }
        NodeType::SUB => { return gen_binop(IRType::SUB, node); }
        NodeType::MUL => { return gen_binop(IRType::MUL, node); }
        NodeType::DIV => { return gen_binop(IRType::DIV, node); }
        NodeType::MOD => { return gen_binop(IRType::MOD, node); }
        NodeType::LT => { return gen_binop(IRType::LT, node); }
        NodeType::LE => { return gen_binop(IRType::LE, node); }
        NodeType::AND => { return gen_binop(IRType::AND, node); }
        NodeType::OR => { return gen_binop(IRType::OR, node); }
        NodeType::XOR => { return gen_binop(IRType::XOR, node); }
        NodeType::SHL => { return gen_binop(IRType::SHL, node); }
        NodeType::SHR => { return gen_binop(IRType::SHR, node); }
        NodeType::NOT => {
            let r = gen_expr(*node.expr.unwrap());
            add_imm(IRType::XOR, r, -1);
            return r;
        }
        NodeType::COMMA => {
            kill(gen_expr(*node.lhs.unwrap()));
            return gen_expr(*node.rhs.unwrap());
        }
        NodeType::POST_INC => { return gen_post_inc(&node, 1); }
        NodeType::POST_DEC => { return gen_post_inc(&node, -1); }
        NodeType::QUEST => {
            let x = bump_nlabel() as i32;
            let y = bump_nlabel() as i32;
            let r = gen_expr(*node.cond.unwrap());

            add(IRType::UNLESS, r, x);
            let r2 = gen_expr(*node.then.unwrap());
            add(IRType::MOV, r, r2);
            kill(r2);
            jmp(y);

            label(x);
            let r3 = gen_expr(*node.els.unwrap());
            add(IRType::MOV, r, r3);
            kill(r2);
            label(y);
            return r;
        }
        NodeType::EXCLAM => {
            let lhs = gen_expr(*node.expr.unwrap());
            let rhs = bump_nreg();
            add(IRType::IMM, rhs, 0);
            add(IRType::EQ, lhs, rhs);
            kill(rhs);
            return lhs;
        }
        t => {
            panic!("unknown AST type {:?}", t);
        }
    }
}

fn gen_stmt(node: Node) {
    if node.op == NodeType::NULL {
        return;
    }

    match node.op {
        NodeType::IF => {
            let cond = *node.cond.unwrap();
            let then = *node.then.unwrap();
            if node.els.is_some() {
                let x = bump_nlabel() as i32;
                let y = bump_nlabel() as i32;
                let r = gen_expr(cond.clone());
                add(IRType::UNLESS, r, x);
                kill(r);
                gen_stmt(then.clone());
                jmp(y);
                label(x);
                gen_stmt(*node.els.unwrap());
                label(y);
                return;
            }

            let x = bump_nlabel() as i32;
            let r = gen_expr(cond);

            add(IRType::UNLESS, r, x);
            kill(r);
            gen_stmt(then);
            label(x);
        }
        NodeType::FOR => {
            let x = bump_nlabel() as i32;

            if node.init.is_some() {
                gen_stmt(*node.init.unwrap());
            }
            label(x);
            if node.cond.is_some() {
                let r = gen_expr(*node.cond.unwrap());
                add(IRType::UNLESS, r, node.break_label as i32);
                kill(r);
            }
            gen_stmt(*node.body.unwrap());
            label(node.continue_label as i32);
            if node.inc.is_some() {
                kill(gen_expr(*node.inc.unwrap()));
            }
            jmp(x);
            label(node.break_label as i32);
        }
        NodeType::DO_WHILE => {
            let x = bump_nlabel() as i32;
            label(x);
            gen_stmt(*node.body.unwrap());
            label(node.continue_label as i32);
            let r = gen_expr(*node.cond.unwrap());
            add(IRType::IF, r, x);
            kill(r);
            label(node.break_label as i32);
        }
        NodeType::BREAK => {
            jmp(node.target.unwrap().break_label as i32);
        }
        NodeType::CONTINUE => {
            jmp(node.target.unwrap().continue_label as i32);
        }
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());

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
        t => {
            panic!("unknown node: {:?}", t);
        }
    }
}

pub fn gen_ir(prog: &mut Program) {
    for func in prog.funcs.iter_mut() {
        let n = func.node.clone();
        let node = n.borrow();

        if node.op == NodeType::DECL {
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_code();

        for i in 0..node.params.len() {
            let var = &node.params[i];
            let ir_idx = add(IRType::STORE_ARG, var.borrow().offset, i as i32);

            CODE.with(|c| {
                let code = &mut *c.borrow_mut();
                let ir = &mut code[ir_idx];
                ir.size = var.borrow().ty.size;
            });
        }
        gen_stmt(*node.body.clone().unwrap());
        func.ir = CODE.with(|code| code.borrow().clone());
    }
}
