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
use crate::sema::*;
use std::cell::RefCell;

thread_local! {
    static CODE: RefCell<Vec<IR>> = RefCell::new(Vec::new());

    static NREG: RefCell<i32> = RefCell::new(1);
    static NLABEL: RefCell<i32> = RefCell::new(1);

    static RETURN_LABEL: RefCell<i32> = RefCell::new(0);
    static RETURN_REG: RefCell<i32> = RefCell::new(0);
    static BREAK_LABEL: RefCell<i32> = RefCell::new(0);
}

//fn nlabel() -> i32 {
//    *NLABEL.lock().unwrap()
//}

fn bump_nlabel() -> i32 {
    NLABEL.with(|v| {
        let ret = *v.borrow();
        *v.borrow_mut() += 1;
        return ret;
    })
}

//fn set_nlabel(i: i32) {
//    let mut nlabel = NLABEL.lock().unwrap();
//    *nlabel = i;
//}

//fn nreg() -> i32 {
//    *NREG.lock().unwrap()
//}

fn bump_nreg() -> i32 {
    NREG.with(|v| {
        let ret = *v.borrow();
        *v.borrow_mut() += 1;
        return ret;
    })
}

//fn set_nreg(i: i32) {
//    let mut nreg = NREG.lock().unwrap();
//    *nreg = i;
//}

fn return_label() -> i32 {
    RETURN_LABEL.with(|v| {
        *v.borrow()
    })
}

fn set_return_label(i: i32) {
    RETURN_LABEL.with(|v| {
        *v.borrow_mut() = i;
    })
}

fn return_reg() -> i32 {
    RETURN_REG.with(|v| {
        *v.borrow()
    })
}

fn set_return_reg(i: i32) {
    RETURN_REG.with(|v| {
        *v.borrow_mut() = i;
    })
}

fn break_label() -> i32 {
    BREAK_LABEL.with(|v| {
        *v.borrow()
    })
}

fn set_break_label(i: i32) {
    BREAK_LABEL.with(|v| {
        *v.borrow_mut() = i;
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
    NEG,
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

fn store_arg(node: & Node, bpoff: i32, argreg: i32) {
    let ir_idx = add(IRType::STORE_ARG, bpoff, argreg);

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
        add_imm(IRType::ADD, r, node.offset);
        return r;
    }

    if node.op == NodeType::LVAR {
        let r = bump_nreg();
        add(IRType::BPREL, r, node.offset);
        return r;
    }

    assert!(node.op == NodeType::GVAR);
    let r = bump_nreg();
    let ir_idx = add(IRType::LABEL_ADDR, r, -1);
    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        code[ir_idx].name = node.name.clone();
    });
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
        NodeType::BITAND_EQ => IRType::AND,
        NodeType::XOR_EQ => IRType::XOR,
        NodeType::BITOR_EQ => IRType::OR,
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

        NodeType::GVAR |
            NodeType::LVAR |
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
            let r = gen_expr(*node.clone().expr.unwrap());
            load(&node, r, r);
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

        NodeType::MUL_EQ |
            NodeType::DIV_EQ |
            NodeType::MOD_EQ |
            NodeType::ADD_EQ |
            NodeType::SUB_EQ |
            NodeType::SHL_EQ |
            NodeType::SHR_EQ |
            NodeType::BITAND_EQ |
            NodeType::XOR_EQ |
            NodeType::BITOR_EQ => {
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
        NodeType::NEG => {
            let r = gen_expr(*node.expr.unwrap());
            add(IRType::NEG, r, -1);
            return r;
        }
        NodeType::COMMA => {
            kill(gen_expr(*node.lhs.unwrap()));
            return gen_expr(*node.rhs.unwrap());
        }
        NodeType::POST_INC => { return gen_post_inc(&node, 1); }
        NodeType::POST_DEC => { return gen_post_inc(&node, -1); }
        NodeType::QUEST => {
            let x = bump_nlabel();
            let y = bump_nlabel();
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
        _ => {
            panic!("unknown AST type {:?}", node);
        }
    }
}

fn gen_stmt(node: Node) {
    if node.op == NodeType::NULL {
        return;
    }

    match node.op {
        NodeType::VARDEF => {
            if node.init.is_none() {
                return;
            }

            let rhs = gen_expr(*node.init.clone().unwrap());
            let lhs = bump_nreg();
            add(IRType::BPREL, lhs, node.offset);
            store(&node, lhs, rhs);
            kill(lhs);
            kill(rhs);
        }
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
                jmp(y);
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
            let orig = break_label();
            set_break_label(bump_nlabel());

            gen_stmt(*node.init.unwrap());
            label(x);
            if node.cond.is_some() {
                let r = gen_expr(*node.cond.unwrap());
                add(IRType::UNLESS, r, y);
                kill(r);
            }
            gen_stmt(*node.body.unwrap());
            if node.inc.is_some() {
                gen_stmt(*node.inc.unwrap());
            }
            jmp(x);
            label(y);
            label(break_label());
            set_break_label(orig);
        }
        NodeType::DO_WHILE => {
            let x = bump_nlabel();
            let orig = break_label();
            set_break_label(bump_nlabel());
            label(x);
            gen_stmt(*node.body.unwrap());
            let r = gen_expr(*node.cond.unwrap());
            add(IRType::IF, r, x);
            kill(r);
            label(break_label());
            set_break_label(orig);
        }
        NodeType::BREAK => {
            if break_label() == 0 {
                panic!("stray 'break' statement");
            }
            jmp(break_label());
        }
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());

            // Statement expression (GNU extension)
            if return_label() != 0 {
                add(IRType::MOV, return_reg(), r);
                kill(r);
                jmp(return_label());
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
            panic!("unknown node: {:?}", node.ty);
        }
    }
}

pub fn gen_ir(nodes: Vec<Node>) -> Vec<IR> {
    let mut v = Vec::new();

    for i in 0..nodes.len() {
        let node = nodes[i].clone();

        if node.op == NodeType::VARDEF || node.op == NodeType::DECL {
            continue;
        }

        assert!(node.op == NodeType::FUNC);

        init_code();

        for i in 0..node.args.len() {
            let arg = &node.args[i];
            store_arg(&arg, arg.offset, i as i32);
        }
        gen_stmt(*node.body.unwrap());

        let mut fun = alloc_ir();
        fun.name = node.name.clone();
        fun.stacksize = node.stacksize;
        fun.ir = CODE.with(|code| code.borrow().clone());
        fun.globals = node.globals;

        v.push(fun);
    }

    return v;
}
