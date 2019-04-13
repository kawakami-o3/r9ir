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


// let mut off = 0;
// for v in func.lvars.iter_mut() {
//     off = roundup(off, v.borrow().ty.align);
//     off += v.borrow().ty.size;
//     v.borrow_mut().offset = off;
// }
// func.stacksize = off;

#![allow(non_camel_case_types)]

use crate::parse::*;
use crate::util::*;
use std::cell::RefCell;
use std::rc::Rc;

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

    // Function call
    pub name: String,
    pub nargs: usize,
    pub args: [i32; 6],

    // Function struct fields in 9cc
    pub stacksize: i32,
    pub ir: Vec<IR>,
    pub globals: Vec<Var>,

    // For liveness tracking
    pub kill: Vec<i32>,
}

fn alloc_ir() -> IR {
    return IR {
        op: IRType::NOP,
        lhs: 0,
        rhs: 0,

        size: 0,

        name: String::new(),
        nargs: 0,
        args: [0; 6],

        stacksize: 0,
        ir: Vec::new(),
        globals: Vec::new(),

        kill: Vec::new(),
    };
}

fn emit(op: IRType, lhs: i32, rhs: i32) -> usize {
    let mut ir = alloc_ir();
    ir.op = op;
    ir.lhs = lhs;
    ir.rhs = rhs;

    return CODE.with(|code| {
        (*code.borrow_mut()).push(ir);
        return code.borrow().len() - 1;
    });
}

fn kill(r: i32) {
    CODE.with(|c| {
        let len = c.borrow().len();
        c.borrow_mut()[len-1].kill.push(r);
    })
}

fn label(x: i32) {
    emit(IRType::LABEL, x, -1);
}

fn jmp(x: i32) {
    emit(IRType::JMP, x, -1);
}

fn load(node: & Node, dst: i32, src: i32) {
    let ir_idx = emit(IRType::LOAD, dst, src);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.size = node.ty.borrow().size;
    });
}

fn store(node: & Node, dst: i32, src: i32) {
    let ir_idx = emit(IRType::STORE, dst, src);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.size = node.ty.borrow().size;
    });
}

fn gen_imm(op: IRType, r: i32, imm: i32) {
    let r2 = bump_nreg();
    emit(IRType::IMM, r2, imm);
    emit(op, r, r2);
    kill(r2);
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
        gen_imm(IRType::ADD, r, node.ty.borrow().offset);
        return r;
    }

    assert!(node.op == NodeType::VARREF);
    let var = node.var.unwrap();

    let r = bump_nreg();
    if var.borrow().is_local {
        emit(IRType::BPREL, r, var.borrow().offset);
    } else {
        let ir_idx = emit(IRType::LABEL_ADDR, r, -1);
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
    emit(ty, r1, r2);
    kill(r2);
    return r1;
}

fn gen_expr(node: Node) -> i32 {
    match node.op {
        NodeType::NUM => {
            let r = bump_nreg();
            emit(IRType::IMM, r, node.val);
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
            emit(IRType::UNLESS, r1, x);
            let r2 = gen_expr(*node.rhs.unwrap());
            emit(IRType::MOV, r1, r2);
            kill(r2);
            emit(IRType::UNLESS, r1, x);
            emit(IRType::IMM, r1, 1);
            label(x);
            return r1;
        }

        NodeType::LOGOR => {
            let x = bump_nlabel() as i32;
            let y = bump_nlabel() as i32;

            let r1 = gen_expr(*node.lhs.unwrap());
            emit(IRType::UNLESS, r1, x);
            emit(IRType::IMM, r1, 1);
            jmp(y);
            label(x);

            let r2 = gen_expr(*node.rhs.unwrap());
            emit(IRType::MOV, r1, r2);
            kill(r2);
            emit(IRType::UNLESS, r1, y);
            emit(IRType::IMM, r1, 1);
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

            let ir_idx = emit(IRType::CALL, r, -1);

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
            emit(IRType::IMM, r2, 0);
            emit(IRType::NE, r, r2);
            kill(r2);
            return r;
        }

        NodeType::STMT_EXPR => {
            for n in node.stmts.iter() {
                gen_stmt(n.clone());
            }
            return gen_expr(*node.expr.unwrap());
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
            gen_imm(IRType::XOR, r, -1);
            return r;
        }
        NodeType::COMMA => {
            kill(gen_expr(*node.lhs.unwrap()));
            return gen_expr(*node.rhs.unwrap());
        }
        NodeType::QUEST => {
            let x = bump_nlabel() as i32;
            let y = bump_nlabel() as i32;
            let r = gen_expr(*node.cond.unwrap());

            emit(IRType::UNLESS, r, x);
            let r2 = gen_expr(*node.then.unwrap());
            emit(IRType::MOV, r, r2);
            kill(r2);
            jmp(y);

            label(x);
            let r3 = gen_expr(*node.els.unwrap());
            emit(IRType::MOV, r, r3);
            kill(r2);
            label(y);
            return r;
        }
        NodeType::EXCLAM => {
            let lhs = gen_expr(*node.expr.unwrap());
            let rhs = bump_nreg();
            emit(IRType::IMM, rhs, 0);
            emit(IRType::EQ, lhs, rhs);
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
            let x = bump_nlabel() as i32;
            let y = bump_nlabel() as i32;
            let r = gen_expr(*node.cond.unwrap());
            emit(IRType::UNLESS, r, x);
            kill(r);
            gen_stmt(*node.then.unwrap());
            jmp(y);
            label(x);
            if node.els.is_some() {
                gen_stmt(*node.els.unwrap());
            }
            label(y);
        }
        NodeType::FOR => {
            let x = bump_nlabel() as i32;

            if node.init.is_some() {
                gen_stmt(*node.init.unwrap());
            }
            label(x);
            if node.cond.is_some() {
                let r = gen_expr(*node.cond.unwrap());
                emit(IRType::UNLESS, r, node.break_label as i32);
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
            emit(IRType::IF, r, x);
            kill(r);
            label(node.break_label as i32);
        }
        NodeType::SWITCH => {
            let r = gen_expr(*node.cond.unwrap());
            for c in node.cases {
                let r2 = bump_nreg();
                emit(IRType::IMM, r2, c.val);
                emit(IRType::EQ, r2, r);
                emit(IRType::IF, r2, c.case_label as i32);
                kill(r2);
            }
            kill(r);
            jmp(node.break_label as i32);
            gen_stmt(*node.body.unwrap());
            label(node.break_label as i32);
        }
        NodeType::CASE => {
            label(node.case_label as i32);
            gen_stmt(*node.body.unwrap());
        }
        NodeType::BREAK => {
            jmp(node.target.unwrap().break_label as i32);
        }
        NodeType::CONTINUE => {
            jmp(node.target.unwrap().continue_label as i32);
        }
        NodeType::RETURN => {
            let r = gen_expr(*node.expr.unwrap());

            emit(IRType::RETURN, r, -1);
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

fn gen_param(var: & Rc<RefCell<Var>>, i: usize) {
    let ir_idx = emit(IRType::STORE_ARG, var.borrow().offset, i as i32);

    CODE.with(|c| {
        let code = &mut *c.borrow_mut();
        let ir = &mut code[ir_idx];
        ir.size = var.borrow().ty.size;
    });
}

pub fn gen_ir(prog: &mut Program) {
    for func in prog.funcs.iter_mut() {
        init_code();
        
        assert!(func.node.borrow().op == NodeType::FUNC);

        // Assign an offset from RBP to each local variable.
        let mut off = 0;
        for v in func.lvars.iter_mut() {
            off += v.borrow().ty.size;
            off = roundup(off, v.borrow().ty.align);
            v.borrow_mut().offset = -off;
        }
        func.stacksize = off;

        // Emit IR.
        let params = func.node.borrow().clone().params;
        for i in 0..params.len() {
            gen_param(&params[i], i)
        }

        gen_stmt(*func.node.borrow().body.clone().unwrap());
        func.ir = CODE.with(|code| code.borrow().clone());

        // Later passes shouldn't need the following members,
        // so make it explicit.
        func.lvars = Vec::new();
        func.node = Rc::new(RefCell::new(alloc_node()));
    }
}
