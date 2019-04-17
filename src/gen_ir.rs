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
    static FN: RefCell<Option<Rc<RefCell<Function>>>> = RefCell::new(None);
    static OUT: RefCell<Option<Rc<RefCell<BB>>>> = RefCell::new(None);
    static NREG: RefCell<i32> = RefCell::new(1);
    static BREAK_LABEL: RefCell<i32> = RefCell::new(0);
}

fn set_fn(fun: Rc<RefCell<Function>>) {
    FN.with(|f| {
        *f.borrow_mut() = Some(fun);
    })
}

fn fn_bbs_push(bb: Rc<RefCell<BB>>) {
    FN.with(|f| {
        match *f.borrow() {
            Some(ref fun) => {
                fun.borrow_mut().bbs.push(bb);
            }
            None => {
                panic!();
            }
        }
    })
}

fn set_out(bb: Rc<RefCell<BB>>) {
    OUT.with(|o| {
        *o.borrow_mut() = Some(bb);
    })
}

fn out_ir_push(ir: Rc<RefCell<IR>>) {
    OUT.with(|o| {
        match *o.borrow() {
            Some(ref out) => {
                out.borrow_mut().ir.push(ir);
            }
            None => {
                panic!();
            }
        }
    })
}

fn out_ir_last() -> Rc<RefCell<IR>> {
    OUT.with(|o| {
        match *o.borrow() {
            Some(ref out) => {
                let len = out.borrow().ir.len();
                return out.borrow().ir[len-1].clone();
            }
            None => {
                panic!();
            }
        }
    })
}

fn bump_nreg() -> i32 {
    NREG.with(|v| {
        let ret = *v.borrow();
        *v.borrow_mut() += 1;
        return ret;
    })
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum IRType {
    IMM,
    BPREL,
    MOV,
    RETURN,
    CALL,
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
    BR,
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

#[derive(Clone, Debug, PartialEq)]
pub struct BB {
    pub label: usize,
    pub ir: Vec<Rc<RefCell<IR>>>,
}

pub fn alloc_bb() -> BB {
    BB {
        label: 0,
        ir: Vec::new(),
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct IR {
    pub op: IRType,
    pub lhs: i32,
    pub rhs: i32,

    pub bb1: Rc<RefCell<BB>>,
    pub bb2: Rc<RefCell<BB>>,

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
    IR {
        op: IRType::NOP,
        lhs: 0,
        rhs: 0,

        bb1: Rc::new(RefCell::new(alloc_bb())),
        bb2: Rc::new(RefCell::new(alloc_bb())),

        size: 0,

        name: String::new(),
        nargs: 0,
        args: [0; 6],

        stacksize: 0,
        ir: Vec::new(),
        globals: Vec::new(),

        kill: Vec::new(),
    }
}

fn new_bb() -> Rc<RefCell<BB>> {
    let mut bb = alloc_bb();
    bb.label = bump_nlabel();
    bb.ir = Vec::new();
    let b = Rc::new(RefCell::new(bb));
    fn_bbs_push(b.clone());
    return b;
}

fn new_ir(op: IRType) -> Rc<RefCell<IR>> {
    let mut ir = alloc_ir();
    ir.op = op;
    let i = Rc::new(RefCell::new(ir));
    out_ir_push(i.clone());
    return i;
}

fn emit(op: IRType, lhs: i32, rhs: i32) -> Rc<RefCell<IR>> {
    let ir = new_ir(op);
    ir.borrow_mut().lhs = lhs;
    ir.borrow_mut().rhs = rhs;
    return ir;
}

fn br(r: i32, then: Rc<RefCell<BB>>, els: Rc<RefCell<BB>>) -> Rc<RefCell<IR>> {
    let ir = new_ir(IRType::BR);
    ir.borrow_mut().lhs = r;
    ir.borrow_mut().bb1 = then;
    ir.borrow_mut().bb2 = els;
    return ir;
}

fn kill(r: i32) {
    let ir = out_ir_last();
    ir.borrow_mut().kill.push(r);
}

fn jmp(bb: Rc<RefCell<BB>>) {
    let ir = new_ir(IRType::JMP);
    ir.borrow_mut().bb1 = bb;
}

fn load(node: Rc<RefCell<Node>>, dst: i32, src: i32) {
    let ir = emit(IRType::LOAD, dst, src);
    let ty = node.borrow().ty.clone();
    ir.borrow_mut().size = ty.borrow().size;
}

fn store(node: Rc<RefCell<Node>>, dst: i32, src: i32) {
    let ir = emit(IRType::STORE, dst, src);
    let ty = node.borrow().ty.clone();
    ir.borrow_mut().size = ty.borrow().size;
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
fn gen_lval(node: Rc<RefCell<Node>>) -> i32 {
    let node_op = node.borrow().op.clone();
    if node_op == NodeType::DEREF {
        return gen_expr(node.borrow().expr.clone().unwrap());
    }

    if node_op == NodeType::DOT {
        let r = gen_lval(node.borrow().expr.clone().unwrap());
        let ty = node.borrow().ty.clone();
        gen_imm(IRType::ADD, r, ty.borrow().offset);
        return r;
    }

    assert!(node_op == NodeType::VARREF);
    let var = node.borrow().var.clone().unwrap();

    let r = bump_nreg();
    if var.borrow().is_local {
        emit(IRType::BPREL, r, var.borrow().offset);
    } else {
        let ir = emit(IRType::LABEL_ADDR, r, -1);
        ir.borrow_mut().name = var.borrow().name.clone();
    }
    return r;
}

fn gen_binop(ty: IRType, node: Rc<RefCell<Node>>) -> i32 {
    let r1 = gen_expr(node.borrow().lhs.clone().unwrap());
    let r2 = gen_expr(node.borrow().rhs.clone().unwrap());
    emit(ty, r1, r2);
    kill(r2);
    return r1;
}

fn gen_expr(node: Rc<RefCell<Node>>) -> i32 {
    let op = node.borrow().op.clone();
    match op {
        NodeType::NUM => {
            let r = bump_nreg();
            emit(IRType::IMM, r, node.borrow().val);
            return r;
        }

        NodeType::EQ => {
            return gen_binop(IRType::EQ, node);
        }

        NodeType::NE => {
            return gen_binop(IRType::NE, node);
        }

        NodeType::LOGAND => {
            let bb1 = new_bb();
            let bb2 = new_bb();
            let last = new_bb();

            let r = gen_expr(node.borrow().lhs.clone().unwrap());
            br(r, bb1.clone(), last.clone());

            set_out(bb1);
            let r2 = gen_expr(node.borrow().rhs.clone().unwrap());
            emit(IRType::MOV, r, r2);
            kill(r2);
            br(r, bb2.clone(), last.clone());

            set_out(bb2);

            emit(IRType::IMM, r, 1);
            jmp(last.clone());
            
            set_out(last);
            return r;
        }

        NodeType::LOGOR => {
            let bb = new_bb();
            let set0 = new_bb();
            let set1 = new_bb();
            let last = new_bb();

            let r = gen_expr(node.borrow().lhs.clone().unwrap());
            br(r, set1.clone(), bb.clone());

            set_out(set0.clone());
            emit(IRType::IMM, r, 0);
            jmp(last.clone());

            set_out(set1.clone());
            emit(IRType::IMM, r, 1);
            jmp(last.clone());

            set_out(bb);
            let r2 = gen_expr(node.borrow().rhs.clone().unwrap());
            emit(IRType::MOV, r, r2);
            kill(r2);
            br(r, set1, set0);

            set_out(last);
            return r;
        }

        NodeType::VARREF |
            NodeType::DOT => {
            let r = gen_lval(node.clone());
            load(node, r, r);
            return r;
        }

        NodeType::CALL => {
            let mut args = Vec::new();
            for a in node.borrow().args.iter() {
                args.push(gen_expr(a.clone()));
            }

            let r = bump_nreg();

            let ir = emit(IRType::CALL, r, -1);

            ir.borrow_mut().name = node.borrow().name.clone();
            ir.borrow_mut().nargs = node.borrow().args.len();

            let nargs = ir.borrow().nargs;
            for i in 0..nargs {
                ir.borrow_mut().args[i] = args[i];
            }

            for i in 0..nargs {
                kill(args[i]);
            }
            return r;
        }

        NodeType::ADDR => {
            return gen_lval(node.borrow().expr.clone().unwrap());
        }

        NodeType::DEREF => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            load(node, r, r);
            return r;
        }

        NodeType::CAST => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            let ty = node.borrow().ty.clone();
            if ty.borrow().ty != CType::BOOL {
                return r;
            }
            let r2 = bump_nreg();
            emit(IRType::IMM, r2, 0);
            emit(IRType::NE, r, r2);
            kill(r2);
            return r;
        }

        NodeType::STMT_EXPR => {
            for n in node.borrow().stmts.iter() {
                gen_stmt(n.clone());
            }
            return gen_expr(node.borrow().expr.clone().unwrap());
        }

        NodeType::EQL => {
            let rhs = gen_expr(node.borrow().rhs.clone().unwrap());
            let lhs = gen_lval(node.borrow().lhs.clone().unwrap());
            store(node, lhs, rhs);
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
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            gen_imm(IRType::XOR, r, -1);
            return r;
        }
        NodeType::COMMA => {
            kill(gen_expr(node.borrow().lhs.clone().unwrap()));
            return gen_expr(node.borrow().rhs.clone().unwrap());
        }
        NodeType::QUEST => {
            let then = new_bb();
            let els = new_bb();
            let last = new_bb();

            let r = gen_expr(node.borrow().cond.clone().unwrap());
            br(r, then.clone(), els.clone());

            set_out(then);
            let r2 = gen_expr(node.borrow().then.clone().unwrap());
            emit(IRType::MOV, r, r2);
            kill(r2);
            jmp(last.clone());

            set_out(els);
            let r3 = gen_expr(node.borrow().els.clone().unwrap());
            emit(IRType::MOV, r, r3);
            kill(r2);
            jmp(last.clone());

            set_out(last);
            return r;
        }
        NodeType::EXCLAM => {
            let lhs = gen_expr(node.borrow().expr.clone().unwrap());
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

fn gen_stmt(node: Rc<RefCell<Node>>) {
    if node.borrow().op == NodeType::NULL {
        return;
    }

    let op = node.borrow().op.clone();
    match op {
        NodeType::IF => {
            let then = new_bb();
            let els = new_bb();
            let last = new_bb();

            let r = gen_expr(node.borrow().cond.clone().unwrap());
            br(r, then.clone(), els.clone());
            kill(r);

            set_out(then);
            gen_stmt(node.borrow().then.clone().unwrap());
            jmp(last.clone());

            set_out(els);
            if node.borrow().els.is_some() {
                gen_stmt(node.borrow().els.clone().unwrap());
            }
            jmp(last.clone());
            
            set_out(last);
        }
        NodeType::FOR => {
            let cond = new_bb();
            let body = new_bb();
            node.borrow_mut().break_ = new_bb();
            node.borrow_mut().continue_ = new_bb();

            if node.borrow().init.is_some() {
                gen_stmt(node.borrow().init.clone().unwrap());
            }
            jmp(cond.clone());

            set_out(cond.clone());
            if node.borrow().cond.is_some() {
                let r = gen_expr(node.borrow().cond.clone().unwrap());
                br(r, body.clone(), node.borrow().break_.clone());
                kill(r);
            } else {
                jmp(body.clone());
            }

            set_out(body);
            gen_stmt(node.borrow().body.clone().unwrap());
            jmp(node.borrow().continue_.clone());

            set_out(node.borrow().continue_.clone());
            if node.borrow().inc.is_some() {
                kill(gen_expr(node.borrow().inc.clone().unwrap()));
            }
            jmp(cond);

            set_out(node.borrow().break_.clone());
        }
        NodeType::DO_WHILE => {
            let body = new_bb();
            node.borrow_mut().continue_ = new_bb();
            node.borrow_mut().break_ = new_bb();

            jmp(body.clone());

            set_out(body.clone());
            gen_stmt(node.borrow().body.clone().unwrap());
            jmp(node.borrow().continue_.clone());

            set_out(node.borrow().continue_.clone());
            let r = gen_expr(node.borrow().cond.clone().unwrap());
            br(r, body, node.borrow().break_.clone());
            kill(r);

            set_out(node.borrow().break_.clone());
        }
        NodeType::SWITCH => {
            node.borrow_mut().break_ = new_bb();
            node.borrow_mut().continue_ = new_bb();

            let r = gen_expr(node.borrow().cond.clone().unwrap());
            for c in node.borrow().cases.iter() {
                c.borrow_mut().bb = new_bb();

                let next = new_bb();
                let r2 = bump_nreg();

                emit(IRType::IMM, r2, c.borrow().val);
                emit(IRType::EQ, r2, r);
                br(r2, c.borrow().bb.clone(), next.clone());
                kill(r2);
                set_out(next);
            }
            jmp(node.borrow().break_.clone());
            kill(r);

            gen_stmt(node.borrow().body.clone().unwrap());
            jmp(node.borrow().break_.clone());

            set_out(node.borrow().break_.clone());
        }
        NodeType::CASE => {
            jmp(node.borrow().bb.clone());
            set_out(node.borrow().bb.clone());
            gen_stmt(node.borrow().body.clone().unwrap());
        }
        NodeType::BREAK => {
            let target = node.borrow().target.clone().unwrap();
            jmp(target.borrow().clone().break_);
        }
        NodeType::CONTINUE => {
            let target = node.borrow().target.clone().unwrap();
            jmp(target.borrow().clone().continue_);
        }
        NodeType::RETURN => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());

            emit(IRType::RETURN, r, -1);
            kill(r);

            let bb = new_bb();
            jmp(bb.clone());
            set_out(bb);
        }
        NodeType::EXPR_STMT => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            kill(r);
        }
        NodeType::COMP_STMT => {
            for n in node.borrow().stmts.iter() {
                gen_stmt(n.clone());
            }
        }
        t => {
            panic!("unknown node: {:?}", t);
        }
    }
}

fn gen_param(var: & Rc<RefCell<Var>>, i: usize) {
    let ir = emit(IRType::STORE_ARG, var.borrow().offset, i as i32);
    ir.borrow_mut().size = var.borrow().ty.size;
}

pub fn gen_ir(prog: &mut Program) {
    for func in prog.funcs.iter_mut() {
        set_fn(func.clone());
        set_out(new_bb());
        
        let func_node = func.borrow().node.clone();
        assert!(func_node.borrow().op == NodeType::FUNC);

        // Assign an offset from RBP to each local variable.
        let mut off = 0;
        for v in func.borrow_mut().lvars.iter_mut() {
            off += v.borrow().ty.size;
            off = roundup(off, v.borrow().ty.align);
            v.borrow_mut().offset = -off;
        }
        func.borrow_mut().stacksize = off;

        // Emit IR.
        let params = func_node.borrow().clone().params;
        for i in 0..params.len() {
            gen_param(&params[i], i)
        }

        let node_body = func_node.borrow().body.clone();
        gen_stmt(node_body.unwrap());

        // Later passes shouldn't need the following members,
        // so make it explicit.
        func.borrow_mut().lvars = Vec::new();
        func.borrow_mut().node = Rc::new(RefCell::new(alloc_node()));
    }
}
