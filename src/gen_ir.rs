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
pub struct Reg {
    pub vn: i32, // virtual register number
    pub rn: i32, // real register number
}

fn alloc_reg() -> Rc<RefCell<Reg>> {
    Rc::new(RefCell::new(Reg {
        vn: -1,
        rn: -1,
    }))
}

#[derive(Clone, Debug, PartialEq)]
pub struct IR {
    pub op: IRType,

    pub r0: Option<Rc<RefCell<Reg>>>,
    pub r1: Option<Rc<RefCell<Reg>>>,
    pub r2: Option<Rc<RefCell<Reg>>>,

    pub imm: i32,
    pub imm2: i32,
    pub label: i32,

    pub bb1: Rc<RefCell<BB>>,
    pub bb2: Rc<RefCell<BB>>,

    // Load/store size in bytes
    pub size: i32,

    // Function call
    pub name: String,
    pub nargs: usize,
    pub args: Vec<Rc<RefCell<Reg>>>,

    // Function struct fields in 9cc
    pub stacksize: i32,
    pub ir: Vec<IR>,
    pub globals: Vec<Var>,

    // For liveness tracking
    pub kill: Vec<Rc<RefCell<Reg>>>,
}

pub fn alloc_ir() -> IR {
    IR {
        op: IRType::NOP,

        r0: None,
        r1: None,
        r2: None,

        imm: 0,
        imm2: 0,
        label: 0,

        bb1: Rc::new(RefCell::new(alloc_bb())),
        bb2: Rc::new(RefCell::new(alloc_bb())),

        size: 0,

        name: String::new(),
        nargs: 0,
        args: Vec::new(),

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

fn new_reg() -> Rc<RefCell<Reg>> {
    let r = alloc_reg();
    r.borrow_mut().vn = bump_nreg();
    r.borrow_mut().rn = -1;
    return r;
}

fn emit(op: IRType, r0: Option<Rc<RefCell<Reg>>>, r1: Option<Rc<RefCell<Reg>>>, r2: Option<Rc<RefCell<Reg>>>) -> Rc<RefCell<IR>> {
    let ir = new_ir(op);
    ir.borrow_mut().r0 = r0;
    ir.borrow_mut().r1 = r1;
    ir.borrow_mut().r2 = r2;
    return ir;
}

fn emit1(op: IRType, r: Option<Rc<RefCell<Reg>>>) -> Rc<RefCell<IR>> {
    let ir = new_ir(op);
    ir.borrow_mut().r0 = r;
    return ir;
}

fn br(r: Rc<RefCell<Reg>>, then: Rc<RefCell<BB>>, els: Rc<RefCell<BB>>) -> Rc<RefCell<IR>> {
    let ir = new_ir(IRType::BR);
    ir.borrow_mut().r2 = Some(r);
    ir.borrow_mut().bb1 = then;
    ir.borrow_mut().bb2 = els;
    return ir;
}

fn kill(r: Rc<RefCell<Reg>>) {
    let ir = out_ir_last();
    ir.borrow_mut().kill.push(r);
}

fn jmp(bb: Rc<RefCell<BB>>) {
    let ir = new_ir(IRType::JMP);
    ir.borrow_mut().bb1 = bb;
}

fn imm(r: Rc<RefCell<Reg>>, imm: i32) {
    let ir = new_ir(IRType::IMM);
    ir.borrow_mut().r0 = Some(r);
    ir.borrow_mut().imm = imm;
}

fn load(node: Rc<RefCell<Node>>, dst: Rc<RefCell<Reg>>, src: Rc<RefCell<Reg>>) {
    let ir = emit(IRType::LOAD, Some(dst), None, Some(src));
    let ty = node.borrow().ty.clone();
    ir.borrow_mut().size = ty.borrow().size;
}

fn store(node: Rc<RefCell<Node>>, dst: Rc<RefCell<Reg>>, src: Rc<RefCell<Reg>>) {
    let ir = emit(IRType::STORE, Some(dst), None, Some(src));
    let ty = node.borrow().ty.clone();
    ir.borrow_mut().size = ty.borrow().size;
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
fn gen_lval(node: Rc<RefCell<Node>>) -> Rc<RefCell<Reg>> {
    let node_op = node.borrow().op.clone();
    if node_op == NodeType::DEREF {
        return gen_expr(node.borrow().expr.clone().unwrap());
    }

    if node_op == NodeType::DOT {
        let r = gen_lval(node.borrow().expr.clone().unwrap());
        let ty = node.borrow().ty.clone();
        let r2 = new_reg();
        imm(r2.clone(), ty.borrow().offset);
        emit(IRType::ADD, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
        kill(r2);
        return r;
    }

    assert!(node_op == NodeType::VARREF);
    let var = node.borrow().var.clone().unwrap();

    let r = new_reg();
    if var.borrow().is_local {
        let ir = new_ir(IRType::BPREL);
        ir.borrow_mut().r0 = Some(r.clone());
        ir.borrow_mut().imm = var.borrow().offset;
    } else {
        let ir = emit1(IRType::LABEL_ADDR, Some(r.clone()));
        ir.borrow_mut().name = var.borrow().name.clone();
    }
    return r;
}

fn gen_binop(ty: IRType, node: Rc<RefCell<Node>>) -> Rc<RefCell<Reg>> {
    let r1 = new_reg();
    let r2 = gen_expr(node.borrow().lhs.clone().unwrap());
    let r3 = gen_expr(node.borrow().rhs.clone().unwrap());
    emit(ty, Some(r1.clone()), Some(r2.clone()), Some(r3.clone()));
    kill(r2);
    kill(r3);
    return r1;
}

fn gen_expr(node: Rc<RefCell<Node>>) -> Rc<RefCell<Reg>> {
    let op = node.borrow().op.clone();
    match op {
        NodeType::NUM => {
            let r = new_reg();
            imm(r.clone(), node.borrow().val);
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
            br(r.clone(), bb1.clone(), last.clone());

            set_out(bb1);
            let r2 = gen_expr(node.borrow().rhs.clone().unwrap());
            emit(IRType::MOV, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
            kill(r2);
            br(r.clone(), bb2.clone(), last.clone());

            set_out(bb2);

            imm(r.clone(), 1);
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
            br(r.clone(), set1.clone(), bb.clone());

            set_out(set0.clone());
            imm(r.clone(), 0);
            jmp(last.clone());

            set_out(set1.clone());
            imm(r.clone(), 1);
            jmp(last.clone());

            set_out(bb);
            let r2 = gen_expr(node.borrow().rhs.clone().unwrap());
            emit(IRType::MOV, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
            kill(r2);
            br(r.clone(), set1, set0);

            set_out(last);
            return r;
        }

        NodeType::VARREF |
            NodeType::DOT => {
            let r = gen_lval(node.clone());
            load(node, r.clone(), r.clone());
            return r;
        }

        NodeType::CALL => {
            let mut args = Vec::new();
            for a in node.borrow().args.iter() {
                args.push(gen_expr(a.clone()));
            }

            let r = new_reg();

            let ir = emit1(IRType::CALL, Some(r.clone()));

            ir.borrow_mut().name = node.borrow().name.clone();
            ir.borrow_mut().nargs = node.borrow().args.len();

            let nargs = ir.borrow().nargs;
            for i in 0..nargs {
                ir.borrow_mut().args.push(args[i].clone());
            }

            for i in 0..nargs {
                kill(args[i].clone());
            }
            return r;
        }

        NodeType::ADDR => {
            return gen_lval(node.borrow().expr.clone().unwrap());
        }

        NodeType::DEREF => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            load(node, r.clone(), r.clone());
            return r;
        }

        NodeType::CAST => {
            let r = gen_expr(node.borrow().expr.clone().unwrap());
            let ty = node.borrow().ty.clone();
            if ty.borrow().ty != CType::BOOL {
                return r;
            }
            let r2 = new_reg();
            imm(r2.clone(), 0);
            emit(IRType::NE, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
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
            store(node, lhs.clone(), rhs.clone());
            kill(lhs.clone());
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
            let r2 = new_reg();
            imm(r2.clone(), -1);
            emit(IRType::XOR, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
            kill(r2);
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
            br(r.clone(), then.clone(), els.clone());

            set_out(then);
            let r2 = gen_expr(node.borrow().then.clone().unwrap());
            emit(IRType::MOV, Some(r.clone()), Some(r.clone()), Some(r2.clone()));
            kill(r2.clone());
            jmp(last.clone());

            set_out(els);
            let r3 = gen_expr(node.borrow().els.clone().unwrap());
            emit(IRType::MOV, Some(r.clone()), Some(r.clone()), Some(r3));
            kill(r2);
            jmp(last.clone());

            set_out(last);
            return r;
        }
        NodeType::EXCLAM => {
            let lhs = gen_expr(node.borrow().expr.clone().unwrap());
            let rhs = new_reg();
            imm(rhs.clone(), 0);
            emit(IRType::EQ, Some(lhs.clone()), Some(lhs.clone()), Some(rhs.clone()));
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
            br(r.clone(), then.clone(), els.clone());
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
                br(r.clone(), body.clone(), node.borrow().break_.clone());
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
            br(r.clone(), body, node.borrow().break_.clone());
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
                let r2 = new_reg();

                imm(r2.clone(), c.borrow().val);
                emit(IRType::EQ, Some(r2.clone()), Some(r2.clone()), Some(r.clone()));
                br(r2.clone(), c.borrow().bb.clone(), next.clone());
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

            emit1(IRType::RETURN, Some(r.clone()));
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
    let ir = new_ir(IRType::STORE_ARG);
    ir.borrow_mut().imm = var.borrow().offset;
    ir.borrow_mut().imm2 = i as i32;
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
