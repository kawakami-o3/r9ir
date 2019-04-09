// Semantics analyzer. This pass plays a few important roles as shown
// below:
//
// - Add types to nodes. For example, a tree that represents "1+2" is
//   typed as INT because the result type of an addition of two
//   integers is integer.
//
// - Insert nodes to make array-to-pointer conversion explicit.
//   Recall that, in C, "array of T" is automatically converted to
//   "pointer to T" in most contexts.
//
// - Scales operands for pointer arithmetic. E.g. ptr+1 becomes ptr+4
//   for integer and becomes ptr+8 for pointer.
//
// - Reject bad assignments, such as `1=2+3`.

use crate::parse::*;
use crate::token::*;
use crate::util::*;
use std::cell::RefCell;
use std::rc::Rc;

fn maybe_decay(base: & mut Node, decay: bool) -> & mut Node {
    if !decay || base.ty.borrow().ty != CType::ARY {
        return base;
    }

    let tmp = base.clone();
    *base = alloc_node();
    base.op = NodeType::ADDR;

    let node_ty = tmp.clone().ty;
    let ty_tmp = *node_ty.borrow().clone().ary_of.unwrap();
    base.ty = Rc::new(RefCell::new(ptr_to(Rc::new(RefCell::new(ty_tmp)))));

    base.expr = Some(Box::new(tmp.clone()));
    base.token = tmp.token.clone();
    return base;
}

macro_rules! bad_node {
    ($node:expr, $msg:expr) => {
        bad_token(&*$node.token.clone().unwrap(), $msg.to_string());
        panic!();
    };
}

fn check_lval(node: Box<Node>) {
    match node.op {
        NodeType::VAR | NodeType::DEREF | NodeType::DOT => { }
        _ => {
            bad_node!(node, "not an lvalue");
        }
    }
}

fn scale_ptr(op: NodeType, base: Node, ty: Type) -> Node {
    let mut node = alloc_node();
    node.op = op;
    node.lhs = Some(Box::new(base.clone()));
    let ptr = ty.ptr_to.unwrap();
    node.rhs = Some(Box::new(new_int_node(ptr.borrow().size, base.token.clone())));
    node.token = base.token.clone();
    return node;
}

fn check_int(node: & Node) {
    if node.ty.borrow().ty != CType::INT {
        if node.ty.borrow().ty != CType::CHAR {
            bad_node!(node, "not an integer");
        }
    }
}

fn walk<'a>(node: &'a mut Node, prog: &'a mut Program) -> &'a Node {
    return do_walk(node, true, prog);
}

fn walk_nodecay<'a>(node: &'a mut Node, prog: &'a mut Program) -> &'a Node {
    return do_walk(node, false, prog);
}

fn do_walk<'a>(node: &'a mut Node, decay: bool, prog: &'a mut Program) -> &'a Node {
    match node.op {
        NodeType::NUM |
            NodeType::NULL |
            NodeType::BREAK |
            NodeType::CONTINUE => {
            return node;
        }
        NodeType::VAR => {
            return maybe_decay(node, decay);
        }
        NodeType::VARDEF => {
            match node.init {
                Some(_) => {
                    node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), prog).clone()));
                }
                None => { }
            }
            return node;
        }
        NodeType::IF => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), prog).clone()));

            if node.els.is_some() {
                node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), prog).clone()));
            }
            return node;
        }
        NodeType::FOR => {
            node.init = Some(Box::new(walk(&mut *node.init.clone().unwrap(), prog).clone()));
            if let Some(mut cond) = node.cond.clone() {
                node.cond = Some(Box::new(walk(&mut cond, prog).clone()));
            }
            if let Some(mut inc) = node.inc.clone() {
                node.inc = Some(Box::new(walk(&mut inc, prog).clone()));
            }
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));
            return node;
        }
        NodeType::DO_WHILE => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.body = Some(Box::new(walk(&mut *node.body.clone().unwrap(), prog).clone()));
            return node;
        }
        NodeType::ADD => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

            match (&mut node.lhs, &mut node.rhs) {
                (Some(ref mut lhs), Some(ref mut rhs)) => {
                    if rhs.ty.borrow().ty == CType::PTR {
                        //use std::mem;
                        //mem::swap(lhs, rhs);
                        let n = lhs.clone();
                        *lhs = rhs.clone();
                        *rhs = n;
                    }
                }
                _ => {}
            }

            if let Some(ref rhs) = node.rhs {
                check_int(rhs);
            }

            if let Some(ref lhs) = node.lhs {
                if lhs.ty.borrow().ty == CType::PTR {
                    node.rhs = Some(Box::new(scale_ptr(NodeType::MUL, *node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
                    node.ty = node.lhs.clone().unwrap().ty;
                } else {
                    node.ty = Rc::new(RefCell::new(int_ty()));
                }
            }

            return node;
        }
        NodeType::SUB => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

            let l = &node.rhs.clone().unwrap().ty;
            let r = &node.lhs.clone().unwrap().ty;
            if l.borrow().ty == CType::PTR && r.borrow().ty == CType::PTR {
                if !same_type(r.clone(), l.clone()) {
                    bad_node!(node, "incompatible pointer");
                }
                *node = scale_ptr(NodeType::DIV, node.clone(), l.borrow().clone());
            }

            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::ADD_EQ |
            NodeType::SUB_EQ => {
                node.lhs = Some(Box::new(walk_nodecay(&mut *node.lhs.clone().unwrap(), prog).clone()));
                check_lval(node.lhs.clone().unwrap());
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
                if let Some(ref lhs) = node.lhs {
                    node.ty = lhs.ty.clone();
                }

                if let Some(ref lhs) = node.lhs {
                    if lhs.ty.borrow().ty == CType::PTR {
                        node.rhs = Some(Box::new(scale_ptr(NodeType::MUL, *node.rhs.clone().unwrap(), lhs.ty.borrow().clone())));
                    }
                }
                return node;
        }

        NodeType::EQL |
            NodeType::MUL_EQ |
            NodeType::DIV_EQ |
            NodeType::MOD_EQ |
            NodeType::SHL_EQ |
            NodeType::SHR_EQ |
            NodeType::AND_EQ |
            NodeType::XOR_EQ |
            NodeType::OR_EQ => {
            node.lhs = Some(Box::new(walk_nodecay(&mut *node.lhs.clone().unwrap(), prog).clone()));
            check_lval(node.lhs.clone().unwrap());

            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
            node.ty = node.lhs.clone().unwrap().ty;
            return node;
        }
        NodeType::DOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            let node_ty = node.expr.clone().unwrap().ty;
            if node_ty.borrow().ty != CType::STRUCT {
                bad_node!(node, "struct expected before '.'");
            }

            let ty = node.expr.clone().unwrap().ty;
            if ty.borrow().members == None {
                bad_node!(node, format!("incomplete type: {:?}", node.expr));
            }

            for m in ty.borrow().clone().members.unwrap().iter() {
                if m.name != node.name {
                    continue;
                }

                node.ty = m.ty.clone();
                return maybe_decay(node, decay);
            }
            bad_node!(node, format!("member missing: {}", node.name));
        }
        NodeType::QUEST => {
            node.cond = Some(Box::new(walk(&mut *node.cond.clone().unwrap(), prog).clone()));
            node.then = Some(Box::new(walk(&mut *node.then.clone().unwrap(), prog).clone()));
            node.els = Some(Box::new(walk(&mut *node.els.clone().unwrap(), prog).clone()));
            node.ty = node.then.clone().unwrap().ty;
            return node;
        }
        NodeType::MUL |
            NodeType::DIV |
            NodeType::MOD |
            NodeType::LT |
            NodeType::OR |
            NodeType::XOR |
            NodeType::AND |
            NodeType::EQ |
            NodeType::NE |
            NodeType::LE |
            NodeType::SHL |
            NodeType::SHR |
            NodeType::LOGAND |
            NodeType::LOGOR => {
                node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
                node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));

                check_int(&*node.lhs.clone().unwrap());
                check_int(&*node.rhs.clone().unwrap());

                match node.lhs {
                    Some(ref lhs) => {
                        node.ty = lhs.ty.clone();
                    }
                    None => { }
                }
                node.ty = Rc::new(RefCell::new(int_ty()));
                return node;
            }
        NodeType::COMMA => {
            node.lhs = Some(Box::new(walk(&mut *node.lhs.clone().unwrap(), prog).clone()));
            node.rhs = Some(Box::new(walk(&mut *node.rhs.clone().unwrap(), prog).clone()));
            node.ty = node.rhs.clone().unwrap().ty;
            return node;
        }
        NodeType::POST_INC | NodeType::POST_DEC => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            node.ty = node.expr.clone().unwrap().ty;
            return node;
        }
        NodeType::EXCLAM | NodeType::NOT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            check_int(&*node.expr.clone().unwrap());
            node.ty = node.expr.clone().unwrap().ty;
            return node;
        }
        NodeType::ADDR => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            check_lval(node.expr.clone().unwrap());
            node.ty = Rc::new(RefCell::new(ptr_to(node.expr.clone().unwrap().ty)));
            return node;
        }
        NodeType::DEREF => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));

            match &mut node.expr {
                Some(ref expr) => {
                    if expr.ty.borrow().ty != CType::PTR {
                        bad_node!(node, format!("operand must be a pointer: {:?}", expr));
                    }

                    let ptr = expr.ty.borrow().ptr_to.clone().unwrap();
                    if ptr.borrow().ty == CType::VOID {
                        bad_node!(node, "operand dereference void pointer");
                    }
                }
                None => {}
            }


            let ty_tmp = node.expr.clone().unwrap().ty;
            node.ty = ty_tmp.borrow().clone().ptr_to.unwrap();
            return maybe_decay(node, decay);
        }
        NodeType::RETURN |
            NodeType::EXPR_STMT => {
            node.expr = Some(Box::new(walk(&mut *node.expr.clone().unwrap(), prog).clone()));
            return node;
        }
        NodeType::CALL => {
            for i in 0..node.args.len() {
                node.args[i] = walk(&mut node.args[i], prog).clone();
            }
            let node_ty = *node.ty.borrow().clone().returning.unwrap();
            node.ty = Rc::new(RefCell::new(node_ty));
            return node;
        }
        NodeType::COMP_STMT => {
            for i in 0..node.stmts.len() {
                node.stmts[i] = walk(&mut node.stmts[i], prog).clone();
            }
            return node;
        }
        NodeType::STMT_EXPR => {
            let mut body = walk(&mut *node.body.clone().unwrap(), prog).clone();

            if body.stmts.len() == 0 {
                bad_node!(node, "empty statement expression");
            }

            let n = body.stmts.pop().unwrap();
            if n.op != NodeType::EXPR_STMT {
                bad_node!(node, "statement expression returning void");
            }

            node.body = Some(Box::new(body));
            node.expr = n.expr.clone();
            node.ty = n.expr.clone().unwrap().ty;
            return node;
        }
        _ => {
            panic!("unknown node type: {:?}", node);
        }
    }
}

pub fn get_type(node: &mut Node) -> Type {
    let mut prog = new_program();
    return walk_nodecay(node, &mut prog).ty.borrow().clone();
}

pub fn sema(prog: &mut Program) {
    let mut funcs = prog.funcs.clone();

    for func in funcs.iter_mut() {
        let node = func.node.clone();
        if node.borrow().op == NodeType::DECL {
            continue;
        }

        assert!(node.borrow().op == NodeType::FUNC);

        let mut args = Vec::new();
        for a in node.borrow_mut().args.iter_mut() {
            args.push(walk(&mut a.clone(), prog).clone());
        }
        node.borrow_mut().args = args;

        let mut body = node.borrow_mut().body.clone().unwrap();
        node.borrow_mut().body = Some(Box::new(walk(&mut body, prog).clone()));

        let mut off = 0;
        for v in func.lvars.iter_mut() {
            off = roundup(off, v.borrow().ty.align);
            off += v.borrow().ty.size;
            v.borrow_mut().offset = off;
        }
        func.stacksize = off;
    }

    prog.funcs = funcs;
}
