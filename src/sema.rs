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
// - Insert nodes for implicit cast so that they are explicitly
//   represented in AST.
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

fn maybe_decay(tmp: Rc<RefCell<Node>>, decay: bool) -> Rc<RefCell<Node>> {
    let tmp_ty = tmp.borrow().ty.clone();
    if !decay || tmp_ty.borrow().ty != CType::ARY {
        return tmp;
    }

    //let tmp = base.clone();
    let base = Rc::new(RefCell::new(alloc_node()));
    base.borrow_mut().op = NodeType::ADDR;

    let node_ty = tmp.borrow().ty.clone();
    let ty_tmp = *node_ty.borrow().clone().ary_of.unwrap();
    base.borrow_mut().ty = Rc::new(RefCell::new(ptr_to(Rc::new(RefCell::new(ty_tmp)))));

    base.borrow_mut().expr = Some(tmp.clone());
    base.borrow_mut().token = tmp.borrow().token.clone();
    return base;
}

macro_rules! bad_node {
    ($node:expr, $msg:expr) => {
        bad_token(&*$node.borrow().token.clone().unwrap(), $msg.to_string());
        panic!();
    };
}

fn check_lval(node: Rc<RefCell<Node>>) {
    let op = node.borrow().op.clone();
    match op {
        NodeType::VARREF | NodeType::DEREF | NodeType::DOT => { }
        _ => {
            bad_node!(node, "not an lvalue");
        }
    }
}

fn scale_ptr(op: NodeType, base: Rc<RefCell<Node>>, ty: Type) -> Rc<RefCell<Node>> {
    let mut node = alloc_node();
    node.op = op;
    node.lhs = Some(base.clone());
    let ptr = ty.ptr_to.unwrap();
    node.rhs = Some(new_int_node(ptr.borrow().size, base.borrow().token.clone()));
    node.token = base.borrow().token.clone();
    return Rc::new(RefCell::new(node));
}

fn cast(base: Rc<RefCell<Node>>, ty: Type) -> Rc<RefCell<Node>> {
    let mut node = alloc_node();
    node.op = NodeType::CAST;
    node.ty = Rc::new(RefCell::new(ty));
    node.expr = Some(base.clone());
    node.token = base.borrow().token.clone();
    return Rc::new(RefCell::new(node));
}

fn check_int(node: Rc<RefCell<Node>>) {
    let node_ty = node.borrow().ty.clone();
    let ty = node_ty.borrow();
    if ty.ty != CType::INT && ty.ty != CType::CHAR && ty.ty != CType::BOOL {
        bad_node!(node, "not an integer");
    }
}

fn walk(node: Rc<RefCell<Node>>, prog: &mut Program) -> Rc<RefCell<Node>> {
    return do_walk(node, true, prog);
}

fn walk_nodecay(node: Rc<RefCell<Node>>, prog: &mut Program) -> Rc<RefCell<Node>> {
    return do_walk(node, false, prog);
}

fn do_walk(node: Rc<RefCell<Node>>, decay: bool, prog: &mut Program) -> Rc<RefCell<Node>> {
    let op = node.borrow().op.clone();
    match op {
        NodeType::NUM |
            NodeType::NULL |
            NodeType::BREAK |
            NodeType::CONTINUE => {
            return node;
        }
        NodeType::VARREF => {
            return maybe_decay(node, decay);
        }
        NodeType::IF => {
            let cond = node.borrow().cond.clone();
            let then = node.borrow().then.clone();
            let els = node.borrow().els.clone();

            node.borrow_mut().cond = Some(walk(cond.unwrap(), prog));
            node.borrow_mut().then = Some(walk(then.unwrap(), prog));

            if els.is_some() {
                node.borrow_mut().els = Some(walk(els.unwrap(), prog));
            }
            return node;
        }
        NodeType::FOR => {
            let init = node.borrow().init.clone();
            let cond = node.borrow().cond.clone();
            let inc = node.borrow().inc.clone();
            let body = node.borrow().body.clone();

            if init.is_some() {
                node.borrow_mut().init = Some(walk(init.unwrap(), prog));
            }
            if cond.is_some() {
                node.borrow_mut().cond = Some(walk(cond.unwrap(), prog));
            }
            if inc.is_some() {
                node.borrow_mut().inc = Some(walk(inc.unwrap(), prog));
            }
            node.borrow_mut().body = Some(walk(body.unwrap(), prog).clone());
            return node;
        }
        NodeType::DO_WHILE | NodeType::SWITCH => {
            let cond = node.borrow().cond.clone();
            node.borrow_mut().cond = Some(walk(cond.unwrap(), prog));
            let body = node.borrow().body.clone();
            node.borrow_mut().body = Some(walk(body.unwrap(), prog));
            return node
        }
        NodeType::CASE => {
            let body = node.borrow().body.clone();
            node.borrow_mut().body = Some(walk(body.unwrap(), prog));
            return node;
        }
        NodeType::ADD => {
            let lhs = node.borrow().lhs.clone();
            let rhs = node.borrow().rhs.clone();

            node.borrow_mut().lhs = Some(walk(lhs.unwrap(), prog));
            node.borrow_mut().rhs = Some(walk(rhs.unwrap(), prog));

            let lhs_is_some = node.borrow().lhs.is_some();
            let rhs_is_some = node.borrow().rhs.is_some();
            if lhs_is_some && rhs_is_some {
                let rhs = node.borrow().rhs.clone().unwrap();
                let rhs_ty = rhs.borrow().ty.clone();
                if rhs_ty.borrow().ty == CType::PTR {
                    //use std::mem;
                    //mem::swap(lhs, rhs);
                    let a = node.borrow().lhs.clone();
                    let b = node.borrow().rhs.clone();
                    node.borrow_mut().lhs = b;
                    node.borrow_mut().rhs = a;
                }
            }

            let rhs_is_some = node.borrow().rhs.is_some();
            if rhs_is_some {
                let node_rhs = node.borrow().rhs.clone();
                check_int(node_rhs.unwrap());
            }

            let lhs_is_some = node.borrow().lhs.is_some();
            if lhs_is_some {
                let lhs = node.borrow().lhs.clone().unwrap();
                let lhs_ty = lhs.borrow().ty.clone();
                let is_ptr = lhs_ty.borrow().ty == CType::PTR;
                if is_ptr {
                    let rhs = node.borrow().rhs.clone().unwrap();
                    node.borrow_mut().rhs = Some(scale_ptr(NodeType::MUL, rhs, lhs_ty.borrow().clone()));
                    node.borrow_mut().ty = lhs.borrow().ty.clone();
                } else {
                    node.borrow_mut().ty = Rc::new(RefCell::new(int_ty()));
                }
            }

            return node;
        }
        NodeType::SUB => {
            let lhs = node.borrow().lhs.clone();
            let rhs = node.borrow().rhs.clone();
            node.borrow_mut().lhs = Some(walk(lhs.unwrap(), prog));
            node.borrow_mut().rhs = Some(walk(rhs.unwrap(), prog));

            let lhs = node.borrow().lhs.clone().unwrap();
            let rhs = node.borrow().rhs.clone().unwrap();
 
            let lty = rhs.borrow().ty.clone();
            let rty = lhs.borrow().ty.clone();

            let mut ret = node.clone();
            if lty.borrow().ty == CType::PTR && rty.borrow().ty == CType::PTR {
                if !same_type(rty.clone(), lty.clone()) {
                    bad_node!(node, "incompatible pointer");
                }
                ret = scale_ptr(NodeType::DIV, node.clone(), lty.borrow().clone());
                ret.borrow_mut().ty = lty.clone();
            } else {
                ret.borrow_mut().ty = Rc::new(RefCell::new(int_ty()));
            }

            let node_lhs = ret.borrow().lhs.clone().unwrap();
            let ty = node_lhs.borrow().ty.clone();
            ret.borrow_mut().ty = ty;
            return ret;
        }
        NodeType::EQL => {
            let lhs = node.borrow().lhs.clone();

            node.borrow_mut().lhs = Some(walk_nodecay(lhs.unwrap(), prog));
            check_lval(node.borrow().lhs.clone().unwrap());

            let rhs = node.borrow().rhs.clone();
            node.borrow_mut().rhs = Some(walk(rhs.unwrap(), prog));
            let lhs = node.borrow().lhs.clone().unwrap();
            let lty = lhs.borrow().ty.clone();
            if lty.borrow().ty == CType::BOOL {
                let rhs = node.borrow().rhs.clone();
                node.borrow_mut().rhs = Some(cast(rhs.unwrap(), bool_ty()));
            }
            node.borrow_mut().ty = lhs.borrow().ty.clone();
            return node;
        }
        NodeType::DOT => {
            let expr = node.borrow().expr.clone();

            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));
            let node_expr = node.borrow().expr.clone().unwrap();
            let node_ty = node_expr.borrow().clone().ty;
            if node_ty.borrow().ty != CType::STRUCT {
                bad_node!(node, "struct expected before '.'");
            }

            let ty = node_expr.borrow().clone().ty;
            if ty.borrow().members == None {
                bad_node!(node, format!("incomplete type: {:?}", node.borrow().expr));
            }

            let members = ty.borrow().members.clone().unwrap();
            let node_ty = members.get(&node.borrow().name);
            if node_ty.is_none() {
                bad_node!(node, format!("member missing: {}", node.borrow().name));
            }

            node.borrow_mut().ty = node_ty.unwrap().clone();
            return maybe_decay(node, decay);
        }
        NodeType::QUEST => {
            let cond = node.borrow().cond.clone();
            let then = node.borrow().then.clone();
            let els = node.borrow().els.clone();

            node.borrow_mut().cond = Some(walk(cond.unwrap(), prog));
            node.borrow_mut().then = Some(walk(then.unwrap(), prog));
            node.borrow_mut().els = Some(walk(els.unwrap(), prog));
            
            let then = node.borrow().then.clone().unwrap();
            node.borrow_mut().ty = then.borrow().ty.clone();
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
                let lhs = node.borrow().lhs.clone();
                let rhs = node.borrow().rhs.clone();

                node.borrow_mut().lhs = Some(walk(lhs.unwrap(), prog));
                node.borrow_mut().rhs = Some(walk(rhs.unwrap(), prog));

                check_int(node.borrow().lhs.clone().unwrap());
                check_int(node.borrow().rhs.clone().unwrap());

                let lhs = node.borrow().lhs.clone();
                if lhs.is_some() {
                    let l = lhs.unwrap();
                    node.borrow_mut().ty = l.borrow().ty.clone();
                }
                node.borrow_mut().ty = Rc::new(RefCell::new(int_ty()));
                return node;
            }
        NodeType::COMMA => {
            let lhs = node.borrow().lhs.clone();
            let rhs = node.borrow().rhs.clone();

            node.borrow_mut().lhs = Some(walk(lhs.unwrap(), prog));
            node.borrow_mut().rhs = Some(walk(rhs.unwrap(), prog));

            let rhs = node.borrow().rhs.clone().unwrap();
            node.borrow_mut().ty = rhs.borrow().ty.clone();
            return node;
        }
        NodeType::EXCLAM | NodeType::NOT => {
            let expr = node.borrow().expr.clone();
            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));
            let expr = node.borrow().expr.clone();
            check_int(expr.unwrap());
            node.borrow_mut().ty = Rc::new(RefCell::new(int_ty()));
            return node;
        }
        NodeType::ADDR => {
            let expr = node.borrow().expr.clone();
            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));
            check_lval(node.borrow().expr.clone().unwrap());
            let expr = node.borrow().expr.clone().unwrap();
            node.borrow_mut().ty = Rc::new(RefCell::new(ptr_to(expr.borrow().ty.clone())));
            return node;
        }
        NodeType::DEREF => {
            let expr = node.borrow().expr.clone();
            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));

            let e = node.borrow().expr.clone();
            if e.is_some() {
                let expr = e.unwrap();
                let expr_ty = expr.borrow().ty.clone();
                if expr_ty.borrow().ty != CType::PTR {
                    bad_node!(node, format!("operand must be a pointer: {:?}", expr));
                }

                let ptr = expr_ty.borrow().ptr_to.clone().unwrap();
                if ptr.borrow().ty == CType::VOID {
                    bad_node!(node, "operand dereference void pointer");
                }
            }

            let node_expr = node.borrow().expr.clone().unwrap();
            let ty_tmp = node_expr.borrow().ty.clone();
            node.borrow_mut().ty = ty_tmp.borrow().clone().ptr_to.unwrap();
            return maybe_decay(node, decay);
        }
        NodeType::RETURN | NodeType::EXPR_STMT => {
            let expr = node.borrow().expr.clone();
            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));
            return node;
        }
        NodeType::CALL => {
            let args = node.borrow().args.clone();
            for i in 0..args.len() {
                node.borrow_mut().args[i] = walk(args[i].clone(), prog);
            }
            let node_ty = node.borrow().ty.clone();
            let ty = node_ty.borrow().clone().returning.unwrap();
            node.borrow_mut().ty = Rc::new(RefCell::new(*ty));
            return node;
        }
        NodeType::COMP_STMT => {
            let stmts = node.borrow().stmts.clone();
            for i in 0..stmts.len() {
                node.borrow_mut().stmts[i] = walk(stmts[i].clone(), prog);
            }
            return node;
        }
        NodeType::STMT_EXPR => {
            let stmts = node.borrow().stmts.clone();
            for i in 0..stmts.len() {
                node.borrow_mut().stmts[i] = walk(stmts[i].clone(), prog);
            }
            let expr = node.borrow().expr.clone();
            node.borrow_mut().expr = Some(walk(expr.unwrap(), prog));
            let expr = node.borrow().expr.clone().unwrap();
            node.borrow_mut().ty = expr.borrow().ty.clone();
            return node;
        }
        _ => {
            panic!("unknown node type: {:?}", node);
        }
    }
}

pub fn get_type(node: Rc<RefCell<Node>>) -> Type {
    let mut prog = new_program();
    let n = walk_nodecay(node, &mut prog);
    let ty = n.borrow().ty.clone();
    return ty.borrow().clone();
}

pub fn sema(prog: &mut Program) {
    let mut funcs = prog.funcs.clone();

    for func in funcs.iter_mut() {
        let node = func.borrow().node.clone();
        if node.borrow().op == NodeType::DECL {
            continue;
        }

        assert!(node.borrow().op == NodeType::FUNC);

        let mut body = node.borrow_mut().body.clone().unwrap();
        node.borrow_mut().body = Some(walk(body, prog));
    }

    prog.funcs = funcs;
}
