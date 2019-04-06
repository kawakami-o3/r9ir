// util

use crate::parse::*;
use std::cell::RefCell;
use std::rc::Rc;

thread_local! {
    static NLABEL: RefCell<usize> = RefCell::new(1);
}

pub fn bump_nlabel() -> usize {
    NLABEL.with(|n| {
        let mut nlabel = n.borrow_mut();
        let ret = *nlabel;
        *nlabel += 1;
        return ret;
    })
}

pub fn first_char(s: &str) -> char {
    if s.len() == 0 {
        return '\0';
    }
    return char::from((&s[0..1].as_bytes())[0]);
}

pub fn roundup(x: i32, align: i32) -> i32 {
    return (x + align - 1) & (!(align - 1));
}

pub fn ptr_to(base: Rc<RefCell<Type>>) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::PTR;
    ty.size = 8;
    ty.align = 8;
    ty.ptr_to = Some(base);
    return ty;
}

pub fn ary_of(base: Type, len: i32) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::ARY;
    ty.size = base.size * len;
    ty.align = base.align;
    ty.ary_of = Some(Box::new(base));
    ty.len = len;
    return ty;
}

pub fn same_type(x: Rc<RefCell<Type>>, y: Rc<RefCell<Type>>) -> bool {
    if x.borrow().ty != y.borrow().ty {
        return false;
    }

    let xx = x.borrow();
    let yy = y.borrow();
    match xx.ty {
        CType::PTR => {
            let xptr = xx.ptr_to.clone().unwrap();
            let yptr = yy.ptr_to.clone().unwrap();
            same_type(xptr, yptr)
        }
        CType::ARY => {
            let xary = xx.ary_of.clone().unwrap();
            let yary = yy.ary_of.clone().unwrap();
            xx.size == yy.size && same_type(Rc::new(RefCell::new(*xary)), Rc::new(RefCell::new(*yary)))
        }
        CType::STRUCT | CType::FUNC => *xx == *yy,
        _ => true,
    }
}

