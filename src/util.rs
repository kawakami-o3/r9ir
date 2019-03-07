// util

use crate::parse::*;

pub fn ptr_of(base: Type) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::PTR;
    ty.ptr_of = Some(Box::new(base));
    return ty;
}

pub fn ary_of(base: Type, len: i32) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::ARY;
    ty.ary_of = Some(Box::new(base));
    ty.len = len;
    return ty;
}

pub fn size_of(ty: Type) -> i32 {
    match ty.ty {
        CType::CHAR => 1,
        CType::INT => 4,
        CType::PTR => 8,
        _ => {
            assert!(ty.ty == CType::ARY);
            size_of(*ty.ary_of.unwrap()) * ty.len
        }
    }
}

