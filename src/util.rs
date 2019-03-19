// util

use crate::parse::*;

pub fn ptr_to(base: Type) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::PTR;
    ty.size = 8;
    ty.align = 8;
    ty.ptr_to = Some(Box::new(base));
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

pub fn struct_of(members: &mut Vec<Node>) -> Type {
    let mut ty = alloc_type();
    ty.ty = CType::STRUCT;

    let mut off = 0;
    for node in members.iter_mut() {
        assert!(node.op == NodeType::VARDEF);

        let ty_tmp = node.ty.clone();
        let t = &mut node.ty;
        off = roundup(off, t.align);
        t.offset = off;
        off += t.size;

        if ty.align < ty_tmp.align {
            ty.align = ty_tmp.align;
        }
    }
    ty.size = roundup(off, ty.align);
    return ty;
}

pub fn roundup(x: i32, align: i32) -> i32 {
    return (x + align - 1) & (!(align - 1));
}
