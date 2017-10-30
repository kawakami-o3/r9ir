use std::cell::Cell;
use std::io;
use std::io::Read;
use std::collections::LinkedList;

use lex::*;

pub struct Env {
    pub buffer: Buffer,
    pub vars: LinkedList<Ast>,
    pub strings: LinkedList<Ast>,
    pub token_stored: Token,
}

impl Env {
    pub fn new() -> Env {
        let mut ret = Env {
            buffer: Buffer::new(),
            vars: LinkedList::new(),
            strings: LinkedList::new(),
            token_stored: Token::Null,
        };

        ret.vars.push_back(Ast::Null);

        ret
    }

    pub fn null(&mut self) -> Ast {
        Ast::Null
        /*
        Var {
            name: String::from("null"),
            pos: 0
        }
        */
    }

    pub fn new_var(&mut self, ctype: CType, name:String) -> Ast {
        let v = Var {
            name: name,
            pos: self.vars.len()
        };

        self.vars.push_back(Ast::Var(v.clone(), ctype.clone()));
        Ast::Var(v, ctype)
    }

    pub fn find_var(&mut self, name: &String) -> Ast {
        for x in self.vars.iter() {
            match x {
                &Ast::Var(ref v, ref ctype) => {
                    if v.name == *name {
                        return Ast::Var(v.clone(), ctype.clone())
                    }
                }
                _ => { }
            }
        }
        self.null()
    }

    pub fn new_str(&mut self, s: &String) -> Ast {
        let id = self.strings.len();
        self.strings.push_back(Ast::Str(id, s.clone()));
        Ast::Str(id, s.clone())
    }

    pub fn store(&mut self, t: Token) {
        self.token_stored = t
    }

    pub fn get_token(&mut self) -> Token {
        self.token_stored.clone()
    }
}


pub struct Buffer {
    chars: Vec<char>,
    idx: usize,
}

impl Buffer {
    pub fn new() -> Buffer {
        let mut vec = Vec::new();

        for i in io::stdin().bytes() {
            vec.push(char::from(i.unwrap()));
        }

        Buffer {
            chars: vec,
            idx: 0,
        }
    }
/*
    fn print(& self) {
        //println!("{:?}", self.chars)

        let mut bytes: Vec<u8> = Vec::new();

        //let a = self.chars[0].as_byte();

        for c in self.chars.clone() {
            let mut bs = [0; 2];
            c.encode_utf8(&mut bs);

            for b in bs.iter() {
                bytes.push(*b);
            }
        }

        //write_debug("hello".as_bytes())
        write_debug(&bytes)
    }
*/
    pub fn getc(&mut self) -> char {
        self.idx += 1;
        return self.chars[self.idx - 1];
    }

    pub fn ungetc(&mut self) {
        self.idx -= 1;
    }

    pub fn can_read(& self) -> bool {
        return self.chars.len() > self.idx;
    }

    pub fn is_end(& self) -> bool {
        return !self.can_read();
    }

    pub fn getc_nonspace(&mut self) -> Option<char> {
        while self.can_read() {
            let c = self.getc();
            if c.is_whitespace() {
                continue;
            }

            return Some(c);
        }
        return None
    }
}

/*
impl fmt::Display for Buffer {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = String::new();
        for i in &self.chars {
            for c in i.escape_default() {
                s.push(c);
            }
            s.push(',');
        }
        write!(f, "['{}', {}]", s, self.idx)
    }
}
*/
#[derive(Clone)]
pub struct Var {
    pub name: String,
    pub pos: usize,
}

impl Var {
    /*
    fn new(name:String) -> Var {
        unsafe {
            Var {
                name: name,
                pos: VARS.unwrap().len(), // + 1,
            }
        }
    }
    */

    pub fn clone(&self) -> Var {
        Var {
            name: self.name.clone(),
            pos: self.pos,
        }
    }
}

#[derive(Clone)]
pub struct Func {
    pub name: String,
    pub args: Vec<Ast>
}

impl Func {
    pub fn get_args(&self) -> Vec<Ast> {
        self.args.clone()
    }
}

#[derive(Clone)]
pub enum Ast {
    Op {op:char, ctype:CType, left: Box<Ast>, right: Box<Ast>},
    Int(u32),
    Char(char),
    Str(usize, String),
    Var(Var, CType),
    Func(Func, CType),
    Decl {var: Box<Ast>, init: Box<Ast>, ctype:CType},
    Null
}

impl Ast {
    pub fn is_null(&self) -> bool {
        match *self {
            Ast::Null => true,
            _ => false
        }
    }

    pub fn get_ctype(&self) -> CType {
        match *self {
            Ast::Op {ref op, ref ctype, ref left, ref right} => ctype.clone(),
            Ast::Int(_) => CType::Int,
            Ast::Char(_) => CType::Char,
            Ast::Str(_, _) => CType::Str,
            Ast::Var(_, ref ctype) => ctype.clone(),
            Ast::Func(_, ref ctype) => ctype.clone(),
            Ast::Decl {ref var, ref init, ref ctype} => ctype.clone(),
            Ast::Null => CType::Void
        }
    }
}

#[derive(Clone)]
pub enum CType {
    Void,
    Int,
    Char,
    Str
}

impl CType {
    pub fn to_string(&self) -> String {
        String::from(match *self {
            CType::Void => "void",
            CType::Int => "int",
            CType::Char => "char",
            CType::Str => "string"
        })
    }
}
