
use std::io;
use std::io::Read;
use std::collections::LinkedList;


pub struct Env {
    pub buffer: Buffer,
    pub vars: LinkedList<Ast>,
    pub strings: LinkedList<Ast>,
}

impl Env {
    pub fn new() -> Env {
        let mut ret = Env {
            buffer: Buffer::new(),
            vars: LinkedList::new(),
            strings: LinkedList::new()
        };
        /*
        ret.vars.push_back(Ast::Var(Var {
            name: String::from("null"),
            pos: 0
        }));
        */

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

    pub fn new_var(&mut self, name:String) -> Ast {
        let v = Var {
            name: name,
            pos: self.vars.len()
        };

        self.vars.push_back(Ast::Var(v.clone()));
        Ast::Var(v)
    }

    pub fn find_var(&mut self, name: &String) -> Ast {
        for x in self.vars.iter() {
            match x {
                &Ast::Var(ref v) => {
                    if v.name == *name {
                        return Ast::Var(v.clone())
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

    pub fn skip_space(&mut self) {
        while self.can_read() {
            let c = self.getc();
            if c.is_whitespace() {
                continue;
            }

            self.ungetc();
            return;
        }
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

pub struct Func {
    pub name: String,
    pub args: Vec<Ast>
}

pub enum Ast {
    Op {op:char, left: Box<Ast>, right: Box<Ast>},
    Int(u32),
    Str(usize, String),
    Var(Var),
    Func(Func),
    Null
}

impl Ast {
    pub fn is_null(&self) -> bool {
        match *self {
            Ast::Null => true,
            _ => false
        }
    }
}



