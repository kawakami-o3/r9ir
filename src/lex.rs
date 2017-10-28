use rcc_env::*;
use rcc_env::Buffer;

#[derive(Clone)]
pub enum Token {
    Ident(String),
    Punct(char),
    Int(u32),
    Char(char),
    Str(String),
    Null
}

impl Token {
    pub fn is_punct(&self, c: char) -> bool {
        match *self {
            Token::Null => panic!("Token is null"),
            Token::Punct(chr) => chr == c,
            _ => false
        }
    }

    pub fn is_null(&self) -> bool {
        match *self {
            Token::Null => true,
            _ => false
        }
    }

    pub fn string(&self) -> String {
        match *self {
            Token::Ident(ref c) => format!("{}", c),
            Token::Punct(c) => format!("{}", c),
            Token::Char(c) => format!("{}", c),
            Token::Int(c) => format!("{}", c),
            Token::Str(ref s) => format!("\"{}\"", s),
            Token::Null => "Null".to_string()
        }
    }
}

fn read_number(environment: &mut Env, c: char) -> Token {
    let mut n = c.to_digit(10).unwrap();
    loop {
        let c = environment.buffer.getc();
        if !c.is_digit(10) {
            environment.buffer.ungetc();
            return Token::Int(n);
        }
        n = n * 10 + c.to_digit(10).unwrap();
    }
}

fn read_string(environment: &mut Env) -> Token {
    let mut buf = String::new();
    loop {
        if environment.buffer.is_end() {
            panic!("Unterminated string");
        }
        let c = environment.buffer.getc();
        if c == '"' {
            break;
        }

        if c == '\\' {
            if environment.buffer.is_end() {
                panic!("Unterminated \\");
            }
            buf.push(c);
            buf.push(environment.buffer.getc());
        } else {
            buf.push(c);
        }
    }

    Token::Str(buf)
}


fn read_char(environment: &mut Env) -> Token {
    fn unterminated() {
        panic!("Unterminated char");
    }

    if environment.buffer.is_end() {
        unterminated();
    }

    let chr = match environment.buffer.getc() {
        '\\' => {
            if environment.buffer.is_end() {
                unterminated();
            }
            environment.buffer.getc()
        }
        c => { c }
    };

    if environment.buffer.is_end() {
        unterminated();
    }
    if environment.buffer.getc() != '\'' {
        panic!("Malformed char constant");
    }
    Token::Char(chr)
}

fn read_ident(environment: &mut Env, c: char) -> Token {
    let mut sym = String::new(); // no limit symbol length

    sym.push(c);

    loop {
        let c = environment.buffer.getc();
        if c.is_digit(10) || c.is_alphabetic() {
            sym.push(c);
        } else {
            environment.buffer.ungetc();
            return Token::Ident(sym)
        }
    }
}


pub fn read_token_int(environment: &mut Env) -> Token {
    environment.buffer.skip_space();
    if environment.buffer.is_end() {
        return Token::Null;
    }

    let c = environment.buffer.getc();
    match c {
        '0' ... '9' => read_number(environment, c),
        '"' => read_string(environment),
        '\'' => read_char(environment),
        'A' ... 'Z' | 'a' ... 'z' | '_' => read_ident(environment, c),
        '/' | '=' | '*' | '+' | '-' | '(' | ')' | ',' | ';'
            => Token::Punct(c),
        _ => panic!("Unexpected character: '{}'", c)
    }
}

pub fn unget_token(environment: &mut Env, tok: Token) {
    if ! environment.get_token().is_null() {
        panic!("Push back buffer is already full");
    }
    environment.store(tok)
}

pub fn read_token(environment: &mut Env) -> Token {
    if ! environment.get_token().is_null() {
        let ret = environment.get_token();
        environment.store(Token::Null);
        return ret;
    }
    return read_token_int(environment);
}
