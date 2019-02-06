#[derive(Debug, PartialEq)]
pub enum TokenType {
    NUM,
    ADD,
    SUB,
    EOF,
}

pub struct Token {
    pub ty: TokenType,
    pub val: usize,
    pub input: String,
}

pub fn tokenize(p: &String) -> Vec<Token> {
    let mut token_strs = Vec::new();
    let mut t = String::new();
    for c in p.chars() {
        if c.is_whitespace() {
            continue;
        }
        if c == '+' || c == '-' {
            token_strs.push(t.clone());
            t = String::new();
            token_strs.push(format!("{}", c));
        }
        if c.is_digit(10) {
            t.push(c);
            continue;
        }
    }
    token_strs.push(t);

    let mut tokens = Vec::new();
    for s in token_strs {
        if s == "+" || s == "-" {
            let ty = match s.as_str() {
                "+" => TokenType::ADD,
                "-" => TokenType::SUB,
                _ => panic!(),
            };
            let tok = Token {
                ty: ty,
                val: 0,
                input: s,
            };

            tokens.push(tok);
        } else {
            let tok = Token {
                ty: TokenType::NUM,
                val: usize::from_str_radix(&s, 10).unwrap(),
                input: s,
            };

            tokens.push(tok);
        }
    }

    let tok = Token {
        ty: TokenType::EOF,
        val: 0,
        input: String::new(),
    };
    tokens.push(tok);

    return tokens;
}
