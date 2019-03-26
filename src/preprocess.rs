// C preprocessor

use crate::token::*;

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    let mut v: Vec<Token> = Vec::new();

    let mut i = 0;
    while i < tokens.len() {
        let mut t = &tokens[i];
        if t.ty != TokenType::SHARP {
            i += 1;
            v.push(t.clone());
            continue;
        }

        i += 1;
        t = &tokens[i];
        if t.ty != TokenType::IDENT || t.name != "include" {
            bad_token(&t, "'include' expected".to_string());
        }
        
        i += 1;
        t = &tokens[i];
        if t.ty != TokenType::STR {
            bad_token(&t, "string expected".to_string());
        }

        let path = t.str_cnt.clone();

        i += 1;
        t = &tokens[i];
        if t.ty != TokenType::NEW_LINE {
            bad_token(&t, "newline expected".to_string());
        }

        v.append(&mut tokenize(path, false));
    }
    return v;
}
