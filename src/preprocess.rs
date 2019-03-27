// C preprocessor

use crate::token::*;
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static DEFINED: RefCell<HashMap<String, Vec<Token>>> = RefCell::new(HashMap::new());
}

fn defined_get(key: &String) -> Option<Vec<Token>> {
    DEFINED.with(|defined| {
        match defined.borrow().get(key) {
            Some(v) => Some(v.clone()),
            None => None,
        }
    })
}

fn defined_put(key: String, value: Vec<Token>) {
    DEFINED.with(|defined| {
        defined.borrow_mut().insert(key, value);
    })
}

pub fn preprocess(tokens: Vec<Token>) -> Vec<Token> {
    // if(!defined) ...

    let mut v: Vec<Token> = Vec::new();

    let mut i = 0;
    while i < tokens.len() {
        let mut t = &tokens[i];
        i += 1;

        if t.ty == TokenType::IDENT {
            match defined_get(&t.name) {
                Some(macro_token) => {
                    v.append(&mut macro_token.clone());
                }
                None => {
                    v.push(t.clone());
                }
            }
            continue;
        }

        if t.ty != TokenType::SHARP {
            v.push(t.clone());
            continue;
        }

        t = &tokens[i];
        i += 1;
        if t.ty != TokenType::IDENT {
            bad_token(&t, "identifier expected".to_string());
        }

        if t.name == "define" {
            t = &tokens[i];
            i += 1;
            if t.ty != TokenType::IDENT {
                bad_token(&t, "macro name expected".to_string());
            }
            let name = t.name.clone();

            let mut v2 = Vec::new();
            while i < tokens.len() {
                t = &tokens[i];
                i += 1;
                if t.ty == TokenType::NEW_LINE {
                    break;
                }
                v2.push(t.clone());
            }
            defined_put(name, v2);
            continue;
        }

        if t.name == "include" {
            t = &tokens[i];
            i += 1;
            if t.ty != TokenType::STR {
                bad_token(&t, "string expected".to_string());
            }

            let path = t.str_cnt.clone();

            t = &tokens[i];
            i += 1;
            if t.ty != TokenType::NEW_LINE {
                bad_token(&t, "newline expected".to_string());
            }

            v.append(&mut tokenize(path, false));
            continue;
        }
    }

    return v;
}
