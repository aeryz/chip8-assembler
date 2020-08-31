use std::collections::HashSet;
use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    INSTRUCTION(&'a str),
    UNKNOWN(char),

    REGISTER(u16),
    INDEX,
    DT,
    ST,
    KEY,
    F,
    B,
    NEWLINE,
    MINUS,
    COMMA,
    EOF,
    LBRACKET,
    RBRACKET,

    BYTE(u16),
    ADDR(u16),
    NIBBLE(u16),
}

pub struct Instructions(HashSet<&'static str>);

impl Instructions {
    pub fn new() -> Self {
        let mut ins = HashSet::new();
        ins.insert("sys");
        ins.insert("cls");
        ins.insert("ret");
        ins.insert("jp");
        ins.insert("call");
        ins.insert("se");
        ins.insert("sne");
        ins.insert("ld");
        ins.insert("add");
        ins.insert("or");
        ins.insert("and");
        ins.insert("sub");
        ins.insert("xor");
        ins.insert("shl");
        ins.insert("shr");
        ins.insert("subn");
        ins.insert("rnd");
        ins.insert("drw");
        ins.insert("skp");
        ins.insert("sknp");
        ins.insert("drw");

        Self(ins)
    }
}

impl<'a> Token<'a> {
    pub fn from(inst: &Instructions, ident: &'a str) -> Result<Token<'a>, Cow<'static, str>> {
        match ident {
            ident if inst.0.contains(ident) => Ok(Token::INSTRUCTION(ident)),
            "i" => Ok(Token::INDEX),
            "k" => Ok(Token::KEY),
            "dt" => Ok(Token::DT),
            "st" => Ok(Token::ST),
            "f" => Ok(Token::F),
            "b" => Ok(Token::B),
            reg_str if ident.chars().nth(0) == Some('v') => {
                if reg_str.len() != 2 {
                    return Err(format!("Invalid register {}", reg_str).into());
                }
                let hex_val = u16::from_str_radix(&reg_str[1..], 16)
                    .map_err(|_| format!("Invalid hex value {}", reg_str))?;
                Ok(Token::REGISTER(hex_val))
            }
            token => Err(format!("Invalid token {}", token).into())
        }
    }
}
