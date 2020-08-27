use crate::lexer::Lexer;
use crate::token::Token;
use std::collections::HashMap;
use std::borrow::Cow;

type Opcodes = Vec<u16>;
type ParseResult<T> = Result<T, Cow<'static, str>>;
type ParseFn<'a> = fn(&mut Parser<'a>) -> ParseResult<u16>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    opcodes: Opcodes,
    parse_fns: HashMap<&'static str, ParseFn<'a>>,
}

impl<'a> Parser<'a> {
    
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut parse_fns: HashMap<&'static str, ParseFn<'a>> = HashMap::new();
        parse_fns.insert("cls", Parser::parse_ins_cls);
        parse_fns.insert("ret", Parser::parse_ins_ret);
        parse_fns.insert("sys", Parser::parse_ins_sys);
        Self {
            lexer,
            opcodes: Opcodes::new(),
            parse_fns 
        }
    }

    pub fn parse(mut self) -> ParseResult<Opcodes> {
        loop {
            let token = self.lexer.next_token()?;
            match token {
                Token::EOF => return Ok(self.opcodes),
                Token::INSTRUCTION(ins) => {
                    let parse_fn = self.parse_fns.get(ins);
                    if parse_fn.is_none() {
                        return Err(format!("Invalid instruction {}", ins).into());
                    }
                    let res = parse_fn.unwrap()(&mut self)?;
                    self.opcodes.push(res);
                }
                _ => return Err(format!("Expected instruction, got {:?}", token).into()),
            }
        }
    }

}

impl<'a> Parser<'a> {

    fn parse_ins_cls(&mut self) -> ParseResult<u16> {
        Ok(0x00e0)
    }

    fn parse_ins_ret(&mut self) -> ParseResult<u16> {
        Ok(0x00ee)
    }

    fn parse_ins_sys(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::ADDR(addr) => Ok(addr),
            token => Err(format!("Expected Token::ADDR, got {:?}", token).into()),
        }
    } 

}

#[cfg(test)]
mod tests {
    use super::*;

    fn prepare_parser<'a>(program: &'static str) -> Parser<'a> {
        Parser::new(Lexer::new(program))
    }

    #[test]
    fn test_ins_cls() {
        let parser = prepare_parser("cls");
        assert_eq!(parser.parse().unwrap(), vec![0x00e0]);
    }

    #[test]
    fn test_ins_sys() {
        let parser = prepare_parser("sys @1c2");
        assert_eq!(parser.parse().unwrap(), vec![0x01c2]);
    }

}
