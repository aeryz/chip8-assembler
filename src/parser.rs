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
        parse_fns.insert("jp", Parser::parse_ins_jp);
        parse_fns.insert("call", Parser::parse_ins_call);
        parse_fns.insert("se", Parser::parse_ins_se);
        parse_fns.insert("sne", Parser::parse_ins_sne);
        parse_fns.insert("and", Parser::parse_ins_and);
        parse_fns.insert("or", Parser::parse_ins_or);
        parse_fns.insert("xor", Parser::parse_ins_xor);
        parse_fns.insert("sub", Parser::parse_ins_sub);
        parse_fns.insert("subn", Parser::parse_ins_subn);
        parse_fns.insert("shl", Parser::parse_ins_shl);
        parse_fns.insert("shr", Parser::parse_ins_shr);
        parse_fns.insert("ld", Parser::parse_ins_ld);
        parse_fns.insert("add", Parser::parse_ins_add);
        parse_fns.insert("rnd", Parser::parse_ins_rnd);
        parse_fns.insert("drw", Parser::parse_ins_drw);
        parse_fns.insert("skp", Parser::parse_ins_skp);
        parse_fns.insert("sknp", Parser::parse_ins_sknp);

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
            match self.lexer.next_token()? {
                Token::EOF => return Ok(self.opcodes),
                Token::NEWLINE => {},
                token => return Err(format!("Expected Token::EOF or Token::NEWLINE, got {:?}", token).into()),
            }
        }
    }

}

impl<'a> Parser<'a> {

    #[inline]
    fn parse_layout_addr(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::ADDR(addr) => Ok(addr),
            token => Err(format!("Expected Token::ADDR, got {:?}", token).into())
        }
    }

    #[inline]
    fn parse_layout_reg_b_or_r(&mut self, opcodes: (u16, u16)) -> ParseResult<u16> {
        let register = self.expect_register()?;

        self.expect_token(Token::COMMA)?;

        match self.lexer.next_token()? {
            Token::BYTE(byte) => Ok(opcodes.0 + 0x100 * register + byte),
            Token::REGISTER(reg) => Ok(opcodes.1 + 0x100 * register + 0x10 * reg),
            token => Err(format!("Expected Token::BYTE, got {:?}", token).into())
        }
    }

    #[inline]
    fn parse_layout_reg_reg(&mut self, opcode: u16) -> ParseResult<u16> {
        let lhs_reg = self.expect_register()?;
        self.expect_token(Token::COMMA)?;
        let rhs_reg = self.expect_register()?;

        Ok(opcode + lhs_reg * 0x100 + rhs_reg * 0x10)
    }

    #[inline]
    fn parse_layout_reg_byte(&mut self, opcode: u16) -> ParseResult<u16> {
        let register = self.expect_register()?;
        self.expect_token(Token::COMMA)?;
        let byte = self.expect_byte()?;

        Ok(opcode + register * 0x100 + byte)
    }

    fn parse_layout_addr_or_reg(&mut self, opcodes: (u16, u16)) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::ADDR(addr) => Ok(opcodes.0 + addr),
            Token::REGISTER(_) => {
                let addr = self.expect_addr()?;
                Ok(opcodes.1 + addr)
            },
            token => Err(format!("Expected Token::ADDR or Token::REGISTER, got {:?}", token).into()),
        }
    }

    fn expect_token(&mut self, token: Token<'a>) -> ParseResult<()> {
        let next_token = self.lexer.next_token()?;
        if token != next_token {
            return Err(format!("Expected {:?}, got {:?}", token, next_token).into());
        }
        Ok(())
    }

    fn expect_register(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::REGISTER(reg) => Ok(reg),
            token => Err(format!("Expected Token::REGISTER, got {:?}", token).into()),
        }
    }

    fn expect_addr(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::ADDR(addr) => Ok(addr),
            token => Err(format!("Expected Token::ADDR, got {:?}", token).into()),
        }
    }

    fn expect_byte(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::BYTE(byte) => Ok(byte),
            token => Err(format!("Expected Token::BYTE, got {:?}", token).into()),
        }
    }

    fn expect_nibble(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::NIBBLE(nibble) => Ok(nibble),
            token => Err(format!("Expected Token::NIBBLE, got {:?}", token).into()),
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
        self.parse_layout_addr()
    } 

    fn parse_ins_jp(&mut self) -> ParseResult<u16> {
        self.parse_layout_addr_or_reg((0x1000, 0xB000))
    }

    fn parse_ins_call(&mut self) -> ParseResult<u16> {
        Ok(0x2000 + self.parse_layout_addr()?)
    }

    fn parse_ins_se(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_b_or_r((0x3000, 0x5000))
    }

    fn parse_ins_sne(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_b_or_r((0x4000, 0x9000))
    }

    fn parse_ins_or(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8000)
    }

    fn parse_ins_xor(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8003)
    }
    
    fn parse_ins_and(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8002)
    }

    fn parse_ins_sub(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8005)
    }

    fn parse_ins_subn(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8007)
    }

    fn parse_ins_shr(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x8006)
    }

    fn parse_ins_shl(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_reg(0x800E)
    }

    fn parse_ins_rnd(&mut self) -> ParseResult<u16> {
        self.parse_layout_reg_byte(0xC000)
    }

    fn parse_ins_skp(&mut self) -> ParseResult<u16> {
        Ok(0xE09E + 0x100 * self.expect_register()?)
    }

    fn parse_ins_sknp(&mut self) -> ParseResult<u16> {
        Ok(0xE0A1 + 0x100 * self.expect_register()?)
    }

    fn parse_ins_drw(&mut self) -> ParseResult<u16> {
        let lhs_reg = self.expect_register()?;
        self.expect_token(Token::COMMA)?;
        let rhs_reg = self.expect_register()?;
        self.expect_token(Token::COMMA)?;
        let nibble = self.expect_nibble()?;

        Ok(0xD000 + 0x100 * lhs_reg + 0x10 * rhs_reg + nibble)
    }

    fn parse_ins_add(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::INDEX => Ok(0xF01E + 0x100 * self.expect_register()?),
            Token::REGISTER(register) => {
                match self.lexer.next_token()? {
                    Token::REGISTER(reg) => Ok(0x8004 + 0x100 * register + 0x10 * reg),
                    Token::BYTE(byte) => Ok(0x7000 + 0x100 * register + byte),
                    token => Err(format!("Unexpected token {:?}", token).into()),
                }
            }
            token => Err(format!("Unexpected token {:?}", token).into()),
        }
    }

    fn parse_ins_ld(&mut self) -> ParseResult<u16> {
        match self.lexer.next_token()? {
            Token::REGISTER(reg) => {
                match self.lexer.next_token()? {
                    Token::DT => Ok(0xF007 + reg),
                    Token::KEY => Ok(0xF00A + reg),
                    Token::LBRACKET => {
                        self.expect_token(Token::INDEX)?;
                        self.expect_token(Token::RBRACKET)?;
                        Ok(0xF065 + 0x100 * self.expect_register()?)
                    },
                    token => Err(format!("Unexpected token {:?}", token).into())
                }
            }
            Token::F => Ok(0xF029 + 0x100 * self.expect_register()?),
            Token::B => Ok(0xF033 + 0x100 * self.expect_register()?),
            Token::DT => Ok(0xF015 + 0x100 * self.expect_register()?),
            Token::ST => Ok(0xF018 + 0x100 * self.expect_register()?),
            Token::LBRACKET => {
                self.expect_token(Token::INDEX)?;
                self.expect_token(Token::RBRACKET)?;
                Ok(0xF055 + 0x100 * self.expect_register()?)
            }
            Token::INDEX => Ok(0xA000 + self.expect_addr()?),
            token => Err(format!("Unexpected token {:?}", token).into()),
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

    #[test]
    fn test_ins_se() {
        let parser = prepare_parser("se v1, #12\n se v2, v3");
        assert_eq!(parser.parse().unwrap(), vec![0x3112, 0x5230]);
    }
}
