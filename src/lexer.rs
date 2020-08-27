use crate::token::*;
use std::str::Chars;
use std::borrow::Cow;

type LResult<T> = Result<T, Cow<'static, str>>;

pub struct Lexer<'a> {
    text: &'a str,
    next_it: Chars<'a>,
    cur_pos: usize,
    cur_char: Option<char>,
    instructions: Instructions,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        let mut next_it = text.chars();
        next_it.next();
        Self {
            text,
            next_it,
            cur_pos: 0,
            cur_char: text.chars().next(),
            instructions: Instructions::new(),
        }
    }

    pub fn next_token(&mut self) -> LResult<Token<'a>> {
        self.eat_whitespace();
        let token = match self.cur_char {
            Some(',') => Token::COMMA,
            Some('-') => Token::MINUS,
            Some('\n') => Token::NEWLINE,
            Some('[') => Token::LBRACKET,
            Some(']') => Token::RBRACKET,
            Some('$') => return Ok(Token::NIBBLE(self.read_hex(0xF)?)),
            Some('#') => return Ok(Token::BYTE(self.read_hex(0xFF)?)),
            Some('@') => return Ok(Token::ADDR(self.read_hex(0xFFF)?)),
            Some(ch) => {
                if ch.is_ascii_alphabetic() {
                    let key = self.read_key();
                    return Ok(Token::from(&self.instructions, key)?);
                } else {
                    Token::UNKNOWN(ch)
                }
            }
            None => Token::EOF,
        };
        let _ = self.read_char();
        Ok(token)
    }
}

impl<'a> Lexer<'a> {
    fn read_key(&mut self) -> &'a str {
        let cur_pos = self.cur_pos;
        while let Some(ch) = self.cur_char {
            if !ch.is_ascii_alphanumeric() {
                break;
            }
            let _ = self.read_char();
        }
        &self.text[cur_pos..self.cur_pos]
    }

    fn read_hex(&mut self, max: u16) -> LResult<u16> {
        let _ = self.read_char();
        let cur_pos = self.cur_pos;
        while let Some(ch) = self.cur_char {
            if !ch.is_ascii_hexdigit() {
                break;
            }
            let _ = self.read_char();
        }
        let hex_str = &self.text[cur_pos..self.cur_pos];
        let hex = u16::from_str_radix(hex_str, 16)
            .map_err(|_| format!("Invalid hex value {}", hex_str))?;
        if hex > max {
            return Err(format!("Hex value should not be greater than {}", max).into());
        }
        Ok(hex)
    }

    fn eat_whitespace(&mut self) {
        while let Some(ch) = self.cur_char {
            if ch != '\t' && ch != '\r' && ch != '\x0C' && ch != ' ' {
                break;
            }
            let _ = self.read_char();
        }
    }

    fn read_char(&mut self) -> Option<char> {
        self.cur_pos += 1;
        self.cur_char = self.next_it.next();
        self.cur_char
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn correct_syntax() {
        let program = "
            cls
            ret
            sys @0ff
            se va, #12
            drw v1, v2, $b";

        let tokens = [
            Token::NEWLINE,
            Token::INSTRUCTION("cls"),
            Token::NEWLINE,
            Token::INSTRUCTION("ret"),
            Token::NEWLINE,
            Token::INSTRUCTION("sys"),
            Token::ADDR(0xff),
            Token::NEWLINE,
            Token::INSTRUCTION("se"),
            Token::REGISTER(0xa),
            Token::COMMA,
            Token::BYTE(0x12),
            Token::NEWLINE,
            Token::INSTRUCTION("drw"),
            Token::REGISTER(0x1),
            Token::COMMA,
            Token::REGISTER(0x2),
            Token::COMMA,
            Token::NIBBLE(0xb),
        ];

        let mut lexer = Lexer::new(program);
        let mut token_iter = tokens.iter();
        loop {
            let token = lexer.next_token();
            assert!(token.is_ok());
            let token = token.unwrap();
            if token == Token::EOF {
                assert_eq!(token_iter.next(), None);
                break;
            }
            assert_eq!(token, *token_iter.next().unwrap());
        }
    }
}
