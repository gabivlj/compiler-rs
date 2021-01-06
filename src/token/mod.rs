#[derive(Debug, PartialEq, Copy, Clone)]
pub enum TokenType {
    NotEqual,
    Equal,
    Illegal,
    EOF,
    Ident,
    Assign,
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    Int,
    Bang,
    Asterisk,
    Slash,
    GreaterThan,
    LessThan,
    Minus,
    If,
    Else,
    Return,
    True,
    False,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub string: String,
}

impl Token {
    pub fn empty() -> Self {
        Self {
            token_type: TokenType::EOF,
            string: String::with_capacity(0),
        }
    }

    pub fn copy(&self) -> Self {
        Self {
            string: self.string.clone(),
            token_type: self.token_type,
        }
    }

    pub fn new<S: Into<String>>(string: S, token: TokenType) -> Self {
        Self {
            string: string.into(),
            token_type: token,
        }
    }
}

mod test {
    #![allow(unused_imports)]
    use super::{Token, TokenType};

    #[test]
    fn check_lifetimes() {
        let token = Token::new("+", TokenType::Plus);
        let heap_all = String::from("heap allocated as well");
        let other_token = Token::new(&heap_all, TokenType::Ident);
        let token_copied = token.copy();
        let other_token_copied = other_token.copy();
        assert_eq!(token, token_copied);
        assert_eq!(other_token, other_token_copied);
    }
}
