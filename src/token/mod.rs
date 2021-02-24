use std::borrow::Cow;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    NotEqual,
    Equal,
    Illegal(String),
    EOF,
    Ident(Cow<'static, str>),
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
    Int(u64),
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
    For,
    Struct,
    Type,
    DoubleDot,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub string: String,
}

impl std::fmt::Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenType::LParen => write!(f, "("),
            TokenType::RParen => write!(f, ")"),
            TokenType::Ident(s) => write!(f, "{}", s),
            TokenType::Illegal(s) => write!(f, "{}", s),
            TokenType::Assign => write!(f, "="),
            TokenType::Plus => write!(f, "+"),
            TokenType::LBrace => write!(f, "{{"),
            TokenType::RBrace => write!(f, "}}"),
            TokenType::Equal => write!(f, "=="),
            TokenType::NotEqual => write!(f, "!="),
            TokenType::Bang => write!(f, "!"),
            TokenType::Asterisk => write!(f, "*"),
            TokenType::Comma => write!(f, ","),
            TokenType::Slash => write!(f, "/"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::DoubleDot => write!(f, ":"),
            TokenType::Else => write!(f, "else"),
            TokenType::If => write!(f, "if"),
            TokenType::True => write!(f, "true"),
            TokenType::False => write!(f, "false"),
            TokenType::Minus => write!(f, "-"),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::LessThan => write!(f, "<"),
            TokenType::EOF => write!(f, ""),
            TokenType::Return => write!(f, "return"),
            TokenType::Let => write!(f, "let"),
            TokenType::Function => write!(f, "fn"),
            TokenType::For => write!(f, "for"),
            TokenType::Int(u64) => write!(f, "{}", u64),
            TokenType::Struct => write!(f, "struct"),
            TokenType::Type => write!(f, "type"),
        }
    }
}

mod test {
    #![allow(unused_imports)]
    use super::{Token, TokenType};
}
