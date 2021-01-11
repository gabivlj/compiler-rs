#![allow(dead_code)]
use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    string: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    /// returns the current character that is peeking
    fn char(&mut self) -> Option<char> {
        self.input.peek().map(|char| *char)
    }

    fn next(&mut self) {
        self.input.next();
        self.position += 1;
    }

    pub fn new(str: &'a str) -> Self {
        let iter = str.chars().peekable();
        Self {
            input: iter,
            string: str,
            position: 0,
        }
    }

    /// looksup in the identifier table if the string being passed is a string
    fn lookup_ident(&self, string: &str) -> TokenType {
        match string {
            "let" => TokenType::Let,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "true" => TokenType::True,
            "false" => TokenType::False,
            "return" => TokenType::Return,
            "else" => TokenType::Else,
            _ => TokenType::Ident(string.to_string()),
        }
    }

    /// skips all the possible whitespace
    fn skip_whitespace(&mut self) {
        while let Some(char) = self.char() {
            if char.is_whitespace() {
                self.next();
                continue;
            }
            break;
        }
    }

    /// read_identifier reads an entire string, and returns the string with the starting_char.
    /// `starting_char` parameter is really useful because we can't go back in the iterator
    fn read_identifier(&mut self) -> String {
        let start = self.position - 1;
        while let Some(char) = self.char() {
            if !char.is_ascii_alphanumeric() {
                break;
            }
            self.next();
        }
        self.string[start..self.position].to_string()
    }

    fn peek_possible_two_len(&mut self, expected: char) -> Option<char> {
        if let Some(c) = self.char() {
            if c == expected {
                Some(expected)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> TokenType {
        self.skip_whitespace();
        let c = self.char();
        // go to the next character so we can peek in internal logic
        self.next();
        match c {
            // Single tokens
            Some(';') => TokenType::Semicolon,
            Some('(') => TokenType::LParen,
            Some(')') => TokenType::RParen,
            Some('+') => TokenType::Plus,
            Some('{') => TokenType::LBrace,
            Some('}') => TokenType::RBrace,
            Some(',') => TokenType::Comma,
            Some('-') => TokenType::Minus,
            Some('>') => TokenType::GreaterThan,
            Some('<') => TokenType::LessThan,
            Some('/') => TokenType::Slash,
            Some('*') => TokenType::Asterisk,
            // Possible single tokens or with more combinations
            Some('=') => match self.peek_possible_two_len('=') {
                Some(_) => {
                    self.next();
                    TokenType::Equal
                }
                None => TokenType::Assign,
            },
            Some('!') => match self.peek_possible_two_len('=') {
                Some(_) => {
                    self.next();
                    TokenType::NotEqual
                }
                None => TokenType::Bang,
            },
            // Definitely an EOF
            None => TokenType::EOF,
            // check for an identifier
            Some(c) => {
                // read entire identifier string with our unknown starting char to be included
                let string = self.read_identifier();
                // check if it's an integer
                let ident = match c {
                    '0'..='9' => {
                        // validate integer
                        let valid_integer = string.parse::<u64>();
                        if valid_integer.is_err() {
                            return TokenType::Illegal(string);
                        }
                        TokenType::Int(valid_integer.unwrap())
                    }
                    // lookup identifier type
                    _ => self.lookup_ident(&string),
                };
                ident
            }
        }
    }
}

mod test {
    #![allow(dead_code)]
    #![allow(unused_imports)]
    use crate::lexer::Lexer;
    use crate::token::{Token, TokenType};
    #[test]
    fn check_next_token() {
        let tests = vec![
            (TokenType::Assign, "="),
            (TokenType::Plus, "+"),
            (TokenType::LParen, "("),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::LBrace, "{"),
            (TokenType::Comma, ","),
            (TokenType::Let, "let"),
        ];
        let input = tests.iter().fold(String::from(""), |string, (_, char)| {
            format!("{}{}", string, char)
        });
        let mut lexer = Lexer::new(&input);
        for (ii, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            if tok != test.0 {
                panic!(
                    "tests[{}] - tokentype wrong. expected={:?}, got: {:?}",
                    ii, tok, test.0
                );
            }
        }
    }

    #[test]
    fn test_raw_text() {
        let input = "let five = 5;
        let ten = 10      ;
           let add = fn    (x, y)     {
             x +              y;
        };
        let result = add
        
        (five,              ten
        
        
    );
    !-/*5;
    5 < 10 > 5;
    if (5 < 10) {
        return true;
    } else {
        return false;
    }
    5 != 10
    5 == 10
    ";

        let tests = vec![
            (TokenType::Let, "let"),
            (TokenType::Ident("five".to_string()), "five"),
            (TokenType::Assign, "="),
            (TokenType::Int(5), "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident("ten".to_string()), "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int(10), "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident("add".to_string()), "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident("x".to_string()), "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident("y".to_string()), "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident("x".to_string()), "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident("y".to_string()), "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident("result".to_string()), "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident("add".to_string()), "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident("five".to_string()), "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident("ten".to_string()), "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int(5), "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int(5), "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int(10), "10"),
            (TokenType::GreaterThan, ">"),
            (TokenType::Int(5), "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int(5), "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int(10), "10"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::True, "true"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Else, "else"),
            (TokenType::LBrace, "{"),
            (TokenType::Return, "return"),
            (TokenType::False, "false"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Int(5), "5"),
            (TokenType::NotEqual, "!="),
            (TokenType::Int(10), "10"),
            (TokenType::Int(5), "5"),
            (TokenType::Equal, "=="),
            (TokenType::Int(10), "10"),
            (TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new(&input);
        for (ii, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            if tok != test.0 {
                panic!(
                    "tests[{}] - tokentype `{:?}` wrong. expected={:?}",
                    ii, tok, test.0
                );
            }
            // if tok.string != test.1 {
            //     panic!(
            //         "tests[{}] - token literal wrong. expected={:?}, got: {:?}",
            //         ii, test.1, tok.string,
            //     );
            // }
        }
    }
}
