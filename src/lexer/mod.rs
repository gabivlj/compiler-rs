#![allow(dead_code)]
use crate::token::{Token, TokenType};
use std::iter::Peekable;
use std::str::Chars;

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    /// returns the current character that is peeking
    fn char(&mut self) -> Option<char> {
        self.input.peek().map(|char| *char)
    }

    pub fn new(str: &'a str) -> Self {
        Self {
            input: str.chars().peekable(),
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
            _ => TokenType::Ident,
        }
    }

    /// skips all the possible whitespace
    fn skip_whitespace(&mut self) {
        while let Some(char) = self.char() {
            if char.is_whitespace() {
                self.input.next();
                continue;
            }
            break;
        }
    }

    /// read_identifier reads an entire string, and returns the string with the starting_char.
    /// `starting_char` parameter is really useful because we can't go back in the iterator
    fn read_identifier(&mut self, starting_char: char) -> String {
        let mut string = String::with_capacity(10);
        string.push(starting_char);
        while let Some(char) = self.char() {
            if !char.is_ascii_alphanumeric() {
                break;
            }
            string.push(char);
            self.input.next();
        }
        string
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let c = self.char();
        // go to the next character so we can peek in internal logic
        self.input.next();
        match c {
            // Single tokens
            Some(';') => Token::new(";", TokenType::Semicolon),
            Some('(') => Token::new("(", TokenType::LParen),
            Some(')') => Token::new(")", TokenType::RParen),
            Some('+') => Token::new("+", TokenType::Plus),
            Some('{') => Token::new("{", TokenType::LBrace),
            Some('}') => Token::new("}", TokenType::RBrace),
            Some(',') => Token::new(",", TokenType::Comma),
            Some('-') => Token::new("-", TokenType::Minus),
            Some('>') => Token::new(">", TokenType::GreaterThan),
            Some('<') => Token::new("<", TokenType::LessThan),
            Some('/') => Token::new("/", TokenType::Slash),
            Some('*') => Token::new("*", TokenType::Asterisk),
            // Possible single tokens or with more combinations
            Some('=') => match self.peek_possible_two_len('=') {
                Some(_) => {
                    self.input.next();
                    Token::new("==", TokenType::Equal)
                }
                None => Token::new("=", TokenType::Assign),
            },
            Some('!') => match self.peek_possible_two_len('=') {
                Some(_) => {
                    self.input.next();
                    Token::new("!=", TokenType::NotEqual)
                }
                None => Token::new("!", TokenType::Bang),
            },
            // Definitely an EOF
            None => Token::new("", TokenType::EOF),
            // check for an identifier
            Some(c) => {
                // read entire identifier string with our unknown starting char to be included
                let string = self.read_identifier(c);
                // check if it's an integer
                let ident = match c {
                    '0'..='9' => {
                        // validate integer
                        let valid_integer = string
                            .chars()
                            .filter(|x| '0' > *x || *x > '9')
                            .next()
                            .is_none();
                        if !valid_integer {
                            return Token::new(&string, TokenType::Illegal);
                        }
                        TokenType::Int
                    }
                    // lookup identifier type
                    _ => self.lookup_ident(&string),
                };
                Token::new(&string, ident)
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
            if tok.token_type != test.0 {
                panic!(
                    "tests[{}] - tokentype wrong. expected={:?}, got: {:?}",
                    ii, tok.token_type, test.0
                );
            }
            if tok.string != test.1 {
                panic!(
                    "tests[{}] - token literal wrong. expected={:?}, got: {:?}",
                    ii, tok.string, test.1
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
            (TokenType::Ident, "five"),
            (TokenType::Assign, "="),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "ten"),
            (TokenType::Assign, "="),
            (TokenType::Int, "10"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "add"),
            (TokenType::Assign, "="),
            (TokenType::Function, "fn"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "x"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "y"),
            (TokenType::RParen, ")"),
            (TokenType::LBrace, "{"),
            (TokenType::Ident, "x"),
            (TokenType::Plus, "+"),
            (TokenType::Ident, "y"),
            (TokenType::Semicolon, ";"),
            (TokenType::RBrace, "}"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Ident, "result"),
            (TokenType::Assign, "="),
            (TokenType::Ident, "add"),
            (TokenType::LParen, "("),
            (TokenType::Ident, "five"),
            (TokenType::Comma, ","),
            (TokenType::Ident, "ten"),
            (TokenType::RParen, ")"),
            (TokenType::Semicolon, ";"),
            (TokenType::Bang, "!"),
            (TokenType::Minus, "-"),
            (TokenType::Slash, "/"),
            (TokenType::Asterisk, "*"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::Int, "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int, "10"),
            (TokenType::GreaterThan, ">"),
            (TokenType::Int, "5"),
            (TokenType::Semicolon, ";"),
            (TokenType::If, "if"),
            (TokenType::LParen, "("),
            (TokenType::Int, "5"),
            (TokenType::LessThan, "<"),
            (TokenType::Int, "10"),
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
            (TokenType::Int, "5"),
            (TokenType::NotEqual, "!="),
            (TokenType::Int, "10"),
            (TokenType::Int, "5"),
            (TokenType::Equal, "=="),
            (TokenType::Int, "10"),
            (TokenType::EOF, ""),
        ];
        let mut lexer = Lexer::new(&input);
        for (ii, test) in tests.iter().enumerate() {
            let tok = lexer.next_token();
            if tok.token_type != test.0 {
                panic!(
                    "tests[{}] - tokentype `{}` wrong. expected={:?}, got: {:?}",
                    ii, tok.string, test.0, tok.token_type
                );
            }
            if tok.string != test.1 {
                panic!(
                    "tests[{}] - token literal wrong. expected={:?}, got: {:?}",
                    ii, test.1, tok.string,
                );
            }
        }
    }
}
