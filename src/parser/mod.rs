mod expressions;

use std::unimplemented;

use crate::ast::node::{Expression, Node, NodeToken, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use expressions::Precedence;

pub struct Parser<'a> {
    current_token: TokenType,
    lexer: Lexer<'a>,
    peek_token: TokenType,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            current_token: TokenType::EOF,
            peek_token: TokenType::EOF,
        };
        p.next_token();
        p.next_token();
        p
    }

    fn next_token(&mut self) -> TokenType {
        std::mem::swap(&mut self.current_token, &mut self.peek_token);
        std::mem::replace(&mut self.peek_token, self.lexer.next_token())
    }

    pub fn parse_program(&mut self) -> Result<NodeToken<Statement>, String> {
        let mut statements = Vec::new();
        while self.current_token != TokenType::EOF {
            let stmt = self.parse_stmt();
            if let Ok(s) = stmt {
                statements.push(s);
            } else {
                // We map to get the type that we want
                return stmt.map(|_| NodeToken::new(Statement::Empty, TokenType::EOF));
            }
        }
        Ok(NodeToken::new(
            Statement::Program(statements),
            TokenType::EOF,
        ))
    }

    fn parse_stmt(&mut self) -> Result<NodeToken<Statement>, String> {
        match self.current_token {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Ident(_) => {
                if self.is_peek(&TokenType::Assign) {
                    Err("unimplemented".to_string())
                } else {
                    self.parse_expression_statement()
                }
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let token = self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;
        while self.is_current(&TokenType::Semicolon) {
            self.next_token();
        }
        Ok(NodeToken::new(Statement::Return(Box::new(exp)), token))
    }

    fn parse_let_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let token = self.next_token();
        self.expect_peek(&TokenType::Assign)?;
        let name = self.next_token();
        if let TokenType::Ident(name_s) = name {
            self.next_token();
            let expr = self.parse_expression(Precedence::Lowest)?;
            while self.is_current(&TokenType::Semicolon) {
                self.next_token();
            }
            Ok(NodeToken::new(
                Statement::Var(name_s, Box::new(expr)),
                token,
            ))
        } else {
            Err("expected identifier".to_string())
        }
    }
}

impl<'a> Parser<'a> {
    fn is_peek(&self, token_type: &TokenType) -> bool {
        &self.peek_token == token_type
    }

    fn expect_peek(&self, token_type: &TokenType) -> Result<(), String> {
        if !self.is_peek(token_type) {
            Err(format!(
                "expected token type: {:?} got={:?}",
                token_type, self.current_token
            ))
        } else {
            Ok(())
        }
    }

    fn is_current(&self, token_type: &TokenType) -> bool {
        &self.current_token == token_type
    }

    fn expect_current(&self, token_type: &TokenType) -> Result<(), String> {
        if !self.is_current(&token_type) {
            Err(format!(
                "expected: {:?} got={:?}",
                token_type, self.current_token
            ))
        } else {
            Ok(())
        }
    }
}

mod test {
    use core::panic;

    use crate::ast::node::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::TokenType;
    #[test]
    fn test_let_stmt() {
        let input = "let x = 5;;;;;;
        let y = 10;;;;;;
        let foobar = 500343;;;;;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let program = program.expect("program to be parsed correctly");
        let program = if let Statement::Program(stmts) = program.node {
            stmts
        } else {
            panic!("not a program");
        };
        if program.len() != 3 {
            println!("{:?}", program);
            panic!("number of statements is {} instead of 3", program.len());
        }
        println!("{:?}", program);
        // Expected identifiers, later on we will add expected value as pairs
        let tests = vec!["x", "y", "foobar"];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            if stmt.token != TokenType::Let {
                panic!("got: {} instead of let", stmt.token);
            }
            let (identifier_expr, _) = if let Statement::Var(identifier, value) = &stmt.node {
                (identifier, value)
            } else {
                panic!("not a let statement");
            };
            assert_eq!(&identifier_expr, test);
        }
    }

    #[test]
    fn test_return_stmt() {
        let input = "return 5;
        return 10;
        return 500343;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser
            .parse_program()
            .expect("program to be parsed correctly");
        let program = if let Statement::Program(stmts) = program.node {
            stmts
        } else {
            panic!("not a program");
        };
        if program.len() != 3 {
            panic!("number of statements is {} instead of 3", program.len());
        }
        println!("{:?}", program);
        // Expected identifiers, later on we will add expected value as pairs
        let tests = vec!["x", "y", "foobar"];
        for (i, test) in tests.iter().enumerate() {
            let stmt = &program[i];
            if stmt.token.to_string() != "return" {
                panic!("got: {} instead of return", stmt.token);
            }
            let return_expr = if let Statement::Return(identifier) = &stmt.node {
                identifier
            } else {
                panic!("not a return statement");
            };
        }
    }
}
