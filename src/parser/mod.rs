mod expressions;

use crate::ast::node::{NodeToken, Statement};
use crate::lexer::Lexer;
use crate::token::TokenType;
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
            TokenType::For => self.parse_for_statement(),
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

    fn parse_for_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        self.expect_current(&TokenType::For)?;
        let for_token = self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        let block = self.parse_block_statement()?;
        let for_stmt = Statement::While(Box::new(condition), block);
        self.skip_semicolons();
        Ok(NodeToken::new(for_stmt, for_token))
    }

    fn parse_return_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let token = self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;
        self.skip_semicolons();
        Ok(NodeToken::new(Statement::Return(Box::new(exp)), token))
    }

    // let_stm ::= 'let' <identifier> ':' <identifier> '=' <expression> [';']*
    fn parse_let_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let token = self.next_token();
        self.expect_peek(&TokenType::DoubleDot)?;
        let name = self.next_token();
        self.next_token();
        self.expect_peek(&TokenType::Assign)?;
        let type_token = self.next_token();
        let type_string = if let TokenType::Ident(s) = type_token {
            s.as_ref().to_string()
        } else {
            return Err("expected an identifier on let statement".to_string());
        };
        if let TokenType::Ident(name_s) = name {
            self.next_token();
            let expr = self.parse_expression(Precedence::Lowest)?;
            while self.is_current(&TokenType::Semicolon) {
                self.next_token();
            }
            Ok(NodeToken::new(
                Statement::Var(name_s, Box::new(expr), type_string),
                token,
            ))
        } else {
            Err("expected identifier".to_string())
        }
    }

    fn skip_semicolons(&mut self) {
        while self.is_current(&TokenType::Semicolon) {
            self.next_token();
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
    #![allow(unused_imports)]
    #![allow(dead_code)]
    use crate::ast::node::{Expression, NodeToken, Statement, Str};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::TokenType;
    use core::panic;

    fn get_program(input: &str) -> Vec<NodeToken<Statement>> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = if let Statement::Program(stmts) = parser.parse_program().unwrap().node {
            stmts
        } else {
            panic!("not a program")
        };
        program
    }

    #[test]
    fn test_parse_for_loops() {
        let input = [
            (
                "for true { x + y; y + z; if x == y { 1 } };",
                "for true { (x + y) (y + z) if (x == y) { 1 }}",
            ),
            (
                "for true { for true+false > 0 { x + y; y + z; if x == y { 1 } } };",
                "for true { for ((true + false) > 0) { (x + y) (y + z) if (x == y) { 1 }}}",
            ),
        ];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(program.len(), 1);
            assert_eq!(program[0].str(), test.1);
        }
    }

    #[test]
    fn test_let_stmt() {
        let input = "let x: string = 5;;;;;;
        let y: int = 10;;;;;;
        let foobar: thing = 500343;;;;;";
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
            let (identifier_expr, _) = if let Statement::Var(identifier, value, _) = &stmt.node {
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
        for (i, _) in tests.iter().enumerate() {
            let stmt = &program[i];
            if stmt.token.to_string() != "return" {
                panic!("got: {} instead of return", stmt.token);
            }
            let _ = if let Statement::Return(identifier) = &stmt.node {
                identifier
            } else {
                panic!("not a return statement");
            };
        }
    }
}
