use std::unimplemented;

use crate::ast::node::{Expression, NodeToken, Statement};
use crate::parser::Parser;
use crate::token::{Token, TokenType};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Equals = 1,
    LessGreater = 2,
    Sum = 3,
    Product = 4,
    Prefix = 5,
    Call = 6,
}

impl<'a> Parser<'a> {
    pub fn parse_expression_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let node = NodeToken::new(
            Statement::ExpressionStatement(Box::new(expression)),
            // Empty because we wrap the true token inside
            Token::empty(),
        );
        if self.is_current(TokenType::Semicolon) {
            self.next_token();
        }
        Ok(node)
    }

    fn parse_expression(&mut self, prec: Precedence) -> Result<NodeToken<Expression>, String> {
        let left_expr = self.prefix_expression()?;
        Ok(left_expr)
    }

    fn prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        match self.current_token.token_type {
            TokenType::Ident => Ok(NodeToken::new(
                Expression::Id(self.current_token.string.clone()),
                self.next_token(),
            )),
            TokenType::Int => Ok(NodeToken::new(
                Expression::Number(
                    self.current_token
                        .string
                        .parse::<u64>()
                        .expect("expected a good number from the lexer"),
                ),
                self.next_token(),
            )),
            TokenType::Bang => self.parse_prefix_expression(),
            TokenType::Minus => self.parse_prefix_expression(),
            _ => Err(format!(
                "can't parse unknown prefix: {:?}",
                self.current_token
            )),
        }
    }

    fn parse_prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        let token = self.next_token();
        let op = token.string.clone();
        let right = self.parse_expression(Precedence::Prefix)?;
        let expr = NodeToken::new(Expression::PrefixOp(op, Box::new(right)), token);
        Ok(expr)
    }
}

mod test {
    use crate::ast::node::{Expression, NodeToken, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

    #[test]
    fn test_id() {
        let input = "variable";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = if let Statement::Program(stmts) = parser.parse_program().unwrap().node {
            stmts
        } else {
            panic!("not a program")
        };
        assert_eq!(program.len(), 1);
        let stmt =
            if let Statement::ExpressionStatement(variable) = program.pop().expect("len=1").node {
                variable
            } else {
                panic!("not an expression statement")
            };
        let (id, token) = if let Expression::Id(identifier) = stmt.node {
            (identifier, stmt.token)
        } else {
            panic!("not an identifier");
        };
        assert_eq!(&id, "variable");
        assert_eq!(token.string, "variable");
    }

    #[test]
    fn test_int() {
        let input = "5;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let mut program = if let Statement::Program(stmts) = parser.parse_program().unwrap().node {
            stmts
        } else {
            panic!("not a program")
        };
        assert_eq!(program.len(), 1);
        let stmt =
            if let Statement::ExpressionStatement(variable) = program.pop().expect("len=1").node {
                variable
            } else {
                panic!("not an expression statement")
            };
        let (id, token) = if let Expression::Number(identifier) = stmt.node {
            (identifier, stmt.token)
        } else {
            panic!("not an identifier");
        };
        assert_eq!(id, 5);
        assert_eq!(token.string, "5");
    }

    #[test]
    fn parse_prefix_expr() {
        let tests = [("!5;", "!", 5), ("-15;", "-", 15)];
        for test in tests.iter() {
            let (input, op, value_test) = test;
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let mut program =
                if let Statement::Program(stmts) = parser.parse_program().unwrap().node {
                    stmts
                } else {
                    panic!("not a program")
                };
            assert_eq!(program.len(), 1);
            let stmt = if let Statement::ExpressionStatement(variable) =
                program.pop().expect("len=1").node
            {
                variable
            } else {
                panic!("not an expression statement")
            };
            let (id, expr, token) = if let Expression::PrefixOp(op, expr) = stmt.node {
                (op, expr, stmt.token)
            } else {
                panic!("not a prefix op");
            };
            assert_eq!(&id, op);
            assert_eq!(&token.string, op);
            let value = if let Expression::Number(value) = expr.node {
                value
            } else {
                panic!("not a prefix op");
            };
            assert_eq!(value, *value_test);
        }
    }
}
