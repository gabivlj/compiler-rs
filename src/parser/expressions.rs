use std::unimplemented;

use crate::ast::node::{Expression, NodeToken, OpType, Statement};
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
        // Get left expression
        let mut left_expr = self.prefix_expression()?;
        // Iterate and recursive doing pratt
        while !self.is_current(TokenType::Semicolon) && prec < self.current_precedence() {
            // Get possible infix, if there is an error, just return itself (ownership)
            let possible_infix = self.infix_expression(left_expr);
            // it's not an infix, stop
            if possible_infix.is_err() {
                left_expr = possible_infix
                    .expect_err("we got an error, time to unwrap the value to get back ownership");
                return Ok(left_expr);
            }
            // check if infix is an error
            let infix = possible_infix.expect("already checked error")?;
            left_expr = infix;
        }
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
                        .parse::<i64>()
                        .expect("expected a good number from the lexer"),
                ),
                self.next_token(),
            )),
            TokenType::Minus | TokenType::Bang | TokenType::Plus => self.parse_prefix_expression(),
            _ => Err(format!(
                "can't parse unknown prefix: {:?}",
                self.current_token
            )),
        }
    }

    fn infix_expression(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<Result<NodeToken<Expression>, String>, NodeToken<Expression>> {
        match self.current_token.token_type {
            TokenType::Minus
            | TokenType::Slash
            | TokenType::GreaterThan
            | TokenType::LessThan
            | TokenType::Plus
            | TokenType::Asterisk
            | TokenType::Equal
            | TokenType::NotEqual => Ok(self.parse_infix_expression(left)),
            _ => Err(left),
        }
    }

    fn parse_infix_expression(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        let precedence = self.current_precedence();
        let token = self.next_token();
        let right = self.parse_expression(precedence)?;
        Ok(NodeToken::new(
            Expression::BinaryOp(
                Box::new(left),
                Box::new(right),
                OpType::from(token.string.as_ref()),
            ),
            token,
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        let token = self.next_token();
        let op = token.string.clone();
        let right = self.parse_expression(Precedence::Prefix)?;
        let expr = NodeToken::new(Expression::PrefixOp(op, Box::new(right)), token);
        Ok(expr)
    }

    fn get_precedence(token: &TokenType) -> Precedence {
        match token {
            TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
            TokenType::LessThan | TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Asterisk | TokenType::Slash => Precedence::Product,
            TokenType::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn peek_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.peek_token.token_type)
    }

    fn current_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.current_token.token_type)
    }
}

mod test {

    #![allow(unused_variables)]
    #![allow(dead_code)]
    #![allow(unused_imports)]
    use crate::ast::node::{Expression, NodeToken, OpType, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

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

    fn test_integer(expr: &NodeToken<Expression>, expected: i64) {
        let value = if let Expression::Number(value) = expr.node {
            value
        } else {
            panic!("not a number");
        };
        assert_eq!(&expected.to_string(), &expr.token.string);
        assert_eq!(value, expected);
    }

    #[test]
    fn test_id() {
        let input = "variable";

        let mut program = get_program(input);
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
        let mut program = get_program(input);
        assert_eq!(program.len(), 1);
        let stmt =
            if let Statement::ExpressionStatement(variable) = program.pop().expect("len=1").node {
                variable
            } else {
                panic!("not an expression statement")
            };
        test_integer(&stmt, 5);
    }

    #[test]
    fn test_prefix_expr() {
        let tests = [("!5;", "!", 5), ("-15;", "-", 15)];
        for test in tests.iter() {
            let (input, op, value_test) = test;
            let mut program = get_program(input);
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
            test_integer(&expr, *value_test);
        }
    }

    #[test]
    fn test_infix_expressions() {
        struct InfixTest {
            input: &'static str,
            values: (i64, i64),
            operator: &'static str,
        }
        let tests = [
            InfixTest {
                input: "5 + 5",
                values: (5, 5),
                operator: "+",
            },
            InfixTest {
                input: "5 - 5",
                values: (5, 5),
                operator: "-",
            },
            InfixTest {
                input: "5 > 5",
                values: (5, 5),
                operator: ">",
            },
            InfixTest {
                input: "5 != 5",
                values: (5, 5),
                operator: "!=",
            },
            InfixTest {
                input: "5 < 5",
                values: (5, 5),
                operator: "<",
            },
            InfixTest {
                input: "5 == 5",
                values: (5, 5),
                operator: "==",
            },
        ];
        for test in tests.iter() {
            let mut program = get_program(test.input);
            assert_eq!(program.len(), 1);
            let stmt = if let Statement::ExpressionStatement(variable) =
                program.pop().expect("len=1").node
            {
                variable
            } else {
                panic!("not an expression statement")
            };
            let (left, right, op) = if let Expression::BinaryOp(left, right, op) = stmt.node {
                (left, right, op)
            } else {
                panic!("not a binary operation")
            };
            test_integer(&left, test.values.0);
            test_integer(&right, test.values.1);
            assert_eq!(op.to_string(), test.operator);
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let input = [
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a+b*c+d/e-f", "(((a + (b * c)) + (d / e)) - f)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for test in input.iter() {
            let mut program = get_program(test.0);
            let str = Statement::Program(program).string(&Token::empty());
            assert_eq!(&str, test.1);
        }
    }
}
