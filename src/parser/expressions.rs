use std::unimplemented;

use crate::ast::node::{Expression, NodeToken, OpType, Statement, Str};
use crate::parser::Parser;
use crate::token::{Token, TokenType};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
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

    /// parse expression with pratt's
    pub fn parse_expression(&mut self, prec: Precedence) -> Result<NodeToken<Expression>, String> {
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

    fn parse_identifier(&mut self) -> Result<NodeToken<Expression>, String> {
        self.expect_current(TokenType::Ident)?;
        Ok(NodeToken::new(
            Expression::Id(self.current_token.string.clone()),
            self.next_token(),
        ))
    }

    /// parses a prefix expression depending on the current token type
    fn prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        match self.current_token.token_type {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => Ok(NodeToken::new(
                Expression::Number(
                    self.current_token
                        .string
                        .parse::<i64>()
                        .expect("expected a good number from the lexer"),
                ),
                self.next_token(),
            )),
            TokenType::True => Ok(NodeToken::new(Expression::Boolean(true), self.next_token())),
            TokenType::False => Ok(NodeToken::new(
                Expression::Boolean(false),
                self.next_token(),
            )),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if(),
            TokenType::Minus | TokenType::Bang | TokenType::Plus => self.parse_prefix_expression(),
            TokenType::Function => self.parse_function_expression(),
            _ => Err(format!(
                "can't parse unknown prefix: {:?}",
                self.current_token
            )),
        }
    }

    fn parse_fn_call(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        let left_boxed = Box::new(left);
        let call_token = self.next_token();
        let params = self.parse_parameters()?;
        let fn_call = Expression::Call(left_boxed, params);
        Ok(NodeToken::new(fn_call, call_token))
    }

    fn parse_parameters(&mut self) -> Result<Vec<NodeToken<Expression>>, String> {
        let mut params = vec![];
        while !self.is_current(TokenType::RParen) && !self.is_current(TokenType::EOF) {
            let exp = self.parse_expression(Precedence::Lowest)?;
            // println!("{}", exp.str());
            params.push(exp);
            if self.is_current(TokenType::Comma) && !self.is_peek(TokenType::Comma) {
                self.next_token();
            } else if self.is_current(TokenType::Comma) {
                return Err("unexpected comma".to_string());
            }
        }
        self.expect_current(TokenType::RParen)?;
        self.next_token();
        Ok(params)
    }

    fn parse_function_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        assert_eq!(self.current_token.token_type, TokenType::Function);
        let function_token = self.next_token();
        let parameters = self.parse_fn_def_parameters()?;
        let block = self.parse_block_statement()?;
        let func = Expression::FunctionDefinition(parameters, block);
        Ok(NodeToken::new(func, function_token))
    }

    fn parse_fn_def_parameters(&mut self) -> Result<Vec<NodeToken<Expression>>, String> {
        self.expect_current(TokenType::LParen)?;
        self.next_token();
        let mut params = vec![];
        while !self.is_current(TokenType::RParen) && !self.is_current(TokenType::EOF) {
            let identifier = self.parse_identifier()?;
            params.push(identifier);
            if self.is_current(TokenType::Comma) && self.is_peek(TokenType::Ident) {
                self.next_token();
            } else if self.is_current(TokenType::Comma) {
                return Err("unexpected comma".to_string());
            }
        }
        self.expect_current(TokenType::RParen)?;
        self.next_token();
        Ok(params)
    }

    /// get the left expression and with the current token return an infix expression
    /// if the result is an error, it will return the expression ownership to the caller,
    /// otherwise, it will return as normal the infix parse.
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
            TokenType::LParen => Ok(self.parse_fn_call(left)),
            _ => Err(left),
        }
    }

    /// parse binary operations like == or + or -
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

    fn parse_block_statement(&mut self) -> Result<Vec<NodeToken<Statement>>, String> {
        self.expect_current(TokenType::LBrace)?;
        self.next_token();
        let mut block = vec![];
        while !self.is_current(TokenType::RBrace) && !self.is_current(TokenType::EOF) {
            block.push(self.parse_expression_statement()?);
        }
        self.expect_current(TokenType::RBrace)?;
        self.next_token();
        Ok(block)
    }

    /// parse if statements
    fn parse_if(&mut self) -> Result<NodeToken<Expression>, String> {
        self.expect_current(TokenType::If)?;
        let if_token = self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        let block = self.parse_block_statement()?;
        let mut wasted_last_else = false;
        let mut ifs = vec![];
        let mut last_else = None;
        while self.is_current(TokenType::Else) {
            self.next_token();
            if self.is_current(TokenType::If) && !wasted_last_else {
                let curr_if = self.parse_if()?;
                ifs.push(curr_if);
            } else if !wasted_last_else {
                wasted_last_else = true;
                last_else = Some(self.parse_block_statement()?);
            } else {
                return Err(format!(
                    "Can't have multiple elses or adding a last else before an else condition"
                ));
            }
        }
        let expr = Expression::If {
            block,
            else_ifs: if ifs.len() == 0 { None } else { Some(ifs) },
            last_else,
            condition: Box::new(condition),
        };
        Ok(NodeToken::new(expr, if_token))
    }

    /// parse ! or - operators
    fn parse_prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        let token = self.next_token();
        let op = token.string.clone();
        let right = self.parse_expression(Precedence::Prefix)?;
        let expr = NodeToken::new(Expression::PrefixOp(op, Box::new(right)), token);
        Ok(expr)
    }

    /// Parse expressions inside parenthesis
    fn parse_grouped_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        assert_eq!(self.current_token.token_type, TokenType::LParen);
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest);
        self.expect_current(TokenType::RParen)?;
        self.next_token();
        exp
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
    use crate::ast::node::{Expression, NodeToken, OpType, Statement, Str};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::Token;

    #[test]
    fn test_parse_function() {
        let input = [
            (
                "fn(x, y) { x + y; y + z; if x == y { 1 } };",
                "fn (x, y) { (x + y);  (y + z);  if (x == y) { 1; }; }",
            ),
            (
                "fn(x, y) { x + y; y + z; if x == y { 1 } else if !!!!!!!---true { 5; } else { false; } };",
                "fn (x, y) { (x + y);  (y + z);  if (x == y) { 1; } else if (!(!(!(!(!(!(!(-(-(-true)))))))))) { 5; } else { false; }; }",
            ),
            (
                "fn(){}",
                "fn () {}"
            ),
            (
                "let f = fn(){}",
                "let f = fn () {};"
            )
        ];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(program.len(), 1);
            assert_eq!(test.1, program[0].str());
        }
    }

    #[test]
    fn test_if() {
        let input = [
            ("
            if (x == true) {
                1 + 2;
                if (x == false) {
                    3 + 3;                 
                }                
            } else if 1 + 3 { 
                3 
            } else if !!!!!---((((1+3)))) {
                1992192 + 33 * 3
            } else {
                3 + 5
            }
        ", "if (x == true) { (1 + 2); if (x == false) { (3 + 3); }; } else if (1 + 3) { 3; } else if (!(!(!(!(!(-(-(-(1 + 3))))))))) { (1992192 + (33 * 3)); } else { (3 + 5); }"),
        ("
            let ifs = if (x == true) {
                1 + 2;
                if (x == false) {
                    3 + 3;
                    3 * 3;
                    3 / 3;         
                } else {
                    3
                }   
            } else if 1 + 3 { 
                3 
            } else if !!!!!---((((1+3)))) {
                1992192 + 33 * 3
            } else {
                3 + 5
            }
        ", "let ifs = if (x == true) { (1 + 2); if (x == false) { (3 + 3); (3 * 3); (3 / 3); } else { 3; }; } else if (1 + 3) { 3; } else if (!(!(!(!(!(-(-(-(1 + 3))))))))) { (1992192 + (33 * 3)); } else { (3 + 5); };"),
        ];
        for test in input.iter() {
            let mut program = get_program(test.0);
            assert_eq!(program.len(), 1);
            assert_eq!(test.1, program[0].str());
        }
    }

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
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("false", "false"),
            ("!!!!false", "(!(!(!(!false))))"),
            ("3 > 5 == true", "((3 > 5) == true)"),
            ("true", "true"),
            ("!!!!true", "(!(!(!(!true))))"),
            ("(5 + 5) / 2", "((5 + 5) / 2)"),
            ("add(x, y)", "add(x, y)"),
            (
                "let added = add(x, y, fn(x,y,y){ 5; }, if x == y {},) + 10 / 30 * f()",
                "let added = (add(x, y, fn (x, y, y) { 5; }, if (x == y) { }) + ((10 / 30) * f()));",
            ),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for test in input.iter() {
            let program = get_program(test.0);
            let str = Statement::Program(program).string(&Token::empty());
            assert_eq!(&str, test.1);
        }
    }
    #[test]
    fn test_parse_boolean() {
        let input = [("true", true), ("false;", false)];
        for test in input.iter() {
            let mut program = get_program(test.0);
            assert_eq!(program.len(), 1);
            if let Statement::ExpressionStatement(val) = program.pop().unwrap().node {
                if let Expression::Boolean(value) = val.node {
                    assert_eq!(test.1, value);
                } else {
                    panic!("not a boolean expression")
                }
            } else {
                panic!("not an expression statement")
            }
        }
    }
}
