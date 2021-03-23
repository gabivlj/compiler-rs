mod expressions;

use crate::ast::node::{NodeToken, Statement, TypeExpr};
use crate::lexer::Lexer;
use crate::string_interning::StringId;
use crate::token::TokenType;
use expressions::Precedence;

pub struct Parser<'a> {
    current_token: TokenType,
    lexer: Lexer<'a>,
    peek_token: TokenType,
    can_assign: bool,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let mut p = Self {
            lexer,
            current_token: TokenType::EOF,
            peek_token: TokenType::EOF,
            can_assign: true,
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

    // pairs ::= [<identifier> ':' <type> [';']+ ]+
    // consumes the last brace
    fn parse_record_pairs(&mut self) -> Result<Vec<(StringId, TypeExpr)>, String> {
        let mut pairs = Vec::new();
        while !self.is_current(&TokenType::RBrace) && !self.is_current(&TokenType::EOF) {
            let identifier = self.next_token();
            self.expect_current(&TokenType::DoubleDot)?;
            self.next_token();
            let t = self.parse_type_notation()?;
            if let TokenType::Ident(s) = identifier {
                pairs.push((s, t));
                self.expect_current(&TokenType::Semicolon)?;
                self.skip_semicolons();
            } else {
                return Err(format!("expected identifier, got a {}", identifier));
            }

            // if let TokenType::Ident(identifier_string_left) = &identifier {
            //     if let TokenType::Ident(identifier_string_right) = &type_identifier {
            //         pairs.push((
            //             identifier_string_left.to_string(),
            //             identifier_string_right.to_string(),
            //         ));
            //         continue;
            //     }
            // }
            // return Err("expected a identifier on type declaration".to_string());
        }
        if pairs.len() <= 0 {
            return Err("needs at least minimum 1 pair on type expression".to_string());
        }
        self.expect_current(&TokenType::RBrace)?;
        self.next_token();
        Ok(pairs)
    }

    // ::= <identifier> |  ('{' <pairs> '}' )
    fn parse_type_expression(&mut self) -> Result<TypeExpr, String> {
        self.expect_current(&TokenType::Assign)?;
        self.next_token();
        self.parse_type_notation()
    }

    // parses the following gramatical rule
    // type_stm ::= 'type' <identifier> '=' <identifier> | ('{' <pairs> '}' )
    fn parse_type(&mut self) -> Result<NodeToken<Statement>, String> {
        let type_token = self.next_token();
        let token_ident = self.next_token();
        if let TokenType::Ident(identifier) = token_ident {
            let expression_type = self.parse_type_expression()?;
            self.skip_semicolons();
            Ok(NodeToken::new(
                Statement::Type(identifier, expression_type),
                type_token,
            ))
        } else {
            Err(format!(
                "expected identifier on type declaration, not {}",
                token_ident
            ))
        }
    }

    fn parse_stmt(&mut self) -> Result<NodeToken<Statement>, String> {
        self.can_assign = true;
        match self.current_token {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::For => self.parse_for_statement(),
            TokenType::Type => self.parse_type(),
            /*TokenType::Ident(ref id) => {
                if self.is_peek(&TokenType::LBracket) {
                    let id = id.clone();
                    let ident = self.next_token();
                    let expr =
                        self.parse_index_access(NodeToken::new(Expression::Id(id.clone()), ident))?;
                    self.expect_current(&TokenType::Assign)?;
                    let t = self.next_token();
                    let expr_token = NodeToken::new_boxed(
                        Expression::Assignment(
                            Box::new(expr),
                            Box::new(self.parse_expression(Precedence::Lowest)?),
                        ),
                        TokenType::Assign,
                    );
                    self.skip_semicolons();
                    return Ok(NodeToken::new(
                        Statement::ExpressionStatement(expr_token),
                        t,
                    ));
                }
                if self.is_peek(&TokenType::Assign) {
                    let ident = self.next_token();
                    if let TokenType::Ident(identifier) = &ident {
                        self.next_token();
                        let identifier = identifier.clone();
                        let expr_left = self.parse_expression(Precedence::Lowest).expect("todo");
                        let expr_token = NodeToken::new_boxed(
                            Expression::Assignment(
                                NodeToken::new_boxed(Expression::Id(identifier.clone()), ident),
                                Box::new(expr_left),
                            ),
                            TokenType::Assign,
                        );
                        self.skip_semicolons();
                        Ok(NodeToken::new(
                            Statement::ExpressionStatement(expr_token),
                            TokenType::Ident(identifier),
                        ))
                    } else {
                        unreachable!();
                    }
                } else {
                    self.parse_expression_statement()
                }
            }*/
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

    // Parses what is underlined:
    // 'let' <ident>':' _<type_notation>_ '=' <expression> [';']*
    fn parse_type_notation(&mut self) -> Result<TypeExpr, String> {
        let type_token = self.next_token();
        let type_to_return = match type_token {
            TokenType::Ident(s) => Ok(TypeExpr::Variable(s)),
            TokenType::LBracket => {
                let result = self.parse_type_notation()?;
                let is_bracket = self.expect_current(&TokenType::RBracket);
                if is_bracket.is_err() {
                    return Err("expected end bracket for expression type".to_string());
                }
                self.next_token();
                Ok(TypeExpr::Array(Box::new(result)))
            }
            TokenType::LBrace => {
                let pairs = self.parse_record_pairs()?;
                Ok(TypeExpr::Struct(pairs))
            }
            TokenType::LParen => {
                let mut vec_of_types: Vec<TypeExpr> = vec![];
                while !self.is_current(&TokenType::RParen) && !self.is_current(&TokenType::EOF) {
                    let type_notation = self.parse_type_notation()?;
                    if self.is_current(&TokenType::Comma) {
                        self.next_token();
                    }
                    vec_of_types.push(type_notation);
                }
                self.expect_current(&TokenType::RParen)?;
                self.next_token();
                if self.is_current(&TokenType::Arrow) {
                    self.next_token();
                    let return_type = self.parse_type_notation()?;
                    Ok(TypeExpr::Function(
                        vec_of_types,
                        Some(Box::new(return_type)),
                    ))
                } else {
                    Ok(TypeExpr::Function(vec_of_types, None))
                }
            }
            c => Err(format!("incorrect token on expression type='{}'", c)),
        };
        type_to_return
    }

    // let_stm ::= 'let' <identifier> ':' <identifier> '=' <expression> [';']*
    fn parse_let_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let token = self.next_token();
        self.expect_peek(&TokenType::DoubleDot)?;
        let name = self.next_token();
        self.next_token();
        // let type_token = self.next_token();
        // let type_string = if let TokenType::Ident(s) = type_token {
        //     s.as_ref().to_string()
        // } else {
        //     return Err("expected an identifier on let statement".to_string());
        // };
        let type_notation = self.parse_type_notation()?;
        self.expect_current(&TokenType::Assign)?;
        if let TokenType::Ident(name_s) = name {
            self.next_token();
            let expr = self.parse_expression(Precedence::Lowest)?;
            while self.is_current(&TokenType::Semicolon) {
                self.next_token();
            }
            Ok(NodeToken::new(
                Statement::Var(name_s, Box::new(expr), type_notation),
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
    use crate::string_interning::StringInternal;
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
    fn test_parse_index_access() {
        let input = [
            ("hash[1 + 3 + 4] = 5;", "hash[((1 + 3) + 4)] = 5"),
            (
                "hash[((1 + 3) + 4)] = hash[12345 + 111 + hash[111] + hash[[[[[[hash[1]]]]]]]]",
                "hash[((1 + 3) + 4)] = hash[(((12345 + 111) + hash[111]) + hash[[[[[[hash[1]]]]]]])]",
            ),
            (
                "hash[1] = hash[1][1][hash[hash[1 + hash[3 + hash[3]]]]];",
                "hash[1] = hash[1][1][hash[hash[(1 + hash[(3 + hash[3])])]]]",
            ),
        ];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(program[0].str(), test.1);
        }
    }

    #[test]
    fn test_parse_type_loops() {
        let input = [
            (
                "type Integer = { thing: int; thing02: int; whatever: String; }; ",
                "type Integer = { thing: int; thing02: int; whatever: String; };",
            ),
            (
                "type Integer = 
                    
                    {
                             thing: int;;;;;;;;
                                    ;;;;; thing02: int; whatever: String;;;;; } ",
                "type Integer = { thing: int; thing02: int; whatever: String; };",
            ),
            ("type Integer = int;", "type Integer = int;"),
            (
                "let matrix: [[Integer]] = 3;",
                "let matrix: [[Integer]] = 3;",
            ),
            (
                "let func: 
                    (String, [String], () -> Int) -> Int = fn(x: String, y: [String], z: () -> Int) -> Int { 
                    return 1 + z(); 
                }",
                "let func: (String, [String], () -> Int) -> Int = fn (x: String, y: [String], z: () -> Int) -> Int { return (1 + z()); };",
            ),
            (
                "let func: 
                    (String, [String], () -> Int) -> Int = fn(x: String, y: [String], z: () -> Int) -> Int { 
                    return 1 + z(); 
                }",
                "let func: (String, [String], () -> Int) -> Int = fn (x: String, y: [String], z: () -> Int) -> Int { return (1 + z()); };",
            ),
            (" type struct = {
                thing: thing;
                thing01: thing_02;
                thing3: [thing_02];
            }", "type struct = { thing: thing; thing01: thing_02; thing3: [thing_02]; };")
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
            assert_eq!(*identifier_expr, StringInternal::add_string(test));
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
