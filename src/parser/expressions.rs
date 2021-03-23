use crate::ast::node::{Expression, NodeToken, OpType, Statement, TypeExpr};
use crate::parser::Parser;
use crate::string_interning::{StringId, StringInternal};
use crate::token::TokenType;

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 0,
    Ands = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl<'a> Parser<'a> {
    pub fn parse_expression_statement(&mut self) -> Result<NodeToken<Statement>, String> {
        let expression = self.parse_expression(Precedence::Lowest)?;
        let node = NodeToken::new(
            Statement::ExpressionStatement(Box::new(expression)),
            // Empty because we wrap the true token inside
            TokenType::EOF,
        );
        while self.is_current(&&TokenType::Semicolon) {
            self.next_token();
        }
        Ok(node)
    }

    pub fn parse_array_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        let left_brace = self.next_token();
        let mut vec_exprs = vec![];
        while !self.is_current(&TokenType::RBracket) && !self.is_current(&TokenType::EOF) {
            vec_exprs.push(self.parse_expression(Precedence::Lowest)?);
            if self.is_current(&TokenType::Comma) {
                self.next_token();
            }
        }
        self.expect_current(&TokenType::RBracket)?;
        self.next_token();
        Ok(NodeToken::new(Expression::Array(vec_exprs), left_brace))
    }

    /// parse expression with pratt's
    pub fn parse_expression(&mut self, prec: Precedence) -> Result<NodeToken<Expression>, String> {
        // Get left expression
        let mut left_expr = self.prefix_expression()?;
        // Iterate and recursive doing pratt
        while !self.is_current(&&TokenType::Semicolon) && prec < self.current_precedence() {
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
        let token = self.next_token();
        if let TokenType::Ident(id) = &token {
            Ok(NodeToken::new(Expression::Id(id.clone()), token))
        } else {
            Err("expected identifier".to_string())
        }
    }

    fn parse_string(&mut self) -> Result<NodeToken<Expression>, String> {
        let identifier = self.next_token();
        if let TokenType::Quotes(ident) = &identifier {
            return Ok(NodeToken::new(Expression::String(*ident), identifier));
        }
        return Err(format!("expected normal string"));
    }

    /// parses a prefix expression depending on the current token type
    fn prefix_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        if self.can_assign {
            if let TokenType::Ident(_) = &self.current_token {
            } else {
                self.can_assign = false;
            }
        }
        match self.current_token {
            TokenType::LBracket => self.parse_array_expression(),
            TokenType::Quotes(_) => self.parse_string(),
            TokenType::Ident(_) => self.parse_identifier(),
            TokenType::Int(n) => Ok(NodeToken::new(
                Expression::Number(n as i64),
                self.next_token(),
            )),
            TokenType::True => Ok(NodeToken::new(Expression::Boolean(true), self.next_token())),
            TokenType::False => Ok(NodeToken::new(
                Expression::Boolean(false),
                self.next_token(),
            )),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if(true),
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
        while !self.is_current(&TokenType::RParen) && !self.is_current(&TokenType::EOF) {
            let exp = self.parse_expression(Precedence::Lowest)?;
            params.push(exp);
            if self.is_current(&TokenType::Comma) && !self.is_peek(&TokenType::Comma) {
                self.next_token();
            } else if self.is_current(&TokenType::Comma) {
                return Err("unexpected comma".to_string());
            }
        }
        self.expect_current(&TokenType::RParen)?;
        self.next_token();
        Ok(params)
    }

    fn parse_function_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        assert_eq!(self.current_token, TokenType::Function);
        let function_token = self.next_token();
        let parameters = self.parse_fn_def_parameters()?;
        let return_type = if self.is_current(&TokenType::Arrow) {
            self.next_token();
            self.parse_type_notation()
        } else {
            Ok(TypeExpr::Variable(StringInternal::add_string("void")))
        }?;
        let block = self.parse_block_statement()?;
        let func = Expression::FunctionDefinition {
            block,
            parameters: parameters.0,
            types: parameters.1,
            return_type,
        };
        Ok(NodeToken::new(func, function_token))
    }

    fn is_identifier_peek(&self) -> bool {
        if let TokenType::Ident(_) = self.peek_token {
            true
        } else {
            false
        }
    }

    fn parse_fn_def_parameters(
        &mut self,
    ) -> Result<(Vec<NodeToken<Expression>>, Vec<TypeExpr>), String> {
        self.expect_current(&TokenType::LParen)?;
        self.next_token();
        let mut params = vec![];
        let mut types: Vec<TypeExpr> = vec![];
        while !self.is_current(&TokenType::RParen) && !self.is_current(&TokenType::EOF) {
            let identifier = self.parse_identifier()?;
            params.push(identifier);
            self.expect_current(&TokenType::DoubleDot)?;
            self.next_token();
            let a_type = self.parse_type_notation()?;
            types.push(a_type);
            if self.is_current(&TokenType::Comma) && self.is_identifier_peek() {
                self.next_token();
            } else if self.is_current(&TokenType::Comma) {
                return Err("unexpected comma".to_string());
            }
        }
        self.expect_current(&TokenType::RParen)?;
        self.next_token();
        Ok((params, types))
    }

    pub fn parse_index_access(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        let prev_assign = self.can_assign;
        let t = self.next_token();
        let right = self.parse_expression(Precedence::Lowest)?;
        self.expect_current(&TokenType::RBracket)?;
        self.next_token();
        self.can_assign = prev_assign;
        Ok(NodeToken::new(
            Expression::IndexAccess(Box::new(left), Box::new(right)),
            t,
        ))
    }

    /// key_pairs ::=  [<identifier> '=' <expression>] [',' <identifier> '=' <expression>]* '}'
    fn parse_key_pairs(&mut self) -> Result<Vec<(StringId, NodeToken<Expression>)>, String> {
        let mut vec_pairs = vec![];
        while !self.is_current(&TokenType::RBrace) && !self.is_current(&TokenType::EOF) {
            if vec_pairs.len() >= 1 {
                self.expect_current(&TokenType::Comma)?;
                self.next_token();
            }
            if let TokenType::Ident(identifier) = self.next_token() {
                self.expect_current(&TokenType::Assign)?;
                self.next_token();
                vec_pairs.push((identifier, self.parse_expression(Precedence::Lowest)?));
            } else {
                return Err(format!("expected identifier on key expression"));
            }
        }
        self.expect_current(&TokenType::RBrace)?;
        self.next_token();
        Ok(vec_pairs)
    }

    /// struct_init ::= <identifier> '=>' '{' <key_pairs>
    fn parse_struct_init(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        if let Expression::Id(type_id) = &left.node {
            let id = *type_id;
            let arrow = self.next_token();
            self.expect_current(&TokenType::LBrace)?;
            self.next_token();
            let key_pairs = self.parse_key_pairs()?;
            Ok(NodeToken::new(Expression::TypeInit(id, key_pairs), arrow))
        } else {
            Err(format!("expected type identifier, got={:?}", left))
        }
    }

    fn parse_assign(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        if !self.can_assign {
            return Err(format!("can't assign expression {}", left.node.string()));
        }
        let equal = self.next_token();
        let right = self.parse_expression(Precedence::Lowest)?;
        let assign_node = NodeToken::new(
            Expression::Assignment(Box::new(left), Box::new(right)),
            equal,
        );
        self.can_assign = true;
        Ok(assign_node)
    }

    /// get the left expression and with the current token return an infix expression
    /// if the result is an error, it will return the expression ownership to the caller,
    /// otherwise, it will return as normal the infix parse.
    fn infix_expression(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<Result<NodeToken<Expression>, String>, NodeToken<Expression>> {
        if let TokenType::Dot = self.current_token {
        } else if let TokenType::Assign = self.current_token {
        } else if let TokenType::LBracket = self.current_token {
        } else {
            self.can_assign = false;
        }
        match self.current_token {
            TokenType::Minus
            | TokenType::Slash
            | TokenType::GreaterThan
            | TokenType::LessThan
            | TokenType::Plus
            | TokenType::Asterisk
            | TokenType::Equal
            | TokenType::NotEqual
            | TokenType::And
            | TokenType::Or => Ok(self.parse_infix_expression(left)),
            TokenType::Assign => Ok(self.parse_assign(left)),
            TokenType::LBracket => Ok(self.parse_index_access(left)),
            TokenType::Dot => Ok(self.parse_property_access(left)),
            TokenType::LParen => Ok(self.parse_fn_call(left)),
            TokenType::Arrow => Ok(self.parse_struct_init(left)),
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
            Expression::BinaryOp(Box::new(left), Box::new(right), OpType::from(&token)),
            token,
        ))
    }

    pub fn parse_block_statement(&mut self) -> Result<Vec<NodeToken<Statement>>, String> {
        self.expect_current(&TokenType::LBrace)?;
        self.next_token();
        let mut block = vec![];
        while !self.is_current(&TokenType::RBrace) && !self.is_current(&TokenType::EOF) {
            block.push(self.parse_stmt()?);
        }
        self.expect_current(&TokenType::RBrace)?;
        self.next_token();
        Ok(block)
    }

    /// parse if statements
    fn parse_if(&mut self, keep_going: bool) -> Result<NodeToken<Expression>, String> {
        self.expect_current(&TokenType::If)?;
        let if_token = self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        let block = self.parse_block_statement()?;
        if !keep_going {
            let expr = Expression::If {
                block,
                else_ifs: None,
                last_else: None,
                condition: Box::new(condition),
            };
            return Ok(NodeToken::new(expr, if_token));
        }
        let mut wasted_last_else = false;
        let mut ifs = vec![];
        let mut last_else = None;
        while self.is_current(&TokenType::Else) {
            self.next_token();
            if self.is_current(&TokenType::If) && !wasted_last_else {
                let curr_if = self.parse_if(false)?;
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
        let op = OpType::from(&token);
        let right = self.parse_expression(Precedence::Prefix)?;
        let expr = NodeToken::new(Expression::PrefixOp(op, Box::new(right)), token);
        Ok(expr)
    }

    /// Parse expressions inside parenthesis
    fn parse_grouped_expression(&mut self) -> Result<NodeToken<Expression>, String> {
        assert_eq!(self.current_token, TokenType::LParen);
        let prev_assign = self.can_assign;
        self.next_token();
        self.can_assign = true;
        let exp = self.parse_expression(Precedence::Lowest)?;
        self.can_assign = prev_assign;
        self.expect_current(&TokenType::RParen)?;
        self.next_token();
        Ok(exp)
    }

    fn parse_property_access(
        &mut self,
        left: NodeToken<Expression>,
    ) -> Result<NodeToken<Expression>, String> {
        //
        let dot = self.next_token();
        let expression = self.parse_expression(Precedence::Call)?;
        let access = Expression::PropertyAccess(Box::new(left), Box::new(expression));
        let node = NodeToken::new(access, dot);
        Ok(node)
    }

    fn get_precedence(token: &TokenType) -> Precedence {
        match token {
            TokenType::And | TokenType::Or => Precedence::Ands,
            TokenType::Assign | TokenType::Equal | TokenType::NotEqual => Precedence::Equals,
            TokenType::LessThan | TokenType::GreaterThan => Precedence::LessGreater,
            TokenType::Plus | TokenType::Minus => Precedence::Sum,
            TokenType::Asterisk | TokenType::Slash => Precedence::Product,
            TokenType::LParen | TokenType::LBracket | TokenType::Arrow | TokenType::Dot => {
                Precedence::Call
            }
            _ => Precedence::Lowest,
        }
    }

    #[allow(dead_code)]
    fn peek_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.peek_token)
    }

    fn current_precedence(&self) -> Precedence {
        Parser::get_precedence(&self.current_token)
    }
}

mod test {

    #![allow(unused_variables)]
    #![allow(dead_code)]
    #![allow(unused_imports)]
    use crate::ast::node::{Expression, NodeToken, OpType, Statement, Str};
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::string_interning::{StringId, StringInternal};
    use crate::token::TokenType;

    #[test]
    fn test_parse_str() {
        let input = [(
            "let thing: string = \"hellow worldw\";
            s=3;
            wwww[1*2+4+(s=3)]=1; ",
            "let thing: string = \"hellow worldw\"; s = 3 wwww[(((1 * 2) + 4) + s = 3)] = 1",
        )];
        for test in input.iter() {
            let program_text = get_program_text(test.0);
            assert_eq!(test.1, program_text);
        }
    }

    #[test]
    fn test_parse_arrays() {
        let input = [(
            "let thing: string = [\"hellow worldw\", 1, 2, fn (x: int,y:int) { return 3; }, [[[[ 1 ]]]] ];",
            "let thing: string = [\"hellow worldw\", 1, 2, fn (x: int, y: int) -> void { return 3; }, [[[[1]]]]];",
        )];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(test.1, program[0].str());
        }
    }

    #[test]
    fn test_parse_struct_init() {
        let input = [(
            "let thing: Thing = Thing -> {w=1, h=2, q=Thing->{w=4}}",
            "let thing: Thing = Thing -> {w=1, h=2, q=Thing -> {w=4}};",
        )];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(program.len(), 1);
            assert_eq!(test.1, program[0].str());
        }
    }

    #[test]
    fn test_parse_property_access() {
        let input = [("a.b.c + a.d.s", "(a.b.c + a.d.s)")];
        for test in input.iter() {
            let program = get_program(test.0);
            assert_eq!(program.len(), 1);
            assert_eq!(test.1, program[0].str());
        }
    }

    #[test]
    fn test_parse_function() {
        let input = [
            (
                "fn (x: int, y: int) { x + y; y + z; if x == y { 1 } };",
                "fn (x: int, y: int) -> void { (x + y)  (y + z)  if (x == y) { 1 } }",
            ),
            (
                "fn(x: int, y: int) { x + y; y + z; if x == y { 1 } else if !!!!!!!---true { 5; } else { false; } };",
                "fn (x: int, y: int) -> void { (x + y)  (y + z)  if (x == y) { 1 } else if (!(!(!(!(!(!(!(-(-(-true)))))))))) { 5 } else { false } }",
            ),
            (
                "fn(){}",
                "fn () -> void {}"
            ),
            (
                "let f: int = fn(){}",
                "let f: int = fn () -> void {};"
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
            ("return if 1 == 2 { 1 } else { 2 }", "return if (1 == 2) { 1 } else { 2 };"),
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
        ", "if (x == true) { (1 + 2) if (x == false) { (3 + 3) } } else if (1 + 3) { 3 } else if (!(!(!(!(!(-(-(-(1 + 3))))))))) { (1992192 + (33 * 3)) } else { (3 + 5) }"),
        ("
            let ifs: int = if (x == true) {
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
        ", "let ifs: int = if (x == true) { (1 + 2) if (x == false) { (3 + 3) (3 * 3) (3 / 3) } else { 3 } } else if (1 + 3) { 3 } else if (!(!(!(!(!(-(-(-(1 + 3))))))))) { (1992192 + (33 * 3)) } else { (3 + 5) };"),
        ];
        for test in input.iter() {
            let program = get_program(test.0);
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

    fn get_program_text(input: &str) -> String {
        let program = get_program(input);
        let text = program
            .iter()
            .map(|el| format!("{}", el.str()))
            .collect::<Vec<String>>()
            .join(" ");
        text
    }

    fn test_integer(expr: &NodeToken<Expression>, expected: i64) {
        let value = if let Expression::Number(value) = expr.node {
            value
        } else {
            panic!("not a number");
        };
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
        assert_eq!(string!(id), "variable");
        assert_eq!(format!("{}", token), "variable");
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
            assert_eq!(&id.to_string(), op);
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
            ("-a * b && -a * b != 0 || 5 == 5", "((((-a) * b) && (((-a) * b) != 0)) || (5 == 5))"),
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
                "let added: int = add(x, y, fn(x:_,y:_,y:_){ 5; }, if x == y {},) + 10 / 30 * f()",
                "let added: int = (add(x, y, fn (x: _, y: _, y: _) -> void { 5 }, if (x == y) { }) + ((10 / 30) * f()));",
            ),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("!(true == true)", "(!(true == true))"),
        ];

        for test in input.iter() {
            let program = get_program(test.0);
            let str = Statement::Program(program).string(&TokenType::EOF);
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
