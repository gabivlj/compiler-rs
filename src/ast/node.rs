use crate::token::Token;
use std::{boxed::Box, unimplemented};

#[derive(Debug, Clone)]
/// Node represents a AST node. We intend this node to fill 32 bytes in memory maximum everytime
pub enum Node {
    Expression(Expression),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct NodeToken<T> {
    pub token: Token,
    pub node: T,
}

impl<T: Clone> NodeToken<T> {
    pub fn new(node: T, token: Token) -> Self {
        Self { token, node }
    }

    pub fn new_boxed(node: T, token: Token) -> Box<Self> {
        Box::new(Self::new(node, token))
    }
}

#[derive(Debug, Clone)]
pub enum Expression {
    /// We'll only have u64 values atm
    Number(u64),

    /// Id is an identifier
    /// `let f = 0;` would create an identifier called f
    Id(String),

    BinaryOp(
        Box<NodeToken<Expression>>,
        Box<NodeToken<Expression>>,
        OpType,
    ),

    /// Call is a function call like `f(x)`
    /// - First value is the callee
    /// - Second value are the arguments
    Call(Box<NodeToken<Expression>>, Vec<Expression>),

    /// If refers to an if node
    If {
        condition: Box<NodeToken<Expression>>,
        block: Box<Vec<NodeToken<Statement>>>,
        else_ifs: Option<Vec<NodeToken<Statement>>>,
        last_else: Option<Vec<NodeToken<Statement>>>,
    },

    /// FunctionDefinition the parameter list and the Block node
    FunctionDefinition(String, Vec<NodeToken<Statement>>),

    /// Almost equal to Var, but instead will override the already declared variable
    Assignment(String, Box<NodeToken<Expression>>),

    PrefixOp(String, Box<NodeToken<Expression>>),
}

#[derive(Debug, Clone)]
pub enum Statement {
    /// While contains the conditional expr that keeps the loop going and
    /// the block of code
    While(Box<NodeToken<Expression>>, Vec<NodeToken<Statement>>),

    /// Var contains the variable name and the expression value that is assigned to
    Var(Box<NodeToken<Expression>>, Box<NodeToken<Expression>>),

    /// Return represents a return statement
    Return(Box<NodeToken<Expression>>),

    ExpressionStatement(Box<NodeToken<Expression>>),

    Program(Vec<NodeToken<Statement>>),

    Empty,
}

#[derive(Debug, Clone, Copy)]
pub enum OpType {
    Add,
    Substract,
    Multiply,
    Equal,
    NotEqual,
    Divide,
    And,
    Or,
}

impl Node {
    // pub fn token_literal<'a>(&'a self) -> &'a Token {
    //     match &self {
    //         Node::Expression { token, expr: _ } => token,
    //         Node::Statement {
    //             token,
    //             statement: _,
    //         } => token,
    //     }
    // }

    pub fn get_statement(self) -> Statement {
        match self {
            Node::Expression(_) => panic!("not an statement"),
            Node::Statement(statement) => statement,
        }
    }

    pub fn get_statement_ref<'a>(&'a self) -> &'a Statement {
        match self {
            Node::Expression(_) => panic!("not an statement"),
            Node::Statement(statement) => statement,
        }
    }

    pub fn get_expression(self) -> Expression {
        match self {
            Node::Expression(expr) => expr,
            Node::Statement(_) => panic!("not an expression"),
        }
    }

    pub fn get_expression_ref<'a>(&'a self) -> &'a Expression {
        match self {
            Node::Expression(expr) => expr,
            Node::Statement(_) => panic!("not an expression"),
        }
    }
}

impl Statement {
    fn string(&self, token: &Token) -> String {
        match self {
            Statement::Empty => String::with_capacity(0),
            Statement::Program(tokens) => {
                tokens.iter().fold(String::new(), |acc, x| acc + &x.str())
            }
            Statement::Var(identifier, expression) => format!(
                "{} {} = {};",
                token.string,
                identifier.str(),
                expression.str()
            ),
            Statement::ExpressionStatement(expr) => expr.str(),
            Statement::Return(expr) => format!("{} {};", token.string, expr.str()),
            _ => unimplemented!(),
        }
    }
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Id(identifier) => identifier.clone(),
            Expression::Number(n) => n.to_string(),
            _ => unimplemented!(),
        }
    }
}

pub trait ToString {
    fn str(&self) -> String;
}

impl ToString for NodeToken<Statement> {
    fn str(&self) -> String {
        self.node.string(&self.token)
    }
}

impl ToString for NodeToken<Expression> {
    fn str(&self) -> String {
        self.node.string()
    }
}

mod test {
    use crate::ast::node::{Expression, Node, NodeToken, Statement};
    use crate::token::{Token, TokenType};
    #[test]
    fn check_string() {
        let program = Statement::Program(vec![NodeToken::new(
            Statement::Var(
                NodeToken::new_boxed(
                    Expression::Id("myVar".to_string()),
                    Token::new("myVar", TokenType::Ident),
                ),
                NodeToken::new_boxed(
                    Expression::Id("anotherVar".to_string()),
                    Token::new("anotherVar", TokenType::Ident),
                ),
            ),
            Token::new("let", TokenType::Let),
        )]);
        if program.string(&Token::empty()) != "let myVar = anotherVar;" {
            panic!(
                "program to string wrong. got={}",
                program.string(&Token::empty())
            )
        }
    }
}
