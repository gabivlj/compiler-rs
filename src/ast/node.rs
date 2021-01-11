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
    Number(i64),

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
    Call(Box<NodeToken<Expression>>, Vec<NodeToken<Expression>>),

    /// If refers to an if node
    If {
        condition: Box<NodeToken<Expression>>,
        block: Vec<NodeToken<Statement>>,
        else_ifs: Option<Vec<NodeToken<Expression>>>,
        last_else: Option<Vec<NodeToken<Statement>>>,
    },

    /// FunctionDefinition the parameter list and the Block node
    FunctionDefinition(Vec<NodeToken<Expression>>, Vec<NodeToken<Statement>>),

    /// Almost equal to Var, but instead will override the already declared variable
    Assignment(String, Box<NodeToken<Expression>>),

    PrefixOp(String, Box<NodeToken<Expression>>),

    Block(Vec<NodeToken<Statement>>),

    Boolean(bool),
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
    GreaterThan,
    LessThan,
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
    pub fn string(&self, token: &Token) -> String {
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

fn parameters_to_string(vec: &Vec<NodeToken<Expression>>) -> String {
    format!(
        "({})",
        vec.iter()
            .map(|node| node.str())
            .collect::<Vec<String>>()
            .join(", ")
    )
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::Id(identifier) => identifier.clone(),
            Expression::Number(n) => n.to_string(),
            Expression::PrefixOp(op, val) => format!("({}{})", op, val.str()),
            Expression::BinaryOp(left, right, op) => {
                format!("({} {} {})", left.str(), op.to_string(), right.str())
            }
            Expression::FunctionDefinition(parameters, block) => format!(
                "fn {} {{{}}}",
                parameters_to_string(parameters),
                block.iter().fold(String::new(), |prev, now| format!(
                    "{} {}; ",
                    prev,
                    now.str()
                ))
            ),
            Expression::If {
                condition,
                last_else,
                else_ifs,
                block,
            } => {
                let last_else_string = if last_else.is_none() {
                    String::new()
                } else {
                    format!(
                        " else {{{}}}",
                        last_else
                            .as_ref()
                            .unwrap()
                            .iter()
                            .fold(String::new(), |prev, now| {
                                format!("{} {}; ", prev, now.str())
                            })
                    )
                };
                let elses = if else_ifs.is_none() {
                    String::new()
                } else {
                    format!(
                        "{}",
                        else_ifs
                            .as_ref()
                            .unwrap()
                            .iter()
                            .fold(String::new(), |prev, now| {
                                format!("{} else {}", prev, now.str())
                            })
                    )
                };
                let bl_str = block.iter().fold(String::new(), |prev, now| {
                    format!("{}{}; ", prev, now.str())
                });
                format!(
                    "{} {} {{ {}}}{}{}",
                    "if",
                    condition.str(),
                    bl_str,
                    elses,
                    last_else_string
                )
            }
            Expression::Boolean(bool) => format!("{}", bool),
            _ => unimplemented!(),
        }
    }
}

pub trait Str {
    fn str(&self) -> String;
}

impl Str for NodeToken<Statement> {
    fn str(&self) -> String {
        self.node.string(&self.token)
    }
}

impl Str for NodeToken<Expression> {
    fn str(&self) -> String {
        self.node.string()
    }
}

use std::string::ToString;

impl ToString for OpType {
    fn to_string(&self) -> String {
        match *self {
            OpType::Add => "+".to_string(),
            OpType::Substract => "-".to_string(),
            OpType::Divide => "/".to_string(),
            OpType::Multiply => "*".to_string(),
            OpType::And => "&&".to_string(),
            OpType::Or => "||".to_string(),
            OpType::Equal => "==".to_string(),
            OpType::NotEqual => "!=".to_string(),
            OpType::GreaterThan => ">".to_string(),
            OpType::LessThan => "<".to_string(),
        }
    }
}

impl From<&str> for OpType {
    fn from(value: &str) -> Self {
        match value {
            "+" => OpType::Add,
            "-" => OpType::Substract,
            "/" => OpType::Divide,
            "*" => OpType::Multiply,
            "&&" => OpType::And,
            "||" => OpType::Or,
            "==" => OpType::Equal,
            "!=" => OpType::NotEqual,
            ">" => OpType::GreaterThan,
            "<" => OpType::LessThan,
            _ => panic!("unknown operator {}", value),
        }
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
