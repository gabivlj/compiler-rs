use crate::token::TokenType;
use std::{boxed::Box, unimplemented};

#[derive(Debug, Clone)]
pub struct NodeToken<T> {
    pub token: TokenType,
    pub node: T,
}

impl<T: Clone> NodeToken<T> {
    pub fn new(node: T, token: TokenType) -> Self {
        Self { token, node }
    }

    pub fn new_boxed(node: T, token: TokenType) -> Box<Self> {
        Box::new(Self::new(node, token))
    }
}

use std::borrow::Cow;

#[derive(Debug, Clone)]
pub enum Expression {
    /// We'll only have u64 values atm
    Number(i64),

    /// Id is an identifier
    /// `let f = 0;` would create an identifier called f
    Id(Cow<'static, str>),

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
    FunctionDefinition {
        parameters: Vec<NodeToken<Expression>>,
        types: Vec<TypeExpr>,
        return_type: TypeExpr,
        block: Vec<NodeToken<Statement>>,
    },

    /// Almost equal to Var, but instead will override the already declared variable
    Assignment(Box<NodeToken<Expression>>, Box<NodeToken<Expression>>),

    PrefixOp(OpType, Box<NodeToken<Expression>>),

    // Block(Vec<NodeToken<Statement>>),
    Array(Vec<NodeToken<Expression>>),

    Boolean(bool),

    String(String),

    IndexAccess(Box<NodeToken<Expression>>, Box<NodeToken<Expression>>),

    TypeInit(String, Vec<(String, NodeToken<Expression>)>),

    PropertyAccess(Box<NodeToken<Expression>>, Box<NodeToken<Expression>>),
}

#[derive(Clone, Debug)]
pub enum TypeExpr {
    Variable(String),
    Array(Box<TypeExpr>),
    Function(Vec<TypeExpr>, Option<Box<TypeExpr>>),
    Struct(Vec<(String, TypeExpr)>),
}

use std::fmt;

impl std::fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExpr::Variable(s) => f.write_fmt(format_args!("{}", s)),
            TypeExpr::Array(s) => f.write_fmt(format_args!("[{}]", s)),
            TypeExpr::Function(s, ret) => {
                let str: Vec<String> = s.iter().map(|s| format!("{}", s)).collect();
                let joined = str.join(", ");
                if let Some(ret) = ret {
                    f.write_fmt(format_args!("({}) -> {}", joined, ret))
                } else {
                    f.write_fmt(format_args!("({}) -> void", joined))
                }
            }
            TypeExpr::Struct(pairs) => {
                let pairs_str = pairs.iter().fold(String::new(), |prev, now| {
                    format!("{}{}: {}; ", prev, now.0, now.1)
                });
                f.write_fmt(format_args!("{{ {}}}", pairs_str))
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    Type(String, TypeExpr),

    /// While contains the conditional expr that keeps the loop going and
    /// the block of code
    While(Box<NodeToken<Expression>>, Vec<NodeToken<Statement>>),

    /// Var contains the variable name and the expression value that is assigned to and the type
    Var(Cow<'static, str>, Box<NodeToken<Expression>>, TypeExpr),

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
    Bang,
}

impl Statement {
    pub fn string(&self, token: &TokenType) -> String {
        match self {
            Statement::Empty => String::with_capacity(0),
            Statement::Program(tokens) => {
                tokens.iter().fold(String::new(), |acc, x| acc + &x.str())
            }
            Statement::Var(identifier, expression, type_of_variable) => format!(
                "{} {}: {} = {};",
                token,
                identifier,
                type_of_variable,
                expression.str()
            ),
            Statement::ExpressionStatement(expr) => expr.str(),
            Statement::Return(expr) => format!("{} {};", token, expr.str()),
            Statement::While(condition, block) => format!(
                "{} {} {{{}}}",
                token,
                condition.str(),
                block
                    .iter()
                    .fold(String::new(), |prev, now| format!("{} {}", prev, now.str()))
            ),
            Statement::Type(name, type_expr) => format!("type {} = {};", name, type_expr),
            _ => String::with_capacity(0),
        }
    }
}

fn call_parameters_to_string(vec: &Vec<NodeToken<Expression>>) -> String {
    format!(
        "({})",
        vec.iter()
            .map(|node| { node.str() })
            .collect::<Vec<String>>()
            .join(", ")
    )
}

fn parameters_fn_to_string(
    vec: &Vec<NodeToken<Expression>>,
    type_params: &Vec<TypeExpr>,
) -> String {
    let mut idx = 0;
    format!(
        "({})",
        vec.iter()
            .map(|node| {
                idx += 1;
                format!("{}: {}", node.str(), type_params[idx - 1])
            })
            .collect::<Vec<String>>()
            .join(", ")
    )
}

impl Expression {
    pub fn string(&self) -> String {
        match self {
            Expression::IndexAccess(left, right) => {
                format!("{}[{}]", left.node.string(), right.node.string())
            }

            Expression::Assignment(left, right) => {
                format!("{} = {}", left.node.string(), right.node.string())
            }

            Expression::String(identifier) => format!("\"{}\"", identifier.to_string()),

            Expression::Id(identifier) => identifier.to_string(),

            Expression::Number(n) => n.to_string(),

            Expression::PrefixOp(op, val) => format!("({}{})", op.to_string(), val.str()),

            Expression::BinaryOp(left, right, op) => {
                format!("({} {} {})", left.str(), op.to_string(), right.str())
            }

            Expression::Array(expressions) => format!(
                "[{}]",
                expressions
                    .iter()
                    .map(|expr| expr.node.string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),

            Expression::Call(id, params) => {
                format!("{}{}", id.str(), call_parameters_to_string(params))
            }

            Expression::FunctionDefinition {
                parameters,
                types,
                return_type,
                block,
            } => format!(
                "fn {} -> {} {{{}}}",
                parameters_fn_to_string(parameters, types),
                return_type,
                block.iter().fold(String::new(), |prev, now| format!(
                    "{} {} ",
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
                                format!("{} {} ", prev, now.str())
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
                let bl_str = block
                    .iter()
                    .fold(String::new(), |prev, now| format!("{}{} ", prev, now.str()));
                format!(
                    "{} {} {{ {}}}{}{}",
                    "if",
                    condition.str(),
                    bl_str,
                    elses,
                    last_else_string
                )
            }

            Expression::TypeInit(type_name, pairs) => {
                let s = pairs
                    .iter()
                    .map(|pair| format!("{}={}", pair.0, pair.1.node.string()))
                    .collect::<Vec<String>>()
                    .join(", ");
                format!("{} -> {{{}}}", type_name, s)
            }

            Expression::Boolean(bool) => format!("{}", bool),

            Expression::PropertyAccess(left, right) => {
                format!("{}.{}", left.node.string(), right.node.string())
            }

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
            OpType::Bang => "!".to_string(),
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

impl From<&TokenType> for OpType {
    fn from(value: &TokenType) -> Self {
        match value {
            TokenType::Plus => OpType::Add,
            TokenType::Minus => OpType::Substract,
            TokenType::Slash => OpType::Divide,
            TokenType::Asterisk => OpType::Multiply,
            TokenType::Equal => OpType::Equal,
            TokenType::NotEqual => OpType::NotEqual,
            TokenType::GreaterThan => OpType::GreaterThan,
            TokenType::LessThan => OpType::LessThan,
            TokenType::Bang => OpType::Bang,
            TokenType::And => OpType::And,
            TokenType::Or => OpType::Or,
            _ => panic!("unknown operator {}", value),
        }
    }
}

mod test {
    #[allow(unused_imports)]
    use crate::ast::node::{Expression, NodeToken, Statement, TypeExpr};
    #[allow(unused_imports)]
    use crate::token::{Token, TokenType};
    #[allow(unused_imports)]
    use std::borrow::Cow;
    #[test]
    fn check_string() {
        let program = Statement::Program(vec![NodeToken::new(
            Statement::Var(
                Cow::Borrowed("myVar"),
                NodeToken::new_boxed(
                    Expression::Id(Cow::Borrowed("anotherVar")),
                    TokenType::Ident(Cow::Borrowed("anotherVar")),
                ),
                TypeExpr::Variable("int".to_string()),
            ),
            TokenType::Let,
        )]);
        if program.string(&TokenType::EOF) != "let myVar: int = anotherVar;" {
            panic!(
                "program to string wrong. got={}",
                program.string(&TokenType::EOF)
            )
        }
    }
}
