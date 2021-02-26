use crate::ast::node::{Expression, OpType, Statement};
use crate::hash_undo::HashUndo;
use std::rc::Rc;
use std::{collections::HashMap, unimplemented};

#[derive(PartialEq)]
pub enum Type {
    Void,
    Int,
    String,
    Record(Vec<(String, Rc<Type>)>),
    Array(Rc<Type>),
    Nil,
    Name(String, Rc<Type>),
}

struct FunctionEntry {
    formals: Vec<Rc<Type>>,
    result: Rc<Type>,
}

enum Entry {
    Variable(Rc<Type>),
    Function(FunctionEntry),
}

impl Entry {
    pub fn new_variable(type_store: &Rc<Type>) -> Self {
        Entry::Variable(type_store.clone())
    }

    pub fn new_function(formals: Vec<Rc<Type>>, result: &Rc<Type>) -> Self {
        Entry::Function(FunctionEntry {
            formals,
            result: result.clone(),
        })
    }
}

struct SemanticAnalysis<'a> {
    types: HashUndo<'a, String, Rc<Type>>,
    variables: HashMap<String, Entry>,
}

#[derive(Copy, Clone)]
struct Translation;

struct ExpressionType {
    exp_type: Rc<Type>,
    translation: Option<Translation>,
}

impl ExpressionType {
    fn new(translation: Option<Translation>, exp_type: Rc<Type>) -> Self {
        Self {
            translation,
            exp_type,
        }
    }
}

impl<'a> SemanticAnalysis<'a> {
    pub fn new() -> Self {
        let mut me = Self {
            types: HashUndo::new(),
            variables: HashMap::new(),
        };
        me.types.add("int".to_string(), Rc::new(Type::Int));
        me.types.add("string".to_string(), Rc::new(Type::String));
        me
    }

    pub fn expects_type(&self, the_type: &Type, expects: &Type) -> Result<(), String> {
        if the_type != expects {
            return Err(
                "missed type on thing, i guess this error isn't properly redacted yet :)"
                    .to_string(),
            );
        }
        Ok(())
    }

    pub fn translation_variable(
        &mut self,
        variable: &Expression,
    ) -> Result<ExpressionType, String> {
        match variable {
            Expression::Id(identifier) => {
                let id = identifier.to_string();
                let entry = self.variables.get(&id);
                if let Some(entry) = entry {
                    if let Entry::Variable(type_var) = entry {
                        return Ok(ExpressionType::new(
                            None,
                            SemanticAnalysis::actual_type(type_var),
                        ));
                    }
                }
                return Err(format!("undefined variable={}", identifier));
            }
            _ => unimplemented!(),
        }
    }

    fn actual_type(maybe_a_name: &Rc<Type>) -> Rc<Type> {
        if let Type::Name(_, underlying_type) = maybe_a_name.as_ref() {
            return SemanticAnalysis::actual_type(&underlying_type);
        }
        return maybe_a_name.clone();
    }

    pub fn type_check_statement(&mut self, stmt: &mut Statement) -> Result<ExpressionType, String> {
        match stmt {
            Statement::Program(statements) => {
                for stmt in statements {
                    self.type_check_statement(&mut stmt.node)?;
                }
                return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
            }
            Statement::Var(variable, expression, type_name) => {
                let variable_type = self.translation_expression(&expression.as_ref().node)?;
                let expected_type = self
                    .types
                    .get(type_name)
                    .ok_or(format!("unknown type {}", type_name))?;
                if expected_type.as_ref() != variable_type.exp_type.as_ref() {
                    return Err(format!("unmatching types in variable declaration"));
                }
                self.variables.insert(
                    variable.to_string(),
                    Entry::new_variable(&variable_type.exp_type),
                );
            }
            Statement::ExpressionStatement(expr) => {
                return self.translation_expression(&mut expr.node)
            }
            _ => unimplemented!(),
        }
        return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
    }

    fn translation_expression(&mut self, expr: &Expression) -> Result<ExpressionType, String> {
        match expr {
            Expression::String(identifier) => {
                let string = self
                    .types
                    .get(&"string".to_string())
                    .expect("string type must be defined");
                Ok(ExpressionType::new(None, string.clone()))
            }
            Expression::Id(identifier) => {
                return self.translation_variable(expr);
            }
            Expression::Number(_) => {
                let int = self
                    .types
                    .get(&"int".to_string())
                    .expect("Integer type must be defined");
                Ok(ExpressionType::new(None, int.clone()))
            }
            Expression::Assignment(name, expr) => {
                let expr_type = self.translation_expression(&expr.as_ref().node)?;
                let type_variable = self
                    .variables
                    .get(name)
                    .ok_or(format!("undefined variable={}", name))?;
                if let Entry::Variable(type_var) = type_variable {
                    if expr_type.exp_type.as_ref() != type_var.as_ref() {
                        Err(format!("unmatching type"))
                    } else {
                        Ok(ExpressionType::new(None, type_var.clone()))
                    }
                } else {
                    // ?? maybe
                    Err("can't assign a function another time".to_string())
                }
            }
            Expression::BinaryOp(left, right, op) => {
                let left_type = self.translation_expression(&left.node)?;
                let right_type = self.translation_expression(&right.node)?;
                match op {
                    OpType::Add => {
                        if let (Type::Int, Type::Int) =
                            (left_type.exp_type.as_ref(), right_type.exp_type.as_ref())
                        {
                            return Ok(ExpressionType::new(None, Rc::new(Type::Int)));
                        } else {
                            if let (Type::String, Type::String) =
                                (left_type.exp_type.as_ref(), right_type.exp_type.as_ref())
                            {
                                return Ok(ExpressionType::new(None, Rc::new(Type::String)));
                            }
                            return Err("Expected integer type on add operation".to_string());
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }
}

mod test {
    use crate::ast;
    use crate::lexer;
    use crate::parser;
    use crate::semantic;

    #[test]
    fn test_types() {
        let code = "let variable: string = \"www\"; variable = variable + \"damn\"; let anotherthing: int = 4; anotherthing = 4+3;";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }
}
