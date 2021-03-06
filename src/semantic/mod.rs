#![allow(dead_code)]

use crate::ast::node::{Expression, NodeToken, OpType, Statement, TypeExpr};
use crate::hash_undo::HashUndo;
use std::{rc::Rc, unimplemented};
// use std::{collections::HashUndo, unimplemented};

/// Asserts that the type is equal to the expected
/// `$self` is the self that is the context to the function call
/// returns an error if the types are not equal
/// *Examples*:
/// - int == int.
/// - Arr<int> == Arr<void> (Arr<void> only happens because of an empty array)
/// - string != int
/// type str = string;
/// - string != str <-- (??) discuss about this
macro_rules! assert_type {
    ($cond:expr, $expected:expr, $self:expr) => {
        if !$cond.equal_type($expected, $self) {
            return Err(format!("expected type={:?}, got={:?}", $expected, $cond));
        }
    };
}

/// Type contains all the types of the programming language
#[derive(PartialEq, Debug)]
pub enum Type {
    /// Scope is a internal symbol that serves the purpose of indicating on when the scope
    /// starts
    Scope,

    /// Void has many purposes, when a function doesn't have a return type it's a void.
    /// An empty array is of type void.
    Void,

    /// Integer type
    Int,

    /// String type
    String,

    /// Struct type
    Record(Vec<(String, Rc<Type>)>),

    /// Function type
    Function(FunctionEntry),

    /// Array type
    Array(Rc<Type>),

    /// Null type
    Nil,

    /// Name type is for recursive types (Rc<Type> will be later filled)
    /// *This behaviour isn't implemented yet.*
    Name(String, Rc<Type>),
}

/// Function entry is the type of a function
#[derive(PartialEq, Debug)]
pub struct FunctionEntry {
    /// Parameter types
    formals: Vec<Rc<Type>>,

    /// Expected type result of a function
    result: Rc<Type>,
}

impl FunctionEntry {
    /// Returns a new function entry
    fn new(formals: Vec<Rc<Type>>, result: Rc<Type>) -> Self {
        Self { formals, result }
    }
}

#[derive(PartialEq)]
/// This was supposed to be an entry for variables, but I think we will just use types
// !! I think we should delete this and just use Type enum
enum Entry {
    Variable(Rc<Type>),
    Function(FunctionEntry),
    Scope,
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

/// Semantic Analysis struct that contains all the types and variables
struct SemanticAnalysis<'a> {
    /// Types hashmap where we relate the name of a type to it's recursive type
    types: HashUndo<'a, String, Rc<Type>>,

    /// Variables maps a string to its type
    variables: HashUndo<'a, String, Entry>,
}

/// Intermediate Representation *unimplemented*
#[derive(Copy, Clone, Debug)]
struct Translation;

/// Expression type (atm the only translation thing) contains the type of an expression
/// and the possible IR.
struct ExpressionType {
    exp_type: Rc<Type>,
    translation: Option<Translation>,
}

impl Type {
    /// equal_type, read `assert_type!` macro
    fn equal_type<'a>(
        self: &Rc<Self>,
        other: &Rc<Self>,
        semantic: &'a SemanticAnalysis<'a>,
    ) -> bool {
        if let Type::Array(_) = self.as_ref() {
            if let Type::Array(_) = other.as_ref() {
            } else {
                return false;
            }
            let true_type = semantic.unbox_array(other);
            if let Type::Void = true_type.as_ref() {
                return true;
            } else if let Type::Void = semantic.unbox_array(self).as_ref() {
                return true;
            }
        }
        self == other
    }
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
    /// returns a new semantic analysis with int, string and void
    pub fn new() -> Self {
        let mut me = Self {
            types: HashUndo::new(),
            variables: HashUndo::new(),
        };
        me.types.add("int".to_string(), Rc::new(Type::Int));
        me.types.add("string".to_string(), Rc::new(Type::String));
        me.types.add("void".to_string(), Rc::new(Type::Void));
        me
    }

    /// expects type handles type equality
    pub fn expects_type(&self, the_type: &Type, expects: &Type) -> Result<(), String> {
        if the_type != expects {
            return Err(
                "missed type on thing, i guess this error isn't properly redacted yet :)"
                    .to_string(),
            );
        }
        Ok(())
    }

    /// translates a variable expression
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

    /// unwraps a Type::Name type, else just clones the type
    fn actual_type(maybe_a_name: &Rc<Type>) -> Rc<Type> {
        if let Type::Name(_, underlying_type) = maybe_a_name.as_ref() {
            return SemanticAnalysis::actual_type(&underlying_type);
        }
        return maybe_a_name.clone();
    }

    /// unboxes array type
    fn unbox_array(&self, array_type: &Rc<Type>) -> Rc<Type> {
        if let Type::Array(t) = array_type.as_ref() {
            self.unbox_array(t)
        } else {
            array_type.clone()
        }
    }

    /// unwrap_type_expr translates TypeExpr to Type
    fn unwrap_type_expr(&mut self, type_expr: &TypeExpr) -> Option<Rc<Type>> {
        match type_expr {
            TypeExpr::Struct(attributes) => {
                let mut attributes_parsed = Vec::with_capacity(attributes.len());
                for att in attributes {
                    let type_unw = self.unwrap_type_expr(&att.1);
                    if let Some(the_type) = type_unw {
                        attributes_parsed.push((att.0.clone(), the_type));
                    } else {
                        return None;
                    }
                }
                Some(Rc::new(Type::Record(attributes_parsed)))
            }

            TypeExpr::Variable(s) => Some(self.types.get(s)?.clone()),

            TypeExpr::Array(type_expr) => {
                let boxed_type = self.unwrap_type_expr(type_expr.as_ref())?;
                Some(Rc::new(Type::Array(boxed_type)))
            }

            TypeExpr::Function(params, return_type) => {
                let mut has_none = false;
                let types: Vec<Rc<Type>> = params
                    .iter()
                    .map(|element| {
                        let t = self.unwrap_type_expr(element);
                        has_none = if has_none { has_none } else { t.is_none() };
                        if has_none {
                            Rc::new(Type::Void)
                        } else {
                            t.unwrap()
                        }
                    })
                    .collect();
                if has_none {
                    None
                } else {
                    let return_type_unwrapped = if return_type.is_none() {
                        self.types.get(&"void".to_string()).map(|t| t.clone())
                    } else {
                        self.unwrap_type_expr(return_type.as_ref().expect("it's not none"))
                    }?;
                    Some(Rc::new(Type::Function(FunctionEntry::new(
                        types,
                        return_type_unwrapped,
                    ))))
                }
            }

            _ => unimplemented!(),
        }
    }

    /// begin_scope variables
    fn begin_scope(&mut self) {
        self.types
            .add("@_<scope~start>_".to_string(), Rc::new(Type::Scope));
        self.variables
            .add("@_<scope~start>_".to_string(), Entry::Scope);
    }

    /// end_scope of types and variables added in this scope
    fn end_scope(&mut self) {
        let mut possible_scope_type = self.types.pop();
        let mut found_scope_start = false;
        while possible_scope_type != None {
            if let Type::Scope = possible_scope_type.as_ref().unwrap().0.as_ref() {
                found_scope_start = true;
                break;
            }
            possible_scope_type = self.types.pop();
        }
        assert!(found_scope_start);
        found_scope_start = false;
        let mut possible_entry_scope_end = self.variables.pop();
        while possible_entry_scope_end != None {
            if let Entry::Scope = possible_entry_scope_end.as_ref().unwrap().0 {
                found_scope_start = true;
                break;
            }
            possible_entry_scope_end = self.variables.pop();
        }
        assert!(found_scope_start);
    }

    pub fn type_check_statement(
        &mut self,
        stmt: &'a mut Statement,
    ) -> Result<ExpressionType, String> {
        match stmt {
            Statement::Program(statements) => {
                self.begin_scope();
                for stmt in statements {
                    self.type_check_statement(&mut stmt.node)?;
                }
                self.end_scope();
                return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
            }
            Statement::Var(variable, expression, type_name) => {
                let variable_type = self.translation_expression(&mut expression.as_mut().node)?;
                let t = self
                    .unwrap_type_expr(type_name)
                    .ok_or("unknown error".to_string())?;
                if !t.equal_type(&variable_type.exp_type, &self) {
                    return Err(format!(
                        "unmatching types in variable declaration={:?}, {:?}",
                        t, variable_type.exp_type
                    ));
                }
                self.variables.add(
                    variable.to_string(),
                    Entry::new_variable(&variable_type.exp_type),
                );
            }

            Statement::Return(return_expression) => {
                return self.translation_expression(&mut return_expression.node);
            }

            Statement::ExpressionStatement(expr) => {
                // NOTE: Be careful on this, maybe we should return it
                self.translation_expression(&mut expr.node)?;
            }

            Statement::Type(name, type_expression) => {
                let ty = self
                    .unwrap_type_expr(type_expression)
                    .ok_or(format!("unknown type {}", name))?;
                self.types.add_ref(name, ty);
            }

            _ => unimplemented!(),
        }
        return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
    }

    fn get_void(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&"void".to_string()).unwrap().clone())
    }

    fn get_int(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&"int".to_string()).unwrap().clone())
    }

    fn get_string(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&"string".to_string()).unwrap().clone())
    }

    fn block(
        &mut self,
        statements: &'a mut Vec<NodeToken<Statement>>,
    ) -> Result<ExpressionType, String> {
        let mut expression_types: Vec<ExpressionType> = vec![];
        let mut type_atm: Option<Rc<Type>> = None;
        for statement in statements {
            let exp_type = self.type_check_statement(&mut statement.node)?;
            let last_type = exp_type.exp_type.clone();
            expression_types.push(exp_type);
            if last_type.as_ref() == &Type::Void {
                continue;
            }
            if let Some(current_type) = &type_atm {
                if !current_type.equal_type(&last_type, &self) {
                    return Err(format!(
                        "error: tried to return before a {:?}, but got here a {:?}",
                        current_type, last_type
                    ));
                }
            } else {
                type_atm = Some(last_type);
            }
        }

        Ok(ExpressionType::new(
            None,
            type_atm.unwrap_or(Rc::new(Type::Void)),
        ))
    }

    fn translation_expression(
        &mut self,
        expr: &'a mut Expression,
    ) -> Result<ExpressionType, String> {
        match expr {
            // Expression::Struct
            Expression::String(_) => {
                let string = self
                    .types
                    .get(&"string".to_string())
                    .expect("string type must be defined");
                Ok(ExpressionType::new(None, string.clone()))
            }

            Expression::Id(_) => {
                return self.translation_variable(expr);
            }

            Expression::Number(_) => {
                let int = self
                    .types
                    .get(&"int".to_string())
                    .expect("Integer type must be defined");
                Ok(ExpressionType::new(None, int.clone()))
            }

            Expression::Array(arr) => {
                let mut type_consensus: Option<ExpressionType> = None;
                for expr in arr {
                    let type_node = self.translation_expression(&mut expr.node)?;
                    if let Some(type_consensus) = &type_consensus {
                        if !type_consensus
                            .exp_type
                            .equal_type(&type_node.exp_type, &self)
                        {
                            return Err(format!(
                                "different types between array elements={:?}, {:?}",
                                type_consensus.exp_type, type_node.exp_type
                            ));
                        }
                    } else {
                        type_consensus = Some(type_node);
                    }
                }
                if let Some(type_consensus) = type_consensus {
                    Ok(ExpressionType::new(
                        None,
                        Rc::new(Type::Array(type_consensus.exp_type)),
                    ))
                } else {
                    Ok(ExpressionType::new(
                        None,
                        Rc::new(Type::Array(Rc::new(Type::Void))),
                    ))
                }
            }

            Expression::If {
                ref mut block,
                else_ifs,
                last_else,
                condition,
            } => {
                //
                self.begin_scope();

                let condition = self.translation_expression(&mut condition.node)?;
                assert_type!(condition.exp_type, &self.get_int().exp_type, &self);
                let mut expression_types = vec![];
                let type_checked_block = self.block(block)?;
                expression_types.push(type_checked_block);
                if let Some(else_ifs) = else_ifs {
                    for else_if in else_ifs {
                        let else_exp = self.translation_expression(&mut else_if.node)?;
                        assert_type!(else_exp.exp_type, &expression_types[0].exp_type, &self);
                        expression_types.push(else_exp);
                    }
                }
                let _ = if let Some(last_else) = last_else {
                    let b = self.block(last_else)?;
                    assert_type!(b.exp_type, &expression_types[0].exp_type, &self);
                    Some(b)
                } else {
                    None
                };
                self.end_scope();
                Ok(ExpressionType::new(
                    None,
                    expression_types
                        .pop()
                        .expect("expression types is empty")
                        .exp_type,
                ))
            }

            Expression::FunctionDefinition {
                parameters,
                types,
                return_type,
                ref mut block,
            } => {
                let mut has_none = false;
                let types_transformed = types
                    .iter()
                    .map(|t| {
                        let type_expr = self.unwrap_type_expr(t);
                        has_none = if has_none {
                            has_none
                        } else {
                            type_expr.is_none()
                        };
                        if has_none {
                            self.get_void().exp_type
                        } else {
                            type_expr.unwrap()
                        }
                    })
                    .collect::<Vec<Rc<Type>>>();
                let type_return = self
                    .unwrap_type_expr(return_type)
                    .ok_or("unknown type on return".to_string())?;
                self.begin_scope();
                // Declare parameters
                for (idx, parameter) in parameters.iter().enumerate() {
                    if let Expression::Id(var) = &parameter.node {
                        self.variables.add(
                            var.to_string(),
                            Entry::new_variable(&types_transformed[idx]),
                        );
                    }
                }
                let return_type_from_block = self.block(block)?;
                if !return_type_from_block
                    .exp_type
                    .equal_type(&type_return, &self)
                {
                    return Err(format!(
                        "return types differ on function: {:?} != {:?}",
                        return_type_from_block.exp_type, type_return
                    ));
                }
                self.end_scope();
                let function_type =
                    FunctionEntry::new(types_transformed, return_type_from_block.exp_type);
                Ok(ExpressionType::new(
                    None,
                    Rc::new(Type::Function(function_type)),
                ))
            }

            Expression::Assignment(name, expr) => {
                let expr_type = self.translation_expression(&mut expr.as_mut().node)?;
                let expr_to_assign = self.translation_expression(&mut name.as_mut().node)?;
                let type_var = expr_to_assign.exp_type;
                if !expr_type.exp_type.equal_type(&type_var, &self) {
                    Err(format!("unmatching type"))
                } else {
                    Ok(ExpressionType::new(None, type_var.clone()))
                }
            }

            Expression::BinaryOp(left, right, op) => {
                let left_type = self.translation_expression(&mut left.node)?;
                let right_type = self.translation_expression(&mut right.node)?;
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
                            return Err("Expected integer/string type on add operation".to_string());
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
        let code = "
            let variable: string = \"esto es una frase\";                                     
            variable = variable + \"damn\"; let numero: int = 4; numero = 4+3;
            let arr: [int] = [1,2,3];
            let string_combined: string = \"sss\" + \"www\" + \"qqq\";
            let v: [[[[[[string]]]]]] = [[], [[[[[\"string\"]]]]]];

            let func_00: (string) -> string = fn (x: string) -> string { return x; };
            let func_01: ([[[[string]]]], int) -> string = fn (x: [[[[string]]]], y: int) -> string { 
                let v: int = y + y + y + y + y + 1 + 2 + 3;                
                return \"cool\"; 
            };
            let func_02: () -> void = fn() { let thing: string = \"\"; };
            let func_03: (int) -> string = fn (x: int) -> string {
                let v: int = x + x;                
                return \"nice\";
            }
            v = [[], [[[[[\"string\"]]]]]];
        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }

    #[test]
    fn test_types_02() {
        let code = "
            let t: int = 1;
            let f: () -> int = fn() -> int {
                let variable: int = if t {
                        return t;
                    } else if t {
                        return 0;
                    } else {
                        return 5;
                    };
                return variable + 5;
            };
            return f;
        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }

    #[test]
    fn test_types_03() {
        let code = "
            type thing = int;
            type thing_02 = thing;
            type thing_03 = thing_02;
            type thing_04 = thing_03;
            type thing_05 = thing_04;
            type thing_06 = thing_05;
            type struct = {
                thing: thing;
                thing01: thing_02;
                thing3: [thing_02];
            }
            let wow: thing_05 = 3;
        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }
}
