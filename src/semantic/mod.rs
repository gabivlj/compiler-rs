#![allow(dead_code)]

use crate::hash_undo::HashUndo;
use crate::string_interning::StringId;
use crate::{
    ast::node::{Expression, NodeToken, OpType, Statement, TypeExpr},
    string_interning::StringInternal,
};
use std::{rc::Rc, unimplemented};

#[derive(Copy, Clone, PartialEq, Debug)]
enum Flag {
    /// If this bit is set it means that the processed self.type_check_statement
    /// will return *always*, so in the self.block type checking needs to check that
    /// this bit is set to assure the caller that the type is correct
    StatementIsSecureToReturn = 0,
}

type ExpressionAndSecureToReturn = (ExpressionType, bool);

#[derive(Copy, Clone, PartialEq, Debug)]
enum Bit {
    One = 1,
    Zero = 0,
}

impl Flag {
    fn set_flag(self, bits: u64, bit: Bit) -> u64 {
        if let Bit::One = bit {
            let b = bits | (1 << (self as u64));
            assert!(self.is_active(b));
            b
        } else {
            let b = bits & (0 << (self as u64));
            assert!(!self.is_active(b));
            b
        }
    }

    fn is_active(self, bits: u64) -> bool {
        return (bits & ((1 as u64) << (self as u64))) == 1;
    }
}

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

// macro_rules! block {
//     ($self:expr) => {
//         let ___flag = $self.flags;
//     };
// }

// macro_rules! recover_flag {
//     ($self:expr) => {
//         $self.flags = ___flag;
//     };
// }

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
    Record(Vec<(StringId, Rc<Type>)>),

    /// Function type
    Function(FunctionEntry),

    /// Array type
    Array(Rc<Type>),

    /// Null type
    Nil,

    /// Name type is for recursive types (Rc<Type> will be later filled)
    /// *This behaviour isn't implemented yet.*
    Name(StringId),
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
pub struct SemanticAnalysis<'a> {
    /// Types hashmap where we relate the name of a type to it's recursive type
    types: HashUndo<'a, StringId, Rc<Type>>,

    /// Variables maps a string to its type
    variables: HashUndo<'a, StringId, Entry>,

    flags: u64,
}

/// Intermediate Representation *unimplemented*
#[derive(Copy, Clone, Debug)]
struct Translation;

/// Expression type (atm the only translation thing) contains the type of an expression
/// and the possible IR.
pub struct ExpressionType {
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
        if let Type::Nil = self.as_ref() {
            return true;
        }

        if let Type::Nil = other.as_ref() {
            return true;
        }

        if let Type::Name(name) = self.as_ref() {
            return other.equal_type(semantic.types.get(name).unwrap(), semantic);
        }

        if let Type::Name(name) = other.as_ref() {
            return self.equal_type(semantic.types.get(name).unwrap(), semantic);
        }

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
            flags: 0x0,
        };
        me.types.add(string_id!("int"), Rc::new(Type::Int));
        me.types.add(string_id!("string"), Rc::new(Type::String));
        me.types.add(string_id!("void"), Rc::new(Type::Void));
        me.variables
            .add(string_id!("nil"), Entry::Variable(Rc::new(Type::Nil)));
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
                let entry = self.variables.get(identifier);
                if let Some(entry) = entry {
                    if let Entry::Variable(type_var) = entry {
                        return Ok(ExpressionType::new(None, self.actual_type(type_var)));
                    }
                }
                return Err(format!("undefined variable={}", identifier));
            }
            _ => unimplemented!(),
        }
    }

    /// unwraps a Type::Name type, else just clones the type
    fn actual_type(&self, maybe_a_name: &Rc<Type>) -> Rc<Type> {
        if let Type::Name(underlying_type_name) = maybe_a_name.as_ref() {
            return self.types.get(&underlying_type_name).unwrap().clone();
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

                let this_type = Rc::new(Type::Record(attributes_parsed));
                Some(this_type)
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
                        self.types.get(&string_id!("void")).map(|t| t.clone())
                    } else {
                        self.unwrap_type_expr(return_type.as_ref().expect("it's not none"))
                    }?;
                    Some(Rc::new(Type::Function(FunctionEntry::new(
                        types,
                        return_type_unwrapped,
                    ))))
                }
            }
        }
    }

    /// begin_scope variables
    fn begin_scope(&mut self) {
        self.types
            .add(string_id!("@_<scope~start>_"), Rc::new(Type::Scope));
        self.variables
            .add(string_id!("@_<scope~start>_"), Entry::Scope);
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

    /// type checks statemnet
    /// Flags it uses: StatementIsSecureToReturn
    /// Save a copy of the flags
    pub fn type_check_statement(
        &mut self,
        stmt: &'a mut Statement,
    ) -> Result<ExpressionType, String> {
        self.flags = Flag::StatementIsSecureToReturn.set_flag(self.flags, Bit::One);
        match stmt {
            Statement::Program(statements) => {
                self.begin_scope();
                for stmt in statements {
                    let f = self.flags;
                    self.type_check_statement(&mut stmt.node)?;
                    self.flags = f;
                }
                self.end_scope();
                return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
            }

            Statement::While(condition, block) => {
                let condition = self.translation_expression(&mut condition.node)?;
                // if condition_type.
                assert_type!(condition.exp_type, &self.get_int().exp_type, &self);
                let f = self.flags;
                let (type_checked_block, _) = self.block(block)?;
                self.flags = f;
                self.flags = Flag::StatementIsSecureToReturn.set_flag(self.flags, Bit::Zero);
                return Ok(ExpressionType::new(None, type_checked_block.exp_type));
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
                self.variables
                    .add(*variable, Entry::new_variable(&variable_type.exp_type));
            }

            Statement::Return(return_expression) => {
                return self.translation_expression(&mut return_expression.node);
            }

            Statement::ExpressionStatement(expr) => {
                // NOTE: Be careful on this, maybe we should return it
                let (t, ok_to_return) = self.type_check_if_statement(&mut expr.node)?;
                if !ok_to_return {
                    self.flags = Flag::StatementIsSecureToReturn.set_flag(self.flags, Bit::Zero);
                }
                return Ok(t);
            }

            Statement::Type(name, type_expression) => {
                self.begin_scope();
                self.types.add_ref(name, Rc::new(Type::Name(name.clone())));
                let ty = self
                    .unwrap_type_expr(type_expression)
                    .ok_or(format!("unknown type {}", name))?;
                self.end_scope();
                self.types.add_ref(name, ty);
            }

            _ => unimplemented!(),
        }
        return Ok(ExpressionType::new(None, Rc::new(Type::Void)));
    }

    fn get_void(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&string_id!("void")).unwrap().clone())
    }

    fn get_int(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&string_id!("int")).unwrap().clone())
    }

    fn get_string(&self) -> ExpressionType {
        ExpressionType::new(None, self.types.get(&string_id!("string")).unwrap().clone())
    }

    fn handle_else_if(
        &mut self,
        if_expr: &'a mut Expression,
    ) -> Result<ExpressionAndSecureToReturn, String> {
        if let Expression::If {
            ref mut block,
            else_ifs: _,
            last_else: _,
            condition,
        } = if_expr
        {
            let condition = self.translation_expression(&mut condition.node)?;
            assert_type!(condition.exp_type, &self.get_int().exp_type, &self);
            self.block(block)
        } else {
            Err(format!("not an else if"))
        }
    }

    /// Handles an if expression that is an statement by itself
    /// If it's not an if it just returns self.translate_expression
    /// ## Rules
    /// * If there is a branch that returns Void this function can return Ok and false (false means that this statement is not reliable to end here)
    /// * If there is a branch that returns differing types (there can be a differing type like void and int but not int string), this will return an error
    /// * If there is not a branch that returns a differing type and also doesn't return Void, this will return Ok and true
    fn type_check_if_statement(
        &mut self,
        if_expr: &'a mut Expression,
    ) -> Result<ExpressionAndSecureToReturn, String> {
        if let Expression::If {
            ref mut block,
            else_ifs,
            last_else,
            condition,
        } = if_expr
        {
            self.begin_scope();
            let condition = self.translation_expression(&mut condition.node)?;
            assert_type!(condition.exp_type, &self.get_int().exp_type, &self);
            let mut expression_types = vec![];
            let f = self.flags;
            let (type_checked_block, reliable_to_return_if) = self.block(block)?;
            self.flags = f;
            expression_types.push(type_checked_block);
            let mut reliable_to_return = reliable_to_return_if;
            if let Some(else_ifs) = else_ifs {
                for else_if in else_ifs {
                    let (else_exp, reliable_to_return_else) =
                        self.handle_else_if(&mut else_if.node)?;
                    if let Type::Void = else_exp.exp_type.as_ref() {
                        // 1st rule applies => This if is unreliable to return
                        reliable_to_return = false;
                    } else {
                        if !reliable_to_return_else {
                            reliable_to_return = false;
                        }
                        assert_type!(else_exp.exp_type, &expression_types[0].exp_type, &self);
                    }
                    expression_types.push(else_exp);
                }
            }
            let _ = if let Some(last_else) = last_else {
                let (b, reliable_to_return_else) = self.block(last_else)?;
                // println!("{:?}", b.exp_type);
                if let Type::Void = b.exp_type.as_ref() {
                    // panic!("UNREALIABLE TO RETURN");
                    // 1st rule applies => This if is unreliable to return
                    reliable_to_return = false;
                } else {
                    if !reliable_to_return_else {
                        reliable_to_return = false;
                    }
                    assert_type!(b.exp_type, &expression_types[0].exp_type, &self);
                }
                Some(b)
            } else {
                // 1st rule applies => This if is unreliable to return
                reliable_to_return = false;
                None
            };
            self.end_scope();
            Ok((
                ExpressionType::new(
                    None,
                    expression_types
                        .pop()
                        .expect("expression types is empty")
                        .exp_type,
                ),
                reliable_to_return,
            ))
        } else {
            self.translation_expression(if_expr).map(|res| (res, false))
        }
    }

    /// Parses a list of statements
    /// ### Rules
    /// * If there is no return in the list of statements this will return a ExpressionType with a type of `void`
    /// * If there is always a path that is gonna return, this will return the correct type
    fn block(
        &mut self,
        statements: &'a mut Vec<NodeToken<Statement>>,
    ) -> Result<ExpressionAndSecureToReturn, String> {
        let mut expression_types: Vec<ExpressionType> = vec![];
        let mut type_atm: Option<Rc<Type>> = None;
        if statements.len() == 0 {
            return Ok((ExpressionType::new(None, Rc::new(Type::Void)), false));
        }
        let mut secure_to_return: bool = false;
        for statement in statements {
            let f = self.flags;
            let exp_type = self.type_check_statement(&mut statement.node)?;
            let last_type = exp_type.exp_type.clone();
            expression_types.push(exp_type);
            if last_type.as_ref() == &Type::Void {
                self.flags = f;
                continue;
            }
            if Flag::StatementIsSecureToReturn.is_active(self.flags) {
                secure_to_return = true;
            }
            self.flags = f;
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
        if type_atm.is_some() {
            Ok((
                ExpressionType::new(None, type_atm.unwrap()),
                secure_to_return,
            ))
        } else {
            Ok((
                ExpressionType::new(None, Rc::new(Type::Void)),
                secure_to_return,
            ))
        }
    }

    fn structs_are_equal(&mut self, the_true_type: &Type, right: &Type) -> bool {
        if let Type::Nil = the_true_type {
            return true;
        }
        if let Type::Nil = right {
            return true;
        }
        if let Type::Record(the_true_type) = the_true_type {
            if let Type::Record(right) = right {
                if the_true_type.len() != right.len() {
                    return false;
                }
                for i in 0..the_true_type.len() {
                    let ty = self.get_type_of_struct(the_true_type[i].0, right);
                    if let Some(ty) = ty {
                        if !the_true_type[i].1.equal_type(ty, self) {
                            return false;
                        }
                    } else {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    fn get_type_of_struct<'b>(
        &mut self,
        key: StringId,
        record: &'b Vec<(StringId, Rc<Type>)>,
    ) -> Option<&'b Rc<Type>> {
        // let key = key.as_ref();
        for pair in record {
            if pair.0 == key {
                return Some(&pair.1);
            }
        }
        None
    }

    fn handle_bin_op(
        &mut self,
        left: &'a mut Expression,
        right: &'a mut Expression,
        operation: OpType,
    ) -> Result<ExpressionType, String> {
        match operation {
            OpType::Add => {
                // We do the translation here because we might refactor this to other behaviours
                // also, if we translate above the `match` we encounter a borrow checker error
                // on the error case
                let translation_left = self.translation_expression(left)?;
                let translation_right = self.translation_expression(right)?;
                assert_type!(translation_left.exp_type, &translation_right.exp_type, self);
                let type_of_thing = translation_left.exp_type.clone();
                // we do this ifs because later on we will include
                // custom behaviour for expression
                // (or create a function on IR that does this)
                if let Type::String = type_of_thing.as_ref() {
                    Ok(ExpressionType::new(None, translation_left.exp_type))
                } else if let Type::Int = type_of_thing.as_ref() {
                    Ok(ExpressionType::new(None, translation_left.exp_type))
                } else if let Type::Array(_) = type_of_thing.as_ref() {
                    Ok(ExpressionType::new(None, translation_left.exp_type))
                } else {
                    Err(format!(
                        "can't operate using expression of type {:?}",
                        type_of_thing
                    ))
                }
            }

            OpType::Equal | OpType::NotEqual => {
                // This should be changed
                let translation_left = self.translation_expression(left)?;
                let translation_right = self.translation_expression(right)?;
                assert_type!(translation_left.exp_type, &translation_right.exp_type, self);
                Ok(ExpressionType::new(
                    None,
                    self.types.get(&string_id!("int")).unwrap().clone(),
                ))
            }

            OpType::And
            | OpType::Or
            | OpType::Divide
            | OpType::Multiply
            | OpType::Substract
            | OpType::GreaterThan
            | OpType::LessThan => {
                let translation_left = self.translation_expression(left)?;
                let translation_right = self.translation_expression(right)?;
                assert_type!(translation_left.exp_type, &translation_right.exp_type, self);
                assert_type!(
                    translation_left.exp_type,
                    &self.types.get(&string_id!("int")).unwrap(),
                    self
                );
                Ok(ExpressionType::new(None, translation_left.exp_type))
            }

            _ => Err(format!(
                "unimplemented operation {} {} {:?}",
                left.string(),
                right.string(),
                operation
            )),
        }
    }

    fn translate_bin_op(
        &mut self,
        operation: &'a mut Expression,
    ) -> Result<ExpressionType, String> {
        if let Expression::BinaryOp(left, right, op_type) = operation {
            self.handle_bin_op(&mut left.node, &mut right.node, *op_type)
        } else {
            Err(format!(
                "expression {} is not a binary operation",
                operation.string()
            ))
        }
    }

    fn translation_expression(
        &mut self,
        expr: &'a mut Expression,
    ) -> Result<ExpressionType, String> {
        match expr {
            Expression::String(_) => {
                let string = self
                    .types
                    .get(&string_id!("string"))
                    .expect("string type must be defined");
                Ok(ExpressionType::new(None, string.clone()))
            }

            Expression::Id(_) => {
                return self.translation_variable(expr);
            }

            Expression::Number(_) => {
                let int = self
                    .types
                    .get(&string_id!("int"))
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
                self.begin_scope();
                let condition = self.translation_expression(&mut condition.node)?;
                assert_type!(condition.exp_type, &self.get_int().exp_type, &self);
                let mut expression_types = vec![];
                let (type_checked_block, reliable_to_return) = self.block(block)?;
                if !reliable_to_return {
                    return Err(format!(
                        "an expression if should have a branch that returns always a type"
                    ));
                }
                expression_types.push(type_checked_block);
                if let Some(else_ifs) = else_ifs {
                    for else_if in else_ifs {
                        let (else_exp, reliable_to_return) =
                            self.handle_else_if(&mut else_if.node)?;
                        if !reliable_to_return {
                            return Err(format!(
                                "an expression if should have a branch that returns always a type"
                            ));
                        }
                        assert_type!(else_exp.exp_type, &expression_types[0].exp_type, &self);
                        expression_types.push(else_exp);
                    }
                }
                let _ = if let Some(last_else) = last_else {
                    let (b, reliable_to_return) = self.block(last_else)?;
                    if !reliable_to_return {
                        return Err(format!(
                            "an expression if should have a branch that returns always a type"
                        ));
                    }
                    assert_type!(b.exp_type, &expression_types[0].exp_type, &self);
                    Some(b)
                } else {
                    return Err(format!(
                        "an expression if should have a branch that returns always a type"
                    ));
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
                        self.variables
                            .add(*var, Entry::new_variable(&types_transformed[idx]));
                    }
                }
                let (return_type_from_block, reliable_to_return) = self.block(block)?;
                if !reliable_to_return && type_return.as_ref() != &Type::Void {
                    return Err(format!("a branch of the function doesn't return a type, make sure that you always return"));
                }
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

            Expression::TypeInit(type_name, pairs) => {
                let type_to_check = self.types.get(type_name).ok_or("unknown type")?.clone();
                if let Type::Record(type_struct) = type_to_check.as_ref() {
                    let mut pair_types: Vec<(StringId, Rc<Type>)> = vec![];
                    for pair in pairs {
                        let ty = self.translation_expression(&mut pair.1.node)?;
                        let true_type = self.get_type_of_struct(pair.0, type_struct);
                        if true_type.is_none() {
                            return Err(format!(
                                "unknown type in struct {}, `{}`",
                                type_name, pair.0
                            ));
                        }
                        let true_type = true_type.unwrap();
                        if !ty.exp_type.equal_type(true_type, &self) {
                            return Err(format!(
                                "expected type {:?}, got={:?}",
                                true_type, ty.exp_type
                            ));
                        }
                        pair_types.push((pair.0, ty.exp_type));
                    }
                    let struct_type = Type::Record(pair_types);
                    if !self.structs_are_equal(&type_to_check, &struct_type) {
                        return Err(format!(
                            "bad construction of type {}, expected {:?} and got {:?}",
                            type_name, type_to_check, struct_type
                        ));
                    }
                    Ok(ExpressionType::new(None, self.actual_type(&type_to_check)))
                } else {
                    return Err(format!(
                        "type constructor {} is not a record or struct",
                        type_name
                    ));
                }
            }

            Expression::BinaryOp(_, _, _) => self.translate_bin_op(expr),

            Expression::Call(left, parameters) => {
                let type_function = self.translation_expression(&mut left.node)?;
                if let Type::Function(entry_fn) = type_function.exp_type.as_ref() {
                    for (i, parameter) in parameters.iter_mut().enumerate() {
                        let type_parameter = self.translation_expression(&mut parameter.node)?;
                        if !type_parameter
                            .exp_type
                            .equal_type(&entry_fn.formals[i], self)
                        {
                            return Err(format!(
                                "expected type for parameter {} => {:?}, got={:?}",
                                i + 1,
                                entry_fn.formals[i],
                                type_parameter.exp_type
                            ));
                        }
                    }
                    return Ok(ExpressionType::new(
                        None,
                        self.actual_type(&entry_fn.result),
                    ));
                }
                return Err(format!(
                    "can't call a non function variable: {:?}",
                    type_function.exp_type
                ));
            }

            Expression::Boolean(_) => Ok(ExpressionType::new(None, Rc::new(Type::Int))),

            Expression::IndexAccess(possible_array, index) => {
                let possible_array = self.translation_expression(&mut possible_array.node)?;
                let index = self.translation_expression(&mut index.node)?;
                if let Type::Array(inner) = possible_array.exp_type.as_ref() {
                    assert_type!(
                        index.exp_type,
                        self.types.get(&string_id!("int")).unwrap(),
                        self
                    );
                    Ok(ExpressionType::new(None, self.actual_type(inner)))
                } else {
                    Err(format!(
                        "expected array type on index access, got={:?}",
                        possible_array.exp_type
                    ))
                }
            }

            Expression::PropertyAccess(left, right) => self.property_access(left, right),

            Expression::PrefixOp(_op_type, _expr) => {
                //
                unimplemented!()
            }
        }
    }

    /// recursive function that unwraps a property access until the end
    fn property_access(
        &mut self,
        left: &'a mut NodeToken<Expression>,
        right: &'a mut NodeToken<Expression>,
    ) -> Result<ExpressionType, String> {
        // This is really bad, because we must allocate here the error
        let err = Err(format!(
            "can't access {} with property {}",
            left.node.string(),
            right.node.string()
        ));
        // because the node is borrowed here                vv
        let possible_struct = self.translation_expression(&mut left.node)?;
        if let Type::Record(types) = possible_struct.exp_type.as_ref() {
            let str = if let Expression::PropertyAccess(left, _) = &right.node {
                if let Expression::Id(s) = &left.node {
                    *s
                } else {
                    unreachable!()
                }
            } else if let Expression::Id(s) = &right.node {
                *s
            } else {
                unreachable!();
            };
            if let Some((_ident, the_type)) = types.iter().find(|el| el.0 == str) {
                if let Expression::PropertyAccess(left, right) = &mut right.node {
                    return self.property_access(left, right);
                } else {
                    return Ok(ExpressionType::new(None, self.actual_type(the_type)));
                }
            } else {
                return Err(format!("unknown property {}", str));
            }
        }
        // how do we borrow here then?
        err
    }
}

mod test {
    #[allow(unused_imports)]
    use crate::ast;
    #[allow(unused_imports)]
    use crate::lexer;
    #[allow(unused_imports)]
    use crate::parser;
    #[allow(unused_imports)]
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
        let _thing = analysis
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
            let expects_integer: int = f();
            let more_complex_fn: (string, string) -> string = fn(s: string, s2: string) -> string {
                return s + s2;
            };
            let expects_string: string = more_complex_fn(\"sss\", \"sss\");  
            return f;
        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let _thing = analysis
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
            let wow: thing_05 = 3;
            type a_struct = {
                thing: thing;
                thing01: thing_02;
                thing3: [thing_02];
            }            
            type b_struct = {
                thing: thing;
                thing01: thing_02;
                thing3: [thing_02];
                a_struct: a_struct;
            };
            let a_struct_init: a_struct = a_struct -> {
                thing=1,
                thing01=1,
                thing3=[1]
            };
            let thingy: thing = a_struct_init.thing;
            let a_struct_init: b_struct = b_struct -> {
                thing=1,
                thing01=1,
                thing3=[1],
                a_struct=a_struct_init
            };
            let thingy: thing = a_struct_init.a_struct.thing;
            let thingy: a_struct = a_struct_init.a_struct;
            let thingy: thing = thingy.thing;            
            let thingy_02: thing_02 = a_struct_init.a_struct.thing3[0];
            let null: [string] = nil;
            null = [\"ww\"];
            type recursive = {
                a: recursive;b: int;
            };
            let thing: recursive = recursive -> { a = recursive -> {a = null, b = 3}, b = 2};
            if 1 + 1 > 1 && \"ss\" + \"sss\" != \"ssz\" {
            }
            for 1 == 1 || 1 {

            }

            type struct_thing = {
                a: int;
            };
            type struct_things = [struct_thing];
            let things: struct_things = [struct_thing -> {a=1}];
            let thing: int = things[0].a;

            type e = {
                e: e;
                b: [[[e]]];
                c: int;
            };

            let deep:e = e -> {
                e=null,
                b=null,
                c=1
            };

            let more_e: e = deep.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e;
            let more_e: e = more_e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e;
            let finally: int = more_e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.e.b[0][0][0].e.e.e.e.e.b[0][0][121212].c;

        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let _thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }

    //
    // --->
    //

    // TODO: this should pass
    #[test]
    fn test_types_04() {
        let code = "
            let func_03: (int) -> string = fn (x: int) -> string {
                if true {
                    if 1 { 
                        if 1 {
                            return \"s\";
                        }
                    }
                }
                let v: int = x + x;
                if (v > 3) {
                    return \"s\";
                } else if true {} else {                    
                    return \"s\";
                }
                return \"s\";
            }
            let func_04: (int) -> string = fn (x: int) -> string {
                let v: int = x + x;
                if (v > 3) {
                    for true {
                        if true {
                            return \"ss\";
                        } else {
                            return \"ss\";
                        }
                    }                    
                    return \"ss\";
                } else {
                    if true {
                        return if true { return \"sss\"; } else { return \"ss\"; };
                    } else if false {
                        return \"ss\"; 
                    } else {
                        if true {
                            return \"ss\"; 
                        } else {
                            if true {
                                return \"ss\";
                            } else {
                                if true {
                                    return \"ss\";
                                } else if false { return \"s\"; } else {
                                    return \"ss\";
                                }
                                return \"ss\";
                            }                            
                            return \"ss\";
                        }
                        return \"ss\";
                    }
                }
            }
        ";
        let mut program = parser::Parser::new(lexer::Lexer::new(code))
            .parse_program()
            .expect("parsing to go well");
        let mut analysis = semantic::SemanticAnalysis::new();
        let _thing = analysis
            .type_check_statement(&mut program.node)
            .expect("not an error");
    }
}

/*
let func_03: (int) -> string = fn (x: int) -> string {
          let v: int = x + x;
          if (v > 3) {
              return \"s\";
          } else {

          }
          return \"s\";
      }*/
