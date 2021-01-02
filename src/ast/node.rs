use std::boxed::Box;

/// NodeRef is a heap allocated AST node
#[derive(Debug, Clone)]
pub struct NodeRef {
    pub node: Box<Node>,
}

impl NodeRef {
    pub fn new(node: Node) -> Self {
        Self {
            node: Box::new(node),
        }
    }
}

#[derive(Debug, Clone)]
/// Node represents a AST node. We intend this node to fill 32 bytes in memory maximum everytime
pub enum Node {
    /// We'll only have u64 values atm
    Number(u64),

    /// Multiply contains 2 expressions that will be operated by a multiplication
    Multiply(NodeRef, NodeRef),

    /// Id is an identifier
    /// `let f = 0;` would create an identifier called f
    Id(String),

    /// Not is a bang operator like `!true`
    Not(NodeRef),

    /// Greater is a op like `>`
    Greater(NodeRef, NodeRef),

    /// Less is a less than operator like `<`
    Less(NodeRef, NodeRef),

    /// Equal is a binary comparison between two expressions
    Equal(NodeRef, NodeRef),

    /// NotEqual is a binary comparison between two expressions
    NotEqual(NodeRef, NodeRef),

    /// Divide is a binary operation between two expressions that will be operated by a division
    Divide(NodeRef, NodeRef),

    /// Add is a binary operation between two expressions that will be operated by an addition
    Add(NodeRef, NodeRef),

    /// Substract is a binary operation between two expressions that will be operated by a substraction
    Substract(NodeRef, NodeRef),

    /// Call is a function call like `f(x)`
    /// - First value is the callee
    /// - Second value are the arguments
    Call(NodeRef, Vec<NodeRef>),

    /// Return represents a return statement
    Return(NodeRef),

    /// Block refers to code delimited by curly braces, usually these Nodes are statements
    Block(Vec<NodeRef>),

    /// If refers to an if node which has 3 branches
    /// - A conditional
    /// - A consequence
    /// - An alternative
    If(NodeRef, NodeRef, Option<Vec<NodeRef>>),

    /// FunctionDefinition contains the name of the function,
    /// the parameter list and the Block node
    FunctionDefinition(String, Vec<String>, NodeRef),

    /// Var contains the variable name and the expression value that is assigned to
    Var(String, NodeRef),

    /// Almost equal to Var, but instead will override the already declared variable
    Assignment(String, NodeRef),

    /// While contains the conditional expr that keeps the loop going and
    /// the block of code
    While(NodeRef, NodeRef),
}

/// This default impl. is for or_default() method in the parser combinator
impl std::default::Default for Node {
    fn default() -> Node {
        Node::Number(0)
    }
}

/// This default impl. is for or_default() method in the parser combinator
impl std::default::Default for NodeRef {
    fn default() -> NodeRef {
        NodeRef::new(Node::Number(0))
    }
}

/// BinaryOp is an utility type for turning binary ast ops into their inner references
pub type BinaryOp<'a> = (&'a NodeRef, &'a NodeRef);

/// InnerNode, similar to BinaryOp, returns the inner reference to the node
pub type InnerNode<'a> = &'a NodeRef;

/// IdString is the inner identifier of the Id AST node
pub type IdString<'a> = &'a String;

/// Node into NodeRef constructors for readibility

impl Node {
    pub fn new_number(number: u64) -> NodeRef {
        NodeRef::new(Node::Number(number))
    }

    pub fn new_identifier(identifier: String) -> NodeRef {
        NodeRef::new(Node::Id(identifier))
    }

    pub fn new_not(expr: NodeRef) -> NodeRef {
        NodeRef::new(Node::Not(expr))
    }

    pub fn new_return(expr: NodeRef) -> NodeRef {
        NodeRef::new(Node::Return(expr))
    }

    pub fn new_equal(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::Equal(left, right))
    }

    pub fn new_not_equal(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::NotEqual(left, right))
    }

    pub fn new_add(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::Add(left, right))
    }

    pub fn new_divide(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::Divide(left, right))
    }

    pub fn new_substract(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::Substract(left, right))
    }

    pub fn new_multiplication(left: NodeRef, right: NodeRef) -> NodeRef {
        NodeRef::new(Node::Multiply(left, right))
    }

    pub fn new_call(callee: NodeRef, args: Vec<NodeRef>) -> NodeRef {
        NodeRef::new(Node::Call(callee, args))
    }

    pub fn new_block(code: Vec<NodeRef>) -> NodeRef {
        NodeRef::new(Node::Block(code))
    }

    pub fn new_while(condition: NodeRef, block: NodeRef) -> NodeRef {
        NodeRef::new(Node::While(condition, block))
    }

    pub fn new_if(
        condition: NodeRef,
        consequence: NodeRef,
        alternatives: Option<Vec<NodeRef>>,
    ) -> NodeRef {
        NodeRef::new(Node::If(condition, consequence, alternatives))
    }

    pub fn new_fn_def(name: String, params: Vec<String>, block: NodeRef) -> NodeRef {
        NodeRef::new(Node::FunctionDefinition(name, params, block))
    }

    pub fn new_var_def(name: String, expr: NodeRef) -> NodeRef {
        NodeRef::new(Node::Var(name, expr))
    }

    pub fn new_assignment(name: String, expr: NodeRef) -> NodeRef {
        NodeRef::new(Node::Assignment(name, expr))
    }
}

impl<'a> Into<Option<u64>> for &'a Node {
    fn into(self) -> Option<u64> {
        if let Node::Number(n) = self {
            return Some(*n);
        }
        None
    }
}

impl<'a> Into<Option<BinaryOp<'a>>> for &'a Node {
    fn into(self) -> Option<BinaryOp<'a>> {
        match self {
            Node::Multiply(left, right) => Some((left, right)),
            Node::Equal(left, right) => Some((left, right)),
            Node::NotEqual(left, right) => Some((left, right)),
            Node::Substract(left, right) => Some((left, right)),
            Node::Add(left, right) => Some((left, right)),
            Node::Divide(left, right) => Some((left, right)),
            _ => None,
        }
    }
}

impl<'a> Into<Option<IdString<'a>>> for &'a Node {
    fn into(self) -> Option<IdString<'a>> {
        match self {
            Node::Id(string) => Some(string),
            _ => None,
        }
    }
}

impl<'a> Into<Option<InnerNode<'a>>> for &'a Node {
    fn into(self) -> Option<InnerNode<'a>> {
        match self {
            Node::Not(inner) => Some(inner),
            _ => None,
        }
    }
}

impl<'a> Into<Option<&'a Vec<NodeRef>>> for &'a Node {
    fn into(self) -> Option<&'a Vec<NodeRef>> {
        match self {
            Node::Block(inner) => Some(inner),
            _ => None,
        }
    }
}

pub struct ASTConstructorSingleExpr {
    pub constructor: fn(NodeRef) -> NodeRef,
}

pub fn default(n: NodeRef) -> NodeRef {
    NodeRef::new(Node::default())
}

impl std::default::Default for ASTConstructorSingleExpr {
    fn default() -> Self {
        Self {
            constructor: default,
        }
    }
}

impl ASTConstructorSingleExpr {
    pub fn new(n: fn(NodeRef) -> NodeRef) -> Self {
        Self { constructor: n }
    }
}

pub struct ASTConstructorDoubleExpr {
    pub constructor: fn(NodeRef, NodeRef) -> NodeRef,
}

pub fn default_double(n: NodeRef, n1: NodeRef) -> NodeRef {
    NodeRef::new(Node::default())
}

impl std::default::Default for ASTConstructorDoubleExpr {
    fn default() -> Self {
        Self {
            constructor: default_double,
        }
    }
}

impl ASTConstructorDoubleExpr {
    pub fn new(n: fn(NodeRef, NodeRef) -> NodeRef) -> Self {
        Self { constructor: n }
    }
}

macro_rules! str {
    () => {
        String::new()
    };
    ($x:expr) => {
        ToString::to_string(&$x)
    };
}

mod test {
    use super::{BinaryOp, IdString, Node};

    #[test]
    fn check_number_mul() {
        let multiplication = Node::new_multiplication(Node::new_number(1), Node::new_number(2));
        let binary_op: Option<BinaryOp> = multiplication.node.as_ref().into();
        let (left, right) = binary_op.expect("to be a binary op");
        let l: Option<u64> = left.node.as_ref().into();
        let r: Option<u64> = right.node.as_ref().into();
        assert_eq!(l.unwrap(), 1);
        assert_eq!(r.unwrap(), 2);
    }

    #[test]
    fn check_id() {
        let id = Node::new_identifier(str!("identifier"));
        let identifier: Option<IdString> = id.node.as_ref().into();
        assert_eq!(
            identifier.expect("to be an identifier"),
            &str!("identifier")
        );
    }

    #[test]
    fn check_call() {
        let call = Node::new_call(
            Node::new_identifier(str!("f")),
            vec![Node::new_number(1), Node::new_number(2)],
        );
        if let Node::Call(callee, args) = call.node.as_ref() {
            let id: Option<IdString> = callee.node.as_ref().into();
            assert_eq!(id.unwrap(), &str!("f"));
            let left: Option<u64> = args[0].node.as_ref().into();
            let right: Option<u64> = args[1].node.as_ref().into();
            assert_eq!(left.unwrap(), 1);
            assert_eq!(right.unwrap(), 2);
        }
    }
}
