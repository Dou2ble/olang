use crate::location::Region;
use strum::{Display, EnumDiscriminants};

#[derive(Debug, Clone)]
pub enum BinaryOperationOperator {
    Plus,                 // +
    Minus,                // -
    Multiply,             // *
    Divide,               // /
    Modulus,              // %
    Exponentiation,       // ^
    IsLessThan,           // <
    IsLessThanOrEqual,    // <=
    IsGreaterThan,        // >
    IsGreaterThanOrEqual, // >=
    IsEqual,              // ==
    IsNotEqual,           // !=
    LogicalAnd,           // &&
    LogicalOr,            // ||
}

#[derive(Debug, Clone)]
pub enum AssignmentOperator {
    Set,      // =
    Plus,     // +=
    Minus,    // -=
    Multiply, // *=
    Divide,   // /=
    Modulo,   // %=
}

#[derive(Debug, Clone)]
pub enum UpdateOperator {
    Increment, // ++
    Decremet,  // --
}

pub type Block = Vec<Expression>;

#[derive(Debug, Clone)]
pub struct DefinedFunction {
    pub parameters: Vec<String>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct IfClause {
    pub test: Box<Expression>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, EnumDiscriminants)]
#[strum_discriminants(derive(Display))]
pub enum ExpressionValue {
    Int(i64),
    String(String),
    Bool(bool),
    Null,
    List(Vec<Expression>),
    Block(Block),
    Identifier(String),
    Binary {
        left: Box<Expression>,
        operator: BinaryOperationOperator,
        right: Box<Expression>,
    },
    VariableDeclaration {
        identifier: String,
        expression: Box<Expression>,
    },
    Assign {
        identifier: String,
        operator: AssignmentOperator,
        expression: Box<Expression>,
    },
    Update {
        identifier: String,
        operator: UpdateOperator,
    },
    Function(DefinedFunction),
    Call {
        identifier: String,
        arguments: Vec<Expression>,
    },
    Index {
        identifier: String,
        indexes: Vec<Expression>,
    },
    If {
        clauses: Vec<IfClause>,
        else_block: Option<Block>,
    },
    Loop {
        init: Option<Box<Expression>>,
        test: Option<Box<Expression>>,
        update: Option<Box<Expression>>,
        body: Block,
    },
    Continue,
    Break,
    Throw(String),
}

#[derive(Clone, Debug)]
pub struct Expression {
    pub region: Region,
    pub value: ExpressionValue,
}

#[derive(Debug)]
pub struct Program {
    pub ast: Vec<Expression>,
}
