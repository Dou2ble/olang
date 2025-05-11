use crate::{location::Region, types::Type};
use strum::{Display, EnumDiscriminants};

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssignmentOperator {
    Set,      // =
    Plus,     // +=
    Minus,    // -=
    Multiply, // *=
    Divide,   // /=
    Modulo,   // %=
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UpdateOperator {
    Increment, // ++
    Decremet,  // --
}

pub type Block = Vec<Expression>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Parameter {
    pub id: String,
    pub _type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DefinedFunction {
    pub parameters: Vec<Parameter>,
    pub body: Block,
    pub return_type: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IfClause {
    pub test: Box<Expression>,
    pub body: Vec<Expression>,
}

#[derive(Debug, Clone, EnumDiscriminants, PartialEq, Eq)]
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
        variable_type: Option<Type>,
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
        left: Box<Expression>,
        index: Box<Expression>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    pub region: Region,
    pub value: ExpressionValue,
}

#[derive(Debug)]
pub struct Program {
    pub ast: Vec<Expression>,
}
