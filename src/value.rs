use std::fmt::{self};

use crate::{ast::DefinedFunction, location::Region};
use strum::Display;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Function {
    Defined(DefinedFunction),
    Builtin(fn(Vec<Value>, region: &Region) -> Result<Value, ControlFlowValue>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Function(Function),
    String(String),
    Int(i64),
    Bool(bool),
    List(Vec<Value>),
    Null,
}

#[derive(Debug, Display, PartialEq, Eq)]
pub enum ExceptionKind {
    WrongNumberOfArguments,
    NestedReturns,
    UndeclaredIdentifier,
    CalledValueIsNotFunction,
    ValueIsWrongType,
    ExponentiationOverflowed,
    IndexOutOfRange,
    Custom(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Exception {
    pub kind: ExceptionKind,
    pub region: Region,
}

impl fmt::Display for Exception {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Exception at {}->{}: {}",
            self.region.start, self.region.end, self.kind
        )
    }
}

#[derive(Error, Debug, Display)]
pub enum ControlFlowValue {
    Exception(Exception),
    Continue,
    Break,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(v) => write!(f, "{:?}", v),
            Value::Null => write!(f, "null"),
            Value::List(list) => {
                write!(f, "[")?;
                for (i, value) in list.iter().enumerate() {
                    write!(f, "{}", value)?;
                    if i != list.len() - 1 {
                        write!(f, " ")?;
                    }
                }
                write!(f, "]")
            }
        }
    }
}

impl Value {
    pub fn into_int(&self, region: &Region) -> Result<&i64, ControlFlowValue> {
        match self {
            Value::Int(v) => Ok(v),
            _ => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ValueIsWrongType,
                region: region.clone(),
            })),
        }
    }

    pub fn into_bool(&self, region: &Region) -> Result<&bool, ControlFlowValue> {
        match self {
            Value::Bool(v) => Ok(v),
            _ => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ValueIsWrongType,
                region: region.clone(),
            })),
        }
    }

    pub fn into_str(&self, region: &Region) -> Result<&str, ControlFlowValue> {
        match self {
            Value::String(v) => Ok(v),
            _ => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ValueIsWrongType,
                region: region.clone(),
            })),
        }
    }

    pub fn into_list(&self, region: &Region) -> Result<&Vec<Value>, ControlFlowValue> {
        match self {
            Value::List(v) => Ok(v),
            _ => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ValueIsWrongType,
                region: region.clone(),
            })),
        }
    }
}
