use crate::{
    ast::{Expression, ExpressionValue, IfClause},
    lexer::LexerError,
    parser::{Parser, ParserError},
    types::Type,
};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error(transparent)]
    Parser(#[from] ParserError),
    #[error(transparent)]
    Lexer(#[from] LexerError), // do i really need this error kind?
    #[error("Undeclared identifier: \"{0}\"")]
    UndeclaredIdentifier(String),
    #[error("Attempted type conversion of variable: \"{0}\"")]
    Conversion(String),
    #[error("Attempted redefinition of identifier: \"{0}\"")]
    Redifinition(String),
}

pub struct Checker {
    scopes: Vec<HashMap<String, Type>>,
}

impl Checker {
    fn scope_get(&self, id: &str) -> Result<Type, Error> {
        for value in self.scopes.iter().rev() {
            match value.get(id) {
                Some(v) => {
                    return Ok(v.clone());
                }
                _ => {}
            }
        }

        Err(Error::UndeclaredIdentifier(id.to_string()))
    }
    pub fn scope_declare(&mut self, id: String, _type: Type) -> Result<(), Error> {
        let last_scope = self.scopes.last_mut().unwrap();
        if last_scope.contains_key(&id) {
            return Err(Error::Redifinition(id.to_string()));
        }

        self.scopes.last_mut().unwrap().insert(id, _type);
        Ok(())
    }

    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub fn check_if(
        &mut self,
        clauses: Vec<IfClause>,
        else_block: Option<Vec<Expression>>,
    ) -> Result<Type, Error> {
        todo!("If not implemented")
    }

    pub fn check(&mut self, source: &str) -> Result<(), Error> {
        let program = Parser::new(source)?.parse()?;

        for expression in program.ast {
            match expression.value {
                ExpressionValue::Int(_) => Ok(Type::Int),
                ExpressionValue::String(_) => Ok(Type::String),
                ExpressionValue::Bool(_) => Ok(Type::Bool),
                ExpressionValue::Null => todo!("Null not implemented"),
                ExpressionValue::If {
                    clauses,
                    else_block,
                } => self.check_if(clauses, else_block),
                _ => todo!("not implemented"),
            };
        }

        Ok(())
    }
}
