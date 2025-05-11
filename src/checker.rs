use crate::{
    ast::{DefinedFunction, Expression, ExpressionValue, IfClause},
    lexer::LexerError,
    parser::{Parser, ParserError},
    types::Type,
};
use std::{any::Any, collections::HashMap};
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
    #[error("Function returns incompatible types")]
    FunctionIncompatibleTypes,
    #[error("If test must return a boolean")]
    IfTestNotBool,
    #[error("If expression must have at least one clause")]
    IfExpressionEmpty,
    #[error("If block returns incompatible types")]
    IfIncompatibleTypes,
    #[error("Loop test must return a boolean")]
    LoopTestNotBool,
    #[error("The called identifier is not a function")]
    CalledIdentifierIsNotFunction,
    #[error("The function is called with the wrong number of arguments")]
    CallWrongArgumentCount,
    #[error("The function is called with the wrong type(s)")]
    CallWrongArgumentType,
    #[error("Mismatched types inside of a list")]
    ListMistmatchedTypes,
    #[error("Cannot index into types other than lists")]
    IndexIntoWrongType,
    #[error("Cannot index empty list without type")]
    IndexCannotIndexListWithoutType,
    #[error("Cannot index the variable with this type")]
    IndexCannotIndexWithType,
    #[error("Variable initialized as null must be typed")]
    VariableDeclarationInitializedNullMustBeTyped,
    #[error("Variable initialized as an empty list must be typed")]
    VariableDeclarationInitializedEmptyListMustBeTyped,
    #[error("Variable is initialized with the wrong type")]
    VariableDelcarationMismatchedTypes,
    #[error("Variable is assigned to another type")]
    AssignMismatch,
    #[error("You can only update integers")]
    UpdateNotInt,
}

pub struct Checker {
    scopes: Vec<HashMap<String, Type>>,
}

impl Checker {
    fn scope_push(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn scope_pop(&mut self) {
        self.scopes.pop();
    }
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
    fn scope_declare(&mut self, id: String, _type: Type) -> Result<(), Error> {
        let last_scope = self.scopes.last_mut().unwrap();
        if last_scope.contains_key(&id) {
            return Err(Error::Redifinition(id.to_string()));
        }

        self.scopes.last_mut().unwrap().insert(id, _type);
        Ok(())
    }

    fn check_if_clause(&mut self, if_clause: &IfClause) -> Result<Type, Error> {
        if (self.check_expression(&if_clause.test)?) != Type::Bool {
            return Err(Error::IfTestNotBool);
        }

        self.check_block(&if_clause.body)
    }

    fn check_if(
        &mut self,
        clauses: &Vec<IfClause>,
        else_block: &Option<Vec<Expression>>,
    ) -> Result<Type, Error> {
        let mut _type = self.check_if_clause(clauses.first().ok_or(Error::IfExpressionEmpty)?)?;

        // we have already checked the first so we can skip it
        for clause in clauses.iter().skip(1) {
            if self.check_if_clause(clause)? != _type {
                return Err(Error::IfIncompatibleTypes);
            }
        }

        if let Some(else_block) = else_block {
            if self.check_block(else_block)? != _type {
                return Err(Error::IfIncompatibleTypes);
            }
        }

        Ok(Type::Nullable(None))
    }

    fn check_loop(
        &mut self,
        init: &Option<Box<Expression>>,
        test: &Option<Box<Expression>>,
        update: &Option<Box<Expression>>,
        body: &Vec<Expression>,
    ) -> Result<Type, Error> {
        if let Some(init) = init {
            self.check_expression(init)?;
        }
        if let Some(test) = test {
            if self.check_expression(test)? != Type::Bool {
                return Err(Error::LoopTestNotBool);
            };
        };
        if let Some(update) = update {
            self.check_expression(update)?;
        }

        self.check_block(body)
    }

    fn check_function(&mut self, function: &DefinedFunction) -> Result<Type, Error> {
        self.scope_push();

        let mut parameters = vec![];

        for parameter in &function.parameters {
            self.scope_declare(parameter.id.to_string(), parameter._type.clone())?;
            parameters.push(parameter._type.clone());
        }

        let return_type = Box::new(self.check_block(&function.body)?);

        self.scope_pop();

        Ok(Type::Function {
            parameters,
            return_type,
        })
    }

    fn check_call(&mut self, identifier: &str, arguments: &Vec<Expression>) -> Result<Type, Error> {
        let (parameters, return_type) = match self.scope_get(identifier)? {
            Type::Function {
                parameters,
                return_type,
            } => (parameters, return_type),
            _ => return Err(Error::CalledIdentifierIsNotFunction),
        };

        if arguments.len() != parameters.len() {
            return Err(Error::CallWrongArgumentCount);
        };

        for (argument, parameter) in arguments.iter().zip(parameters.iter()) {
            if &(self.check_expression(argument))? != parameter {
                return Err(Error::CallWrongArgumentType);
            }
        }

        Ok(*return_type)
    }

    fn check_list(&mut self, list: &Vec<Expression>) -> Result<Type, Error> {
        let mut list_type = None;

        for item in list {
            let item_type = self.check_expression(item)?;

            match &list_type {
                None => {
                    list_type = Some(Box::new(item_type));
                }
                Some(_type) => {
                    if item_type != *_type.clone() {
                        return Err(Error::ListMistmatchedTypes);
                    }
                }
            }
        }

        Ok(Type::List(list_type))
    }

    fn check_index(&mut self, left: &Expression, index: &Expression) -> Result<Type, Error> {
        let _type = match self.check_expression(left)? {
            Type::List(v) => match v {
                Some(v) => v,
                None => return Err(Error::IndexCannotIndexListWithoutType),
            },
            _ => return Err(Error::IndexIntoWrongType),
        };

        if self.check_expression(index)? != Type::Int {
            return Err(Error::IndexCannotIndexWithType);
        };

        Ok(*_type)
    }

    fn check_variable_declaration(
        &mut self,
        identifier: &str,
        variable_type: &Option<Type>,
        expression: &Expression,
    ) -> Result<Type, Error> {
        let value_type = self.check_expression(expression)?;

        if variable_type == &None {
            match value_type {
                Type::Nullable(None) => {
                    return Err(Error::VariableDeclarationInitializedNullMustBeTyped)
                }
                Type::List(None) => {
                    return Err(Error::VariableDeclarationInitializedEmptyListMustBeTyped)
                }
                _ => {}
            };
        } else if let Some(variable_type) = variable_type {
            if *variable_type != value_type {
                return Err(Error::VariableDelcarationMismatchedTypes);
            }
        }

        self.scope_declare(identifier.to_string(), value_type)?;

        Ok(Type::Nullable(None))
    }

    fn check_assign(&mut self, identifier: &str, expression: &Expression) -> Result<Type, Error> {
        let variable_type = self.scope_get(identifier)?;
        let value_type = self.check_expression(expression)?;

        match value_type {
            Type::Nullable(None) => match variable_type {
                Type::Nullable(_) => {}
                _ => return Err(Error::AssignMismatch),
            },
            Type::List(None) => match variable_type {
                Type::List(_) => {}
                _ => return Err(Error::AssignMismatch),
            },
            _ => {
                if variable_type != value_type {
                    return Err(Error::AssignMismatch);
                }
            }
        }

        Ok(Type::Nullable(None))
    }

    fn check_update(&mut self, identifier: &str) -> Result<Type, Error> {
        if self.scope_get(identifier)? != Type::Int {
            return Err(Error::UpdateNotInt);
        }
        Ok(Type::Nullable(None))
    }

    fn check_expression(&mut self, expression: &Expression) -> Result<Type, Error> {
        match &expression.value {
            ExpressionValue::Int(_) => Ok(Type::Int),
            ExpressionValue::String(_) => Ok(Type::String),
            ExpressionValue::Bool(_) => Ok(Type::Bool),
            ExpressionValue::Block(block) => self.check_block(block),
            ExpressionValue::Identifier(id) => self.scope_get(id),
            ExpressionValue::Null => Ok(Type::Nullable(None)),
            ExpressionValue::Continue => Ok(Type::Nullable(None)),
            ExpressionValue::Break => Ok(Type::Nullable(None)),
            ExpressionValue::Throw(_) => Ok(Type::Nullable(None)),
            ExpressionValue::If {
                clauses,
                else_block,
            } => self.check_if(clauses, else_block),
            ExpressionValue::Loop {
                init,
                test,
                update,
                body,
            } => self.check_loop(init, test, update, body),
            ExpressionValue::Function(function) => self.check_function(function),
            ExpressionValue::Call {
                identifier,
                arguments,
            } => self.check_call(identifier, arguments),
            ExpressionValue::List(list) => self.check_list(list),
            ExpressionValue::Index { left, index } => self.check_index(left, index),
            ExpressionValue::VariableDeclaration {
                identifier,
                variable_type,
                expression,
            } => self.check_variable_declaration(identifier, variable_type, expression),
            ExpressionValue::Assign {
                identifier,
                operator: _,
                expression,
            } => self.check_assign(identifier, expression),
            ExpressionValue::Update {
                identifier,
                operator: _,
            } => self.check_update(identifier),
            _ => todo!("not implemented"),
        }
    }

    fn check_block(&mut self, block: &Vec<Expression>) -> Result<Type, Error> {
        self.scope_push();

        let mut _type = Type::Nullable(None);

        for expression in block {
            _type = self.check_expression(expression)?;
        }

        self.scope_pop();

        return Ok(_type);
    }

    pub fn new() -> Self {
        Self { scopes: vec![] }
    }

    pub fn check(&mut self, source: &str) -> Result<(), Error> {
        let program = Parser::new(source)?.parse()?;

        self.check_block(&program.ast)?;

        Ok(())
    }
}
