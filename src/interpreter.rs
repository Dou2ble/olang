use crate::{
    environment::Environment,
    lexer::LexerError,
    location::Region,
    parser::{
        AssignmentOperator, BinaryOperationOperator, Block, Expression, ExpressionValue, IfClause,
        Parser, ParserError, UpdateOperator,
    },
    value::{ControlFlowValue, Exception, ExceptionKind, Function, Value},
};
use thiserror::Error;

pub struct Interpreter {
    environment: Environment,
}

#[derive(Error, Debug)]
pub enum EvalError {
    #[error("Unhandled exception: {0}")]
    UnhandledException(Exception),
    #[error("\"continue\" keyword used outside of loop")]
    ContinueOutsideLoop,
    #[error("\"break\" keyword used outside of loop")]
    BreakOutsideLoop,
    #[error(transparent)]
    Parser(#[from] ParserError),
    #[error(transparent)]
    Lexer(#[from] LexerError),
}

impl EvalError {
    pub fn unwrap_exception(&self) -> &ExceptionKind {
        match self {
            Self::UnhandledException(v) => &v.kind,
            _ => {
                panic!("called `EvalError::unwrap_exception()` on something else than a `UnhandledException` error")
            }
        }
    }
}

fn plus(left: Value, right: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    Ok(match left {
        Value::Int(left) => Value::Int(left + right.into_int(region)?),
        Value::String(left) => Value::String(left + right.into_str(region)?),
        Value::List(mut left) => {
            left.push(right);
            Value::List(left)
        }
        _ => {
            return Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ValueIsWrongType,
                region: region.clone(),
            }))
        }
    })
}
fn minus(left: Value, right: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    Ok(Value::Int(left.into_int(region)? - right.into_int(region)?))
}
fn multiply(left: Value, right: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    Ok(Value::Int(left.into_int(region)? * right.into_int(region)?))
}
fn divide(left: Value, right: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    Ok(Value::Int(left.into_int(region)? / right.into_int(region)?))
}
fn modulo(left: Value, right: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    Ok(Value::Int(left.into_int(region)? % right.into_int(region)?))
}
fn exponent(base: Value, exponent: Value, region: &Region) -> Result<Value, ControlFlowValue> {
    let base_int = *base.into_int(region)?;
    let exponent_int = *exponent.into_int(region)?;
    Ok(match (base_int as u64).checked_pow(exponent_int as u32) {
        Some(v) => Value::Int(v as i64),
        None => {
            return Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::ExponentiationOverflowed,
                region: region.clone(),
            }))
        }
    })
}
fn is_equal(left: Value, right: Value) -> bool {
    left == right
}
fn is_not_equal(left: Value, right: Value) -> bool {
    left != right
}
fn is_less_than(left: Value, right: Value, region: &Region) -> Result<bool, ControlFlowValue> {
    Ok(left.into_int(region)? < right.into_int(region)?)
}
fn is_less_than_or_equal(
    left: Value,
    right: Value,
    region: &Region,
) -> Result<bool, ControlFlowValue> {
    Ok(left.into_int(region)? <= right.into_int(region)?)
}
fn is_greater_than(left: Value, right: Value, region: &Region) -> Result<bool, ControlFlowValue> {
    Ok(left.into_int(region)? > right.into_int(region)?)
}
fn is_greater_than_or_equal(
    left: Value,
    right: Value,
    region: &Region,
) -> Result<bool, ControlFlowValue> {
    Ok(left.into_int(region)? >= right.into_int(region)?)
}
fn logical_and(left: Value, right: Value, region: &Region) -> Result<bool, ControlFlowValue> {
    Ok(*left.into_bool(region)? && *right.into_bool(region)?)
}
fn logical_or(left: Value, right: Value, region: &Region) -> Result<bool, ControlFlowValue> {
    Ok(*left.into_bool(region)? || *right.into_bool(region)?)
}

impl Interpreter {
    fn eval_binary(
        &mut self,
        left_expression: &Expression,
        operator: &BinaryOperationOperator,
        right_expression: &Expression,
    ) -> Result<Value, ControlFlowValue> {
        let left = self.eval_expression(left_expression)?;
        let right = self.eval_expression(right_expression)?;

        let region = Region {
            start: left_expression.region.start.clone(),
            end: right_expression.region.end.clone(),
        };

        Ok(match operator {
            BinaryOperationOperator::Plus => plus(left, right, &region)?,
            BinaryOperationOperator::Minus => minus(left, right, &region)?,
            BinaryOperationOperator::Multiply => multiply(left, right, &region)?,
            BinaryOperationOperator::Divide => divide(left, right, &region)?,
            BinaryOperationOperator::Modulus => modulo(left, right, &region)?,
            BinaryOperationOperator::Exponentiation => exponent(left, right, &region)?,
            BinaryOperationOperator::IsEqual => Value::Bool(is_equal(left, right)),
            BinaryOperationOperator::IsNotEqual => Value::Bool(is_not_equal(left, right)),
            BinaryOperationOperator::IsLessThan => Value::Bool(is_less_than(left, right, &region)?),
            BinaryOperationOperator::IsLessThanOrEqual => {
                Value::Bool(is_less_than_or_equal(left, right, &region)?)
            }
            BinaryOperationOperator::IsGreaterThan => {
                Value::Bool(is_greater_than(left, right, &region)?)
            }
            BinaryOperationOperator::IsGreaterThanOrEqual => {
                Value::Bool(is_greater_than_or_equal(left, right, &region)?)
            }
            BinaryOperationOperator::LogicalAnd => Value::Bool(logical_and(left, right, &region)?),
            BinaryOperationOperator::LogicalOr => Value::Bool(logical_or(left, right, &region)?),
        })
    }

    fn eval_block(
        &mut self,
        private_environment: bool,
        block: &Block,
    ) -> Result<Value, ControlFlowValue> {
        if private_environment {
            self.environment.push();
        }

        let mut result = Value::Null;
        for expression in block {
            result = self.eval_expression(expression)?;
        }

        if private_environment {
            self.environment.pop();
        }

        Ok(result)
    }

    fn eval_identifier(&mut self, id: &str, region: &Region) -> Result<Value, ControlFlowValue> {
        self.environment.get_or_undeclared(id, region)
    }

    fn eval_call(
        &mut self,
        id: &String,
        arguments: &Vec<Expression>,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        let function_value = match self.environment.get(id) {
            Some(v) => v,
            _ => {
                return Err(ControlFlowValue::Exception(Exception {
                    kind: ExceptionKind::UndeclaredIdentifier,
                    region: region.clone(),
                }))
            }
        };

        match function_value {
            Value::Function(function) => {
                let mut evaluated_arguments = vec![];
                for argument in arguments.iter() {
                    evaluated_arguments.push(self.eval_expression(argument)?)
                }

                match function {
                    Function::Builtin(function) => function(evaluated_arguments, region),
                    Function::Defined(defined) => {
                        self.environment.push();

                        if defined.parameters.len() != arguments.len() {
                            return Err(ControlFlowValue::Exception(Exception {
                                kind: ExceptionKind::WrongNumberOfArguments,
                                region: region.clone(),
                            }));
                        }

                        for (i, parameter) in defined.parameters.iter().enumerate() {
                            self.environment
                                .declare(parameter.clone(), evaluated_arguments[i].clone());
                        }

                        let result = self.eval_block(false, &defined.body);

                        self.environment.pop();

                        result
                    }
                }
            }
            _ => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::CalledValueIsNotFunction,
                region: region.clone(),
            })),
        }
    }

    fn eval_list(&mut self, expressions: &Vec<Expression>) -> Result<Value, ControlFlowValue> {
        let mut values = vec![];

        for expression in expressions {
            values.push(self.eval_expression(expression)?);
        }

        Ok(Value::List(values))
    }

    fn eval_index(
        &mut self,
        identifier: &str,
        indexes: &Vec<Expression>,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        let mut value = self.environment.get_or_undeclared(identifier, region)?;
        for index in indexes {
            value = value
                .into_list(region)?
                .get(*self.eval_expression(index)?.into_int(region)? as usize)
                .ok_or(ControlFlowValue::Exception(Exception {
                    kind: ExceptionKind::IndexOutOfRange,
                    region: region.clone(),
                }))?
                .clone()
        }
        Ok(value)
    }

    fn eval_declare_variable(
        &mut self,
        id: &String,
        expression: &Box<Expression>,
    ) -> Result<Value, ControlFlowValue> {
        let value = self.eval_expression(expression)?;
        self.environment.declare(id.clone(), value);
        Ok(Value::Null)
    }

    fn eval_if(
        &mut self,
        clauses: &Vec<IfClause>,
        else_block: &Option<Block>,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        let mut run_else_block = true;
        let mut result = Value::Null;

        for clause in clauses {
            let test_value = self.eval_expression(clause.test.as_ref())?;
            if *test_value.into_bool(region)? {
                result = self.eval_block(true, &clause.body)?;
                run_else_block = false;
                break;
            }
        }

        if run_else_block {
            if let Some(block) = else_block {
                result = self.eval_block(true, &block)?;
            }
        }

        Ok(result)
    }

    fn eval_assign(
        &mut self,
        id: &str,
        operator: &AssignmentOperator,
        expression: &Box<Expression>,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        let value = self.eval_expression(expression)?;

        match operator {
            AssignmentOperator::Set => {
                self.environment.assign(id, value, region)?;
            }
            AssignmentOperator::Plus => {
                self.environment.assign(
                    id,
                    plus(
                        self.environment.get_or_undeclared(id, region)?,
                        value,
                        region,
                    )?,
                    region,
                )?;
            }
            AssignmentOperator::Minus => {
                self.environment.assign(
                    id,
                    minus(
                        self.environment.get_or_undeclared(id, region)?,
                        value,
                        region,
                    )?,
                    region,
                )?;
            }
            AssignmentOperator::Multiply => {
                self.environment.assign(
                    id,
                    multiply(
                        self.environment.get_or_undeclared(id, region)?,
                        value,
                        region,
                    )?,
                    region,
                )?;
            }
            AssignmentOperator::Divide => {
                self.environment.assign(
                    id,
                    divide(
                        self.environment.get_or_undeclared(id, region)?,
                        value,
                        region,
                    )?,
                    region,
                )?;
            }
            AssignmentOperator::Modulo => {
                self.environment.assign(
                    id,
                    modulo(
                        self.environment.get_or_undeclared(id, region)?,
                        value,
                        region,
                    )?,
                    region,
                )?;
            }
        }

        Ok(Value::Null)
    }

    fn eval_update(
        &mut self,
        identifier: &str,
        operator: &UpdateOperator,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        match operator {
            UpdateOperator::Increment => self.environment.assign(
                identifier,
                plus(
                    self.environment.get_or_undeclared(identifier, region)?,
                    Value::Int(1),
                    region,
                )?,
                region,
            ),
            UpdateOperator::Decremet => self.environment.assign(
                identifier,
                minus(
                    self.environment.get_or_undeclared(identifier, region)?,
                    Value::Int(1),
                    region,
                )?,
                region,
            ),
        }?;

        Ok(Value::Null)
    }

    fn eval_loop(
        &mut self,
        init: &Option<Box<Expression>>,
        test: &Option<Box<Expression>>,
        update: &Option<Box<Expression>>,
        body: &Block,
        region: &Region,
    ) -> Result<Value, ControlFlowValue> {
        let mut result = Value::Null;

        self.environment.push();

        if let Some(init) = init {
            self.eval_expression(init)?;
        }

        loop {
            if let Some(test) = test {
                if !*self.eval_expression(test)?.into_bool(region)? {
                    break;
                }
            }
            match self.eval_block(false, body) {
                Ok(v) => {
                    result = v;
                }
                Err(ControlFlowValue::Continue) => {}
                Err(ControlFlowValue::Break) => break,
                Err(e) => {
                    return Err(e);
                }
            }
            if let Some(update) = update {
                self.eval_expression(update)?;
            }
        }

        self.environment.pop();

        Ok(result)
    }

    fn eval_expression(&mut self, expression: &Expression) -> Result<Value, ControlFlowValue> {
        match &expression.value {
            ExpressionValue::Int(v) => Ok(Value::Int(*v)),
            ExpressionValue::String(v) => Ok(Value::String(v.clone())),
            ExpressionValue::Bool(v) => Ok(Value::Bool(*v)),
            ExpressionValue::Null => Ok(Value::Null),
            ExpressionValue::If {
                clauses,
                else_block,
            } => self.eval_if(clauses, else_block, &expression.region),
            ExpressionValue::Loop {
                init,
                test,
                update,
                body,
            } => self.eval_loop(init, test, update, body, &expression.region),
            ExpressionValue::Continue => Err(ControlFlowValue::Continue),
            ExpressionValue::Break => Err(ControlFlowValue::Break),
            ExpressionValue::Throw(v) => Err(ControlFlowValue::Exception(Exception {
                kind: ExceptionKind::Custom(v.clone()),
                region: expression.region.clone(),
            })),
            ExpressionValue::Function(v) => Ok(Value::Function(Function::Defined(v.clone()))),
            ExpressionValue::Block(v) => self.eval_block(true, v),
            ExpressionValue::Identifier(id) => self.eval_identifier(id, &expression.region),
            ExpressionValue::Call {
                identifier,
                arguments,
            } => self.eval_call(identifier, arguments, &expression.region),
            ExpressionValue::List(expressions) => self.eval_list(expressions),
            ExpressionValue::Index {
                identifier,
                indexes,
            } => self.eval_index(identifier, indexes, &expression.region),
            ExpressionValue::VariableDeclaration {
                identifier,
                expression,
            } => self.eval_declare_variable(identifier, expression),
            ExpressionValue::Assign {
                identifier,
                operator,
                expression,
            } => self.eval_assign(identifier, operator, expression, &expression.region),
            ExpressionValue::Update {
                identifier,
                operator,
            } => self.eval_update(identifier, operator, &expression.region),
            ExpressionValue::Binary {
                left,
                operator,
                right,
            } => self.eval_binary(left, operator, right),
        }
    }

    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::default(),
        }
    }

    pub fn eval(&mut self, source: &str) -> Result<Value, EvalError> {
        let program = Parser::new(source)?.parse()?;
        let mut result = Value::Null;

        for expression in program.ast {
            match self.eval_expression(&expression) {
                Ok(v) => Ok(result = v),
                Err(err) => match err {
                    ControlFlowValue::Exception(e) => Err(EvalError::UnhandledException(e)),
                    ControlFlowValue::Continue => Err(EvalError::ContinueOutsideLoop),
                    ControlFlowValue::Break => Err(EvalError::BreakOutsideLoop),
                },
            }?;
        }

        return Ok(result);
    }
}
