use crate::lexer::{Lexer, LexerError, Region, Token, TokenValue, TokenValueDiscriminants};
use strum::{Display, EnumDiscriminants};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParserError {
    #[error("{0} unexpected token found while parsing \"{1}\" expression, expected token of value \"{2}\", found \"{3}\"", .found.region, .while_parsing, .expected, .found.value)]
    ExpectedToken {
        while_parsing: ExpressionValueDiscriminants,
        expected: TokenValueDiscriminants,
        found: Token,
    },
    #[error("{0} unexpected token found while parsing \"{1}\" expression, found token of value \"{2}\"", .found.region, match .while_parsing {Some(v) => v.to_string(), None => "generic".to_string()}, .found.value)]
    UnexpectedToken {
        while_parsing: Option<ExpressionValueDiscriminants>,
        found: Token,
    },
}

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
        expression: Box<Expression>,
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
}

#[derive(Clone, Debug)]
pub struct Expression {
    region: Region,
    pub value: ExpressionValue,
}

#[derive(Debug)]
pub struct Program {
    pub ast: Vec<Expression>,
}

pub struct Parser {
    tokens: Vec<Token>,
    t: usize,
}

impl Parser {
    pub fn new(source: &str) -> Result<Parser, LexerError> {
        Ok(Parser {
            tokens: Lexer::new(source).tokenize()?,
            t: 0,
        })
    }

    fn advance(&mut self) {
        self.t += 1
    }

    fn current(&self) -> &Token {
        &self.tokens[self.t]
    }

    fn current_val(&self) -> &TokenValue {
        &self.tokens[self.t].value
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.t - 1]
    }

    fn next_val(&self) -> &TokenValue {
        &self.tokens[self.t + 1].value
    }

    #[track_caller]
    fn expect_token_discriminant(
        &mut self,
        while_parsing: ExpressionValueDiscriminants,
        value: TokenValueDiscriminants,
    ) -> Result<(), ParserError> {
        if value != self.current_val().into() {
            Err(ParserError::ExpectedToken {
                expected: value,
                found: self.current().clone(),
                while_parsing,
            })
        } else {
            Ok(())
        }
    }

    #[track_caller]
    fn expect_token_err(
        &self,
        while_parsing: ExpressionValueDiscriminants,
        value: TokenValueDiscriminants,
    ) -> ParserError {
        ParserError::ExpectedToken {
            expected: value,
            found: self.current().clone(),
            while_parsing,
        }
    }

    fn parse_block(&mut self) -> Result<Block, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Block,
            TokenValueDiscriminants::OpenBrace,
        )?;
        self.advance();

        let mut expressions: Vec<Expression> = vec![];
        loop {
            match self.current_val() {
                TokenValue::CloseBrace => break,
                _ => expressions.push(self.parse_expression()?),
            };
        }
        self.advance(); // skip the closing brace

        Ok(expressions)
    }

    fn parse_list(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::List,
            TokenValueDiscriminants::OpenBracket,
        )?;
        self.advance();

        let mut expressions: Vec<Expression> = vec![];
        loop {
            match self.current_val() {
                TokenValue::CloseBracket => break,
                _ => expressions.push(self.parse_expression()?),
            };
        }
        self.advance(); // skip the closing bracket ]

        Ok(ExpressionValue::List(expressions))
    }

    fn parse_identifier(&mut self) -> Result<ExpressionValue, ParserError> {
        let value = match self.current_val() {
            TokenValue::Identifier(v) => Ok(v.clone()),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::Identifier,
                TokenValueDiscriminants::Identifier,
            )),
        }?;
        self.advance();
        Ok(ExpressionValue::Identifier(value))
    }

    fn parse_int(&mut self) -> Result<ExpressionValue, ParserError> {
        let value = match self.current_val() {
            TokenValue::Int(v) => Ok(*v),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::Int,
                TokenValueDiscriminants::Int,
            )),
        }?;
        self.advance();
        Ok(ExpressionValue::Int(value))
    }

    fn parse_string(&mut self) -> Result<ExpressionValue, ParserError> {
        let value = match self.current_val() {
            TokenValue::String(v) => Ok(v.clone()),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::String,
                TokenValueDiscriminants::String,
            )),
        }?;
        self.advance();
        Ok(ExpressionValue::String(value))
    }

    fn parse_null(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Null,
            TokenValueDiscriminants::KeywordNull,
        )?;
        self.advance();
        Ok(ExpressionValue::Null)
    }

    fn parse_bool(&mut self) -> Result<ExpressionValue, ParserError> {
        let value = match self.current_val() {
            TokenValue::KeywordTrue => Ok(true),
            TokenValue::KeywordFalse => Ok(false),
            _ => Err(ParserError::UnexpectedToken {
                while_parsing: Some(ExpressionValueDiscriminants::Bool),
                found: self.current().clone(),
            }),
        }?;
        self.advance();
        Ok(ExpressionValue::Bool(value))
    }

    fn parse_variable_declaration(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::VariableDeclaration,
            TokenValueDiscriminants::KeywordVar,
        )?;
        self.advance();

        let identifier = match self.current_val() {
            TokenValue::Identifier(v) => Ok(v),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::VariableDeclaration,
                TokenValueDiscriminants::Identifier,
            )),
        }?
        .clone();
        self.advance();

        self.expect_token_discriminant(
            ExpressionValueDiscriminants::VariableDeclaration,
            TokenValueDiscriminants::EqualSign,
        )?;
        self.advance();

        Ok(ExpressionValue::VariableDeclaration {
            identifier,
            expression: Box::new(self.parse_expression()?),
        })
    }

    fn parse_call(&mut self) -> Result<ExpressionValue, ParserError> {
        let identifier = match self.current_val() {
            TokenValue::Identifier(v) => Ok(v.clone()),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::Call,
                TokenValueDiscriminants::Identifier,
            )),
        }?;
        self.advance();

        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Call,
            TokenValueDiscriminants::OpenParenthesis,
        )?;
        self.advance();

        let mut arguments = vec![];
        while *self.current_val() != TokenValue::CloseParenthesis {
            arguments.push(self.parse_expression()?);
        }
        self.advance(); // skip the clogin parenthesis )

        Ok(ExpressionValue::Call {
            identifier,
            arguments,
        })
    }

    fn parse_function(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Function,
            TokenValueDiscriminants::KeywordFun,
        )?;
        self.advance();

        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Function,
            TokenValueDiscriminants::OpenParenthesis,
        )?;
        self.advance();

        let mut parameters = vec![];
        loop {
            match self.current_val() {
                TokenValue::CloseParenthesis => {
                    self.advance();
                    break;
                }
                TokenValue::Identifier(v) => {
                    parameters.push(v.clone());
                }
                _ => {
                    return Err(ParserError::UnexpectedToken {
                        while_parsing: Some(ExpressionValueDiscriminants::Function),
                        found: self.current().clone(),
                    })
                }
            }
            self.advance();
        }

        Ok(ExpressionValue::Function(DefinedFunction {
            parameters,
            body: self.parse_block()?,
        }))
    }

    fn parse_if(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::If,
            TokenValueDiscriminants::KeywordIf,
        )?;
        self.advance();

        // parse the first if
        let first_test = self.parse_expression()?;
        let first_body = self.parse_block()?;
        let mut clauses = vec![IfClause {
            test: Box::new(first_test),
            body: first_body,
        }];

        // parse any amount of elifs
        while self.current_val() == &TokenValue::KeywordElif {
            self.advance();

            let test = self.parse_expression()?;
            let body = self.parse_block()?;

            clauses.push(IfClause {
                test: Box::new(test),
                body,
            })
        }

        let mut else_block = None;
        match self.current_val() {
            TokenValue::KeywordElse => {
                // parse the else block
                self.advance();
                else_block = Some(self.parse_block()?);
            }
            _ => {}
        }

        Ok(ExpressionValue::If {
            clauses,
            else_block,
        })
    }

    fn parse_loop(&mut self) -> Result<ExpressionValue, ParserError> {
        enum LoopType {
            While,
            For,
            Loop,
        }
        let loop_type = match self.current_val() {
            TokenValue::KeywordWhile => LoopType::While,
            TokenValue::KeywordLoop => LoopType::Loop,
            TokenValue::KeywordFor => LoopType::For,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    while_parsing: Some(ExpressionValueDiscriminants::Loop),
                    found: self.current().clone(),
                })
            }
        };
        self.advance();

        let mut init = None;
        let mut test = None;
        let mut update = None;

        // parse the loop initalization expression
        match loop_type {
            LoopType::For => init = Some(Box::new(self.parse_expression()?)),
            _ => {}
        };
        // parse the test expression
        match loop_type {
            LoopType::For | LoopType::While => test = Some(Box::new(self.parse_expression()?)),
            _ => {}
        };
        // parse update expression
        match loop_type {
            LoopType::For => update = Some(Box::new(self.parse_expression()?)),
            _ => {}
        };

        let body = self.parse_block()?;

        Ok(ExpressionValue::Loop {
            init,
            test,
            update,
            body,
        })
    }

    fn parse_assign(&mut self) -> Result<ExpressionValue, ParserError> {
        let identifier = match self.current_val() {
            TokenValue::Identifier(v) => Ok(v),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::Assign,
                TokenValueDiscriminants::Identifier,
            )),
        }?
        .clone();
        self.advance();

        let operator = match self.current_val() {
            TokenValue::EqualSign => AssignmentOperator::Set,
            TokenValue::AdditionAssign => AssignmentOperator::Plus,
            TokenValue::SubtractionAssign => AssignmentOperator::Minus,
            TokenValue::MultiplicationAssign => AssignmentOperator::Multiply,
            TokenValue::DivisionAssign => AssignmentOperator::Divide,
            TokenValue::ModuloAssign => AssignmentOperator::Modulo,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    while_parsing: Some(ExpressionValueDiscriminants::Assign),
                    found: self.current().clone(),
                })
            }
        };
        self.advance();

        Ok(ExpressionValue::Assign {
            identifier,
            operator,
            expression: Box::new(self.parse_expression()?),
        })
    }

    fn parse_update(&mut self) -> Result<ExpressionValue, ParserError> {
        let identifier = match self.current_val() {
            TokenValue::Identifier(v) => Ok(v),
            _ => Err(self.expect_token_err(
                ExpressionValueDiscriminants::Assign,
                TokenValueDiscriminants::Identifier,
            )),
        }?
        .clone();
        self.advance();

        let operator = match self.current_val() {
            TokenValue::Increment => UpdateOperator::Increment,
            TokenValue::Decrement => UpdateOperator::Decremet,
            _ => {
                return Err(ParserError::UnexpectedToken {
                    while_parsing: Some(ExpressionValueDiscriminants::Update),
                    found: self.current().clone(),
                })
            }
        };
        self.advance();

        Ok(ExpressionValue::Update {
            identifier,
            operator,
        })
    }

    fn parse_continue(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Continue,
            TokenValueDiscriminants::KeywordContinue,
        )?;
        self.advance();
        Ok(ExpressionValue::Continue)
    }

    fn parse_break(&mut self) -> Result<ExpressionValue, ParserError> {
        self.expect_token_discriminant(
            ExpressionValueDiscriminants::Break,
            TokenValueDiscriminants::KeywordBreak,
        )?;
        self.advance();
        Ok(ExpressionValue::Break)
    }

    fn parse_primary(&mut self) -> Result<Expression, ParserError> {
        let start = self.current().region.start.clone();
        let value = match self.current_val() {
            TokenValue::Int(_) => self.parse_int(),
            TokenValue::String(_) => self.parse_string(),
            TokenValue::Identifier(_) => match self.next_val() {
                TokenValue::OpenParenthesis => self.parse_call(),
                TokenValue::EqualSign
                | TokenValue::AdditionAssign
                | TokenValue::SubtractionAssign
                | TokenValue::MultiplicationAssign
                | TokenValue::DivisionAssign
                | TokenValue::ModuloAssign => self.parse_assign(),
                TokenValue::Increment | TokenValue::Decrement => self.parse_update(),
                _ => self.parse_identifier(),
            },
            TokenValue::OpenParenthesis => {
                self.advance(); // skip the open parenthesis (

                let expression = self.parse_expression()?;

                self.expect_token_discriminant(
                    ExpressionValueDiscriminants::Binary, // FIXME: this dosen't have a type, binary is the closest but idk
                    TokenValueDiscriminants::CloseParenthesis,
                )?;
                self.advance();

                Ok(expression.value)
            }
            TokenValue::KeywordNull => self.parse_null(),
            TokenValue::KeywordTrue | TokenValue::KeywordFalse => self.parse_bool(),
            TokenValue::OpenBrace => Ok(ExpressionValue::Block(self.parse_block()?)),
            TokenValue::OpenBracket => self.parse_list(),
            TokenValue::KeywordVar => self.parse_variable_declaration(),
            TokenValue::KeywordFun => self.parse_function(),
            TokenValue::KeywordIf => self.parse_if(),
            TokenValue::KeywordWhile | TokenValue::KeywordFor | TokenValue::KeywordLoop => {
                self.parse_loop()
            }
            TokenValue::KeywordContinue => self.parse_continue(),
            TokenValue::KeywordBreak => self.parse_break(),
            _ => Err(ParserError::UnexpectedToken {
                while_parsing: None,
                found: self.current().clone(),
            }),
        }?;
        let end = self.previous().region.end.clone();

        Ok(Expression {
            region: Region { start, end },
            value,
        })
    }

    fn parse_index(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_primary()?;

        while self.current_val() == &TokenValue::DoubleExclamationMark {
            self.advance(); // skip the exclamation mark (!)

            let right = self.parse_primary()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Index {
                    expression: Box::new(left),
                    index: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_exponentiative(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_index()?;

        loop {
            let operator = match self.current_val() {
                TokenValue::ExponentSign => BinaryOperationOperator::Exponentiation,
                _ => {
                    break;
                }
            };
            self.advance();

            let right = self.parse_index()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_exponentiative()?;

        loop {
            let operator = match self.current_val() {
                TokenValue::MultiplicationSign => BinaryOperationOperator::Multiply,
                TokenValue::DivisionSign => BinaryOperationOperator::Divide,
                TokenValue::ModuloSign => BinaryOperationOperator::Modulus,
                _ => {
                    break;
                }
            };
            self.advance();

            let right = self.parse_exponentiative()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_multiplicative()?;

        loop {
            let operator = match self.current_val() {
                TokenValue::PlusSign => BinaryOperationOperator::Plus,
                TokenValue::MinusSign => BinaryOperationOperator::Minus,
                _ => {
                    break;
                }
            };
            self.advance();

            let right = self.parse_multiplicative()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_comparative(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_additive()?;

        loop {
            let operator = match self.current_val() {
                TokenValue::IsEqual => BinaryOperationOperator::IsEqual,
                TokenValue::IsNotEqual => BinaryOperationOperator::IsNotEqual,
                TokenValue::IsGreaterThan => BinaryOperationOperator::IsGreaterThan,
                TokenValue::IsGreaterThanOrEqual => BinaryOperationOperator::IsGreaterThanOrEqual,
                TokenValue::IsLessThan => BinaryOperationOperator::IsLessThan,
                TokenValue::IsLessThanOrEqual => BinaryOperationOperator::IsLessThanOrEqual,
                _ => {
                    break;
                }
            };
            self.advance();

            let right = self.parse_additive()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_logical(&mut self) -> Result<Expression, ParserError> {
        let mut left = self.parse_comparative()?;

        loop {
            let operator = match self.current_val() {
                TokenValue::And => BinaryOperationOperator::LogicalAnd,
                TokenValue::Or => BinaryOperationOperator::LogicalOr,
                _ => {
                    break;
                }
            };
            self.advance();

            let right = self.parse_comparative()?;
            left = Expression {
                region: Region {
                    start: left.region.start.clone(),
                    end: right.region.end.clone(),
                },
                value: ExpressionValue::Binary {
                    left: Box::new(left),
                    operator,
                    right: Box::new(right),
                },
            }
        }

        Ok(left)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_logical()
    }

    pub fn parse(&mut self) -> Result<Program, ParserError> {
        self.t = 0;
        let mut program: Program = Program { ast: vec![] };

        while self.tokens[self.t].value != TokenValue::EndOfFile {
            program.ast.push(self.parse_expression()?);
        }

        Ok(program)
    }
}
