use crate::location::{Location, Region};
use phf::phf_map;
use std::{string::String, vec::Vec};
use strum::{Display, EnumDiscriminants};
use thiserror::Error;

static KEYWORDS: phf::Map<&'static str, TokenValue> = phf_map! {
    "fun" => TokenValue::KeywordFun,
    "true" => TokenValue::KeywordTrue,
    "false" => TokenValue::KeywordFalse,
    "null" => TokenValue::KeywordNull,
    "var" => TokenValue::KeywordVar,
    "if" => TokenValue::KeywordIf,
    "elif" => TokenValue::KeywordElif,
    "else" => TokenValue::KeywordElse,
    "while" => TokenValue::KeywordWhile,
    "for" => TokenValue::KeywordFor,
    "loop" => TokenValue::KeywordLoop,
    "continue" => TokenValue::KeywordContinue,
    "break" => TokenValue::KeywordBreak,
    "throw" => TokenValue::KeywordThrow,
};

static ESCAPE_SEQUENCES: phf::Map<char, char> = phf_map! {
    'n' => '\n', // newline
    'r' => '\r', // carriage return
    't' => '\t', // tab
    'b' => '\x08', // backspace
    'f' => '\x0c', // form feed
    'v' => '\x0b', // vertical tab
    '\\' => '\\', // backslash
    '\'' => '\'', // single quote
    '\"' => '\"', // double quote
    '0' => '\0', // null character
};

#[derive(EnumDiscriminants, Display, Debug, PartialEq, Clone, Eq)]
#[strum_discriminants(derive(Display))]
pub enum TokenValue {
    KeywordFun,           // fun
    KeywordTrue,          // true
    KeywordFalse,         // false
    KeywordNull,          // null
    KeywordVar,           // var
    KeywordIf,            // if
    KeywordElif,          // elif
    KeywordElse,          // else
    KeywordWhile,         // while
    KeywordFor,           // for
    KeywordLoop,          // loop
    KeywordContinue,      // continue
    KeywordBreak,         // break
    KeywordThrow,         // throw
    Period,               // .
    EqualSign,            // =
    CloseParenthesis,     // )
    OpenParenthesis,      // (
    OpenBracket,          // [
    CloseBracket,         // ]
    OpenBrace,            // {
    CloseBrace,           // }
    PlusSign,             // +
    MinusSign,            // -
    DivisionSign,         // /
    MultiplicationSign,   // *
    ExponentSign,         // **
    ModuloSign,           // %
    EndOfFile,            // EOF
    Identifier(String),   // print
    String(String),       // "Hello World"
    Int(i64),             // 100
    IsLessThan,           // <
    IsLessThanOrEqual,    // <=
    IsGreaterThan,        // >
    IsGreaterThanOrEqual, // >=
    IsEqual,              // ==
    IsNotEqual,           // !=
    And,                  // &&
    Or,                   // ||
    AdditionAssign,       // +=
    SubtractionAssign,    // -=
    MultiplicationAssign, // *=
    DivisionAssign,       // /=
    ModuloAssign,         // %=
    Increment,            // ++
    Decrement,            // --
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub value: TokenValue,
    pub region: Region,
}

impl Token {
    pub fn new(region: Region, value: TokenValue) -> Token {
        Token { value, region }
    }
}

#[derive(Debug, Error, PartialEq, Eq)]
pub enum LexerError {
    #[error("{location} unexpected character found during lexical analysis: {char}")]
    UnexpectedCharacter { location: Location, char: char },
    #[error(
        "{location} unexpected escape sequence found in string during lexical analysis: \\{char}"
    )]
    UnexpectedEscapeSequence { location: Location, char: char },
    #[error("{location} expected digit in int token, found: {char}")]
    NotDigit { location: Location, char: char },
}

pub struct Lexer {
    source: Vec<char>,
    c: usize,
}

impl Lexer {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.chars().collect(),
            c: 0,
        }
    }

    fn current_location(&self) -> Location {
        Location::from_index(&self.source, self.c)
    }

    fn advance(&mut self) -> &mut Self {
        self.c += 1;
        self
    }

    fn current(&self) -> char {
        self.source[self.c]
    }

    fn next_or_space(&self) -> &char {
        match self.source.get(self.c + 1) {
            Some(v) => v,
            None => &' ',
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, LexerError> {
        let mut result: Vec<Token> = vec![];
        self.c = 0;

        while self.c < self.source.len() {
            let mut region = Region {
                start: Location { row: 0, col: 0 },
                end: Location { row: 0, col: 0 },
            };

            region.start = self.current_location();

            // match for simple one char poiters
            match match self.source[self.c] {
                '(' => Some(TokenValue::OpenParenthesis),
                ')' => Some(TokenValue::CloseParenthesis),
                '{' => Some(TokenValue::OpenBrace),
                '}' => Some(TokenValue::CloseBrace),
                '[' => Some(TokenValue::OpenBracket),
                ']' => Some(TokenValue::CloseBracket),
                '.' => Some(TokenValue::Period),
                '+' => match self.next_or_space() {
                    '+' => {
                        self.advance();
                        Some(TokenValue::Increment)
                    }
                    '=' => {
                        self.advance();
                        Some(TokenValue::AdditionAssign)
                    }
                    _ => Some(TokenValue::PlusSign),
                },
                '-' => match self.next_or_space() {
                    '-' => {
                        self.advance();
                        Some(TokenValue::Decrement)
                    }
                    '=' => {
                        self.advance();
                        Some(TokenValue::SubtractionAssign)
                    }
                    _ => Some(TokenValue::MinusSign),
                },
                '/' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::DivisionAssign)
                    }
                    _ => Some(TokenValue::DivisionSign),
                },
                '%' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::ModuloAssign)
                    }
                    _ => Some(TokenValue::ModuloSign),
                },
                '*' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::MultiplicationAssign)
                    }
                    '*' => {
                        self.advance();
                        Some(TokenValue::ExponentSign)
                    }
                    _ => Some(TokenValue::MultiplicationSign),
                },
                '&' => match self.next_or_space() {
                    '&' => {
                        self.advance();
                        Some(TokenValue::And)
                    }
                    _ => None,
                },
                '|' => match self.next_or_space() {
                    '|' => {
                        self.advance();
                        Some(TokenValue::Or)
                    }
                    _ => None,
                },
                '!' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::IsNotEqual)
                    }
                    _ => None,
                },
                '=' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::IsEqual)
                    }
                    _ => Some(TokenValue::EqualSign),
                },
                '<' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::IsLessThanOrEqual)
                    }
                    _ => Some(TokenValue::IsLessThan),
                },
                '>' => match self.next_or_space() {
                    '=' => {
                        self.advance();
                        Some(TokenValue::IsGreaterThanOrEqual)
                    }
                    _ => Some(TokenValue::IsGreaterThan),
                },
                _ => None,
            } {
                Some(v) => {
                    region.end = self.current_location();
                    result.push(Token::new(region, v));
                    self.advance();
                    continue;
                }
                _ => {}
            }

            if self.current().is_whitespace() {
                self.advance();
                continue;
            }

            // check for comments
            if self.current() == '#' {
                self.advance();
                // block comment
                if self.current() == '[' {
                    while self.c < self.source.len()
                        && !(self.current() == ']' && self.next_or_space() == &'#')
                    {
                        self.advance();
                    }
                    self.advance();
                    // else single line comments
                } else {
                    while self.c < self.source.len() && self.current() != '\n' {
                        self.advance();
                    }
                }
                self.advance();
                continue;
            }
            // string token
            if self.current() == '"' {
                let mut value = "".to_string();
                self.advance();
                while self.c < self.source.len() && self.current() != '"' {
                    match self.current() {
                        '\\' => {
                            self.advance();
                            let char = ESCAPE_SEQUENCES.get(&self.current()).ok_or(
                                LexerError::UnexpectedEscapeSequence {
                                    location: self.current_location(),
                                    char: self.current(),
                                },
                            )?;
                            value.push(char.clone());
                        }
                        _ => {
                            value.push(self.current());
                        }
                    }
                    self.advance();
                }
                self.advance();

                region.end = self.current_location();
                result.push(Token::new(region, TokenValue::String(value)));
            }
            // int token
            else if self.current().is_digit(10) || self.current() == '-' {
                let mut value: i64 = 0;
                let mut negative = false;

                if self.current() == '-' {
                    negative = true;
                    self.c += 1;
                };

                while self.c < self.source.len() && self.current().is_digit(10) {
                    value = value * 10
                        + self.current().to_digit(10).ok_or(LexerError::NotDigit {
                            location: self.current_location(),
                            char: self.source[self.c],
                        })? as i64;
                    self.advance();
                }

                if negative {
                    value *= -1
                }

                region.end = self.current_location();
                result.push(Token::new(region, TokenValue::Int(value)));
            }
            // identifier or keyword
            else if self.current().is_alphanumeric() && !self.current().is_whitespace() {
                let mut value = "".to_string();

                while self.c < self.source.len()
                    && (self.current().is_alphanumeric() || self.current() == '_')
                    && !self.current().is_whitespace()
                {
                    value.push(self.current());
                    self.advance();
                }

                region.end = self.current_location();

                result.push(Token::new(
                    region,
                    match KEYWORDS.get(value.as_str()) {
                        Some(v) => v.clone(),
                        None => TokenValue::Identifier(value),
                    },
                ))
            } else {
                return Err(LexerError::UnexpectedCharacter {
                    location: self.current_location(),
                    char: self.current(),
                });
            }
        }

        result.push(Token::new(
            Region {
                start: Location::from_index(&self.source, usize::MAX),
                end: Location::from_index(&self.source, usize::MAX),
            },
            TokenValue::EndOfFile,
        ));
        Ok(result)
    }
}
