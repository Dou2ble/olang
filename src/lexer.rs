use phf::phf_map;
use std::{fmt, string::String, vec::Vec};
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

#[derive(EnumDiscriminants, Display, Debug, PartialEq, Clone)]
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

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub region: Region,
}

impl Token {
    pub fn new(region: Region, value: TokenValue) -> Token {
        Token { value, region }
    }
}

#[derive(Debug, Clone)]
pub struct Location {
    pub row: usize,
    pub col: usize,
}

impl Location {
    fn from_index(source: &Vec<char>, index: usize) -> Self {
        let mut location = Location { row: 1, col: 1 };

        let target = if index > source.len() {
            // if the index is out of bounds
            // return the last character in the source
            source.len() - 1
        } else {
            index
        };

        for i in 0..target {
            if source[i] == '\n' {
                location.row += 1;
                location.col = 1;
            } else {
                location.col += 1
            };
        }

        location
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

#[derive(Debug, Clone)]
pub struct Region {
    pub start: Location,
    pub end: Location,
}

impl fmt::Display for Region {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.start, self.end)
    }
}

#[derive(Debug, Error)]
pub enum LexerError {
    #[error("{location} unexpected character found during parsing: {char}")]
    UnexpectedCharacter { location: Location, char: char },
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
                    value.push(self.current());
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
