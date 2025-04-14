use crate::frontend::ast::{Expression, Statement};
use crate::frontend::token::Token;
use std::path::Path;
use std::str::Chars;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Location {
    pub fn new() -> Self {
        Location {
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    pub fn advance(&mut self, c: char) {
        self.offset += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
    }
}
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TokenWithSpan {
    pub token: Token,
    pub span: Span,
    pub lexeme: String,
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span,
    pub source_line: String,
}

pub struct Lexer<'a> {
    source: &'a str,
    chars: Chars<'a>,
    current_char: Option<char>,
    location: Location,
    source_lines: Vec<String>,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut chars = source.chars();
        let current_char = chars.next();
        let source_lines = source.lines().map(String::from).collect();

        Lexer {
            source,
            chars,
            current_char,
            location: Location::new(),
            source_lines,
        }
    }

    fn advance(&mut self) {
        if let Some(c) = self.current_char {
            self.location.advance(c);
        }
        self.current_char = self.chars.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn token(&mut self, token: Token, start_loc: Location) -> TokenWithSpan {
        let span = Span {
            start: start_loc,
            end: self.location,
        };

        let start_offset = start_loc.offset;
        let end_offset = self.location.offset;
        let lexeme = self.source[start_offset..end_offset].to_string();

        TokenWithSpan {
            token,
            span,
            lexeme,
        }
    }

    fn number(&mut self) -> TokenWithSpan {
        let start_loc = self.location;
        let mut number = String::new();

        while let Some(c) = self.current_char {
            if c.is_ascii_digit() {
                number.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let token = Token::Number(number.parse().unwrap());
        self.token(token, start_loc)
    }

    fn identifier(&mut self) -> TokenWithSpan {
        let start_loc = self.location;
        let mut ident = String::new();

        while let Some(c) = self.current_char {
            if c.is_alphanumeric() || c == '_' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        let token = match ident.as_str() {
            "return" => Token::Return,
            "let" => Token::Let,
            _ => Token::Identifier(ident),
        };

        self.token(token, start_loc)
    }

    pub fn next_token(&mut self) -> Option<TokenWithSpan> {
        self.skip_whitespace();

        self.current_char?;

        let start_loc = self.location;

        match self.current_char {
            Some('(') => {
                self.advance();
                Some(self.token(Token::LeftParen, start_loc))
            }
            Some(')') => {
                self.advance();
                Some(self.token(Token::RightParen, start_loc))
            }
            Some('+') => {
                self.advance();
                Some(self.token(Token::Plus, start_loc))
            }
            Some('-') => {
                self.advance();
                Some(self.token(Token::Minus, start_loc))
            }
            Some('*') => {
                self.advance();
                Some(self.token(Token::Star, start_loc))
            }
            Some('/') => {
                self.advance();
                Some(self.token(Token::Slash, start_loc))
            }
            Some('=') => {
                self.advance();
                Some(self.token(Token::Equal, start_loc))
            }
            Some(';') => {
                self.advance();
                Some(self.token(Token::Semicolon, start_loc))
            }
            Some(c) if c.is_ascii_digit() => Some(self.number()),
            Some(c) if c.is_alphabetic() || c == '_' => Some(self.identifier()),
            Some(c) => {
                self.advance();
                Some(self.token(Token::Illegal(c.to_string()), start_loc))
            }
            None => None,
        }
    }

    pub fn get_line(&self, line_num: usize) -> Option<&String> {
        if line_num > 0 && line_num <= self.source_lines.len() {
            Some(&self.source_lines[line_num - 1])
        } else {
            None
        }
    }
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Option<TokenWithSpan>,
    peek_token: Option<TokenWithSpan>,
    errors: Vec<ParseError>,
    source_path: String,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, source_path: impl AsRef<Path>) -> Self {
        let mut lexer = Lexer::new(source);
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let source_path = source_path.as_ref().display().to_string();

        Parser {
            lexer,
            current_token,
            peek_token,
            errors: Vec::new(),
            source_path,
        }
    }

    fn advance(&mut self) {
        self.current_token = self.peek_token.take();
        self.peek_token = self.lexer.next_token();
    }

    fn current_is(&self, token: &Token) -> bool {
        match &self.current_token {
            Some(t) => matches_token_type(&t.token, token),
            None => false,
        }
    }

    fn peek_is(&self, token: &Token) -> bool {
        match &self.peek_token {
            Some(t) => matches_token_type(&t.token, token),
            None => false,
        }
    }

    fn create_error(&mut self, message: String, span: Span) -> ParseError {
        let line_num = span.start.line;
        let source_line = self
            .lexer
            .get_line(line_num)
            .cloned()
            .unwrap_or_else(|| String::from("<source line unavailable>"));

        ParseError {
            message,
            span,
            source_line,
        }
    }

    fn expect_current(&mut self, token: &Token) -> Result<TokenWithSpan, String> {
        match self.current_token.clone() {
            Some(t) if matches_token_type(&t.token, token) => {
                let current = t.clone();
                self.advance();
                Ok(current)
            }
            Some(t) => {
                let error_msg = format!("Expected {:?}, got {:?}", token, t.token);
                let error = self.create_error(error_msg, t.span.clone());
                self.errors.push(error);
                Err(format_error_message(
                    &self.source_path,
                    self.errors.last().unwrap(),
                ))
            }
            None => {
                let span = match self.lexer.source_lines.last() {
                    Some(last_line) => {
                        let line = self.lexer.source_lines.len();
                        let column = last_line.len() + 1;
                        Span {
                            start: Location {
                                line,
                                column,
                                offset: 0,
                            },
                            end: Location {
                                line,
                                column,
                                offset: 0,
                            },
                        }
                    }
                    None => Span {
                        start: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                        end: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                    },
                };

                let error_msg = format!("Expected {token:?}, got EOF");
                let error = self.create_error(error_msg, span);
                self.errors.push(error);
                Err(format_error_message(
                    &self.source_path,
                    self.errors.last().unwrap(),
                ))
            }
        }
    }

    fn parse_factor(&mut self) -> Option<Expression> {
        match self.current_token.clone() {
            Some(token) => match &token.token {
                Token::LeftParen => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    self.expect_current(&Token::RightParen).ok()?;
                    Some(expr)
                }
                Token::Number(n) => {
                    self.advance();
                    Some(Expression::Number(*n))
                }
                Token::Identifier(id) => {
                    self.advance();
                    Some(Expression::Variable(id.clone()))
                }
                _ => {
                    let error_msg = format!("Unexpected token {:?} in factor", token.token);
                    let error = self.create_error(error_msg, token.span.clone());
                    self.errors.push(error);
                    None
                }
            },
            None => {
                let span = match self.lexer.source_lines.last() {
                    Some(last_line) => {
                        let line = self.lexer.source_lines.len();
                        let column = last_line.len() + 1;
                        Span {
                            start: Location {
                                line,
                                column,
                                offset: 0,
                            },
                            end: Location {
                                line,
                                column,
                                offset: 0,
                            },
                        }
                    }
                    None => Span {
                        start: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                        end: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                    },
                };

                let error =
                    self.create_error("Unexpected EOF while parsing factor".to_string(), span);
                self.errors.push(error);
                None
            }
        }
    }

    fn parse_term(&mut self) -> Option<Expression> {
        let mut left = self.parse_factor()?;

        while self.current_is(&Token::Star) || self.current_is(&Token::Slash) {
            let op_token = self.current_token.clone().unwrap();
            self.advance();
            let right = self.parse_factor()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: op_token.token.clone(),
                right: Box::new(right),
            };
        }

        Some(left)
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        let mut left = self.parse_term()?;

        while self.current_is(&Token::Plus) || self.current_is(&Token::Minus) {
            let op_token = self.current_token.clone().unwrap();
            self.advance();
            let right = self.parse_term()?;

            left = Expression::Binary {
                left: Box::new(left),
                op: op_token.token.clone(),
                right: Box::new(right),
            };
        }

        Some(left)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match &self.current_token {
            Some(token) => match &token.token {
                Token::Return => {
                    self.advance();
                    let expr = self.parse_expression()?;
                    Some(Statement::Return(expr))
                }
                Token::Let => {
                    self.advance();

                    let name_token = self
                        .expect_current(&Token::Identifier(String::new()))
                        .ok()?;
                    let Token::Identifier(name) = name_token.token else {
                        unreachable!()
                    };

                    let type_token = self
                        .expect_current(&Token::Identifier(String::new()))
                        .ok()?;
                    let Token::Identifier(type_name) = type_token.token else {
                        unreachable!()
                    };

                    self.expect_current(&Token::Equal).ok()?;

                    let expr = self.parse_expression()?;

                    Some(Statement::VariableCreate {
                        name,
                        _type: type_name,
                        value: expr,
                    })
                }
                Token::Identifier(_) => {
                    let name_token = self.current_token.clone().unwrap();
                    self.advance();
                    let Token::Identifier(name) = name_token.token else {
                        unreachable!()
                    };

                    self.expect_current(&Token::Equal).ok()?;

                    let expr = self.parse_expression()?;

                    Some(Statement::Assign { name, value: expr })
                }
                _ => {
                    let error_msg = format!("Unexpected token {:?} for statement", token.token);
                    let error = self.create_error(error_msg, token.span.clone());
                    self.errors.push(error);
                    None
                }
            },
            None => {
                let span = match self.lexer.source_lines.last() {
                    Some(last_line) => {
                        let line = self.lexer.source_lines.len();
                        let column = last_line.len() + 1;
                        Span {
                            start: Location {
                                line,
                                column,
                                offset: 0,
                            },
                            end: Location {
                                line,
                                column,
                                offset: 0,
                            },
                        }
                    }
                    None => Span {
                        start: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                        end: Location {
                            line: 1,
                            column: 1,
                            offset: 0,
                        },
                    },
                };

                let error =
                    self.create_error("Unexpected EOF while parsing statement".to_string(), span);
                self.errors.push(error);
                None
            }
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Statement>, Vec<String>> {
        let mut statements = Vec::new();

        while self.current_token.is_some() {
            match self.parse_statement() {
                Some(stmt) => {
                    statements.push(stmt);

                    if self.current_is(&Token::Semicolon) {
                        self.advance();
                    } else if self.current_token.is_some() {
                        let error_msg = "Expected semicolon after statement".to_string();
                        let span = match &self.current_token {
                            Some(t) => t.span.clone(),
                            None => unreachable!(),
                        };

                        let error = self.create_error(error_msg, span);
                        self.errors.push(error);
                        break;
                    }
                }
                None => break,
            }
        }

        if self.errors.is_empty() {
            Ok(statements)
        } else {
            let formatted_errors = self
                .errors
                .iter()
                .map(|e| format_error_message(&self.source_path, e))
                .collect();

            Err(formatted_errors)
        }
    }
}

fn matches_token_type(a: &Token, b: &Token) -> bool {
    match (a, b) {
        (Token::LeftParen, Token::LeftParen) => true,
        (Token::RightParen, Token::RightParen) => true,
        (Token::Plus, Token::Plus) => true,
        (Token::Minus, Token::Minus) => true,
        (Token::Star, Token::Star) => true,
        (Token::Slash, Token::Slash) => true,
        (Token::Equal, Token::Equal) => true,
        (Token::Semicolon, Token::Semicolon) => true,
        (Token::Return, Token::Return) => true,
        (Token::Let, Token::Let) => true,
        (Token::Number(_), Token::Number(_)) => true,
        (Token::Identifier(_), Token::Identifier(_)) => true,
        _ => false,
    }
}

fn format_error_message(source_path: &str, error: &ParseError) -> String {
    let line = error.span.start.line;
    let column = error.span.start.column;
    let end_column = error.span.end.column;
    let width = if end_column > column {
        end_column - column
    } else {
        1
    };

    let header = format!("{}:{}:{}: {}", source_path, line, column, error.message);

    let code_line = &error.source_line;

    let mut pointer = String::with_capacity(column + width + 1);
    for _ in 0..column - 1 {
        pointer.push(' ');
    }
    pointer.push('^');
    for _ in 1..width {
        pointer.push('~');
    }

    format!("{header}\n{code_line}\n{pointer}")
}

pub mod parser {
    use super::{Parser, Path, Statement};

    pub fn parse_program(
        input: &str,
        source_path: impl AsRef<Path>,
    ) -> Result<Vec<Statement>, Vec<String>> {
        let mut parser = Parser::new(input, source_path);
        parser.parse_program()
    }
}

pub mod token {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Plus,
        Minus,
        Star,
        Slash,
        LeftParen,
        RightParen,
        Equal,
        Semicolon,
        Return,
        Let,
        Number(u64),
        Identifier(String),
        Illegal(String),
    }
}
pub mod ast {
    use crate::frontend::token;

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Number(u64),
        Variable(String),
        Binary {
            left: Box<Expression>,
            op: token::Token,
            right: Box<Expression>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Return(Expression),
        VariableCreate {
            name: String,
            _type: String,
            value: Expression,
        },
        Assign {
            name: String,
            value: Expression,
        },
    }
}
