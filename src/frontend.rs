pub mod token {
    use thiserror::Error;
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Number(u64),
        Plus,
        Minus,
        Star,
        Slash,
        OpenParen,
        CloseParen,
        Return,
        Let,
        Ident(String),
        Semicolon,
        Colon,
        SingleEq,
    }

    #[derive(Debug, Error)]
    pub enum TokenizerError {
        #[error("Failed to parse this number: {0}")]
        InvalidNumber(String),
    }
    pub type TokenizerResult<T> = Result<T, TokenizerError>;

    pub fn tokenize(mut src: &str) -> TokenizerResult<(Vec<Token>, &str)> {
        let mut tokens: Vec<Token> = vec![];

        while !src.is_empty() {
            src = src.trim();
            if src.starts_with(';') {
                tokens.push(Token::Semicolon);
                src = &src[1..];
                continue;
            }
            if let Some((t, s)) = parse_operator(src) {
                tokens.push(t);
                src = s;
                continue;
            }
            if let Some((t, s)) = parse_paren(src) {
                tokens.push(t);
                src = s;
                continue;
            }

            if src.starts_with(|c: char| c.is_ascii_digit()) {
                let (token, rest) = parse_u64(src)?;
                src = rest;
                tokens.push(token);
                continue;
            }

            let (ident, rest) = parse_ident(src);
            src = rest;
            tokens.push(ident);
        }
        Ok((tokens, src))
    }

    fn parse_u64(src: &str) -> TokenizerResult<(Token, &str)> {
        Ok(match src.find(|c: char| !c.is_ascii_digit()) {
            None => (
                Token::Number(
                    src.parse()
                        .map_err(|_| TokenizerError::InvalidNumber(src.to_string()))?,
                ),
                "",
            ),
            Some(end) => match src.split_at_checked(end) {
                Some((num, rest)) => (
                    Token::Number(
                        num.parse()
                            .map_err(|_| TokenizerError::InvalidNumber(src.to_string()))?,
                    ),
                    rest,
                ),
                None => (
                    Token::Number(
                        src.parse()
                            .map_err(|_| TokenizerError::InvalidNumber(src.to_string()))?,
                    ),
                    "",
                ),
            },
        })
    }

    fn parse_ident(src: &str) -> (Token, &str) {
        let end = src
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .unwrap_or(src.len());
        let (ident, rest) = src.split_at(end);

        match ident {
            "return" => (Token::Return, rest),
            "let" => (Token::Let, rest),
            _ => (Token::Ident(ident.to_string()), rest),
        }
    }

    fn parse_operator(src: &str) -> Option<(Token, &str)> {
        match src.chars().next()? {
            '+' => Some((Token::Plus, &src[1..])),
            '-' => Some((Token::Minus, &src[1..])),
            '*' => Some((Token::Star, &src[1..])),
            '/' => Some((Token::Slash, &src[1..])),
            '=' => Some((Token::SingleEq, &src[1..])),
            ':' => Some((Token::Colon, &src[1..])),
            _ => None,
        }
    }

    fn parse_paren(src: &str) -> Option<(Token, &str)> {
        match src.chars().next()? {
            '(' => Some((Token::OpenParen, &src[1..])),
            ')' => Some((Token::CloseParen, &src[1..])),
            _ => None,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        #[test]
        fn empty() {
            let src = "";
            let (tokens, rest) = tokenize(src).unwrap();
            assert_eq!(tokens, vec![]);
            assert_eq!(rest, "");
        }

        #[test]
        fn some_numbers() {
            let src = "123 69 1337";
            let (tokens, rest) = tokenize(src).unwrap();
            assert_eq!(
                tokens,
                vec![Token::Number(123), Token::Number(69), Token::Number(1337),]
            );
            assert_eq!(rest, "");
        }
        #[test]
        fn some_operators() {
            let src = "+-/*";
            let (tokens, rest) = tokenize(src).unwrap();
            assert_eq!(
                tokens,
                vec![Token::Plus, Token::Minus, Token::Slash, Token::Star]
            );
            assert_eq!(rest, "");
        }
        #[test]
        fn some_operators_and_numbers() {
            let src = "69 + 10 - 5 / 2 *";
            let (tokens, rest) = tokenize(src).unwrap();
            assert_eq!(
                tokens,
                vec![
                    Token::Number(69),
                    Token::Plus,
                    Token::Number(10),
                    Token::Minus,
                    Token::Number(5),
                    Token::Slash,
                    Token::Number(2),
                    Token::Star
                ]
            );
            assert_eq!(rest, "");
        }
        #[test]
        fn some_keywords() {
            let src = "return";
            let (tokens, rest) = tokenize(src).unwrap();
            assert_eq!(tokens, vec![Token::Return,]);
            assert_eq!(rest, "");
        }
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

    pub fn parse(mut tokens: &[token::Token]) -> Option<Vec<Statement>> {
        let mut tree = vec![];
        while !tokens.is_empty() {
            match tokens.first() {
                None => return Some(tree),
                Some(token::Token::Ident(name)) => {
                    tokens = &tokens[1..];
                    let (token::Token::SingleEq, rest) = tokens.split_first()? else {
                        return None;
                    };
                    tokens = rest;
                    let (val, rest) = parse_expr(tokens)?;
                    tokens = rest;
                    tree.push(Statement::Assign {
                        name: name.to_string(),
                        value: val,
                    });
                }
                Some(token::Token::Return) => {
                    tokens = &tokens[1..];
                    let Some((expression, rest)) = parse_expr(tokens) else {
                        unreachable!();
                    };
                    tokens = rest;
                    tree.push(Statement::Return(expression));
                }
                Some(token::Token::Let) => {
                    let (_, rest) = tokens.split_first()?;
                    tokens = rest;
                    let (token::Token::Ident(name), rest) = tokens.split_first()? else {
                        return None;
                    };
                    tokens = rest;

                    let (token::Token::Ident(_type), rest) = tokens.split_first()? else {
                        return None;
                    };
                    tokens = rest;

                    let (token::Token::SingleEq, rest) = tokens.split_first()? else {
                        return None;
                    };
                    tokens = rest;

                    let (parsed, rest) = parse_expr(tokens)?;
                    tokens = rest;
                    tree.push(Statement::VariableCreate {
                        name: name.to_string(),
                        _type: _type.to_string(),
                        value: parsed,
                    });
                }
                _ => {
                    return None;
                }
            }

            if let Some(token::Token::Semicolon) = tokens.first() {
                tokens = &tokens[1..];
            } else {
                return None;
            }
        }
        Some(tree)
    }
    fn parse_expr(tokens: &[token::Token]) -> Option<(Expression, &[token::Token])> {
        let (mut left, mut rest) = parse_term(tokens)?;
        while let Some(op) = rest
            .first()
            .filter(|t| matches!(t, token::Token::Plus | token::Token::Minus))
        {
            let op = op.clone();
            let (right, new_rest) = parse_term(&rest[1..])?;
            left = Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            rest = new_rest;
        }
        Some((left, rest))
    }

    fn parse_term(tokens: &[token::Token]) -> Option<(Expression, &[token::Token])> {
        let (mut left, mut rest) = parse_factor(tokens)?;
        while let Some(op) = rest
            .first()
            .filter(|t| matches!(t, token::Token::Star | token::Token::Slash))
        {
            let op = op.clone();
            let (right, new_rest) = parse_factor(&rest[1..])?;
            left = Expression::Binary {
                left: Box::new(left),
                op,
                right: Box::new(right),
            };
            rest = new_rest;
        }
        Some((left, rest))
    }

    fn parse_factor(tokens: &[token::Token]) -> Option<(Expression, &[token::Token])> {
        match tokens.split_first() {
            Some((token::Token::OpenParen, rest)) => {
                let (expr, rest) = parse_expr(rest)?;
                if let Some((token::Token::CloseParen, rest)) = rest.split_first() {
                    Some((expr, rest))
                } else {
                    None
                }
            }
            Some((token::Token::Number(n), rest)) => Some((Expression::Number(*n), rest)),
            Some((token::Token::Ident(name), rest)) => {
                Some((Expression::Variable(name.clone()), rest))
            }
            _ => None,
        }
    }
}
