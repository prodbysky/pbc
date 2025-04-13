pub mod token {
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
        Ident(String),
    }

    pub fn tokenize(mut src: &str) -> (Vec<Token>, &str) {
        let mut tokens: Vec<Token> = vec![];

        while !src.is_empty() {
            src = src.trim();
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
                let (token, rest) = parse_u64(src).unwrap();
                src = rest;
                tokens.push(token);
                continue;
            }

            let (ident, rest) = parse_ident(src);
            src = rest;
            tokens.push(ident);
        }
        (tokens, src)
    }

    fn parse_u64(src: &str) -> Option<(Token, &str)> {
        Some(match src.find(|c: char| !c.is_ascii_digit()) {
            None => (Token::Number(src.parse().ok()?), ""),
            Some(end) => match src.split_at_checked(end) {
                Some((num, rest)) => (Token::Number(num.parse().ok()?), rest),
                None => (Token::Number(src.parse().ok()?), ""),
            },
        })
    }

    fn parse_ident(src: &str) -> (Token, &str) {
        let end = src
            .find(|c: char| !(c.is_ascii_alphanumeric() || c == '_'))
            .unwrap_or(src.len());
        let (ident, rest) = src.split_at(end);

        if ident == "return" {
            (Token::Return, rest)
        } else {
            (Token::Ident(ident.to_string()), rest)
        }
    }

    fn parse_operator(src: &str) -> Option<(Token, &str)> {
        match src.chars().next()? {
            '+' => Some((Token::Plus, &src[1..])),
            '-' => Some((Token::Minus, &src[1..])),
            '*' => Some((Token::Star, &src[1..])),
            '/' => Some((Token::Slash, &src[1..])),
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
            let (tokens, rest) = tokenize(src);
            assert_eq!(tokens, vec![]);
            assert_eq!(rest, "");
        }

        #[test]
        fn some_numbers() {
            let src = "123 69 1337";
            let (tokens, rest) = tokenize(src);
            assert_eq!(
                tokens,
                vec![Token::Number(123), Token::Number(69), Token::Number(1337),]
            );
            assert_eq!(rest, "");
        }
        #[test]
        fn some_operators() {
            let src = "+-/*";
            let (tokens, rest) = tokenize(src);
            assert_eq!(
                tokens,
                vec![Token::Plus, Token::Minus, Token::Slash, Token::Star]
            );
            assert_eq!(rest, "");
        }
        #[test]
        fn some_operators_and_numbers() {
            let src = "69 + 10 - 5 / 2 *";
            let (tokens, rest) = tokenize(src);
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
            let (tokens, rest) = tokenize(src);
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
        Binary {
            left: Box<Expression>,
            op: token::Token,
            right: Box<Expression>,
        },
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Return(Expression),
    }

    impl ToString for Statement {
        fn to_string(&self) -> String {
            match self {
                Statement::Return(e) => {
                    format!("std::process::exit({})", e.to_string())
                }
            }
        }
    }

    impl ToString for Expression {
        fn to_string(&self) -> String {
            match self {
                Expression::Number(n) => n.to_string(),
                Expression::Binary { left, op, right } => {
                    let mut string = String::new();
                    string.push_str(&left.to_string());
                    match op {
                        token::Token::Plus => {
                            string.push('+');
                        }
                        token::Token::Minus => {
                            string.push('-');
                        }
                        token::Token::Star => {
                            string.push('*');
                        }
                        token::Token::Slash => {
                            string.push('/');
                        }
                        _ => unreachable!(),
                    }
                    string.push_str(&right.to_string());
                    string
                }
            }
        }
    }

    pub fn parse(mut tokens: &[token::Token]) -> Option<Vec<Statement>> {
        let mut tree = vec![];
        while !tokens.is_empty() {
            match tokens.first() {
                None => return Some(tree),
                Some(token::Token::Return) => {
                    tokens = &tokens[1..];
                    let Some((expression, rest)) = parse_expr(tokens) else {
                        unreachable!();
                    };
                    tokens = rest;
                    tree.push(Statement::Return(expression));
                }
                _ => unreachable!(),
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
            _ => None,
        }
    }
}
