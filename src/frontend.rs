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
        Let,
        Mut,
        Ident(String),
        Semicolon,
        SingleEq,
    }

    pub fn tokenize(mut src: &str) -> (Vec<Token>, &str) {
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

        match ident {
            "return" => (Token::Return, rest),
            "let" => (Token::Let, rest),
            "mut" => (Token::Mut, rest),
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
        ConstantAssign(String, Expression),
        MutableAssign(String, Expression),
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
                    if let Some(token::Token::Semicolon) = tokens.first() {
                        tree.push(Statement::Return(expression));
                        tokens = &tokens[1..];
                    } else {
                        return None;
                    }
                }
                Some(token::Token::Let) => {
                    tokens = &tokens[1..];
                    let token::Token::Ident(ref name) = tokens[0] else {
                        unreachable!()
                    };
                    tokens = &tokens[2..];

                    let Some((expression, rest)) = parse_expr(tokens) else {
                        unreachable!();
                    };
                    tokens = rest;
                    if let Some(token::Token::Semicolon) = tokens.first() {
                        tree.push(Statement::ConstantAssign(name.clone(), expression));
                        tokens = &tokens[1..];
                    } else {
                        return None;
                    }
                }
                Some(token::Token::Mut) => {
                    tokens = &tokens[1..];
                    let token::Token::Ident(ref name) = tokens[0] else {
                        unreachable!()
                    };
                    tokens = &tokens[1..];

                    let Some((expression, rest)) = parse_expr(tokens) else {
                        unreachable!();
                    };
                    tokens = rest;
                    if let Some(token::Token::Semicolon) = tokens.first() {
                        tree.push(Statement::MutableAssign(name.clone(), expression));
                        tokens = &tokens[1..];
                    } else {
                        return None;
                    }
                }
                _ => {
                    return None;
                }
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
