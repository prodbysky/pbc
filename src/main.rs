fn main() {
    let src = "123+69".trim();
    let Some((tokens, rest)) = tokenizer::tokenize(src) else {
        eprintln!("Failed to tokenize the source code");
        return;
    };
    println!("Parsed out these tokens: {tokens:?}");
    println!("With this left over: \"{rest}\"");
}

mod ast {
    use crate::tokenizer;
    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Number(u64),
        Binary {
            left: Box<Expression>,
            op: tokenizer::Token,
            right: Box<Expression>,
        },
    }

    pub fn parse(mut tokens: &[tokenizer::Token]) -> Option<Vec<Expression>> {
        let mut tree = vec![];
        while !tokens.is_empty() {
            let (expr, rest) = parse_expr(tokens)?;
            tree.push(expr);
            tokens = rest;
        }
        Some(tree)
    }
    fn parse_expr(tokens: &[tokenizer::Token]) -> Option<(Expression, &[tokenizer::Token])> {
        let (mut left, mut rest) = parse_term(tokens)?;
        while let Some(op) = rest
            .first()
            .filter(|t| matches!(t, tokenizer::Token::Plus | tokenizer::Token::Minus))
        {
            let op = *op;
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

    fn parse_term(tokens: &[tokenizer::Token]) -> Option<(Expression, &[tokenizer::Token])> {
        let (mut left, mut rest) = parse_factor(tokens)?;
        while let Some(op) = rest
            .first()
            .filter(|t| matches!(t, tokenizer::Token::Star | tokenizer::Token::Slash))
        {
            let op = *op;
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

    fn parse_factor(tokens: &[tokenizer::Token]) -> Option<(Expression, &[tokenizer::Token])> {
        match tokens.split_first() {
            Some((tokenizer::Token::OpenParen, rest)) => {
                let (expr, rest) = parse_expr(rest)?;
                if let Some((tokenizer::Token::CloseParen, rest)) = rest.split_first() {
                    Some((expr, rest))
                } else {
                    None
                }
            }
            Some((tokenizer::Token::Number(n), rest)) => Some((Expression::Number(*n), rest)),
            _ => None,
        }
    }

    #[cfg(test)]
    mod tests {
        use super::tokenizer::Token;
        use super::*;
        #[test]
        fn basic_binary_expression() {
            let tokens = vec![Token::Number(1), Token::Plus, Token::Number(2)];
            let tree = parse(&tokens).unwrap();
            assert_eq!(
                tree,
                vec![Expression::Binary {
                    left: Box::new(Expression::Number(1)),
                    op: Token::Plus,
                    right: Box::new(Expression::Number(2))
                }]
            );
        }

        #[test]
        fn parenthesized() {
            let tokens = vec![
                Token::Number(2),
                Token::Star,
                Token::OpenParen,
                Token::Number(2),
                Token::Minus,
                Token::Number(1),
                Token::CloseParen,
            ];
            let tree = parse(&tokens).unwrap();
            assert_eq!(
                tree,
                vec![Expression::Binary {
                    left: Box::new(Expression::Number(2)),
                    op: Token::Star,
                    right: Box::new(Expression::Binary {
                        left: Box::new(Expression::Number(2)),
                        op: Token::Minus,
                        right: Box::new(Expression::Number(1)),
                    }),
                }]
            );
        }
        #[test]
        fn precedence() {
            let tokens = vec![
                Token::Number(2),
                Token::Star,
                Token::Number(2),
                Token::Minus,
                Token::Number(1),
            ];
            let tree = parse(&tokens).unwrap();
            assert_eq!(
                tree,
                vec![Expression::Binary {
                    left: Box::new(Expression::Binary {
                        left: Box::new(Expression::Number(2)),
                        op: Token::Star,
                        right: Box::new(Expression::Number(2))
                    }),
                    op: Token::Minus,
                    right: Box::new(Expression::Number(1)),
                }]
            );
        }
    }
}

mod tokenizer {
    #[derive(Debug, PartialEq, Clone, Copy)]
    pub enum Token {
        Number(u64),
        Plus,
        Minus,
        Star,
        Slash,
        OpenParen,
        CloseParen,
    }

    pub fn tokenize(mut src: &str) -> Option<(Vec<Token>, &str)> {
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
            eprintln!("Unexpected characters found: {src}");
            return None;
        }
        Some((tokens, src))
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
    }
}
