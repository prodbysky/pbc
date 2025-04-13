fn main() {
    let mut src = "123+69".trim();
    let Some((tokens, rest)) = tokenizer::tokenize(src) else {
        eprintln!("Failed to tokenize the source code");
        return;
    };
    dbg!(tokens);
}

mod tokenizer {
    #[derive(Debug, PartialEq)]
    pub enum Token {
        Number(u64),
        Plus,
        Minus,
        Star,
        Slash,
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
