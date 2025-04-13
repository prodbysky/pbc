#[derive(Debug)]
enum Token {
    Number(u64),
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

fn main() {
    let mut src = "123 69".trim();

    let mut tokens: Vec<Token> = vec![];

    while !src.is_empty() {
        src = src.trim();

        if src.starts_with(|c: char| c.is_ascii_digit()) {
            let (token, rest) = parse_u64(src).unwrap();
            src = rest;
            tokens.push(token);
        }
    }

    dbg!(tokens);
}

mod tokenizer {}
