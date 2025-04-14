pub mod parser {
    use crate::frontend::ast::{Expression, Statement};
    use crate::frontend::token::Token;
    peg::parser! {
        pub grammar peg_parser() for str {
            rule ws() -> () = [' ' | '\t' | '\n']* {  }

            rule ws1() -> () = [' ' | '\t' | '\n']+ { }

            rule number() -> Expression
                = n:$(['0'..='9']+) {
                      Expression::Number(n.parse().unwrap())
                  }

            rule ident() -> String
                = id:$((['a'..='z' | 'A'..='Z' | '_'])
                        (['a'..='z' | 'A'..='Z' | '0'..='9' | '_'])*)
                  { id.to_string() }

            rule factor() -> Expression
                = "(" ws() expr:expression() ws() ")" { expr }
                / number()
                / id:ident() { Expression::Variable(id) }

            rule term() -> Expression
                = init:factor()
                  tail:(ws() op:['*' | '/'] ws() f:factor() { (op, f) })* {
                    tail.into_iter().fold(init, |left, (op, right)| {
                        let operator = match op {
                            '*' => Token::Star,
                            '/' => Token::Slash,
                            _   => unreachable!(),
                        };
                        Expression::Binary {
                            left: Box::new(left),
                            op: operator,
                            right: Box::new(right),
                        }
                    })
                  }

            rule expression() -> Expression
                = init:term()
                  tail:(ws() op:['+' | '-'] ws() t:term() { (op, t) })* {
                    tail.into_iter().fold(init, |left, (op, right)| {
                        let operator = match op {
                            '+' => Token::Plus,
                            '-' => Token::Minus,
                            _   => unreachable!(),
                        };
                        Expression::Binary {
                            left: Box::new(left),
                            op: operator,
                            right: Box::new(right),
                        }
                    })
                  }

            rule statement() -> Statement
                = "return" ws1() expr:expression() {
                      Statement::Return(expr)
                  }
                / "let" ws1() name:ident() ws1() type_name:ident() ws() "=" ws() expr:expression() {
                      Statement::VariableCreate {
                          name,
                          _type: type_name,
                          value: expr,
                      }
                  }
                / name:ident() ws() "=" ws() expr:expression() {
                      Statement::Assign { name, value: expr }
                  }

            rule semicolon() = ws() ";" ws()

            pub rule program() -> Vec<Statement>
                = ws() stmts:(statement() ** semicolon()) semicolon()? ws() { stmts }
        }
    }
}

pub mod token {
    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Plus,
        Minus,
        Star,
        Slash,
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
