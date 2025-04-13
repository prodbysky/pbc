mod backend;
mod frontend;

use crate::backend::Backend;

fn main() {
    let src = "return 123+69".trim();
    let before = std::time::Instant::now();
    let (tokens, _rest) = frontend::token::tokenize(src);
    let Some(ast) = frontend::ast::parse(&tokens) else {
        eprintln!("Failed to construct the ast");
        return;
    };
    eprintln!("AST construction took: {:.2?}", before.elapsed());

    let before = std::time::Instant::now();
    let mut backend_engine = backend::InkwellBackend::new();
    backend_engine.generate(&ast);
    eprintln!("Program IR generation took: {:.2?}", before.elapsed());

    unsafe {
        let result = backend_engine.get_main()();
        println!("Program exited with: {result}");
    }
}
