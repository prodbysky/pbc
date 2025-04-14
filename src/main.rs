mod backend;
mod frontend;

use crate::backend::Backend;

use clap::Parser;

fn main() {
    let config = Config::parse();
    let Ok(src) = std::fs::read_to_string(config.input_name) else {
        eprintln!("Failed to read input file");
        return;
    };

    let (tokens, _rest) = frontend::token::tokenize(&src);
    let Some(ast) = frontend::ast::parse(&tokens) else {
        eprintln!("Failed to construct the ast");
        return;
    };

    let mut backend_engine = backend::InkwellBackend::new();
    backend_engine.generate(&ast);
    if config.display_ir {
        println!("{}", backend_engine.module.to_string());
    }
    unsafe {
        let result = backend_engine.get_main()();
        std::process::exit(i32::try_from(result).unwrap());
    }
}

/// The runtime for the PBC programming language
#[derive(Debug, Parser)]
#[command()]
struct Config {
    /// Input file name
    #[arg()]
    input_name: String,
    #[arg(short)]
    display_ir: bool,
}
