mod backend;
mod frontend;

use crate::backend::Backend;

use clap::Parser;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ProgramError {
    #[error("An IO error occured")]
    IO(#[from] std::io::Error),
    #[error("An tokenizer error occured")]
    Tokenizer(#[from] frontend::token::TokenizerError),
    #[error("An compilation error occured")]
    Backend(#[from] backend::InkwellError),
}

pub type ProgramResult<T> = Result<T, ProgramError>;

fn main() -> ProgramResult<()> {
    let config = Config::parse();
    let src = std::fs::read_to_string(config.input_name)?;

    let (tokens, _rest) = frontend::token::tokenize(&src)?;
    let Some(ast) = frontend::ast::parse(&tokens) else {
        eprintln!("Failed to construct the ast");
        return Ok(());
    };

    let mut backend_engine = backend::InkwellBackend::new()?;
    backend_engine.generate(&ast)?;
    if config.display_ir {
        backend_engine.module.print_to_stderr();
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
