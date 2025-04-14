mod backend;
mod frontend;

use crate::backend::Backend;

use clap::Parser;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ProgramError {
    #[error("An IO error occured")]
    IO(#[from] std::io::Error),
    #[error("An compilation error occured")]
    Backend(#[from] backend::InkwellError),
}

pub type ProgramResult<T> = Result<T, ProgramError>;

fn main() -> ProgramResult<()> {
    let config = Config::parse();
    let src = std::fs::read_to_string(&config.input_name)?;

    let ast = match frontend::parser::peg_parser::program(&src) {
        Ok(a) => a,
        Err(e) => {
            let line = e.location.line;
            let line: &str = src.lines().collect::<Vec<&str>>().get(line - 1).unwrap();
            let location = format!(
                "{}:{}:{}",
                config.input_name, e.location.line, e.location.column
            );
            eprintln!("Failed to parse:");
            eprintln!("{location}: {line}");
            let pad = " ".repeat(e.location.column + location.len() + 1);
            eprintln!("{pad}^");
            return Ok(());
        }
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
