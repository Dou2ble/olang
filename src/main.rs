use ::std::fs::File;
use anyhow::Result;
use interpreter::EvalError;
use std::{io::Read, path::PathBuf};
use structopt::StructOpt;
use value::Value;

mod ast;
mod builtin;
mod environment;
mod interpreter;
mod lexer;
mod location;
mod parser;
mod value;

#[cfg(test)]
mod tests;

/// The easy to use interpreter
#[derive(StructOpt, Debug)]
#[structopt(name = "olang")]
struct Options {
    // /// print token sequence and AST of the source code
    // #[structopt(short, long)]
    // debug: bool,
    /// Source string to process
    #[structopt(short, long)]
    command_string: Option<String>,

    /// Source file to process
    #[structopt(name = "FILE", parse(from_os_str))]
    file: Option<PathBuf>,
}

pub fn eval(source: &str) -> Result<Value, EvalError> {
    interpreter::Interpreter::new().eval(source)
}

fn main() -> Result<()> {
    run_cli()?;
    Ok(())
}

pub fn run_cli() -> Result<()> {
    let options = Options::from_args();

    if let Some(command) = options.command_string {
        eval(command.as_str())?;
    };

    if let Some(path) = options.file {
        let mut file = File::open(path)?;
        let mut content = String::new();
        file.read_to_string(&mut content)?;

        eval(content.as_str())?;
    };

    Ok(())
}
