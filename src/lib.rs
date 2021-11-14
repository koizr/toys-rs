use error::ToysError;

mod ast;
pub mod error;
mod interpreter;
mod parser;
#[cfg(test)]
mod tests;

pub fn run(code: &str) -> Result<i32, ToysError> {
    let program = parser::parse(code)?;

    let mut interpreter = interpreter::Interpreter::default();
    let exit = interpreter.call_main(program);

    Ok(exit)
}
