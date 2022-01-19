mod error;
mod token;

pub use error::*;
pub use token::*;

#[derive(Default)]
struct Ctx {
    errors: Vec<Error>,
    warnings: Vec<Warning>,
}

pub fn parse(lines: Vec<String>) -> Result<Vec<Token>, Error> {
    let mut ctx = Ctx::default();
    let tokens = ctx.tokenize(&lines)?;
    Ok(tokens)
}
