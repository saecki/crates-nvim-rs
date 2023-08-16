mod error;
mod parse;
mod lex;

pub use error::*;
pub use lex::*;
pub use parse::*;

#[derive(Default)]
pub struct Ctx {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

pub fn parse(input: &str) -> Result<Vec<Token>, Error> {
    let mut ctx = Ctx::default();
    let tokens = ctx.lex(input)?;
    Ok(tokens)
}
