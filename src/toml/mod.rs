mod error;
mod token;

pub use error::*;
pub use token::*;

#[derive(Default)]
pub struct Ctx {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

pub fn parse(input: &str) -> Result<Vec<Token>, Error> {
    let mut ctx = Ctx::default();
    let tokens = ctx.tokenize(input)?;
    Ok(tokens)
}
