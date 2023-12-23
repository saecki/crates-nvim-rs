pub use error::{Error, Warning};
pub use lex::{Pos, Quote, Span, Token, TokenType};

pub mod datetime;
pub mod error;
mod lex;
pub mod map;

#[macro_use]
pub mod onevec;
pub mod parse;
#[cfg(test)]
mod test;

#[derive(Default)]
pub struct Ctx {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

impl Ctx {
    fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    fn warn(&mut self, warning: Warning) {
        self.warnings.push(warning);
    }
}

// pub struct Toml<'a /* 'b: 'a */> {
//     input: Pin<Box<str>>,
//     asts: Pin<Box<[Ast<'a>]>>,
//     // map: HashMap<&'b str, (&'b Ast<'a>, MapEntry<'b>)>,
// }
