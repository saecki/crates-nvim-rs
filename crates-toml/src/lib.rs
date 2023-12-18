pub use error::{Error, Warning};
pub use lex::{Pos, Quote, Span, Token, TokenType};

pub mod datetime;
pub mod error;
mod lex;
pub mod map;

#[macro_use]
pub mod onevec;
pub mod parse;

#[derive(Default)]
pub struct Ctx {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
}

// pub struct Toml<'a /* 'b: 'a */> {
//     input: Pin<Box<str>>,
//     asts: Pin<Box<[Ast<'a>]>>,
//     // map: HashMap<&'b str, (&'b Ast<'a>, MapEntry<'b>)>,
// }
