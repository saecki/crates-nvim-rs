mod error;
mod lex;
mod parse;

pub use error::*;
pub use lex::*;
pub use parse::*;

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
