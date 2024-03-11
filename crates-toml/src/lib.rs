use bumpalo::Bump;
pub use error::{Error, Hint, Warning};
pub use lex::{lex, Pos, Quote, Span, Token, TokenType, Tokens};
pub use map::{map, MapTable};
pub use parse::{parse, Ast, Asts};

pub mod datetime;
pub mod error;
mod lex;
pub mod map;

#[macro_use]
pub mod onevec;
pub mod container;
pub mod parse;
#[cfg(test)]
mod test;

#[derive(Default)]
pub struct Ctx {
    pub errors: Vec<Error>,
    pub warnings: Vec<Warning>,
    pub hints: Vec<Hint>,
}

impl Ctx {
    pub fn error(&mut self, error: Error) {
        self.errors.push(error);
    }

    pub fn warn(&mut self, warning: Warning) {
        self.warnings.push(warning);
    }

    pub fn hint(&mut self, hint: Hint) {
        self.hints.push(hint);
    }

    pub fn lex<'a>(&mut self, bump: &'a Bump, input: &'a str) -> Tokens<'a> {
        lex(self, bump, input)
    }

    pub fn parse<'a>(&mut self, bump: &'a Bump, tokens: &Tokens<'a>) -> Asts<'a> {
        parse(self, bump, tokens)
    }

    pub fn map<'a>(&mut self, asts: &Asts<'a>) -> MapTable<'a> {
        map(self, asts)
    }
}
