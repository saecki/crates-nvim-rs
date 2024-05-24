use bumpalo::Bump;
use common::{Ctx, Diagnostics};

pub use error::{Error, Hint, Info, Warning};
pub use lex::{lex, Quote, Token, TokenType, Tokens};
pub use map::{map, MapTable};
pub use parse::{parse, Ast, Asts};

pub mod datetime;
pub mod error;
mod lex;
pub mod map;
pub mod util;
#[macro_use]
pub mod onevec;
pub mod container;
pub mod parse;
#[cfg(test)]
mod test;

pub trait TomlCtx:
    Ctx<Error = Self::TomlError, Warning = Self::TomlWarning, Info = Self::TomlInfo>
{
    type TomlError: From<Error>;
    type TomlWarning: From<Warning>;
    type TomlInfo: From<Info>;

    fn lex<'a>(&mut self, bump: &'a Bump, input: &'a str) -> Tokens<'a> {
        lex(self, bump, input)
    }

    fn parse<'a>(&mut self, bump: &'a Bump, tokens: &Tokens<'a>) -> Asts<'a> {
        parse(self, bump, tokens)
    }

    fn map<'a>(&mut self, asts: &Asts<'a>) -> MapTable<'a> {
        map(self, asts)
    }
}

impl<E, W, I> TomlCtx for Diagnostics<E, W, I>
where
    E: From<Error>,
    W: From<Warning>,
    I: From<Info>,
{
    type TomlError = E;
    type TomlWarning = W;
    type TomlInfo = I;
}

pub type TomlDiagnostics = Diagnostics<Error, Warning, Info>;
