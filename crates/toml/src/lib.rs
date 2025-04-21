use bumpalo::Bump;
use common::{Ctx, Diagnostics};

pub use container::{Container, Toml};
pub use error::{Error, Hint, Info, Warning};
pub use lex::{Quote, Token, TokenType, Tokens, lex};
pub use map::{MapTable, map};
pub use parse::{Ast, Toplevel, parse};
#[cfg(feature = "serde")]
pub use serde::deserialize;

mod container;
pub mod datetime;
pub mod error;
mod lex;
pub mod map;
pub mod parse;
#[cfg(test)]
mod test;
pub mod util;

#[cfg(feature = "serde")]
pub mod serde;

pub trait TomlCtx:
    Ctx<Error = Self::TomlError, Warning = Self::TomlWarning, Info = Self::TomlInfo>
{
    type TomlError: From<Error>;
    type TomlWarning: From<Warning>;
    type TomlInfo: From<Info>;

    fn lex<'a>(&mut self, bump: &'a Bump, input: &'a str) -> Tokens<'a> {
        lex(self, bump, input)
    }

    fn parse<'a>(&mut self, bump: &'a Bump, tokens: Tokens<'a>) -> Ast<'a> {
        parse(self, bump, tokens)
    }

    fn map<'a>(&mut self, ast: &Ast<'a>) -> MapTable<'a> {
        map(self, ast)
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
