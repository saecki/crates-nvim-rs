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

    fn parse<'a>(&mut self, bump: &'a Bump, path: &'a str, text: &'a str) -> Toml<'a> {
        let tokens = lex::lex(self, bump, path, text);
        let ast = parse::parse(self, bump, tokens);
        let map = map::map(self, bump, &ast);
        Toml { ast, map }
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
