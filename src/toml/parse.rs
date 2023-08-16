use crate::toml::{Ctx, Error, Token, TokenType};

#[derive(Debug, Default)]
struct State {
    rhs: bool,
}

impl Ctx {
    pub fn parse(&mut self, tokens: Vec<Token>) -> Result<(), Error> {
        let mut state = State::default();
        let mut tokens = tokens.into_iter();

        // while let Some(token) = tokens.next() {
        //     match token.ty {
        //         TokenType::Ident => {
        //
        //         }
        //     }
        // }

        Ok(())
    }
}
