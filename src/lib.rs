pub mod toml;

use nvim_oxi::{Dictionary, Function};

use crate::toml::Ctx;

#[nvim_oxi::module]
pub fn libcrates_nvim() -> nvim_oxi::Result<Dictionary> {
    let parse_toml = Function::from_fn::<_, nvim_oxi::Error>(move |()| {
        let buf = nvim_oxi::api::get_current_buf();
        let num_lines = buf.line_count()?;
        let lines = buf.get_lines(0..num_lines, true)?;
        let mut text = String::new();
        for line in lines {
            let str = unsafe { std::str::from_utf8_unchecked(line.as_bytes()) };
            text.push_str(str);
            text.push('\n');
        }

        let mut ctx = Ctx::default();
        let tokens = ctx.lex(&text);
        let asts = ctx.parse(tokens);
        let _map = ctx.map(&asts);

        // TODO

        Ok(())
    });

    Ok(Dictionary::from_iter([("parse_toml", parse_toml)]))
}
