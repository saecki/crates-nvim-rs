pub mod toml;

use nvim_oxi::{Dictionary, Function};

use crate::toml::{Ast, Ctx, Ident, Key, Value};

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
        let Ok(asts) = ctx.lex(&text).and_then(|tokens| ctx.parse(tokens)) else {
            return Ok(());
        };

        let mut path = Vec::new();
        for ast in asts.iter() {
            match ast {
                Ast::Assignment(a) => {
                    push_key(&mut path, &a.key);
                    validate(&mut path, &a.val);
                }
                Ast::Table(_) => todo!(),
                Ast::Array(_) => todo!(),
                Ast::Comment(_) => todo!(),
            }
        }
        Ok(())
    });

    Ok(Dictionary::from_iter([("parse_toml", parse_toml)]))
}

fn push_key<'a>(path: &mut Vec<&'a Ident<'a>>, key: &'a Key<'a>) {
    match key {
        Key::One(i) => path.push(i),
        Key::Dotted(idents) => path.extend(idents.iter().map(|d| &d.ident)),
    }
}

fn validate(path: &mut Vec<&Ident>, val: &Value) {
    match val {
        Value::String(_) => todo!(),
        Value::Int(_) => todo!(),
        Value::Float(_) => todo!(),
        Value::Bool(_) => todo!(),
        Value::DateTime(_) => todo!(),
        Value::InlineTable(_) => todo!(),
        Value::InlineArray(_) => todo!(),
        Value::Invalid(_, _) => todo!(),
    }
}
