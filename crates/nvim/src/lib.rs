use bumpalo::Bump;
use common::{Diagnostic, Span};
use ide::{IdeCtx, IdeDiagnostics};
use nvim_oxi::conversion::ToObject;
use nvim_oxi::serde::Serializer;
use nvim_oxi::{Dictionary, Function, Object};
use serde::{Deserialize, Serialize};
use toml::TomlCtx;

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct VimDiagnostics {
    pub errors: Vec<VimDiagnostic>,
    pub warnings: Vec<VimDiagnostic>,
    pub infos: Vec<VimDiagnostic>,
}

impl ToObject for VimDiagnostics {
    fn to_object(self) -> Result<Object, nvim_oxi::conversion::Error> {
        self.serialize(Serializer::new()).map_err(Into::into)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct VimDiagnostic {
    pub lnum: u32,
    pub end_lnum: u32,
    pub col: u32,
    pub end_col: u32,
    pub message: String,
}

impl ToObject for VimDiagnostic {
    fn to_object(self) -> Result<Object, nvim_oxi::conversion::Error> {
        self.serialize(Serializer::new()).map_err(Into::into)
    }
}

#[nvim_oxi::module]
pub fn crates_nvim_lib() -> nvim_oxi::Result<Dictionary> {
    let update = Function::from_fn::<_, nvim_oxi::Error>(move |()| update());

    Ok(Dictionary::from_iter([("update", update)]))
}

fn update() -> Result<(), nvim_oxi::Error> {
    let buf = nvim_oxi::api::get_current_buf();
    let num_lines = buf.line_count()?;
    let raw_lines = buf.get_lines(0..num_lines, true)?;

    let mut lines = Vec::with_capacity(raw_lines.len());
    let mut text = String::new();
    for line in raw_lines.into_iter() {
        // HACK
        let str = unsafe { std::str::from_utf8_unchecked(line.as_bytes()) };
        text.push_str(str);
        text.push('\n');

        lines.push(str.to_string());
    }

    let mut ctx = IdeDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, &text);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&asts);
    let state = ctx.check(&map);

    dbg!(state);

    let errors = ctx.errors.iter().map(map_vim_diagnostic).collect();
    let warnings = ctx.warnings.iter().map(map_vim_diagnostic).collect();
    let infos = ctx.infos.iter().map(map_vim_diagnostic).collect();

    let diagnostics = VimDiagnostics {
        errors,
        warnings,
        infos,
    };

    dbg!(diagnostics);

    Ok(())
}

fn map_vim_diagnostic(d: &impl Diagnostic) -> VimDiagnostic {
    let Span { start, end } = d.span();
    let mut message = String::new();
    _ = d.description(&mut message);
    VimDiagnostic {
        lnum: start.line,
        end_lnum: end.line,
        col: start.char,
        end_col: end.char,
        message,
    }
}
