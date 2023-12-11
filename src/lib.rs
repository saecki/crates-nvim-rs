pub mod toml;

use nvim_oxi::{Dictionary, Function};

use crate::toml::{Ctx, MapEntry, MapTable, Scalar, StringVal};

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
        let map = ctx.map(&asts);

        Ok(())
    });

    Ok(Dictionary::from_iter([("parse_toml", parse_toml)]))
}

pub struct Section {
    pub text: String,
    pub invalid: bool,
    pub workspace: bool,
    pub target: Option<String>,
    pub kind: Kind,
    pub name: Option<String>,
    pub name_col: Range,
    pub lines: Range,
}

pub enum Kind {
    Default,
    Dev,
    Build,
}

pub struct Crate {
    /// The explicit name is either the name of the package, or a rename
    /// if the following syntax is used:
    /// explicit_name = { package = "package" }
    pub explicit_name: String,
    pub explicit_name_col: Range,
    pub lines: Range,
    pub syntax: Syntax,
    pub vers: Vers,
    pub registry: Registry,
    pub path: Path,
    pub git: Git,
    pub branch: Branch,
    pub rev: Rev,
    pub pkg: Pkg,
    pub workspace: Workspace,
    pub opt: Opt,
    pub def: Def,
    pub feat: Feat,
    pub section: Section,
    pub dep_kind: DepKind,
}

pub enum Syntax {
    Plain,
    InlineTable,
    Table,
}

pub struct Vers {
    pub reqs: Vec<Requirement>,
    pub text: String,
    pub is_pre: bool,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Registry {
    pub text: String,
    pub is_pre: bool,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Path {
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Git {
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Branch {
    pub text: String,
    pub line: u32,
    /// 0-indexed
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Rev {
    pub text: String,
    pub line: u32,
    /// 0-indexed
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Pkg {
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
    pub quote: Quotes,
}

pub struct Workspace {
    pub enabled: bool,
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
}

pub struct Opt {
    pub enabled: bool,
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
}

pub struct Def {
    pub enabled: bool,
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
}

pub struct Feat {
    pub items: Vec<Feature>,
    pub text: String,
    /// 0-indexed
    pub line: u32,
    pub col: Range,
    pub decl_col: Range,
}

pub enum DepKind {
    Registry,
    Path,
    Git,
    Workspace,
}

pub struct Feature {
    pub name: String,
    /// relative to to the start of the features text
    pub col: Range,
    /// relative to to the start of the features text
    pub decl_col: Range,
    pub quote: Quotes,
    pub comma: bool,
}

pub struct Quotes {
    pub s: String,
    pub e: Option<String>,
}

pub struct Requirement {
    pub cond: Cond,
    /// relative to to the start of the requirement text
    pub cond_col: Range,
    pub vers: SemVer,
    /// relative to to the start of the requirement text
    pub vers_col: Range,
}

pub struct SemVer {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
    pub pre: String,
    pub meta: String,
}

pub enum Cond {
    Eq,
    Lt,
    Le,
    Gt,
    Ge,
    Cr,
    Tl,
    Wl,
    Bl,
}

pub struct Range {
    /// 0-indexed inclusive
    pub s: u32,
    /// 0-indexed exclusive
    pub e: u32,
}

impl Ctx {
    fn find(&mut self, map: &MapTable) -> Vec<Crate> {
        let mut crates = Vec::new();
        if let Some(MapEntry::Table(dependencies)) = map.get("dependencies") {
            for (key, val) in dependencies.iter() {
                match val {
                    MapEntry::Scalar(Scalar::String(s)) => todo!(),
                    MapEntry::Table(t) => {
                        for (k, v) in t.iter() {
                            match *k {
                                "version" => {
                                    if let Some(s) = self.expect_string(v) {
                                        todo!();
                                    }
                                }
                                "registry" => todo!(),
                                "path" => todo!(),
                                "git" => todo!(),
                                "branch" => todo!(),
                                "rev" => todo!(),
                                "package" => todo!(),
                                "default-features" => todo!(),
                                "default_features" => todo!("waring or error?"),
                                "features" => todo!(),
                                "workspace" => todo!(),
                                "optional" => todo!(),
                                _ => todo!(),
                            }
                        }
                    }
                    _ => todo!("error"),
                }
            }
        }
        crates
    }

    fn expect_string<'a>(&mut self, value: &'a MapEntry<'a>) -> Option<&'a StringVal<'a>> {
        match value {
            MapEntry::Scalar(Scalar::String(s)) => Some(s),
            _ => {
                self.errors.push(todo!());
                None
            }
        }
    }
}
