use nvim_oxi::{Dictionary, Function};

use crates_toml::map::{MapNode, MapTable, MapTableEntries, Scalar};
use crates_toml::parse::{Ident, StringVal};
use crates_toml::Ctx;

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
    pub kind: SectionKind,
    pub name: Option<String>,
    pub name_col: Range,
    pub lines: Range,
}

pub enum SectionKind {
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
    pub section: Section,
    pub dep_kind: DepKind,
    pub vers: Option<Vers>,
    pub registry: Option<Registry>,
    pub path: Option<Path>,
    pub git: Option<Git>,
    pub branch: Option<Branch>,
    pub rev: Option<Rev>,
    pub pkg: Option<Pkg>,
    pub workspace: Option<Workspace>,
    pub opt: Option<Opt>,
    pub def: Option<Def>,
    pub feat: Option<Feat>,
}

impl Crate {
    pub fn plain(name: &Ident, version: &StringVal, section: Section) -> Self {
        Self {
            explicit_name: name.text.to_string(),
            explicit_name_col: Range::new(name.lit_span.start.char, name.lit_span.end.char),
            lines: Range::from_start_len(name.lit_span.start.line, 1),
            syntax: Syntax::Plain,
            section,
            dep_kind: DepKind::Registry,
            vers: Some(Vers::plain(version)),
            registry: None,
            path: None,
            git: None,
            branch: None,
            rev: None,
            pkg: None,
            workspace: None,
            opt: None,
            def: None,
            feat: None,
        }
    }
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

impl Vers {
    fn plain(value: &StringVal) -> Self {
        Self {
            reqs: todo!(),
            text: value.text.to_string(),
            is_pre: todo!(),
            line: value.lit_span.start.line,
            col: Range::new(value.lit_span.start.char, value.lit_span.end.char),
            decl_col: Range::new(0, value.lit_span.end.char),
            quote: todo!(),
        }
    }
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

impl Range {
    pub fn new(s: u32, e: u32) -> Self {
        Self { s, e }
    }

    pub fn from_start_len(s: u32, len: u32) -> Self {
        Self { s, e: s + len }
    }
}

fn find(ctx: &mut Ctx, map: &MapTable) -> Vec<Crate> {
    let mut crates = Vec::new();
    for (key, entries) in map.iter() {
        match *key {
            "dependencies" => {
                if let MapNode::Table(dependencies) = &entries.node {
                    parse_dependencies(ctx, &mut crates, dependencies);
                }
            }
            "dev-dependencies" => todo!(),
            "build-dependencies" => todo!(),
            "target" => todo!(),
            _ => todo!(),
        }
    }
    if let Some(MapTableEntries {
        node: MapNode::Table(dependencies),
        reprs,
    }) = map.get("dependencies")
    {}
    crates
}

fn parse_dependencies(ctx: &mut Ctx, crates: &mut Vec<Crate>, dependencies: &MapTable) {
    for (crate_name, entries) in dependencies.iter() {
        let crt = match &entries.node {
            MapNode::Scalar(Scalar::String(version)) => {
                let name = entries.reprs.first().key.repr_ident();
                let section = todo!();
                Crate::plain(name, version, section)
            }
            MapNode::Scalar(_) => todo!("error"),
            MapNode::Table(t) => {
                for (k, v) in t.iter() {
                    match *k {
                        "version" => {
                            if let Some(s) = expect_string(ctx, &v.node) {
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
                        "default_features" => todo!("warning or error"),
                        "features" => todo!(),
                        "workspace" => todo!(),
                        "optional" => todo!(),
                        _ => todo!("warning"),
                    }
                }
                todo!()
            }
            MapNode::Array(_) => todo!("error"),
        };

        crates.push(crt);
    }
}

fn expect_string<'a>(ctx: &mut Ctx, value: &'a MapNode<'a>) -> Option<&'a StringVal<'a>> {
    match value {
        MapNode::Scalar(Scalar::String(s)) => Some(s),
        _ => {
            ctx.errors.push(todo!());
            None
        }
    }
}
