use std::path::Path;
use std::process::ExitCode;

use bumpalo::Bump;
use common::diagnostic::{ANSII_CLEAR, ANSII_COLOR_RED, ANSII_UNDERLINED, DisplayDiagnostic};
use ide::{IdeCtx, IdeDiagnostics};
use toml::TomlCtx;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Command {
    /// Validate arbitrary toml files.
    Validate,
    /// Check a `Cargo.toml` manifest.
    Check,
}

macro_rules! error {
    ($pat:expr) => {{
        eprint!("{ANSII_COLOR_RED}error: ");
        eprint!($pat);
        eprintln!("{ANSII_CLEAR}");
        return ExitCode::FAILURE;
    }};
}

macro_rules! input_error {
    ($pat:expr) => {{
        eprint!("{ANSII_COLOR_RED}argument error: ");
        eprint!($pat);
        eprintln!("{ANSII_CLEAR}");
        eprintln!();
        help_message();
        return ExitCode::FAILURE;
    }};
}

fn main() -> ExitCode {
    let mut args = std::env::args();
    args.next();

    let Some(command_str) = args.next() else {
        input_error!("missing command");
    };
    let command = match command_str.as_str() {
        "validate" => Command::Validate,
        "check" => Command::Check,
        _ => input_error!("invalid command `{command_str}`"),
    };

    let Some(path) = args.next() else {
        input_error!("missing argument <file>");
    };
    if let Some(filename) = AsRef::<Path>::as_ref(&path).file_name() {
        if command == Command::Check && filename != "Cargo.toml" {
            input_error!(
                "file isn't named `Cargo.toml`, use the `validate` command for arbitrary toml files"
            );
        }
    } else {
        input_error!("<file> path is empty");
    }

    let text = match std::fs::read_to_string(&path) {
        Ok(text) => text,
        Err(e) => error!("error reading from file: {e}"),
    };

    let start = std::time::SystemTime::now();
    let mut ctx = IdeDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, &path, &text);
    let lexing = std::time::SystemTime::now();
    let ast = ctx.parse(&bump, tokens);
    let parsing = std::time::SystemTime::now();
    let map = ctx.map(&ast);
    let mapping = std::time::SystemTime::now();
    if command == Command::Check {
        let _state = ctx.check(&map);
    }
    let checking = std::time::SystemTime::now();
    let simple = toml::util::map_simple(&ast, map);
    let end = std::time::SystemTime::now();

    println!("{simple:#?}");
    ctx.sort_diagnostics();
    for error in ctx.errors.iter() {
        println!("{}", error.display(&ast.source));
    }
    for warning in ctx.warnings.iter() {
        println!("{}", warning.display(&ast.source));
    }
    for info in ctx.infos.iter() {
        println!("{}", info.display(&ast.source));
    }

    let us_lexing = lexing.duration_since(start).unwrap().as_micros();
    let us_parsing = parsing.duration_since(lexing).unwrap().as_micros();
    let us_mapping = mapping.duration_since(parsing).unwrap().as_micros();
    let us_checking = checking.duration_since(mapping).unwrap().as_micros();
    let us_simple = end.duration_since(checking).unwrap().as_micros();
    let us_total = end.duration_since(start).unwrap().as_micros();

    println!();
    println!("lexing   {us_lexing:6}us");
    println!("parsing  {us_parsing:6}us");
    println!("mapping  {us_mapping:6}us");
    if command == Command::Check {
        println!("checking {us_checking:4}us");
    }
    println!("simple   {us_simple:6}us");
    println!("-----------------");
    println!("total    {us_total:6}us");

    ExitCode::SUCCESS
}

fn help_message() {
    eprintln!("ctoml <command> <file>");
    eprintln!();
    eprintln!("commands:");
    eprintln!("  {ANSII_UNDERLINED}validate{ANSII_CLEAR}  to validate arbitrary toml files");
    eprintln!("  {ANSII_UNDERLINED}check{ANSII_CLEAR}     to check a `Cargo.toml` manifest");
}
