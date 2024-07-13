use std::path::Path;
use std::process::ExitCode;

use bumpalo::Bump;
use common::diagnostic::{self, ANSII_CLEAR, ANSII_COLOR_RED, ANSII_UNDERLINED};
use ide::{IdeCtx, IdeDiagnostics};
use toml::TomlCtx;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Mode {
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

    let Some(mode_str) = args.next() else {
        input_error!("missing mode");
    };
    let mode = match mode_str.as_str() {
        "validate" => Mode::Validate,
        "check" => Mode::Check,
        _ => input_error!("invalid mode `{mode_str}`"),
    };

    let Some(path) = args.next() else {
        input_error!("missing argument <file>");
    };
    let path: &Path = path.as_ref();
    if let Some(filename) = path.file_name() {
        if mode == Mode::Check && filename != "Cargo.toml" {
            input_error!(
                "file isn't named `Cargo.toml`, use mode `validate` for arbitrary toml files"
            );
        }
    } else {
        input_error!("<file> path is empty");
    }

    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        Err(e) => error!("error reading from file: {e}"),
    };
    let lines = diagnostic::lines(&text);

    let start = std::time::SystemTime::now();
    let mut ctx = IdeDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, &text);
    let lexing = std::time::SystemTime::now();
    let asts = ctx.parse(&bump, &tokens);
    let parsing = std::time::SystemTime::now();
    let map = ctx.map(&asts);
    let mapping = std::time::SystemTime::now();
    if mode == Mode::Check {
        let _state = ctx.check(&map);
    }
    let checking = std::time::SystemTime::now();
    let simple = toml::util::map_simple(map);
    let end = std::time::SystemTime::now();

    println!("{:#?}", simple);
    ctx.sort_diagnostics();
    let mut msg = String::new();
    for error in ctx.errors.iter() {
        diagnostic::display(&mut msg, error, &lines).unwrap();
        println!("{msg}");
        msg.clear()
    }
    for warning in ctx.warnings.iter() {
        diagnostic::display(&mut msg, warning, &lines).unwrap();
        println!("{msg}");
        msg.clear()
    }
    for info in ctx.infos.iter() {
        diagnostic::display(&mut msg, info, &lines).unwrap();
        println!("{msg}");
        msg.clear()
    }

    let us_lexing = lexing.duration_since(start).unwrap().as_micros();
    let us_parsing = parsing.duration_since(lexing).unwrap().as_micros();
    let us_mapping = mapping.duration_since(parsing).unwrap().as_micros();
    let us_checking = checking.duration_since(mapping).unwrap().as_micros();
    let us_simple = end.duration_since(checking).unwrap().as_micros();
    let us_total = end.duration_since(start).unwrap().as_micros();

    println!();
    println!("lexing   {:6}us", us_lexing);
    println!("parsing  {:6}us", us_parsing);
    println!("mapping  {:6}us", us_mapping);
    if mode == Mode::Check {
        println!("checking {:4}us", us_checking);
    }
    println!("simple   {:6}us", us_simple);
    println!("-----------------");
    println!("total    {:6}us", us_total);

    ExitCode::SUCCESS
}

fn help_message() {
    eprintln!("ctoml <mode> <file>");
    eprintln!();
    eprintln!("modes:");
    eprintln!("  {ANSII_UNDERLINED}validate{ANSII_CLEAR}  to validate arbitrary toml files");
    eprintln!("  {ANSII_UNDERLINED}check{ANSII_CLEAR}     to check a `Cargo.toml` manifest");
}
