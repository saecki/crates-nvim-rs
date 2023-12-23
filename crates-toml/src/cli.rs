use std::process::ExitCode;

fn main() -> ExitCode {
    let Some(path) = std::env::args().skip(1).next() else {
        eprintln!("Missing argument <file>");
        return ExitCode::FAILURE;
    };

    let text = match std::fs::read_to_string(path) {
        Ok(text) => text,
        _ => {
            eprintln!("Error reading from file");
            return ExitCode::FAILURE;
        }
    };
    let lines = text.lines().collect::<Vec<_>>();

    let start = std::time::SystemTime::now();
    let mut ctx = crates_toml::Ctx::default();
    let tokens = ctx.lex(&text);
    let lexing = std::time::SystemTime::now();
    let asts = ctx.parse(tokens);
    let parsing = std::time::SystemTime::now();
    let map = ctx.map(&asts);
    let mapping = std::time::SystemTime::now();
    let simple = crates_toml::map::simple::map_table(map);
    let end = std::time::SystemTime::now();

    println!("{:#?}", simple);
    for e in ctx.errors.iter().rev() {
        println!("{}", e.display(&lines));
    }

    let us_lexing = lexing.duration_since(start).unwrap().as_micros();
    let us_parsing = parsing.duration_since(lexing).unwrap().as_micros();
    let us_mapping = mapping.duration_since(parsing).unwrap().as_micros();
    let us_simple = end.duration_since(mapping).unwrap().as_micros();
    let us_total = end.duration_since(start).unwrap().as_micros();

    #[rustfmt::skip]
    {
    println!("lexing {}us", us_lexing);
    println!("parsing {}us", us_parsing);
    println!("mapping {}us", us_mapping);
    println!("simple {}us", us_simple);
    println!("total {}us", us_total);
    };

    ExitCode::SUCCESS
}
