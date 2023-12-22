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

    let mut ctx = crates_toml::Ctx::default();
    let tokens = ctx.lex(&text);
    let asts = ctx.parse(tokens);
    let map = ctx.map(&asts);
    let simple = crates_toml::map::simple::map_table(map);

    println!("{simple:#?}");

    for e in ctx.errors.iter().rev() {
        println!("{}", e.display(&lines));
    }

    ExitCode::SUCCESS
}
