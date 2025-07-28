use std::process::ExitCode;

fn main() -> ExitCode {
    if let Err(e) = crates_ls::run() {
        eprintln!("{e}");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
