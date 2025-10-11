use std::process::ExitCode;

fn main() -> ExitCode {
    if let Err(e) = dingey_ls::run() {
        eprintln!("{e}");
        return ExitCode::FAILURE;
    }
    ExitCode::SUCCESS
}
