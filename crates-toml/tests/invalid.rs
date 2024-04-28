use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Write as _;

use bumpalo::Bump;
use common::DisplayDiagnostic;
use crates_toml::map::simple::SimpleVal;
use crates_toml::{TomlCtx, TomlDiagnostics};
use libtest_mimic::Failed;

#[derive(Clone, Copy, Default)]
enum Mode {
    /// Just fail the tests on mismatch
    #[default]
    Fail,
    /// Ask the user on mismatch
    Review,
    /// Review correct fixtures
    Revise,
    /// Force overwrite the fixtures on mismatch
    Force,
}

fn main() {
    let mode = std::env::var("SNAPSHOT")
        .map(|m| match m.as_str() {
            "fail" | "" => Mode::Fail,
            "review" => Mode::Review,
            "revise" => Mode::Revise,
            "force" => Mode::Force,
            _ => panic!("invalid mode `{m}`"),
        })
        .unwrap_or_default();

    let mut args = libtest_mimic::Arguments::from_args();
    args.test_threads = Some(1);

    let tests = toml_test_data::invalid()
        .map(|case| {
            libtest_mimic::Trial::test(case.name.display().to_string(), move || {
                let expect_path =
                    std::path::Path::new("tests/fixtures").join(case.name.with_extension("stderr"));

                let input = std::str::from_utf8(case.fixture).map_err(|e| e.to_string())?;
                let actual_text = match run_case(input) {
                    Ok(v) => {
                        let msg = format!("Expected error but got:\n{v:?}");
                        return Err(Failed::from(msg));
                    }
                    Err(err) => err,
                };

                let expect_text = match std::fs::read_to_string(&expect_path) {
                    Ok(t) => t,
                    Err(e) => {
                        let mut msg = String::new();
                        _ = writeln!(
                            &mut msg,
                            "Fixture `{}` not found:\n  {e}\n",
                            expect_path.display()
                        );
                        _ = writeln!(
                            &mut msg,
                            "=========================  input   ========================="
                        );
                        _ = write!(&mut msg, "{input}");
                        _ = writeln!(
                            &mut msg,
                            "========================= message  ========================="
                        );
                        _ = write!(&mut msg, "{actual_text}");
                        _ = writeln!(
                            &mut msg,
                            "============================================================"
                        );

                        match mode {
                            Mode::Fail | Mode::Revise => return Err(Failed::from(msg)),
                            Mode::Review => {
                                print!("\n{msg}");
                                return match dialog(["update", "skip", "quit"]) {
                                    "update" => {
                                        let dir = expect_path.parent().unwrap();
                                        std::fs::create_dir_all(dir).unwrap();
                                        std::fs::write(expect_path, actual_text).unwrap();
                                        println!("Updated path");
                                        Ok(())
                                    }
                                    "skip" => Err(Failed::from(msg)),
                                    "quit" => std::process::exit(0),
                                    _ => unreachable!(),
                                };
                            }
                            Mode::Force => {
                                std::fs::write(expect_path, actual_text).unwrap();
                                return Ok(());
                            }
                        }
                    }
                };

                if expect_text == actual_text {
                    if let Mode::Revise = mode {
                        let mut msg = String::new();
                        _ = writeln!(
                            &mut msg,
                            "=========================  input   ========================="
                        );
                        _ = write!(&mut msg, "{input}");
                        _ = writeln!(
                            &mut msg,
                            "========================= message  ========================="
                        );
                        _ = write!(&mut msg, "{actual_text}");
                        _ = writeln!(
                            &mut msg,
                            "============================================================"
                        );

                        print!("\n{msg}");

                        return match dialog(["invalidate", "skip", "quit"]) {
                            "invalidate" => {
                                std::fs::remove_file(expect_path).unwrap();
                                println!("Deleted fixture");
                                Ok(())
                            }
                            "skip" => Ok(()),
                            "quit" => std::process::exit(0),
                            _ => unreachable!(),
                        };
                    }

                    return Ok(());
                }

                let mut msg = String::new();
                _ = writeln!(
                    &mut msg,
                    "=========================  input   ========================="
                );
                _ = write!(&mut msg, "{input}");
                _ = writeln!(
                    &mut msg,
                    "========================= expected ========================="
                );
                _ = write!(&mut msg, "{expect_text}");
                _ = writeln!(
                    &mut msg,
                    "-------------------------  actual  -------------------------"
                );
                _ = write!(&mut msg, "{actual_text}");
                _ = writeln!(
                    &mut msg,
                    "=========================   diff   ========================="
                );
                let comp = pretty_assertions::StrComparison::new(
                    expect_text.as_str(),
                    actual_text.as_str(),
                );
                _ = write!(&mut msg, "{comp}");
                _ = writeln!(
                    &mut msg,
                    "========================= raw diff ========================="
                );
                let comp =
                    pretty_assertions::Comparison::new(expect_text.as_str(), actual_text.as_str());
                _ = write!(&mut msg, "{comp:}");
                _ = writeln!(
                    &mut msg,
                    "============================================================"
                );

                match mode {
                    Mode::Fail | Mode::Revise => Err(Failed::from(msg)),
                    Mode::Review => {
                        print!("\n{msg}");
                        match dialog(["update", "skip", "quit"]) {
                            "update" => {
                                let dir = expect_path.parent().unwrap();
                                std::fs::create_dir_all(dir).unwrap();
                                std::fs::write(expect_path, actual_text).unwrap();
                                println!("Updated path");
                                Ok(())
                            }
                            "skip" => Err(Failed::from(msg)),
                            "quit" => std::process::exit(0),
                            _ => unreachable!(),
                        }
                    }
                    Mode::Force => Err(Failed::from(msg)),
                }
            })
        })
        .collect();
    libtest_mimic::run(&args, tests).exit()
}

fn run_case(input: &str) -> Result<HashMap<String, SimpleVal>, String> {
    let mut ctx = TomlDiagnostics::default();
    let bump = Bump::new();
    let tokens = ctx.lex(&bump, input);
    let asts = ctx.parse(&bump, &tokens);
    let map = ctx.map(&asts);

    if !ctx.errors.is_empty() {
        let lines = input.split('\n').collect::<Vec<_>>();
        let mut msg = String::new();
        for error in ctx.errors.iter() {
            _ = writeln!(&mut msg, "{}", error.header(&lines));
            if let Some(hint) = error.hint() {
                _ = write!(&mut msg, "{}", hint.body(&lines));
            }
            _ = write!(&mut msg, "{}", error.body(&lines));
        }
        return Err(msg);
    }

    Ok(crates_toml::map::simple::map_table(map))
}

fn dialog<'a, const SIZE: usize>(options: [&'a str; SIZE]) -> &'a str {
    for o in options {
        // HACK: only works for ascii
        let (first, remainder) = o.split_at(1);
        println!("\x1b[94m[{first}]\x1b[0m{remainder}");
    }

    loop {
        print!("> ");
        _ = std::io::stdout().flush();

        let stdin = std::io::stdin();
        let mut input = String::new();
        _ = stdin.read_line(&mut input);
        let len = input.trim_end().len();
        input.truncate(len);

        for o in options {
            if input.len() == 1 && input[..1] == o[..1] {
                return o;
            }
            if input == o {
                return o;
            }
        }

        println!("Invalid input {input:?}");
    }
}
