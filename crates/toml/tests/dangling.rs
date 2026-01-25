use indexmap::IndexSet;
use walkdir::WalkDir;

fn main() {
    let root = "tests/fixtures";
    let mut fixtures = WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            if !entry.file_type().is_file() {
                return None;
            }
            let path = entry.path().strip_prefix(root).unwrap();
            Some(path.to_owned())
        })
        .collect::<IndexSet<_>>();

    let test_cases = toml_test_data::valid()
        .map(|case| case.name)
        .chain(toml_test_data::invalid().map(|case| case.name));

    for case in test_cases {
        let path = case.with_added_extension("stderr");
        fixtures.shift_remove(&path);
    }

    if !fixtures.is_empty() {
        fixtures.sort();
        for path in fixtures.iter() {
            eprintln!("dangling fixture {}", path.display());
        }
        std::process::exit(1);
    }
}
