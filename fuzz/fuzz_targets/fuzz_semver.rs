#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let _ = semver::parse_version(input);
        let _ = semver::parse_requirement(input);
    }
});
