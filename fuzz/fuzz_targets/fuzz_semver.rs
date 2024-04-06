#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let vers = semver::parse_version(input);
        if let Ok(version) = vers {
            assert_eq!(input.trim(), version.to_string());
        }

        let req = semver::parse_requirement(input);
        if let Ok(req) = req {
            assert_eq!(input.trim(), req.to_string());
        }
    }
});
