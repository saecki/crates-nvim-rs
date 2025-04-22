#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let vers = semver::parse_version(text, common::Pos::ZERO);
        if let Ok(version) = vers {
            assert_eq!(text.trim(), version.to_string());
        }

        let req = semver::parse_requirement(text, common::Pos::ZERO);
        if let Ok(req) = req {
            assert_eq!(text, req.to_string());
        }
    }
});
