#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(input) = std::str::from_utf8(data) {
        let vers = semver::parse_version(input, common::Pos::ZERO);
        if let Ok(version) = vers {
            assert_eq!(input, version.to_string());
        }

        let req = semver::parse_requirement(input, common::Pos::ZERO);
        if let Ok(req) = req {
            assert_eq!(input, req.to_string());
        }
    }
});
