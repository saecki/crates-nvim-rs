#![no_main]

use libfuzzer_sys::fuzz_target;
use toml::{Toml, TomlCtx, TomlDiagnostics};

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let mut ctx = TomlDiagnostics::default();
        let bump = bumpalo::Bump::new();
        let Toml { ast, map } = ctx.parse(&bump, "<fuzz>", text);
        let _ = toml::util::map_simple(&ast, map);
    }
});
