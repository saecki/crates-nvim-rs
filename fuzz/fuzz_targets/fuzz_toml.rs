#![no_main]

use libfuzzer_sys::fuzz_target;
use toml::{TomlCtx, TomlDiagnostics};

fuzz_target!(|data: &[u8]| {
    if let Ok(text) = std::str::from_utf8(data) {
        let mut ctx = TomlDiagnostics::default();
        let bump = bumpalo::Bump::new();
        let tokens = ctx.lex(&bump, "<fuzz>", text);
        let ast = ctx.parse(&bump, tokens);
        let map = ctx.map(&bump, &ast);
        let _ = toml::util::map_simple(&ast, map);
    }
});
