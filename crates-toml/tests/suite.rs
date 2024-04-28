use bumpalo::Bump;
use common::Diagnostic;
use toml_test_harness::{Decoded, DecodedValue};

use crates_toml::datetime::DateTime;
use crates_toml::map::{MapArray, MapNode, Scalar};
use crates_toml::{MapTable, TomlCtx, TomlDiagnostics};

#[derive(Clone, Copy)]
struct TestDecoder;

impl toml_test_harness::Decoder for TestDecoder {
    fn decode(&self, data: &[u8]) -> Result<toml_test_harness::Decoded, toml_test_harness::Error> {
        let input = std::str::from_utf8(data).map_err(toml_test_harness::Error::new)?;

        let mut ctx = TomlDiagnostics::default();
        let bump = Bump::new();
        let tokens = ctx.lex(&bump, input);
        let asts = ctx.parse(&bump, &tokens);
        let map = ctx.map(&asts);

        if let Some(e) = ctx.errors.first() {
            let span = e.span();
            let mut msg = format!(
                "{}:{} - {}:{}\n",
                span.start.line, span.start.char, span.end.line, span.end.char
            );
            _ = e.description(&mut msg);
            return Err(toml_test_harness::Error::new(msg));
        }

        Ok(map_table(map))
    }

    fn name(&self) -> &str {
        "crates-toml"
    }
}

fn map_decoded(node: MapNode) -> Decoded {
    match node {
        MapNode::Table(t) => map_table(t),
        MapNode::Array(MapArray::Toplevel(a)) => {
            Decoded::Array(a.into_iter().map(|e| map_table(e.node)).collect())
        }
        MapNode::Array(MapArray::Inline(a)) => {
            Decoded::Array(a.into_iter().map(|e| map_decoded(e.node)).collect())
        }
        MapNode::Scalar(s) => Decoded::Value(match s {
            Scalar::String(s) => DecodedValue::String(s.text.to_string()),
            Scalar::Int(i) => DecodedValue::Integer(i.val.to_string()),
            Scalar::Float(f) => DecodedValue::Float({
                let mut str = f.val.to_string();
                str.make_ascii_lowercase();
                str
            }),
            Scalar::Bool(b) => DecodedValue::Bool(b.val.to_string()),
            Scalar::DateTime(d) => match d.val {
                DateTime::OffsetDateTime(_, _, _) => DecodedValue::Datetime(d.lit.to_string()),
                DateTime::LocalDateTime(_, _) => DecodedValue::DatetimeLocal(d.lit.to_string()),
                DateTime::LocalDate(_) => DecodedValue::DateLocal(d.lit.to_string()),
                DateTime::LocalTime(_) => DecodedValue::TimeLocal(d.lit.to_string()),
            },
            Scalar::Invalid(i, s) => unreachable!("{i} at {s:?}"),
        }),
    }
}

fn map_table(table: MapTable) -> Decoded {
    Decoded::Table(
        table
            .into_iter()
            .map(|(k, e)| (k.to_string(), map_decoded(e.node)))
            .collect(),
    )
}

fn main() {
    let mut harness = toml_test_harness::DecoderHarness::new(TestDecoder);
    harness.version("1.0.0");
    harness.ignore([]).unwrap();
    harness.test();
}
