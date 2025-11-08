use bumpalo::Bump;
use common::diagnostic::DisplayDiagnostic;
use toml_test_harness::{Decoded, DecodedValue};

use dingey_toml::datetime::DateTime;
use dingey_toml::map::{MapArray, MapInner, MapNode, Scalar};
use dingey_toml::{Ast, Toml, TomlCtx, TomlDiagnostics};

#[derive(Clone, Copy)]
struct TestDecoder;

impl toml_test_harness::Decoder for TestDecoder {
    fn decode(&self, data: &[u8]) -> Result<toml_test_harness::Decoded, toml_test_harness::Error> {
        let text = std::str::from_utf8(data).map_err(toml_test_harness::Error::new)?;

        let mut ctx = TomlDiagnostics::default();
        let bump = Bump::new();
        let Toml { ast, map } = ctx.parse(&bump, "<case>", text);

        if let Some(error) = ctx.errors.first() {
            let msg = error.display(&ast.source);
            return Err(toml_test_harness::Error::new(msg));
        }

        Ok(map_table(&ast, map))
    }

    fn name(&self) -> &str {
        "dingey-toml"
    }
}

fn map_decoded(ast: &Ast, node: &MapNode) -> Decoded {
    match node {
        MapNode::Table(t) => map_table(ast, t),
        MapNode::Array(MapArray::Toplevel(a)) => {
            Decoded::Array(a.iter().map(|e| map_table(ast, &e.node)).collect())
        }
        MapNode::Array(MapArray::Inline(a)) => {
            Decoded::Array(a.iter().map(|e| map_decoded(ast, &e.node)).collect())
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
            Scalar::DateTime(d) => {
                let str = ast.source.spanned_str(d.lit_span).to_string();
                match d.val {
                    DateTime::OffsetDateTime(_, _, _) => DecodedValue::Datetime(str),
                    DateTime::LocalDateTime(_, _) => DecodedValue::DatetimeLocal(str),
                    DateTime::LocalDate(_) => DecodedValue::DateLocal(str),
                    DateTime::LocalTime(_) => DecodedValue::TimeLocal(str),
                }
            }
            Scalar::Invalid(span) => {
                let str = ast.source.spanned_str(**span);
                unreachable!("`{str}` at {s:?}")
            }
        }),
    }
}

fn map_table<'a, M: AsRef<MapInner<'a>>>(ast: &Ast, map: M) -> Decoded {
    Decoded::Table(
        map.as_ref()
            .iter()
            .map(|(k, e)| (k.to_string(), map_decoded(ast, &e.node)))
            .collect(),
    )
}

fn main() {
    let mut harness = toml_test_harness::DecoderHarness::new(TestDecoder);
    harness.version("1.0.0");
    harness.ignore(["invalid/**/*"]).unwrap();
    harness.test();
}
