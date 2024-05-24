use common::{Ctx, Diagnostic, DiagnosticHint, Diagnostics, FmtStr, Severity, Span};
use toml::util::Datatype;

pub trait CargoCtx:
    Ctx<Error = Self::CargoError, Warning = Self::CargoWarning, Info = Self::CargoInfo>
{
    type CargoError: From<Error>;
    type CargoWarning: From<Warning>;
    type CargoInfo: From<Info>;
}

impl<E, W, I> CargoCtx for Diagnostics<E, W, I>
where
    E: From<Error>,
    W: From<Warning>,
    I: From<Info>,
{
    type CargoError = E;
    type CargoWarning = W;
    type CargoInfo = I;
}

// TODO: add context lines
#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    WrongDatatypeInTable {
        key: FmtStr,
        expected: Datatype,
        found: Datatype,
        span: Span,
    },
    WrongDatatypeInArray {
        index: usize,
        expected: Datatype,
        found: Datatype,
        span: Span,
    },
    /// In the 2024 edition keys with underscores are unsupported.
    UnsupportedUnderscore {
        old: &'static str,
        new: &'static str,
        span: Span,
    },
}

impl Diagnostic for Error {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        use Error::*;
        match self {
            WrongDatatypeInTable { span, .. } => *span,
            WrongDatatypeInArray { span, .. } => *span,
            UnsupportedUnderscore { span, .. } => *span,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            WrongDatatypeInTable {
                key,
                expected,
                found,
                ..
            } => write!(
                f,
                "Expected `{key}` to be of type `{expected}`, found `{found}`"
            ),
            WrongDatatypeInArray {
                index,
                expected,
                found,
                ..
            } => write!(
                f,
                "Expected element {index} to be of type `{expected}`, found `{found}`"
            ),
            UnsupportedUnderscore { old, new, .. } => {
                write!(
                    f,
                    "`{old}` is unsupported in the 2024 edition; instead use `{new}`"
                )
            }
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            WrongDatatypeInTable { expected, .. } => write!(f, "Expected type `{expected}`"),
            WrongDatatypeInArray { expected, .. } => write!(f, "Expected type `{expected}`"),
            UnsupportedUnderscore { new, .. } => write!(f, "Unsupported; instead use `{new}`"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    /// Warn about future removal in the 2024 edition.
    DeprecatedUnderscore {
        old: &'static str,
        new: &'static str,
        span: Span,
    },
    /// Warn about future removal in the 2024 edition.
    RedundantDeprecatedUnderscore {
        old: &'static str,
        new: &'static str,
        old_span: Span,
        new_span: Span,
    },
}

impl Diagnostic for Warning {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        use Warning::*;
        match self {
            DeprecatedUnderscore { span, .. } => *span,
            RedundantDeprecatedUnderscore { old_span, .. } => *old_span,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Warning::*;
        match self {
            DeprecatedUnderscore { old, new, .. } => {
                write!(f, "`{old}` is deprecated in favor of `{new}` and will be unsupported in the 2024 edition")
            }
            RedundantDeprecatedUnderscore { old, new, .. } => {
                write!(
                    f,
                    "`{old}` is redundant with `{new}` and will be unsupported in the 2024 edition"
                )
            }
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Warning::*;
        match self {
            DeprecatedUnderscore { new, .. } => write!(f, "Deprecated in favor of `{new}`"),
            RedundantDeprecatedUnderscore { new, .. } => write!(f, "Redundant with `{new}`"),
        }
    }

    fn hint(&self) -> Option<Self::Hint> {
        use Warning::*;
        match self {
            DeprecatedUnderscore { .. } => None,
            RedundantDeprecatedUnderscore { new_span, .. } => {
                Some(Hint::RedundantDeprecatedUnderscore(*new_span))
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Info {}

impl Diagnostic for Info {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Info;

    fn span(&self) -> Span {
        todo!()
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        _ = f;
        todo!()
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        _ = f;
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {
    RedundantDeprecatedUnderscore(Span),
}

impl DiagnosticHint for Hint {
    fn span(&self) -> Span {
        match self {
            Hint::RedundantDeprecatedUnderscore(s) => *s,
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Hint::RedundantDeprecatedUnderscore(_) => write!(f, "Used instead"),
        }
    }
}
