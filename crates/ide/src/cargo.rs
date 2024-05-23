use common::{Ctx, Diagnostic, Diagnostics, FmtStr, Severity, Span};
use toml::util::Datatype;

pub trait CargoCtx:
    Ctx<Error = Self::CargoError, Warning = Self::CargoWarning, Hint = Self::CargoHint>
{
    type CargoError: From<Error>;
    type CargoWarning: From<Warning>;
    type CargoHint: From<Hint>;
}

impl<E, W, H> CargoCtx for Diagnostics<E, W, H>
where
    E: From<Error>,
    W: From<Warning>,
    H: From<Hint>,
{
    type CargoError = E;
    type CargoWarning = W;
    type CargoHint = H;
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
        span: Span,
    },
}

impl Diagnostic for Warning {
    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        use Warning::*;
        match self {
            DeprecatedUnderscore { span, .. } => *span,
            RedundantDeprecatedUnderscore { span, .. } => *span,
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
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {}

impl Diagnostic for Hint {
    const SEVERITY: Severity = Severity::Hint;

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
