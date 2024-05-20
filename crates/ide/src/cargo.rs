use common::{Ctx, Diagnostic, Diagnostics, FmtStr, Severity, Span};

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

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    ExpectedTable(FmtStr, Span),
    ExpectedStringInTable(FmtStr, Span),
    ExpectedBoolInTable(FmtStr, Span),
    ExpectedArrayInTable(FmtStr, Span),
    ExpectedStringInArray(Span),
}

impl Diagnostic for Error {
    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        match self {
            Error::ExpectedTable(_, s) => *s,
            Error::ExpectedStringInTable(_, s) => *s,
            Error::ExpectedBoolInTable(_, s) => *s,
            Error::ExpectedArrayInTable(_, s) => *s,
            Error::ExpectedStringInArray(s) => *s,
        }
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
pub enum Warning {}

impl Diagnostic for Warning {
    const SEVERITY: Severity = Severity::Warning;

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
