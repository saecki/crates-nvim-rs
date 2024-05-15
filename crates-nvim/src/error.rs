use common::{Diagnostic, Severity, Span};

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Toml(toml::Error),
    Semver(semver::Error),
    Cargo(CargoError),
}

impl Diagnostic for Error {
    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        match self {
            Error::Toml(e) => e.span(),
            Error::Semver(e) => e.span(),
            Error::Cargo(e) => e.span(),
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Error::Toml(e) => e.description(f),
            Error::Semver(e) => e.description(f),
            Error::Cargo(e) => e.description(f),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Error::Toml(e) => e.annotation(f),
            Error::Semver(e) => e.annotation(f),
            Error::Cargo(e) => e.annotation(f),
        }
    }
}

impl From<toml::Error> for Error {
    fn from(value: toml::Error) -> Self {
        Self::Toml(value)
    }
}

impl From<semver::Error> for Error {
    fn from(value: semver::Error) -> Self {
        Self::Semver(value)
    }
}

impl From<CargoError> for Error {
    fn from(value: CargoError) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CargoError {
    ExpectedTable(String, Span),
    ExpectedStringInTable(String, Span),
    ExpectedArrayInTable(String, Span),
    ExpectedStringInArray(Span),
}

impl Diagnostic for CargoError {
    const SEVERITY: Severity = Severity::Error;

    fn span(&self) -> Span {
        match self {
            CargoError::ExpectedTable(_, s) => *s,
            CargoError::ExpectedStringInTable(_, s) => *s,
            CargoError::ExpectedArrayInTable(_, s) => *s,
            CargoError::ExpectedStringInArray(s) => *s,
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
pub enum Warning {
    Toml(toml::Warning),
    Semver(semver::Warning),
    Cargo(CargoWarning),
}

impl Diagnostic for Warning {
    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        match self {
            Warning::Toml(w) => w.span(),
            Warning::Semver(w) => w.span(),
            Warning::Cargo(w) => w.span(),
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Warning::Toml(w) => w.description(f),
            Warning::Semver(w) => w.description(f),
            Warning::Cargo(w) => w.description(f),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Warning::Toml(w) => w.annotation(f),
            Warning::Semver(w) => w.annotation(f),
            Warning::Cargo(w) => w.annotation(f),
        }
    }
}

impl From<toml::Warning> for Warning {
    fn from(value: toml::Warning) -> Self {
        Self::Toml(value)
    }
}

impl From<semver::Warning> for Warning {
    fn from(value: semver::Warning) -> Self {
        Self::Semver(value)
    }
}

impl From<CargoWarning> for Warning {
    fn from(value: CargoWarning) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CargoWarning {}

impl Diagnostic for CargoWarning {
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
pub enum Hint {
    Toml(toml::Hint),
    Semver(semver::Hint),
    Cargo(CargoHint),
}

impl Diagnostic for Hint {
    const SEVERITY: Severity = Severity::Hint;

    fn span(&self) -> Span {
        match self {
            Hint::Toml(h) => h.span(),
            Hint::Semver(h) => h.span(),
            Hint::Cargo(h) => h.span(),
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Hint::Toml(h) => h.description(f),
            Hint::Semver(h) => h.description(f),
            Hint::Cargo(h) => h.description(f),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Hint::Toml(h) => h.annotation(f),
            Hint::Semver(h) => h.annotation(f),
            Hint::Cargo(h) => h.annotation(f),
        }
    }
}

impl From<toml::Hint> for Hint {
    fn from(value: toml::Hint) -> Self {
        Self::Toml(value)
    }
}

impl From<semver::Hint> for Hint {
    fn from(value: semver::Hint) -> Self {
        Self::Semver(value)
    }
}

impl From<CargoHint> for Hint {
    fn from(value: CargoHint) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CargoHint {}

impl Diagnostic for CargoHint {
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
