use common::{Diagnostic, Severity, Span};

use crate::cargo;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Toml(toml::Error),
    Semver(semver::Error),
    Cargo(cargo::Error),
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

impl From<cargo::Error> for Error {
    fn from(value: cargo::Error) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    Toml(toml::Warning),
    Semver(semver::Warning),
    Cargo(cargo::Warning),
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

impl From<cargo::Warning> for Warning {
    fn from(value: cargo::Warning) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {
    Toml(toml::Hint),
    Semver(semver::Hint),
    Cargo(cargo::Hint),
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

impl From<cargo::Hint> for Hint {
    fn from(value: cargo::Hint) -> Self {
        Self::Cargo(value)
    }
}
