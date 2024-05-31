use common::diagnostic::{Diagnostic, DiagnosticHint, Severity};
use common::Span;

use crate::cargo;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Toml(toml::Error),
    Semver(semver::Error),
    Cargo(cargo::Error),
}

impl Diagnostic for Error {
    type Hint = Hint;

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

    fn hint(&self) -> Option<Self::Hint> {
        match self {
            Error::Toml(e) => e.hint().map(Hint::Toml),
            Error::Semver(e) => e.hint().map(Hint::Semver),
            Error::Cargo(e) => e.hint().map(Hint::Cargo),
        }
    }

    fn context_lines(&self) -> Option<&[u32]> {
        match self {
            Error::Toml(e) => e.context_lines(),
            Error::Semver(e) => e.context_lines(),
            Error::Cargo(e) => e.context_lines(),
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
    type Hint = Hint;

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

    fn hint(&self) -> Option<Self::Hint> {
        match self {
            Warning::Toml(w) => w.hint().map(Hint::Toml),
            Warning::Semver(w) => w.hint().map(Hint::Semver),
            Warning::Cargo(w) => w.hint().map(Hint::Cargo),
        }
    }

    fn context_lines(&self) -> Option<&[u32]> {
        match self {
            Warning::Toml(w) => w.context_lines(),
            Warning::Semver(w) => w.context_lines(),
            Warning::Cargo(w) => w.context_lines(),
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
pub enum Info {
    Toml(toml::Info),
    Semver(semver::Info),
    Cargo(cargo::Info),
}

impl Diagnostic for Info {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Info;

    fn span(&self) -> Span {
        match self {
            Info::Toml(h) => h.span(),
            Info::Semver(h) => h.span(),
            Info::Cargo(h) => h.span(),
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Info::Toml(h) => h.description(f),
            Info::Semver(h) => h.description(f),
            Info::Cargo(h) => h.description(f),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Info::Toml(h) => h.annotation(f),
            Info::Semver(h) => h.annotation(f),
            Info::Cargo(h) => h.annotation(f),
        }
    }

    fn hint(&self) -> Option<Self::Hint> {
        match self {
            Info::Toml(i) => i.hint().map(Hint::Toml),
            Info::Semver(i) => i.hint().map(Hint::Semver),
            Info::Cargo(i) => i.hint().map(Hint::Cargo),
        }
    }

    fn context_lines(&self) -> Option<&[u32]> {
        match self {
            Info::Toml(i) => i.context_lines(),
            Info::Semver(i) => i.context_lines(),
            Info::Cargo(i) => i.context_lines(),
        }
    }
}

impl From<toml::Info> for Info {
    fn from(value: toml::Info) -> Self {
        Self::Toml(value)
    }
}

impl From<semver::Info> for Info {
    fn from(value: semver::Info) -> Self {
        Self::Semver(value)
    }
}

impl From<cargo::Info> for Info {
    fn from(value: cargo::Info) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {
    Toml(toml::Hint),
    Semver(semver::Hint),
    Cargo(cargo::Hint),
}

impl DiagnosticHint for Hint {
    fn span(&self) -> Span {
        match self {
            Hint::Toml(h) => h.span(),
            Hint::Semver(h) => h.span(),
            Hint::Cargo(h) => h.span(),
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
