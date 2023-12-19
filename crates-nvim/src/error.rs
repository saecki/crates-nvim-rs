use crates_toml::Span;

#[derive(Debug, PartialEq, Eq)]
pub enum Error {
    Toml(crates_toml::Error),
    Cargo(CargoError),
}

impl From<crates_toml::Error> for Error {
    fn from(value: crates_toml::Error) -> Self {
        Self::Toml(value)
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

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    Toml(crates_toml::Warning),
    Cargo(CargoWarning),
}

impl From<crates_toml::Warning> for Warning {
    fn from(value: crates_toml::Warning) -> Self {
        Self::Toml(value)
    }
}

impl From<CargoWarning> for Warning {
    fn from(value: CargoWarning) -> Self {
        Self::Cargo(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CargoWarning {}
