use common::diagnostic::{Diagnostic, DiagnosticHint, Severity};
use common::{Ctx, Diagnostics, FmtStr, Span};
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
        path: FmtStr,
        expected: Datatype,
        found: Datatype,
        span: Span,
    },
    WrongDatatypeInArray {
        path: FmtStr,
        expected: Datatype,
        found: Datatype,
        span: Span,
    },
    /// In the 2024 edition keys with underscores are unsupported.
    UnsupportedUnderscore {
        path: Option<FmtStr>,
        old: &'static str,
        new: &'static str,
        span: Span,
    },
    DepWorkspaceIsFalse(FmtStr, Span),
    AmbigousDepSpecGitPath(FmtStr, Span),
    AmbigousDepSpecGitRegistry(FmtStr, Span),
    AmbigousGitSpec(FmtStr, Span),
    /// Invalid in the 2024 edition.
    MissingDepSpec(FmtStr, Span),
    DepIgnoredGitKey(FmtStr, &'static str, Span),
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
            DepWorkspaceIsFalse(_, s) => *s,
            AmbigousDepSpecGitPath(_, s) => *s,
            AmbigousDepSpecGitRegistry(_, s) => *s,
            AmbigousGitSpec(_, s) => *s,
            MissingDepSpec(_, s) => *s,
            DepIgnoredGitKey(_, _, s) => *s,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            WrongDatatypeInTable {
                path,
                expected,
                found,
                ..
            } => write!(
                f,
                "Expected `{path}` to be of type {expected}, found {found}"
            ),
            WrongDatatypeInArray {
                path,
                expected,
                found,
                ..
            } => write!(
                f,
                "Expected `{path}` to be of type {expected}, found {found}"
            ),
            UnsupportedUnderscore { path, old, new, .. } => {
                if let Some(path) = path {
                    write!(f, "`{path}`: ")?;
                }
                write!(f, "`{old}` has been replaced with `{new}` and is unsupported in the 2024 edition")
            }
            DepWorkspaceIsFalse(path, _) => write!(f, "Invalid dependency specification `{path}`; `workspace` cannot be false"),
            AmbigousDepSpecGitPath(path, _) => write!(
                f,
                "Dependency specification `{path}` is ambigous, only one of `git` or `path` is allowed",
            ),
            AmbigousDepSpecGitRegistry(path, _) => write!(
                f,
                "Dependency specification `{path}` is ambigous, only one of `git` or `registry` is allowed",
            ),
            AmbigousGitSpec(path, _) => write!(
                f,
                "Dependency specification `{path}` is ambigous, only one of `branch`, `tag` or `rev` is allowed",
            ),
            MissingDepSpec(path, _) => write!(
                f,
                "Dependency `{path}` is missing one of `workspace`, `path`, `git` or `version`, this is unsupported in the 2024 edition",
            ),
            DepIgnoredGitKey(path, key, _) => {
                write!(f, "Invalid dependency specification `{path}`; `{key}` without `git` is not allowed")
            }
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            WrongDatatypeInTable { expected, .. } => write!(f, "Expected {expected}"),
            WrongDatatypeInArray { expected, .. } => write!(f, "Expected {expected}"),
            UnsupportedUnderscore { new, .. } => write!(f, "Unsupported; instead use `{new}`"),
            DepWorkspaceIsFalse(_, _) => write!(f, "`workspace` cannot be false"),
            AmbigousDepSpecGitPath(_, _) => write!(f, "Only one of `git` or `path` is allowed"),
            AmbigousDepSpecGitRegistry(_, _) => {
                write!(f, "Only one of `git` or `registry` is allowed")
            }
            AmbigousGitSpec(_, _) => write!(f, "Only one of `branch`, `tag` or `rev` is allowed"),
            MissingDepSpec(_, _) => {
                write!(f, "Missing one of `workspace`, `path`, `git` or `version`")
            }
            DepIgnoredGitKey(_, _, _) => {
                write!(f, "Not allowed without `git`")
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Warning {
    /// Warn about future removal in the 2024 edition.
    DeprecatedUnderscore {
        path: Option<FmtStr>,
        old: &'static str,
        new: &'static str,
        span: Span,
    },
    /// Warn about future removal in the 2024 edition.
    RedundantDeprecatedUnderscore {
        path: Option<FmtStr>,
        old: &'static str,
        new: &'static str,
        old_span: Span,
        new_span: Span,
    },
    MissingDepSpec(Span),
    WorkspaceDepIgnoredKey {
        path: FmtStr,
        key_span: Span,
        workspace_span: Span,
    },
    IgnoredUnknownKey(FmtStr, Span),
}

impl Diagnostic for Warning {
    type Hint = Hint;

    const SEVERITY: Severity = Severity::Warning;

    fn span(&self) -> Span {
        use Warning::*;
        match self {
            DeprecatedUnderscore { span, .. } => *span,
            RedundantDeprecatedUnderscore { old_span, .. } => *old_span,
            MissingDepSpec(s) => *s,
            WorkspaceDepIgnoredKey { key_span, .. } => *key_span,
            IgnoredUnknownKey(_, s) => *s,
        }
    }

    fn description(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Warning::*;
        match self {
            DeprecatedUnderscore { path, old, new, .. } => {
                if let Some(path) = path {
                    write!(f, "`{path}`: ")?;
                }
                write!(f, "`{old}` is deprecated in favor of `{new}` and will be unsupported in the 2024 edition")
            }
            RedundantDeprecatedUnderscore { path, old, new, .. } => {
                if let Some(path) = path {
                    write!(f, "`{path}`: ")?;
                }
                write!(
                    f,
                    "`{old}` is redundant with `{new}` and will be unsupported in the 2024 edition"
                )
            }
            MissingDepSpec(_) => write!(f, "Missing one of `workspace`, `path`, `git` or `version`, this will be unsupported in the 2024 edition"),
            WorkspaceDepIgnoredKey { path, .. } => write!(f, "Key `{path}` is ignored, because `workspace` is set"),
            IgnoredUnknownKey(path, _) => write!(f, "Unknown key `{path}` is ignored"),
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Warning::*;
        match self {
            DeprecatedUnderscore { new, .. } => write!(f, "Deprecated in favor of `{new}`"),
            RedundantDeprecatedUnderscore { new, .. } => write!(f, "Redundant with `{new}`"),
            MissingDepSpec(_) => {
                write!(f, "Missing one of `workspace`, `path`, `git` or `version`")
            }
            WorkspaceDepIgnoredKey { .. } => write!(f, "Key is ignored"),
            IgnoredUnknownKey(_, _) => write!(f, "Unknown key is ignored"),
        }
    }

    fn hint(&self) -> Option<Self::Hint> {
        use Warning::*;
        match self {
            DeprecatedUnderscore { .. } => None,
            RedundantDeprecatedUnderscore { new_span, .. } => {
                Some(Hint::RedundantDeprecatedUnderscore(*new_span))
            }
            MissingDepSpec(_) => None,
            WorkspaceDepIgnoredKey { workspace_span, .. } => {
                Some(Hint::WorkspaceDepIgnoredKey(*workspace_span))
            }
            IgnoredUnknownKey(_, _) => None,
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

    fn description(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        todo!()
    }

    fn annotation(&self, _f: &mut impl std::fmt::Write) -> std::fmt::Result {
        todo!()
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Hint {
    RedundantDeprecatedUnderscore(Span),
    WorkspaceDepIgnoredKey(Span),
}

impl DiagnosticHint for Hint {
    fn span(&self) -> Span {
        match self {
            Hint::RedundantDeprecatedUnderscore(s) => *s,
            Hint::WorkspaceDepIgnoredKey(s) => *s,
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Hint::RedundantDeprecatedUnderscore(_) => write!(f, "Used instead"),
            Hint::WorkspaceDepIgnoredKey(_) => write!(f, "Set here"),
        }
    }
}
