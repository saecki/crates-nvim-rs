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
        // TODO: is this even useful?
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
    DepWorkspaceIsFalse(Span),
    AmbigousDepSpecGitPath(Span),
    AmbigousDepSpecGitRegistry(Span),
    AmbigousGitSpec(Span),
    /// Invalid in the 2024 edition.
    MissingDepSpec(Span),
    DepIgnoredGitKey(&'static str, Span),
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
            DepWorkspaceIsFalse(s) => *s,
            AmbigousDepSpecGitPath(s) => *s,
            AmbigousDepSpecGitRegistry(s) => *s,
            AmbigousGitSpec(s) => *s,
            MissingDepSpec(s) => *s,
            DepIgnoredGitKey(_, s) => *s,
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
                "Expected `{key}` to be of type {expected}, found {found}"
            ),
            WrongDatatypeInArray {
                expected,
                found,
                ..
            } => write!(
                f,
                "Expected value to be of type {expected}, found {found}"
            ),
            UnsupportedUnderscore { old, new, .. } => {
                write!(
                    f,
                    "`{old}` is unsupported in the 2024 edition; instead use `{new}`"
                )
            }
            DepWorkspaceIsFalse(_) => write!(f, "`workspace` cannot be false"),
            AmbigousDepSpecGitPath(_) => write!(
                f,
                "Ambigous dependency specification, only one of `git` or `path` is allowed"
            ),
            AmbigousDepSpecGitRegistry(_) => write!(
                f,
                "Ambigous dependency specification, only one of `git` or `registry` is allowed"
            ),
            AmbigousGitSpec(_) => write!(
                f,
                "Ambigous dependency specification, only one of `branch`, `tag` or `rev` is allowed",
            ),
            MissingDepSpec(_) => write!(
                f,
                "Missing one of `workspace`, `path`, `git` or `version`, this is unsupported in the 2024 edition",
            ),
            DepIgnoredGitKey(key, _) => {
                write!(f, "Specifying `{key}` without `git` is not allowed")
            }
        }
    }

    fn annotation(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        use Error::*;
        match self {
            WrongDatatypeInTable { expected, .. } => write!(f, "Expected {expected}"),
            WrongDatatypeInArray { expected, .. } => write!(f, "Expected {expected}"),
            UnsupportedUnderscore { new, .. } => write!(f, "Unsupported; instead use `{new}`"),
            DepWorkspaceIsFalse(_) => write!(f, "`workspace` cannot be false"),
            AmbigousDepSpecGitPath(_) => write!(f, "Only one of `git` or `path` is allowed"),
            AmbigousDepSpecGitRegistry(_) => {
                write!(f, "Only one of `git` or `registry` is allowed")
            }
            AmbigousGitSpec(_) => write!(f, "Only one of `branch`, `tag` or `rev` is allowed"),
            MissingDepSpec(_) => {
                write!(f, "Missing one of `workspace`, `path`, `git` or `version`")
            }
            DepIgnoredGitKey(key, _) => {
                write!(f, "Specifying `{key}` without `git` is not allowed")
            }
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
    MissingDepSpec(Span),
    WorkspaceDepIgnoredKey {
        key: &'static str,
        key_span: Span,
        workspace_span: Span,
    },
    DepIgnoredUnknownKey(FmtStr, Span),
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
            DepIgnoredUnknownKey(_, s) => *s,
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
            MissingDepSpec(_) => write!(f, "Missing one of `workspace`, `path`, `git` or `version`, this will be unsupported in the 2024 edition"),
            WorkspaceDepIgnoredKey { key, .. } => write!(f, "Key `{key}` is ignored, because `workspace` is set"),
            DepIgnoredUnknownKey(key, _) => write!(f, "Unknown key `{key}` is ignored"),
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
            DepIgnoredUnknownKey(key, _) => write!(f, "Unknown key `{key}` is ignored"),
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
            DepIgnoredUnknownKey(_, _) => None,
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
