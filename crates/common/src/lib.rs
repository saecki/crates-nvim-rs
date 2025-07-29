use crate::diagnostic::Diagnostic;

pub use onevec::*;
pub use source::*;
pub use span::*;
pub use text::*;

pub mod diagnostic;
mod onevec;
mod source;
mod span;
mod text;

pub trait Ctx: Sized {
    type Error;
    type Warning;
    type Info;

    fn error(&mut self, error: impl Into<Self::Error>);
    fn warn(&mut self, warning: impl Into<Self::Warning>);
    fn info(&mut self, info: impl Into<Self::Info>);

    fn mark(&self) -> DiagnosticMark;

    fn reset(&mut self, mark: DiagnosticMark);
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct DiagnosticMark {
    error: u32,
    warning: u32,
    info: u32,
}

pub struct Diagnostics<E, W, I> {
    pub errors: Vec<E>,
    pub warnings: Vec<W>,
    pub infos: Vec<I>,
}

impl<E, W, I> Diagnostics<E, W, I> {
    pub const fn new() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            infos: Vec::new(),
        }
    }

    /// The combined number of all diagnostics.
    pub fn len(&self) -> usize {
        self.errors.len() + self.warnings.len() + self.infos.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl<E, W, I> Default for Diagnostics<E, W, I> {
    fn default() -> Self {
        Self {
            errors: Vec::new(),
            warnings: Vec::new(),
            infos: Vec::new(),
        }
    }
}

impl<E, W, I> Ctx for Diagnostics<E, W, I> {
    type Error = E;
    type Warning = W;
    type Info = I;

    fn error(&mut self, error: impl Into<E>) {
        self.errors.push(error.into());
    }

    fn warn(&mut self, warning: impl Into<W>) {
        self.warnings.push(warning.into());
    }

    fn info(&mut self, info: impl Into<I>) {
        self.infos.push(info.into());
    }

    fn mark(&self) -> DiagnosticMark {
        DiagnosticMark {
            error: self.errors.len() as u32,
            warning: self.warnings.len() as u32,
            info: self.infos.len() as u32,
        }
    }

    fn reset(&mut self, mark: DiagnosticMark) {
        self.errors.truncate(mark.error as usize);
        self.warnings.truncate(mark.warning as usize);
        self.infos.truncate(mark.info as usize);
    }
}

impl<E, W, I> Diagnostics<E, W, I>
where
    E: Diagnostic,
    W: Diagnostic,
    I: Diagnostic,
{
    pub fn sort_diagnostics(&mut self) {
        self.errors.sort_by(diagnostic::cmp);
        self.warnings.sort_by(diagnostic::cmp);
        self.infos.sort_by(diagnostic::cmp);
    }
}
