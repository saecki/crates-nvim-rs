use common::{Ctx, Diagnostics};
use semver::SemverCtx;

pub use check::{
    check, Dependency, DependencyFeatures, DependencyGitSpec, DependencyKind, DependencySpec, State,
};
pub use error::{CargoError, CargoHint, CargoWarning, Error, Hint, Warning};
use toml::MapTable;

pub mod check;
pub mod error;

#[rustfmt::skip]
pub trait IdeCtx:
    Ctx<Error = Self::IdeError, Warning = Self::IdeWarning, Hint = Self::IdeHint>
    + SemverCtx<SemverError = Self::IdeError, SemverWarning = Self::IdeWarning, SemverHint = Self::IdeHint>
    + CargoCtx<CargoError = Self::IdeError, CargoWarning = Self::IdeWarning, CargoHint = Self::IdeHint>
{
    type IdeError: From<Error> + From<toml::Error> + From<semver::Error> + From<CargoError>;
    type IdeWarning: From<Warning> + From<toml::Warning> + From<semver::Warning> + From<CargoWarning>;
    type IdeHint: From<Hint> + From<toml::Hint> + From<semver::Hint> + From<CargoHint>;

    fn check<'a>(&mut self, map: &'a MapTable<'a>) -> State<'a> {
        check(self, map)
    }
}

impl<E, W, H> IdeCtx for Diagnostics<E, W, H>
where
    E: From<Error> + From<toml::Error> + From<semver::Error> + From<CargoError>,
    W: From<Warning> + From<toml::Warning> + From<semver::Warning> + From<CargoWarning>,
    H: From<Hint> + From<toml::Hint> + From<semver::Hint> + From<CargoHint>,
{
    type IdeError = E;
    type IdeWarning = W;
    type IdeHint = H;
}

pub type IdeDiagnostics = Diagnostics<Error, Warning, Hint>;

pub trait CargoCtx:
    Ctx<Error = Self::CargoError, Warning = Self::CargoWarning, Hint = Self::CargoHint>
{
    type CargoError: From<CargoError>;
    type CargoWarning: From<CargoWarning>;
    type CargoHint: From<CargoHint>;
}

impl<E, W, H> CargoCtx for Diagnostics<E, W, H>
where
    E: From<CargoError>,
    W: From<CargoWarning>,
    H: From<CargoHint>,
{
    type CargoError = E;
    type CargoWarning = W;
    type CargoHint = H;
}
