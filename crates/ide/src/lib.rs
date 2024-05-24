use common::{Ctx, Diagnostics};
use semver::SemverCtx;

pub use cargo::CargoCtx;
pub use check::{
    check, Dependency, DependencyFeatures, DependencyGitSpec, DependencyKind, DependencySpec, State,
};
pub use error::{Error, Hint, Info, Warning};
use toml::MapTable;

pub mod cargo;
pub mod check;
pub mod error;

#[rustfmt::skip]
pub trait IdeCtx:
    Ctx<Error = Self::IdeError, Warning = Self::IdeWarning, Info = Self::IdeInfo>
    + SemverCtx<SemverError = Self::IdeError, SemverWarning = Self::IdeWarning, SemverInfo = Self::IdeInfo>
    + CargoCtx<CargoError = Self::IdeError, CargoWarning = Self::IdeWarning, CargoInfo = Self::IdeInfo>
{
    type IdeError: From<Error> + From<toml::Error> + From<semver::Error> + From<cargo::Error>;
    type IdeWarning: From<Warning> + From<toml::Warning> + From<semver::Warning> + From<cargo::Warning>;
    type IdeInfo: From<Info> + From<toml::Info> + From<semver::Info> + From<cargo::Info>;

    fn check<'a>(&mut self, map: &'a MapTable<'a>) -> State<'a> {
        check(self, map)
    }
}

impl<E, W, H> IdeCtx for Diagnostics<E, W, H>
where
    E: From<Error> + From<toml::Error> + From<semver::Error> + From<cargo::Error>,
    W: From<Warning> + From<toml::Warning> + From<semver::Warning> + From<cargo::Warning>,
    H: From<Info> + From<toml::Info> + From<semver::Info> + From<cargo::Info>,
{
    type IdeError = E;
    type IdeWarning = W;
    type IdeInfo = H;
}

pub type IdeDiagnostics = Diagnostics<Error, Warning, Info>;
