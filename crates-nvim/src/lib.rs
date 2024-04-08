use common::{Ctx, Diagnostics};

use crate::error::{CargoError, CargoHint, CargoWarning, Error, Hint, Warning};

mod check;
mod error;

pub trait NvimCtx:
    Ctx<Error = Self::NvimError, Warning = Self::NvimWarning, Hint = Self::NvimHint>
{
    type NvimError: From<Error> + From<toml::Error> + From<CargoError>;
    type NvimWarning: From<Warning>;
    type NvimHint: From<Hint>;
}

impl<E, W, H> NvimCtx for Diagnostics<E, W, H>
where
    E: From<Error> + From<toml::Error> + From<CargoError>,
    W: From<Warning>,
    H: From<Hint>,
{
    type NvimError = E;
    type NvimWarning = W;
    type NvimHint = H;
}

pub type NvimDiagnostics = Diagnostics<Error, Warning, Hint>;

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
