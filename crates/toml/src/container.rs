use std::mem::ManuallyDrop;

use bumpalo::Bump;

use crate::{Asts, MapTable, Tokens, TomlCtx};

pub struct Toml<'a> {
    pub input: &'a str,
    pub tokens: Tokens<'a>,
    pub asts: Asts<'a>,
    pub map: MapTable<'a>,
}

/// Self contained, movable container for a parsed [`Toml`] structure.
pub struct Container {
    toml: ManuallyDrop<Toml<'static>>,
    bump: &'static Bump,
}

impl Drop for Container {
    fn drop(&mut self) {
        // SAFETY: drop is only ever called once
        unsafe {
            ManuallyDrop::drop(&mut self.toml);
        }

        let ptr = self.bump as *const Bump;
        // SAFETY: `self.bump` is only ever constructed using `Box::leak` and the static reference does
        // never escape the private api. The only references to `self.bump` are in `self.toml`
        // which is explicitly dropped before.
        unsafe {
            let bump = Box::from_raw(ptr.cast_mut());
            drop(bump);
        }
    }
}

impl<'a> Container {
    pub fn parse(ctx: &mut impl TomlCtx, input: &str) -> Container {
        let bump = Box::leak(Box::new(Bump::new()));
        let input = bump.alloc_str(input);

        // SAFETY: bump is constructed using Box::leak and input is allocated in bump
        unsafe { build_container(ctx, bump, input) }
    }

    pub fn parse_with<'b>(
        ctx: &mut impl TomlCtx,
        alloc_input: impl FnOnce(&'b Bump) -> &'b str,
    ) -> Container {
        let bump = Box::leak(Box::new(Bump::new()));

        let input = alloc_input(bump);

        // force lifetime of input to be 'static
        // SAFETY: input was allocated using bump
        let input: &str = unsafe { std::mem::transmute(input) };

        // SAFETY: bump is constructed using Box::leak and input is allocated in bump
        unsafe { build_container(ctx, bump, input) }
    }

    pub fn toml(&'a self) -> &'a Toml<'a> {
        // only give out a reference which is restricted to the container's lifetime
        &self.toml
    }
}

/// SAFETY: `bump` has to be constructed using Box::leak, so it can be freed when the container is
/// dropped, and `input` has to be allocated inside `bump`
unsafe fn build_container(
    ctx: &mut impl TomlCtx,
    bump: &'static Bump,
    input: &'static str,
) -> Container {
    let tokens = ctx.lex(bump, input);
    let asts = ctx.parse(bump, &tokens);
    let map = ctx.map(&asts);

    let toml = Toml {
        input,
        tokens,
        asts,
        map,
    };
    let toml = ManuallyDrop::new(toml);

    Container { toml, bump }
}
