use std::mem::ManuallyDrop;

use bumpalo::Bump;

use crate::{Ast, MapTable, TomlCtx};

pub struct Toml<'a> {
    pub ast: Ast<'a>,
    pub map: &'a MapTable<'a>,
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
    pub fn parse(ctx: &mut impl TomlCtx, path: &str, text: &str) -> Container {
        let bump = Box::leak(Box::new(Bump::new()));
        let path = bump.alloc_str(path);
        let text = bump.alloc_str(text);

        // SAFETY: bump is constructed using Box::leak and text is allocated in bump
        unsafe { build_container(ctx, bump, path, text) }
    }

    pub fn parse_with<'b>(
        ctx: &mut impl TomlCtx,
        path: &str,
        alloc_text: impl FnOnce(&'b Bump) -> &'b str,
    ) -> Container {
        let bump = Box::leak(Box::new(Bump::new()));

        let path = bump.alloc_str(path);

        let text = alloc_text(bump);

        // force lifetime of text to be 'static
        // SAFETY: text was allocated using bump
        let text: &str = unsafe { std::mem::transmute(text) };

        // SAFETY: bump is constructed using Box::leak and text is allocated in bump
        unsafe { build_container(ctx, bump, path, text) }
    }

    pub fn toml(&'a self) -> &'a Toml<'a> {
        // only give out a reference which is restricted to the container's lifetime
        &self.toml
    }
}

/// SAFETY: `bump` has to be constructed using Box::leak, so it can be freed when the container is
/// dropped, and `text` has to be allocated inside `bump`
unsafe fn build_container(
    ctx: &mut impl TomlCtx,
    bump: &'static Bump,
    path: &'static str,
    text: &'static str,
) -> Container {
    let tokens = ctx.lex(bump, path, text);
    let ast = ctx.parse(bump, tokens);
    let map = ctx.map(bump, &ast);

    let toml = Toml { ast, map };
    let toml = ManuallyDrop::new(toml);

    Container { toml, bump }
}
