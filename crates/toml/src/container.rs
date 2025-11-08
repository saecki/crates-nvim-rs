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

    /// ```compile_fail
    /// let mut ctx = TomlDiagnostics::default();
    /// let static_bump: &'static Bump;
    /// let _container = Container::parse_with(&mut ctx, "<test>", |bump| {
    ///     static_bump = bump;
    ///     bump.alloc_str("a = 1")
    /// });
    /// ```
    pub fn parse_with(
        ctx: &mut impl TomlCtx,
        path: &str,
        alloc_text: impl for<'b> FnOnce(&'b Bump) -> &'b str,
    ) -> Container {
        let bump = Box::leak(Box::new(Bump::new()));
        let path = bump.alloc_str(path);
        let text = alloc_text(bump);

        // SAFETY: bump is constructed using Box::leak and text is allocated in bump
        unsafe { build_container(ctx, bump, path, text) }
    }

    pub fn toml(&'a self) -> &'a Toml<'a> {
        // Only give out a reference which is restricted to the container's lifetime.
        let ptr = &*self.toml as *const Toml<'static> as *const Toml<'a>;
        // TODO: safety comment
        unsafe { &*ptr }
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
    let toml = ctx.parse(bump, path, text);
    let toml = ManuallyDrop::new(toml);

    Container { toml, bump }
}
