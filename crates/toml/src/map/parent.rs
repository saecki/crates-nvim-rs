use std::cell::Cell;
use std::marker::PhantomData;
use std::mem::MaybeUninit;

use bumpalo::Bump;

use crate::MapTable;
use crate::map::{
    MapArrayInline, MapArrayInlineEntry, MapArrayToplevel, MapArrayToplevelEntry, MapTableEntry,
    MapTableEntryRepr, MapTableRepr,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReprIdx(pub u32);

impl ReprIdx {
    pub fn idx(self) -> usize {
        self.0 as usize
    }
}

// TODO: The `get` functions of the parent structs should ideally be guarded
// from access using the type system.
// Either have a generic tag that is only changed once the whole map is
// constructed. Or maybe have a generic wrapper struct.
// Generic tag with phantom data on map that allows accessing parent pointers.
// During construction disallow accessing them, then transmute the root table, making
// parent pointers accessible, and making reference

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTable<'a> {
    ptr: Cyclic<'a, MapTable<'a>>,
    idx: ReprIdx,
}

impl<'a> ParentTable<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapTable<'a>>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub(crate) fn insert_repr(
        _bump: &'a Bump,
        map: &mut &'a mut MapTable<'a>,
        repr: MapTableRepr<'a>,
    ) -> Self {
        let idx = ReprIdx(map.reprs.len() as u32);
        map.reprs.push(repr);
        // SAFETY: map is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_ptr(*map) };
        Self { ptr, idx }
    }

    pub(crate) fn with_idx(mut self, idx: ReprIdx) -> Self {
        self.idx = idx;
        self
    }

    pub fn get(self) -> &'a MapTable<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArray<'a> {
    ptr: Cyclic<'a, MapArrayToplevel<'a>>,
}

impl<'a> ParentToplevelArray<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapArrayToplevel<'a>>) -> Self {
        Self { ptr }
    }

    /// The bump allocator is only required to prove that entry is not stack
    /// allocated.
    pub(crate) fn new_from(
        _bump: &'a Bump,
        array: &mut &'a mut MapArrayToplevel<'a>,
    ) -> ParentToplevelArray<'a> {
        // SAFETY: entry is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_ptr(*array) };
        Self { ptr }
    }

    pub fn get(self) -> &'a MapArrayToplevel<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArray<'a> {
    ptr: Cyclic<'a, MapArrayInline<'a>>,
}

impl<'a> ParentInlineArray<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapArrayInline<'a>>) -> Self {
        Self { ptr }
    }

    pub fn get(self) -> &'a MapArrayInline<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParentEntry<'a> {
    Table(ParentTableEntry<'a>),
    ToplevelArray(ParentToplevelArrayEntry<'a>),
    ToplevelArrayExtension(ParentToplevelArrayExtensionEntry<'a>),
    InlineArray(ParentInlineArrayEntry<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTableEntry<'a> {
    ptr: Cyclic<'a, MapTableEntry<'a>>,
    idx: ReprIdx,
}

impl<'a> ParentTableEntry<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapTableEntry<'a>>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    /// The bump allocator is only required to prove that entry is not stack
    /// allocated.
    pub(crate) fn insert_repr(
        _bump: &'a Bump,
        entry: &mut &'a mut MapTableEntry<'a>,
        repr: MapTableEntryRepr<'a>,
    ) -> ParentTableEntry<'a> {
        let idx = ReprIdx(entry.reprs.len() as u32);
        entry.reprs.push(repr);
        // SAFETY: entry is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_ptr(*entry) };
        Self { ptr, idx }
    }

    pub fn get(self) -> &'a MapTableEntry<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableEntryRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }

    pub fn wrap(self) -> ParentEntry<'a> {
        ParentEntry::Table(self)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayEntry<'a> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a>>,
}

impl<'a> ParentToplevelArrayEntry<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a>>) -> Self {
        Self { ptr }
    }

    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }

    pub fn wrap(self) -> ParentEntry<'a> {
        ParentEntry::ToplevelArray(self)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayExtensionEntry<'a> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a>>,
    idx: ReprIdx,
}

impl<'a> ParentToplevelArrayExtensionEntry<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a>>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub(crate) fn insert(
        array_entry: &mut &'a mut MapArrayToplevelEntry<'a>,
        parent_entry: ParentTableEntry<'a>,
    ) -> Self {
        let idx = ReprIdx(array_entry.extensions.len() as u32);
        array_entry.extensions.push(parent_entry);
        // SAFETY: map is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_ptr(*array_entry) };
        Self { ptr, idx }
    }

    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }

    pub fn parent_table_entry(self) -> ParentTableEntry<'a> {
        self.ptr.get().extensions[self.idx.idx()]
    }

    pub fn wrap(self) -> ParentEntry<'a> {
        ParentEntry::ToplevelArrayExtension(self)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArrayEntry<'a> {
    ptr: Cyclic<'a, MapArrayInlineEntry<'a>>,
}

impl<'a> ParentInlineArrayEntry<'a> {
    pub(crate) fn new(ptr: Cyclic<'a, MapArrayInlineEntry<'a>>) -> Self {
        Self { ptr }
    }

    pub fn get(self) -> &'a MapArrayInlineEntry<'a> {
        self.ptr.get()
    }

    pub fn wrap(self) -> ParentEntry<'a> {
        ParentEntry::InlineArray(self)
    }
}

/// Construct A slice with [`Cyclic`] references.
/// NOTE: The [`Cyclic`] references are only valid to access once this function
/// returns and the values are written.
pub(crate) fn cyclic_slice<'a, V, T>(
    bump: &'a Bump,
    values: &'a [V],
    mut f: impl FnMut(u32, Cyclic<'a, T>, &'a V) -> T,
) -> &'a [T] {
    let slice = bump.alloc_slice_fill_with(values.len(), |_| MaybeUninit::uninit());

    for ((loc, idx), val) in slice.iter_mut().zip(0..).zip(values) {
        // SAFETY: loc is allocated inside the bump allocator, we just can't
        // borrow it, because we need to write to it later, after the value has
        // been constructed.
        let ptr = unsafe { Cyclic::new_ptr(loc as *const MaybeUninit<T> as *const T) };
        let init = f(idx, ptr, val);
        loc.write(init);
    }

    // SAFETY: The slice has been initialized by the loop.
    unsafe { std::mem::transmute::<&'a [MaybeUninit<T>], &'a [T]>(slice) }
}

/// Construct A [`Cyclic`] reference.
/// NOTE: The [`Cyclic`] reference is only valid to access once this function
/// returns and the value is written.
pub(crate) fn cyclic<'a, T>(bump: &'a Bump, f: impl FnOnce(Cyclic<'a, T>) -> T) -> &'a mut T {
    let loc = bump.alloc(MaybeUninit::uninit());
    // SAFETY: loc is allocated inside the bump allocator, we just can't borrow
    // it, because we need to write to it later, after the value has been
    // constructed.
    let ptr = unsafe { Cyclic::new_ptr(loc as *const MaybeUninit<T> as *const T) };
    let val = f(ptr);
    loc.write(val)
}

pub(crate) struct CyclicCell<'a, T>(Cell<Option<Cyclic<'a, T>>>);

impl<T> Eq for CyclicCell<'_, T> {}
impl<T> PartialEq for CyclicCell<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        // Can't compare by value, because that might recurse indefinitely.
        todo!("decide what to do here {self:?} {other:?}")
    }
}

impl<T> std::fmt::Debug for CyclicCell<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("...")
    }
}

impl<'a, T> CyclicCell<'a, T> {
    pub(crate) fn new() -> Self {
        Self(Cell::new(None))
    }

    /// # Safety
    ///
    /// This should only be called while the map is being constructed.
    /// That happens on a single thread
    pub(crate) fn set(&self, ptr: Cyclic<'a, T>) {
        self.0.set(Some(ptr));
    }

    /// # Safety
    ///
    /// The caller must guarantee that the cyclic pointer is valid and points to
    /// a valid value.
    pub(crate) unsafe fn get(self) -> Option<&'a T> {
        let ptr = self.0.get()?;
        Some(unsafe { ptr.get() })
    }
}

pub(crate) struct Cyclic<'a, T> {
    ptr: *const T,
    lifetime: PhantomData<&'a T>,
}

impl<T> Eq for Cyclic<'_, T> {}
impl<T> PartialEq for Cyclic<'_, T> {
    fn eq(&self, other: &Self) -> bool {
        // Can't compare by value, because that might recurse indefinitely.
        todo!("decide what to do here {self:?} {other:?}")
    }
}

impl<T> Copy for Cyclic<'_, T> {}
impl<T> Clone for Cyclic<'_, T> {
    fn clone(&self) -> Self {
        Cyclic {
            ptr: self.ptr,
            lifetime: self.lifetime,
        }
    }
}

impl<T> std::fmt::Debug for Cyclic<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("...")
    }
}

impl<'a, T> Cyclic<'a, T> {
    fn new(reference: &'a T) -> Self {
        Self {
            ptr: reference,
            lifetime: PhantomData,
        }
    }

    /// # Safety
    ///
    /// The caller must guarantee that pointer is, or will be valid for the
    /// specified lifetime.
    unsafe fn new_ptr(ptr: *const T) -> Self {
        Self {
            ptr,
            lifetime: PhantomData,
        }
    }

    /// # Safety
    ///
    /// The caller must guarantee that the cyclic pointer is valid and points to
    /// a valid value.
    pub(crate) unsafe fn get(self) -> &'a T {
        unsafe { &*self.ptr }
    }
}
