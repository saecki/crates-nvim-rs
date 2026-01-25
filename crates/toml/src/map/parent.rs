use std::cell::Cell;
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ptr::NonNull;

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

/// A generic tag used to signal that a [`Cyclic`] reference isn't yet safe to
/// access, because it is still being constructed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) struct Incomplete;

/// A generic tag used to signal that a [`Cyclic`] reference is safe to access.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Complete;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTable<'a, S = Complete> {
    ptr: Cyclic<'a, MapTable<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentTable<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapTable<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }
}

impl<'a> ParentTable<'a, Incomplete> {
    pub(super) fn insert_repr(
        _bump: &'a Bump,
        map: &'_ mut &'a mut MapTable<'a, Incomplete>,
        repr: MapTableRepr<'a, Incomplete>,
    ) -> Self {
        let idx = ReprIdx(map.reprs.len() as u32);
        map.reprs.push(repr);
        // SAFETY: map is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_init_ptr(NonNull::from_ref(*map)) };
        Self::new(ptr, idx)
    }
}

impl<'a> ParentTable<'a, Complete> {
    pub fn get(self) -> &'a MapTable<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArray<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayToplevel<'a, S>, S>,
}

impl<'a, S> ParentToplevelArray<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevel<'a, S>, S>) -> Self {
        Self { ptr }
    }
}

impl<'a> ParentToplevelArray<'a, Incomplete> {
    /// The bump allocator is only required to prove that entry is not stack
    /// allocated.
    pub(super) fn new_from(
        _bump: &'a Bump,
        array: &mut &'a mut MapArrayToplevel<'a, Incomplete>,
    ) -> Self {
        // SAFETY: entry is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_init_ptr(NonNull::from_ref(*array)) };
        Self::new(ptr)
    }
}

impl<'a> ParentToplevelArray<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevel<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArray<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayInline<'a, S>, S>,
}

impl<'a, S> ParentInlineArray<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayInline<'a, S>, S>) -> Self {
        Self { ptr }
    }
}

impl<'a> ParentInlineArray<'a, Complete> {
    pub fn get(self) -> &'a MapArrayInline<'a> {
        self.ptr.get()
    }
}

impl<'a> std::ops::Deref for ParentInlineArray<'a, Complete> {
    type Target = MapArrayInline<'a>;

    fn deref(&self) -> &'a Self::Target {
        self.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ParentEntry<'a, S = Complete> {
    Table(ParentTableEntry<'a, S>),
    ToplevelArray(ParentToplevelArrayEntry<'a, S>),
    ToplevelArrayExtension(ParentToplevelArrayExtensionEntry<'a, S>),
    InlineArray(ParentInlineArrayEntry<'a, S>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentTableEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapTableEntry<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentTableEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapTableEntry<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::Table(self)
    }
}

impl<'a> ParentTableEntry<'a, Incomplete> {
    /// The bump allocator is only required to prove that entry is not stack
    /// allocated.
    pub(super) fn insert_repr(
        _bump: &'a Bump,
        entry: &mut &'a mut MapTableEntry<'a, Incomplete>,
        repr: MapTableEntryRepr<'a, Incomplete>,
    ) -> Self {
        let idx = ReprIdx(entry.reprs.len() as u32);
        entry.reprs.push(repr);
        // SAFETY: entry is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_init_ptr(NonNull::from_ref(*entry)) };
        Self::new(ptr, idx)
    }
}

impl<'a> ParentTableEntry<'a, Complete> {
    pub fn get(self) -> &'a MapTableEntry<'a> {
        self.ptr.get()
    }

    pub fn repr(self) -> &'a MapTableEntryRepr<'a> {
        &self.ptr.get().reprs[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>,
}

impl<'a, S> ParentToplevelArrayEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>) -> Self {
        Self { ptr }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::ToplevelArray(self)
    }
}

impl<'a> ParentToplevelArrayEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentToplevelArrayExtensionEntry<'a, S> {
    ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>,
    idx: ReprIdx,
}

impl<'a, S> ParentToplevelArrayExtensionEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayToplevelEntry<'a, S>, S>, idx: ReprIdx) -> Self {
        Self { ptr, idx }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::ToplevelArrayExtension(self)
    }
}

impl<'a> ParentToplevelArrayExtensionEntry<'a, Incomplete> {
    pub(super) fn insert(
        array_entry: &mut &'a mut MapArrayToplevelEntry<'a, Incomplete>,
        parent_entry: ParentTableEntry<'a, Incomplete>,
    ) -> Self {
        let idx = ReprIdx(array_entry.extensions.len() as u32);
        array_entry.extensions.push(parent_entry);
        // SAFETY: map is allocated inside bump and will be valid to access.
        let ptr = unsafe { Cyclic::new_init_ptr(NonNull::from_ref(*array_entry)) };
        Self::new(ptr, idx)
    }
}

impl<'a> ParentToplevelArrayExtensionEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayToplevelEntry<'a> {
        self.ptr.get()
    }

    pub fn parent_table_entry(self) -> ParentTableEntry<'a> {
        self.ptr.get().extensions[self.idx.idx()]
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentInlineArrayEntry<'a, S = Complete> {
    ptr: Cyclic<'a, MapArrayInlineEntry<'a, S>, S>,
}

impl<'a, S> ParentInlineArrayEntry<'a, S> {
    pub(super) fn new(ptr: Cyclic<'a, MapArrayInlineEntry<'a, S>, S>) -> Self {
        Self { ptr }
    }

    pub fn wrap(self) -> ParentEntry<'a, S> {
        ParentEntry::InlineArray(self)
    }
}

impl<'a> ParentInlineArrayEntry<'a, Complete> {
    pub fn get(self) -> &'a MapArrayInlineEntry<'a> {
        self.ptr.get()
    }
}

/// Construct A slice with [`Cyclic`] references.
/// NOTE: The [`Cyclic`] references are only valid to access once this function
/// returns and the values are written.
pub(super) fn cyclic_slice<'a, V, T>(
    bump: &'a Bump,
    values: &'a [V],
    mut f: impl FnMut(u32, Cyclic<'a, T, Incomplete>, &'a V) -> T,
) -> &'a [T] {
    let slice = bump.alloc_slice_fill_with(values.len(), |_| MaybeUninit::uninit());

    for ((loc, idx), val) in slice.iter_mut().zip(0..).zip(values) {
        // SAFETY: loc is allocated inside the bump allocator, we just can't
        // borrow it, because we need to write to it later, after the value has
        // been constructed.
        let ptr = unsafe { Cyclic::new_uninit_ptr(NonNull::from_ref(loc)) };
        let init = f(idx, ptr, val);
        loc.write(init);
    }

    // SAFETY: The slice has been initialized by the loop.
    unsafe { std::mem::transmute::<&'a [MaybeUninit<T>], &'a [T]>(slice) }
}

/// Construct A [`Cyclic`] reference.
/// NOTE: The [`Cyclic`] reference is only valid to access once this function
/// returns and the value is written.
pub(super) fn cyclic<'a, T>(
    bump: &'a Bump,
    f: impl FnOnce(Cyclic<'a, T, Incomplete>) -> T,
) -> &'a mut T {
    let loc = bump.alloc(MaybeUninit::uninit());
    // SAFETY: loc is allocated inside the bump allocator, we just can't borrow
    // it, because we need to write to it later, after the value has been
    // constructed.
    let ptr = unsafe { Cyclic::new_uninit_ptr(NonNull::from_ref(loc)) };
    let val = f(ptr);
    loc.write(val)
}

#[repr(transparent)]
pub(super) struct Cyclic<'a, T, S> {
    ptr: NonNull<MaybeUninit<T>>,
    lifetime: PhantomData<&'a T>,
    state: PhantomData<S>,
}

impl<T, S> Eq for Cyclic<'_, T, S> {}
impl<T, S> PartialEq for Cyclic<'_, T, S> {
    fn eq(&self, _other: &Self) -> bool {
        // Can't compare by value, because that might recurse indefinitely.
        true
    }
}

impl<T, S> Copy for Cyclic<'_, T, S> {}
impl<T, S> Clone for Cyclic<'_, T, S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T, S> std::fmt::Debug for Cyclic<'_, T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("...")
    }
}

impl<'a, T> Cyclic<'a, T, Complete> {
    pub(super) fn new(reference: &'a T) -> Self {
        Self {
            ptr: NonNull::new(reference as *const T as *mut MaybeUninit<T>).unwrap(),
            lifetime: PhantomData,
            state: PhantomData,
        }
    }

    fn get(self) -> &'a T {
        // SAFETY: Either this cyclic cell was constructed in a complete state,
        // or the state has been changed to be complete.
        unsafe { &*(self.ptr.as_ptr() as *const T) }
    }
}

impl<'a, T> Cyclic<'a, T, Incomplete> {
    /// # Safety
    ///
    /// The caller must guarantee that pointer is, or will be valid for the
    /// specified lifetime.
    unsafe fn new_uninit_ptr(ptr: NonNull<MaybeUninit<T>>) -> Self {
        Self {
            ptr,
            lifetime: PhantomData,
            state: PhantomData,
        }
    }

    /// # Safety
    ///
    /// The caller must guarantee that pointer is, or will be valid for the
    /// specified lifetime.
    unsafe fn new_init_ptr(ptr: NonNull<T>) -> Self {
        Self {
            ptr: NonNull::new(ptr.as_ptr() as *mut MaybeUninit<T>).unwrap(),
            lifetime: PhantomData,
            state: PhantomData,
        }
    }
}

/// A cell that is Sync and Send, by requiring manual synchronization from the
/// user.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ManuallySyncCell<T: Copy>(Cell<Option<T>>);

// SAFETY: The user is responsible for synchronization.
unsafe impl<T: Copy> Send for ManuallySyncCell<T> {}
unsafe impl<T: Copy> Sync for ManuallySyncCell<T> {}

impl<T: Copy> ManuallySyncCell<T> {
    pub(crate) const fn empty() -> Self {
        Self(Cell::new(None))
    }

    /// # Safety
    ///
    /// If there are no other references to the inside of this cell, and there
    /// aren't multiple threads accessing the cell this should be safe.
    pub(crate) unsafe fn set(&self, val: T) {
        self.0.set(Some(val));
    }

    pub(crate) fn get(&self) -> Option<T>
    where
        T: Copy,
    {
        self.0.get()
    }
}
