//! This module contains types and functions to [construct] a tree containing
//! cyclic references.
//!
//! Most of the unsafe code relies on interior mutability of [`CyclicCell`] by
//! means of [`UnsafeCell`] and the fact that we only *store* [`Cyclic`]
//! references during the construction, but never actually access them until
//! the map has been fully constructed. After that the map isn't mutated using
//! interior mutability anymore and the shared cyclic references are valid to
//! access.
//!
//! This is enforced using the type-state pattern. During construction, the
//! [`Incomplete`] tag is used as a generic tag to mark the [`Cyclic`]
//! references as not yet accessible. After construction is finished, the whole
//! map is [`std::mem::transmute`]d to having the [`Complete`] tag.
//!
//! [construct]: crate::map::construct

use std::cell::{Cell, UnsafeCell};
use std::marker::PhantomData;
use std::mem::MaybeUninit;
use std::ops::Deref;
use std::ptr::NonNull;

use bumpalo::Bump;

/// A generic tag used to signal that a [`Cyclic`] reference isn't yet safe to
/// access, because it is still being constructed.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(in crate::map) struct Incomplete;

/// A generic tag used to signal that a [`Cyclic`] reference is safe to access.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Complete;

/// Construct a slice with [`Cyclic`] references.
/// NOTE: The [`Cyclic`] references are only valid to access once this function
/// returns and the values are written.
pub(in crate::map) fn cyclic_slice<'a, V, T>(
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
        loc.write(UnsafeCell::new(init));
    }

    // SAFETY: The slice has been initialized by the loop and `UnsafeCell` is
    // `repr(transparent)`.
    unsafe { std::mem::transmute::<&'a [MaybeUninit<UnsafeCell<T>>], &'a [T]>(slice) }
}

/// Construct a [`CyclicCell`] containing [`Cyclic`] references its value.
/// NOTE: The [`Cyclic`] reference is only valid to access once this function
/// returns and the value is written.
pub(in crate::map) fn cyclic_cell<'a, T>(
    bump: &'a Bump,
    f: impl FnOnce(Cyclic<'a, T, Incomplete>) -> T,
) -> CyclicCell<'a, T, Incomplete> {
    let cell_ref = cyclic_inner(bump, f);

    CyclicCell {
        cell_ref,
        state: PhantomData,
    }
}

/// Construct a value containing [`Cyclic`] references.
/// NOTE: The [`Cyclic`] reference is only valid to access once this function
/// returns and the value is written.
pub(in crate::map) fn cyclic<'a, T>(
    bump: &'a Bump,
    f: impl FnOnce(Cyclic<'a, T, Incomplete>) -> T,
) -> &'a T {
    let cell_ref = cyclic_inner(bump, f);

    // SAFETY: `UnsafeCell` is `repr(transparent)`.
    unsafe { std::mem::transmute::<&'a UnsafeCell<T>, &'a T>(cell_ref) }
}

fn cyclic_inner<'a, T>(
    bump: &'a Bump,
    f: impl FnOnce(Cyclic<'a, T, Incomplete>) -> T,
) -> &'a mut UnsafeCell<T> {
    let loc = bump.alloc(MaybeUninit::uninit());
    // SAFETY: loc is allocated inside the bump allocator, we just can't borrow
    // it, because we need to write to it later, after the value has been
    // constructed.
    let mut ptr = unsafe { Cyclic::new_uninit_ptr(NonNull::from_mut(loc)) };
    let val = f(ptr);

    // SAFETY: During construction the cyclic references are never read, only
    // upon construction completed they are used. Thus The mutable reference
    // created here is the only one to the location.
    let loc = unsafe { ptr.ptr.as_mut() };

    loc.write(UnsafeCell::new(val))
}

/// This reference type only allows accessing the inner pointer once its state
/// has been changed to [`Complete`].
#[repr(transparent)]
pub(in crate::map) struct Cyclic<'a, T, S> {
    ptr: NonNull<MaybeUninit<UnsafeCell<T>>>,
    lifetime: PhantomData<&'a T>,
    state: PhantomData<S>,
}

// SAFETY: Once construction is completed, the cell can't be mutated, and is
// essentially just a shared refernece.
unsafe impl<T: Send> Send for Cyclic<'_, T, Complete> {}
unsafe impl<T: Sync> Sync for Cyclic<'_, T, Complete> {}

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
    pub(in crate::map) fn new(reference: &'a T) -> Self {
        Self {
            ptr: NonNull::new(reference as *const T as *mut MaybeUninit<UnsafeCell<T>>).unwrap(),
            lifetime: PhantomData,
            state: PhantomData,
        }
    }

    pub(in crate::map) fn get(self) -> &'a T {
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
    unsafe fn new_uninit_ptr(ptr: NonNull<MaybeUninit<UnsafeCell<T>>>) -> Self {
        Self {
            ptr,
            lifetime: PhantomData,
            state: PhantomData,
        }
    }

    fn new_cell_ref(cell_ref: &'a UnsafeCell<T>) -> Self {
        Self {
            ptr: NonNull::new(cell_ref as *const _ as *mut MaybeUninit<UnsafeCell<T>>).unwrap(),
            lifetime: PhantomData,
            state: PhantomData,
        }
    }
}

/// This cell has different behavior depending on the state tag.
///
/// # [`Incomplete`]
/// During construction this mimics normal Rust borrowing behavior and only
/// gives out shared and unique/mutable references with the lifetime of the
/// struct itself, not the inner reference. This ensures the access patterns are
/// sound.
///
/// # [`Complete`]
/// After construction has been completed, only shared references are given out,
/// but now with the inner `'a` lifetime. This is safe, because all mutable
/// references are only used during construction and have been dropped.
#[derive(Debug)]
pub struct CyclicCell<'a, T, S = Complete> {
    /// The inner shared reference is required, because a mutable reference
    /// would invalidate all [`Cyclic`] references to the same location.
    cell_ref: &'a UnsafeCell<T>,
    state: PhantomData<S>,
}

// SAFETY: Once construction is completed, the cell can't be mutated, and is
// essentially just a shared refernece.
unsafe impl<T: Send> Send for CyclicCell<'_, T, Complete> {}
unsafe impl<T: Sync> Sync for CyclicCell<'_, T, Complete> {}

impl<T: PartialEq, S> PartialEq for CyclicCell<'_, T, S> {
    fn eq(&self, other: &Self) -> bool {
        // SAFETY: Because the public API in combination with the generic state
        // tag guarantees normal Rust lifetime semantics, it's safe to get
        // shared references to the contents of both cells, if references to the
        // cells exist.
        let a = unsafe { &*self.cell_ref.get() };
        let b = unsafe { &*other.cell_ref.get() };
        a == b
    }
}

impl<'a, T, S> CyclicCell<'a, T, S> {
    pub fn into_inner(self) -> &'a T {
        unsafe { &mut *self.cell_ref.get() }
    }
}

impl<'a, T> CyclicCell<'a, T, Incomplete> {
    pub(in crate::map) fn get(&self) -> &T {
        // SAFETY: Because the cell does not implement `Clone`, and the `get`
        // and `get_mut` methods require shared and mutable references
        // respectively, normal rust borrowing rules are enforced by the borrow
        // checker.
        unsafe { &*self.cell_ref.get() }
    }

    pub(in crate::map) fn get_mut(&mut self) -> &mut T {
        // SAFETY: Because the cell does not implement `Clone`, and the `get`
        // and `get_mut` methods require shared and mutable references
        // respectively, normal rust borrowing rules are enforced by the borrow
        // checker.
        unsafe { &mut *self.cell_ref.get() }
    }

    pub(in crate::map) fn cyclic_ref(&self) -> Cyclic<'a, T, Incomplete> {
        Cyclic::new_cell_ref(self.cell_ref)
    }
}

impl<'a, T> CyclicCell<'a, T, Complete> {
    pub fn get(&self) -> &'a T {
        // SAFETY: Mutable references are only used during construction of the
        // map, after that is finished, only shared references are given out.
        unsafe { &*self.cell_ref.get() }
    }
}

impl<'a, T> Deref for CyclicCell<'a, T, Complete> {
    type Target = T;

    fn deref(&self) -> &'a Self::Target {
        self.get()
    }
}

/// A cell that is [`Sync`] and [`Send`], by requiring manual synchronization
/// from the user.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct ManuallySyncCell<T: Copy>(Cell<Option<T>>);

// SAFETY: The user is responsible for synchronization.
unsafe impl<T: Send + Copy> Send for ManuallySyncCell<T> {}
unsafe impl<T: Sync + Copy> Sync for ManuallySyncCell<T> {}

impl<T: Copy> ManuallySyncCell<T> {
    pub const fn empty() -> Self {
        Self(Cell::new(None))
    }

    /// # Safety
    ///
    /// The caller is responsible for synchronization of the mutation. In
    /// general this is always safe, if the cell isn't shared between threads.
    pub(in crate::map) unsafe fn set(&self, val: T) {
        self.0.set(Some(val));
    }

    pub fn get(&self) -> Option<T>
    where
        T: Copy,
    {
        self.0.get()
    }
}
