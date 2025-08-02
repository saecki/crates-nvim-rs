//! Very similar to the `semver` crate's identifier.rs.
//!
//! (for 64-bit architectures)
//! Inline strings can be identified by the MSB being zero:
//! `0xxxxxxx 0xxxxxxx 0xxxxxxx 0xxxxxxx 0xxxxxxx 0xxxxxxx 0xxxxxxx 0xxxxxxx`
//!
//! Empty strings are represented as:
//! `10000000 00000000 00000000 00000000 00000000 00000000 00000000 00000000`
//!
//! And an allocated string is represented as the following:
//! `1ppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp pppppppp`
//!
//! The pointer is

use std::alloc::Layout;
use std::ptr::NonNull;

#[cfg(test)]
mod test;

const PTR_SIZE: usize = std::mem::size_of::<NonNull<u8>>();
const MIN_ALLOC_STR_LEN: usize = PTR_SIZE + 1;
const MAX_STR_LEN: usize = isize::MAX as usize - PTR_SIZE;
const EMPTY: usize = 1_usize.rotate_right(1);

/// A string that only contains `[0-9a-zA-Z-]` characters.
#[derive(Debug)]
pub struct InlineStr {
    repr: NonNull<u8>,
}

unsafe impl Sync for InlineStr {}
unsafe impl Send for InlineStr {}

impl InlineStr {
    pub const fn empty() -> Self {
        let repr = unsafe { NonNull::new_unchecked(EMPTY as *mut u8) };
        Self { repr }
    }

    /// SAFETY: `str` must be ASCII and not contain nul bytes
    pub unsafe fn new_unchecked(str: &str) -> Self {
        let len = str.len();
        match len {
            0 => Self::empty(),
            1..=PTR_SIZE => {
                let mut repr: usize = 0;

                // SAFETY: usize is of size `PTR_SIZE` and the string is at most `PTR_SIZE` bytes long.
                let repr_ptr = ((&mut repr) as *mut usize) as *mut u8;
                unsafe { std::ptr::copy_nonoverlapping(str.as_ptr(), repr_ptr, len) };

                // SAFETY: the string is not empty and must not contain nul bytes, so repr can't be
                // null.
                let repr = unsafe { NonNull::new_unchecked(repr as *mut u8) };
                Self { repr }
            }
            MIN_ALLOC_STR_LEN..=MAX_STR_LEN => {
                let alloc_size = PTR_SIZE + len;

                // SAFETY: align is always 2 (not 0 and a power of 2) and alloc_size doesn't
                // overflow isize::MAX.
                let layout = unsafe { Layout::from_size_align_unchecked(alloc_size, 2) };
                let ptr = unsafe { std::alloc::alloc(layout) };
                if ptr.is_null() {
                    std::alloc::handle_alloc_error(layout);
                }

                // SAFETY: the allocation is always at least `2 * PTR_SIZE` bytes large, the first
                // `PTR_SIZE` bytes being the length field, and the remaining bytes are the
                // allocated string since strings shorter than `PTR_SIZE + 1` are stored inline.
                unsafe { std::ptr::write_unaligned(ptr as *mut usize, len) };
                let str_start = unsafe { ptr.add(PTR_SIZE) };
                unsafe { std::ptr::copy_nonoverlapping(str.as_ptr(), str_start, len) };

                let repr = ptr_to_repr(ptr);
                Self { repr }
            }
            _ => unreachable!("string too large"),
        }
    }

    pub fn is_inline(&self) -> bool {
        self.repr.as_ptr() as usize >> (8 * PTR_SIZE - 1) == 0
    }

    pub fn is_empty(&self) -> bool {
        self.repr.as_ptr() as usize == EMPTY
    }

    pub fn as_str(&self) -> &str {
        if self.is_empty() {
            ""
        } else if self.is_inline() {
            #[cfg(target_endian = "little")]
            let bits = (self.repr.as_ptr() as usize).leading_zeros();
            #[cfg(target_endian = "big")]
            let bits = (self.repr.as_ptr() as usize).trailing_zeros();

            let len = PTR_SIZE - bits as usize / 8;

            let bytes: &[u8; PTR_SIZE] = unsafe { std::mem::transmute(&self.repr) };
            let slice = &bytes[..len];

            // SAFETY: the bytes are all ascii
            unsafe { std::str::from_utf8_unchecked(slice) }
        } else {
            ptr_as_str(&self.repr)
        }
    }
}

impl Clone for InlineStr {
    fn clone(&self) -> Self {
        if self.is_inline() || self.is_empty() {
            Self { repr: self.repr }
        } else {
            let ptr = repr_to_ptr(self.repr);

            // SAFETY: the len field is always stored as the first field inside the allocation
            let len = unsafe { std::ptr::read_unaligned(ptr as *const usize) };

            let alloc_size = PTR_SIZE + len;
            // SAFETY: align is always 2 (not 0 and a power of 2) and alloc_size doesn't
            // overflow isize::MAX.
            let layout = unsafe { Layout::from_size_align_unchecked(alloc_size, 2) };

            let clone = unsafe { std::alloc::alloc(layout) };
            if clone.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            unsafe { std::ptr::copy_nonoverlapping(ptr, clone, alloc_size) };

            let repr = ptr_to_repr(clone);
            Self { repr }
        }
    }
}

impl Drop for InlineStr {
    fn drop(&mut self) {
        if self.is_empty() || self.is_inline() {
            return;
        }

        let ptr = repr_to_ptr(self.repr);

        // SAFETY: the len field is always stored as the first field inside the allocation
        let len = unsafe { std::ptr::read_unaligned(ptr as *const usize) };

        let alloc_size = PTR_SIZE + len;
        // SAFETY: align is always 2 (not 0 and a power of 2) and alloc_size doesn't
        // overflow isize::MAX.
        let layout = unsafe { Layout::from_size_align_unchecked(alloc_size, 2) };

        unsafe { std::alloc::dealloc(ptr, layout) };
    }
}

impl Eq for InlineStr {}

impl PartialEq for InlineStr {
    fn eq(&self, other: &Self) -> bool {
        if self.is_empty() || self.is_inline() {
            self.repr == other.repr
        } else if other.is_empty() || other.is_inline() {
            false
        } else {
            ptr_as_str(&self.repr) == ptr_as_str(&other.repr)
        }
    }
}

fn repr_to_ptr(repr: NonNull<u8>) -> *mut u8 {
    let rotated = repr.as_ptr() as usize;
    let original = rotated << 1;

    // provenance stuff
    let diff = original.wrapping_sub(rotated);
    rotated.wrapping_add(diff) as *mut u8
}

fn ptr_to_repr(ptr: *mut u8) -> NonNull<u8> {
    let original = ptr as usize;
    let rotated = (original | 1).rotate_right(1);

    // provenance stuff
    let diff = rotated.wrapping_sub(original);
    let repr = original.wrapping_add(diff) as *mut u8;

    // SAFETY: repr can't be null because 1 is rotated into the MSB
    unsafe { NonNull::new_unchecked(repr) }
}

fn ptr_as_str(repr: &NonNull<u8>) -> &str {
    let ptr = repr_to_ptr(*repr);

    // SAFETY: the allocation is always at least `2 * PTR_SIZE` bytes large, the first
    // `PTR_SIZE` bytes being the length field, and the remaining bytes are the
    // allocated string since strings shorter than `PTR_SIZE + 1` are stored inline.
    let len = unsafe { std::ptr::read_unaligned(ptr as *const usize) };
    let str_start = unsafe { ptr.add(PTR_SIZE) };
    let slice = unsafe { std::slice::from_raw_parts(str_start, len) };

    // SAFETY: the bytes are all ascii
    unsafe { std::str::from_utf8_unchecked(slice) }
}
