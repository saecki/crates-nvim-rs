/// A wrapper around `Vec` that is guaranteed to always contains at least one element.
pub struct OneVec<T> {
    inner: Vec<T>,
}

impl<T> OneVec<T> {
    pub fn new(elem: T) -> Self {
        Self { inner: vec![elem] }
    }

    /// Constructs [`Self`] from a non-empty [`Vec`].
    ///
    /// # Safety
    /// Calling this function with an empty [`Vec`] is undefined behavior.
    pub unsafe fn from_vec_unchecked(vec: Vec<T>) -> Self {
        Self { inner: vec }
    }

    #[inline]
    #[must_use]
    pub fn first(&self) -> &T {
        // SAFETY: inner should never be empty
        unsafe { self.inner.get_unchecked(0) }
    }

    #[inline]
    #[must_use]
    pub fn first_mut(&mut self) -> &mut T {
        // SAFETY: inner should never be empty
        unsafe { self.inner.get_unchecked_mut(0) }
    }

    #[inline]
    #[must_use]
    pub fn last(&self) -> &T {
        // SAFETY: inner should never be empty
        unsafe { self.inner.get_unchecked(self.inner.len() - 1) }
    }

    #[inline]
    #[must_use]
    pub fn last_mut(&mut self) -> &mut T {
        // SAFETY: inner should never be empty
        let idx = self.inner.len() - 1;
        unsafe { self.inner.get_unchecked_mut(idx) }
    }

    pub fn push(&mut self, val: T) {
        self.inner.push(val);
    }

    pub fn iter(&self) -> std::slice::Iter<T> {
        self.inner.iter()
    }
}

impl<T> IntoIterator for OneVec<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.into_iter()
    }
}

impl<T, I: std::slice::SliceIndex<[T]>> std::ops::Index<I> for OneVec<T> {
    type Output = I::Output;

    #[inline]
    fn index(&self, index: I) -> &Self::Output {
        std::ops::Index::index(&*self.inner, index)
    }
}

impl<T, I: std::slice::SliceIndex<[T]>> std::ops::IndexMut<I> for OneVec<T> {
    #[inline]
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        std::ops::IndexMut::index_mut(&mut *self.inner, index)
    }
}

impl<T: Clone> std::clone::Clone for OneVec<T> {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for OneVec<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

impl<T: PartialEq> PartialEq for OneVec<T> {
    fn eq(&self, other: &Self) -> bool {
        self.inner.eq(&other.inner)
    }
}

impl<T: Eq> Eq for OneVec<T> {}

#[macro_export]
macro_rules! onevec {
    ($($x:expr),+ $(,)?) => {
        // SAFETY: macro rules enforce at least one element
        unsafe {
            OneVec::from_vec_unchecked(
                Vec::from_iter([$($x),+])
            )
        }
    };
}
