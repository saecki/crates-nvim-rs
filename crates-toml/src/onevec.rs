/// A wrapper around `Vec` that is guaranteed to always contains at least one element.
pub struct OneVec<T> {
    inner: Vec<T>,
}

impl<T> OneVec<T> {
    pub fn new(elem: T) -> Self {
        Self { inner: vec![elem] }
    }

    pub unsafe fn from_vec_unchecked(vec: Vec<T>) -> Self {
        Self { inner: vec }
    }

    pub fn first(&self) -> &T {
        self.inner.first().expect("at least one element")
    }

    pub fn first_mut(&mut self) -> &mut T {
        self.inner.first_mut().expect("at least one element")
    }

    pub fn last(&self) -> &T {
        self.inner.last().expect("at least one element")
    }

    pub fn last_mut(&mut self) -> &mut T {
        self.inner.last_mut().expect("at least one element")
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

impl<T> std::ops::Index<usize> for OneVec<T> {
    type Output = T;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl<T> std::ops::IndexMut<usize> for OneVec<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.inner[index]
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
         unsafe {
             OneVec::from_vec_unchecked(
                 Vec::from_iter([$($x),+])
             )
         }
    };
}
