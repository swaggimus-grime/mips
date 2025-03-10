use std::fmt::Debug;
use std::ops;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BoxSlice<T: Debug, const N: usize> {
    slice: Box<[T; N]>,
}

impl<T: Debug, const N: usize> BoxSlice<T, N> {
    /// Creates a new instance of BoxArray from the given Vec. Panics if the length of the vector
    /// is not equal to `N`
    pub fn from_vec(v: Vec<T>) -> BoxSlice<T, N> {
        assert_eq!(
            v.len(),
            N,
            "Attempted to create a BoxArray from a Vec of the wrong size"
        );
        
        BoxSlice {
            slice: Box::try_from(v.into_boxed_slice()).unwrap()
        }
    }
}

impl<T: Debug, const N: usize> ops::Deref for BoxSlice<T, N> {
    type Target = [T; N];

    fn deref(&self) -> &Self::Target {
        &self.slice
    }
}

impl<T: Debug, const N: usize> ops::DerefMut for BoxSlice<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.slice
    }
}
