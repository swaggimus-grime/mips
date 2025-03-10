use crate::psx::addressable::Addressable;

pub trait Loadable<T: Addressable> {
    fn load<const N: usize>(&self, offset: usize) -> [T; N];
}