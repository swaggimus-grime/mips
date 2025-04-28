use crate::psx::addressable::Addressable;
use crate::util::ds::box_slice::BoxSlice;

/// System RAM: 2MB
const RAM_SIZE: usize = 2 * 1024 * 1024;

pub struct Ram {
    data: BoxSlice<u8, RAM_SIZE>
}

impl Ram {
    pub fn new() -> Ram {
        Ram {
            data: BoxSlice::from_vec(vec![0; RAM_SIZE])
        }
    }
}
