pub mod bin {
    use std::fs::File;
    use std::io::Read;
    use std::path::Path;
    use crate::util::ds::box_slice::BoxSlice;
    use crate::error::Result;

    pub fn from_file<const U: usize>(path: &Path) -> Result<BoxSlice<u8, U>> {
        let mut file = File::open(path).unwrap();
        let mut bin = BoxSlice::from_vec(vec![0; U]);
        file.read_exact(&mut *bin).unwrap();
        Ok(bin)
    }
}