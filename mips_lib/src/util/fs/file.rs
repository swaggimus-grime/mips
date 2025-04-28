pub mod bin {
    use std::fs::File;
    use std::io;
    use std::io::Read;
    use std::path::Path;
    use crate::util::ds::box_slice::BoxSlice;
    use crate::error::{MipsError, MipsResult};

    pub fn from_file<const U: usize>(path: &Path) -> MipsResult<BoxSlice<u8, U>> {
        let mut file = File::open(path).unwrap();
        let mut bin = BoxSlice::from_vec(vec![0; U]);
        file.read_exact(&mut *bin).unwrap();
        Ok(bin)
    }

    pub fn slice_from_file<const U: usize>(path: &Path) -> MipsResult<[u8; U]> {
        let mut file = File::open(path).unwrap();
        let mut bin = [0; U];
        file.read_exact(&mut bin).unwrap();
        Ok(bin)
    }
    
    pub fn get_file(path: &Path) -> MipsResult<File> {
        match File::open(path) {
            Ok(f) => Ok(f),
            Err(e) => Err(MipsError::FileOrDirNotFound(path.display().to_string())),
        }
    }
}