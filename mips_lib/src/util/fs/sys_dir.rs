use std::path::{Path, PathBuf};
use crate::error::*;
use crate::psx::bios::bios::BIOS_SIZE;

pub struct SysDir {
    root_dir: PathBuf,
}

impl SysDir {
    pub fn new(root_dir: &Path) -> SysDir {
        SysDir {
            root_dir: root_dir.to_path_buf(),
        }
    }
    
    pub fn search(&self, searchFor: SearchFor) -> Result<PathBuf> {
        let mut root_dir = self.root_dir.clone();
        root_dir.push(match searchFor {
            SearchFor::Bios => "assets/roms"
        });
        let dir = ::std::fs::read_dir(root_dir).unwrap();
        
        for entry in dir {
            let entry = entry.unwrap();
            let path = entry.path();
            let md = entry.metadata().unwrap();
            if !md.is_file() {
                continue;
            }
            let valid = match searchFor {
                SearchFor::Bios => md.len() == BIOS_SIZE as u64
            };
            if valid {
                return Ok(path);
            }
        }
        Err(MipsError::FileNotFound("Could not find file".to_string()))
    }
}

pub enum SearchFor {
    Bios
}