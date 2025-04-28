use std::fs::DirEntry;
use std::path::{Path, PathBuf};
use log::Metadata;
use crate::error::*;
use crate::psx::bios::bios::BIOS_SIZE;
use crate::psx::cd::CDC_ROM_SIZE;

pub struct SysDir {
    root_dir: PathBuf,
}

impl SysDir {
    pub fn new(root_dir: &Path) -> SysDir {
        SysDir {
            root_dir: root_dir.to_path_buf(),
        }
    }
    
    pub fn search(&self, searchFor: SearchFor) -> MipsResult<PathBuf> {
        let assets_dir = self.root_dir.join("assets");
        let roms_dir = assets_dir.join("roms");
        let roms_path = roms_dir.as_path();
        let target_path = match searchFor {
            SearchFor::CdcFirmware => find(roms_path,|e| {
                let md = e.metadata().unwrap();
                return md.is_file() && md.len() == CDC_ROM_SIZE as u64;
            }),
            SearchFor::Bios => find(roms_path, |e| {
                let md = e.metadata().unwrap();
                return md.is_file() && md.len() == BIOS_SIZE as u64;
            }),
            SearchFor::Games => find(roms_path, |e| {
                let md = e.metadata().unwrap();
                return md.is_dir() && e.path().file_name().unwrap() == "games";
            }),
            SearchFor::Executables => find(assets_dir.as_path(), |e| {
                let md = e.metadata().unwrap();
                return md.is_dir() && e.path().file_name().unwrap() == "exe";
            }),
        };
        
        if let Some(path) = target_path {
            return Ok(path);
        }
        
        Err(MipsError::FileOrDirNotFound("Could not find file".to_string()))
    }
}

fn find<F>(path: &Path, valid_predicate: F) -> Option<PathBuf>
where
    F: Fn(&DirEntry) -> bool
{
    let dir = ::std::fs::read_dir(path).unwrap();
    for entry in dir {
        let entry = entry.unwrap();
        let path = entry.path();

        if valid_predicate(&entry) {
            return Some(path);
        }
    }
    None
}

pub enum SearchFor {
    Bios,
    CdcFirmware,
    Games,
    Executables,
}