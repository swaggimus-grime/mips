//! BIOS database, lifted from mednafen

use std::fmt;

use crate::assembler::syntax::*;
use crate::assembler::Assembler;
use crate::psx::cd::disc::Region;
use crate::sha::sha256;

use super::{Bios, BIOS_SIZE};

pub struct Metadata {
    pub sha256: [u8; 32],
    pub version_major: u8,
    pub version_minor: u8,
    pub region: Region,
    /// True if this dump is known to be bad
    pub known_bad: bool,
    /// ROM offset where the jump to the bootup logo animation code can be found. Replacing the
    /// word there with `0` (NOP). That's also where we can hook ourselves up to run extension code
    /// after the BIOS init code and before the CD gets loaded (this is what mednafen does).
    ///
    /// This value can be set to `None` if the correct address has not been determined for this
    /// particular BIOS.
    #[allow(dead_code)]
    pub animation_jump_hook: Option<u32>,
    /// Method used to patch the BIOS to enable the debug UART or `None` if the method hasn't been
    /// found.
    #[allow(dead_code)]
    pub patch_debug_uart: Option<fn(&mut Bios)>,
}

impl fmt::Debug for Metadata {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{:?}/v{}.{}",
            self.region, self.version_major, self.version_minor
        )?;

        if self.known_bad {
            write!(f, "[BAD]")?
        }

        Ok(())
    }
}

/// Attempt to find the metadata for the given BIOS binary blob.  Returns None if this BIOS is not
/// part of the database.
pub fn lookup_blob(binary: &[u8; BIOS_SIZE]) -> Option<&'static Metadata> {
    lookup_sha256(sha256(binary))
}

/// Attempt to find the metadata for the given BIOS SHA-256 hash. Returns None if this BIOS is not
/// part of the database.
pub fn lookup_sha256(sha256: [u8; 32]) -> Option<&'static Metadata> {
    DATABASE.iter().find(|md| md.sha256 == sha256)
}

pub static DATABASE: [Metadata; 24] = [
    Metadata {
        sha256: [
            0xcf, 0xc1, 0xfc, 0x38, 0xeb, 0x44, 0x2f, 0x6f, 0x80, 0x78, 0x14, 0x52, 0x11, 0x9e,
            0x93, 0x1b, 0xca, 0xe2, 0x81, 0x00, 0xc1, 0xc9, 0x7e, 0x7e, 0x6c, 0x5f, 0x27, 0x25,
            0xbb, 0xb0, 0xf8, 0xbb,
        ],
        version_major: 1,
        version_minor: 0,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x5e, 0xb3, 0xae, 0xe4, 0x95, 0x93, 0x75, 0x58, 0x31, 0x2b, 0x83, 0xb5, 0x43, 0x23,
            0xd7, 0x6a, 0x4a, 0x01, 0x51, 0x90, 0xde, 0xcd, 0x40, 0x51, 0x21, 0x4f, 0x1b, 0x6d,
            0xf0, 0x6a, 0xc3, 0x4b,
        ],
        version_major: 1,
        version_minor: 1,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x42, 0xe4, 0x12, 0x4b, 0xe7, 0x62, 0x3e, 0x2e, 0x28, 0xb1, 0xdb, 0x0d, 0x8d, 0x42,
            0x65, 0x39, 0x64, 0x6f, 0xae, 0xe4, 0x9d, 0x74, 0xb7, 0x11, 0x66, 0xd8, 0xba, 0x5b,
            0xd7, 0xc4, 0x72, 0xed,
        ],
        version_major: 2,
        version_minor: 0,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x0a, 0xf2, 0xbe, 0x34, 0x68, 0xd3, 0x0b, 0x60, 0x18, 0xb3, 0xc3, 0xb0, 0xd9, 0x8b,
            0x8b, 0x64, 0x34, 0x7e, 0x25, 0x5e, 0x16, 0xd8, 0x74, 0xd5, 0x5f, 0x03, 0x63, 0x64,
            0x89, 0x73, 0xdb, 0xf0,
        ],
        version_major: 2,
        version_minor: 0,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x6f, 0x71, 0xca, 0x1e, 0x71, 0x6d, 0xa7, 0x61, 0xdc, 0x53, 0x18, 0x7b, 0xd3, 0x9e,
            0x00, 0xc2, 0x13, 0xf5, 0x66, 0xe5, 0x50, 0x90, 0x70, 0x8f, 0xd3, 0xe2, 0xb4, 0xb4,
            0x25, 0xc8, 0xc9, 0x89,
        ],
        version_major: 2,
        version_minor: 1,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x6a, 0xd5, 0x52, 0x1d, 0x10, 0x5a, 0x6b, 0x86, 0x74, 0x1f, 0x1a, 0xf8, 0xda, 0x2e,
            0x6e, 0xa1, 0xc7, 0x32, 0xd3, 0x44, 0x59, 0x94, 0x06, 0x18, 0xc7, 0x03, 0x05, 0xa1,
            0x05, 0xe8, 0xec, 0x10,
        ],
        version_major: 2,
        version_minor: 1,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x1e, 0xfb, 0x0c, 0xfc, 0x5d, 0xb8, 0xa8, 0x75, 0x1a, 0x88, 0x4c, 0x53, 0x12, 0xe9,
            0xc6, 0x26, 0x5c, 0xa1, 0xbc, 0x58, 0x0d, 0xc0, 0xc2, 0x66, 0x3e, 0xb2, 0xde, 0xa3,
            0xbd, 0xe9, 0xfc, 0xf7,
        ],
        version_major: 2,
        version_minor: 1,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x0c, 0x83, 0x59, 0x87, 0x0c, 0xba, 0xc0, 0xea, 0x09, 0x1f, 0x1c, 0x87, 0xf1, 0x88,
            0xcd, 0x33, 0x2d, 0xcc, 0x70, 0x97, 0x53, 0xb9, 0x1c, 0xaf, 0xd9, 0xfd, 0x44, 0xa4,
            0xa6, 0x18, 0x81, 0x97,
        ],
        version_major: 2,
        version_minor: 2,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x8e, 0x03, 0x83, 0x17, 0x1e, 0x67, 0xb3, 0x3e, 0x60, 0xd5, 0xdf, 0x63, 0x94, 0xc5,
            0x88, 0x43, 0xf3, 0xb1, 0x1c, 0x7a, 0x0b, 0x97, 0xf3, 0xbf, 0xcc, 0x43, 0x19, 0xac,
            0x2d, 0x1f, 0x9d, 0x18,
        ],
        version_major: 2,
        version_minor: 2,
        region: Region::Japan,
        known_bad: true,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x71, 0xaf, 0x94, 0xd1, 0xe4, 0x7a, 0x68, 0xc1, 0x1e, 0x8f, 0xdb, 0x9f, 0x83, 0x68,
            0x04, 0x06, 0x01, 0x51, 0x4a, 0x42, 0xa5, 0xa3, 0x99, 0xcd, 0xa4, 0x8c, 0x7d, 0x3b,
            0xff, 0x1e, 0x99, 0xd3,
        ],
        version_major: 2,
        version_minor: 2,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x3d, 0x06, 0xd2, 0xc4, 0x69, 0x31, 0x3c, 0x2a, 0x21, 0x28, 0xd2, 0x4f, 0xe2, 0xe0,
            0xc7, 0x1f, 0xf9, 0x9b, 0xc2, 0x03, 0x2b, 0xe8, 0x9a, 0x82, 0x9a, 0x62, 0x33, 0x71,
            0x87, 0xf5, 0x00, 0xb7,
        ],
        version_major: 2,
        version_minor: 2,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x40, 0x18, 0x74, 0x9b, 0x36, 0x98, 0xb8, 0x69, 0x43, 0x87, 0xbe, 0xeb, 0xcb, 0xab,
            0xfb, 0x48, 0x47, 0x05, 0x13, 0x06, 0x68, 0x40, 0xf9, 0x44, 0x14, 0x59, 0xee, 0x4c,
            0x9f, 0x0f, 0x39, 0xbc,
        ],
        version_major: 2,
        version_minor: 2,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x9c, 0x04, 0x21, 0x85, 0x8e, 0x21, 0x78, 0x05, 0xf4, 0xab, 0xe1, 0x86, 0x98, 0xaf,
            0xea, 0x8d, 0x5a, 0xa3, 0x6f, 0xf0, 0x72, 0x7e, 0xb8, 0x48, 0x49, 0x44, 0xe0, 0x0e,
            0xb5, 0xe7, 0xea, 0xdb,
        ],
        version_major: 3,
        version_minor: 0,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x11, 0x05, 0x2b, 0x64, 0x99, 0xe4, 0x66, 0xbb, 0xf0, 0xa7, 0x09, 0xb1, 0xf9, 0xcb,
            0x68, 0x34, 0xa9, 0x41, 0x8e, 0x66, 0x68, 0x03, 0x87, 0x91, 0x24, 0x51, 0xe9, 0x71,
            0xcf, 0x8a, 0x1f, 0xef,
        ],
        version_major: 3,
        version_minor: 0,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: Some(0x6990),
        patch_debug_uart: Some(patch_debug_uart_na_30),
    },
    Metadata {
        sha256: [
            0x1f, 0xaa, 0xa1, 0x8f, 0xa8, 0x20, 0xa0, 0x22, 0x5e, 0x48, 0x8d, 0x9f, 0x08, 0x62,
            0x96, 0xb8, 0xe6, 0xc4, 0x6d, 0xf7, 0x39, 0x66, 0x60, 0x93, 0x98, 0x7f, 0xf7, 0xd8,
            0xfd, 0x35, 0x2c, 0x09,
        ],
        version_major: 3,
        version_minor: 0,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x9e, 0x1f, 0x8f, 0xb4, 0xfa, 0x35, 0x6a, 0x5a, 0xc2, 0x9d, 0x7c, 0x72, 0x09, 0x62,
            0x6d, 0xcc, 0x1b, 0x30, 0x38, 0xc0, 0xe5, 0xa8, 0x5b, 0x0e, 0x99, 0xd1, 0xdb, 0x96,
            0x92, 0x66, 0x47, 0xca,
        ],
        version_major: 3,
        version_minor: 0,
        region: Region::Europe,
        known_bad: true,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0xe9, 0x00, 0x50, 0x4d, 0x17, 0x55, 0xf0, 0x21, 0xf8, 0x61, 0xb8, 0x2c, 0x82, 0x58,
            0xc5, 0xe6, 0x65, 0x8c, 0x7b, 0x59, 0x2f, 0x80, 0x0c, 0xcc, 0xd9, 0x1f, 0x5d, 0x32,
            0xea, 0x38, 0x0d, 0x28,
        ],
        version_major: 4,
        version_minor: 0,
        region: Region::Japan,
        known_bad: false,
        // Same patch as NA/3.0
        animation_jump_hook: Some(0x6990),
        patch_debug_uart: Some(patch_debug_uart_na_30),
    },
    Metadata {
        sha256: [
            0xb3, 0xaa, 0x63, 0xcf, 0x30, 0xc8, 0x1e, 0x0a, 0x40, 0x64, 0x17, 0x40, 0xf4, 0xa4,
            0x3e, 0x25, 0xfd, 0xa0, 0xb2, 0x1b, 0x79, 0x2f, 0xa9, 0xaa, 0xef, 0x60, 0xce, 0x16,
            0x75, 0x76, 0x14, 0x79,
        ],
        version_major: 4,
        version_minor: 1,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x39, 0xdc, 0xc1, 0xa0, 0x71, 0x70, 0x36, 0xc9, 0xb6, 0xac, 0x52, 0xfe, 0xfd, 0x1e,
            0xe7, 0xa5, 0x7d, 0x38, 0x08, 0xe8, 0xcf, 0xbc, 0x75, 0x58, 0x79, 0xfa, 0x68, 0x5a,
            0x0a, 0x73, 0x82, 0x78,
        ],
        version_major: 4,
        version_minor: 1,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x5e, 0x84, 0xa9, 0x48, 0x18, 0xcf, 0x52, 0x82, 0xf4, 0x21, 0x75, 0x91, 0xfe, 0xfd,
            0x88, 0xbe, 0x36, 0xb9, 0xb1, 0x74, 0xb3, 0xcc, 0x7c, 0xb0, 0xbc, 0xd7, 0x51, 0x99,
            0xbe, 0xb4, 0x50, 0xf1,
        ],
        version_major: 4,
        version_minor: 1,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0xb2, 0x9b, 0x4b, 0x5f, 0xcd, 0xde, 0xf3, 0x69, 0xbd, 0x66, 0x40, 0xac, 0xac, 0xda,
            0x08, 0x65, 0xe0, 0x36, 0x6f, 0xcf, 0x7e, 0xa5, 0x4e, 0x40, 0xb2, 0xf1, 0xa8, 0x17,
            0x80, 0x04, 0xf8, 0x9a,
        ],
        version_major: 4,
        version_minor: 3,
        region: Region::Japan,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x5c, 0x01, 0x66, 0xda, 0x24, 0xe2, 0x7d, 0xea, 0xa8, 0x22, 0x46, 0xde, 0x8f, 0xf0,
            0x10, 0x82, 0x67, 0xfe, 0x4b, 0xb5, 0x9f, 0x6d, 0xf0, 0xfd, 0xec, 0x50, 0xe0, 0x5e,
            0x62, 0x44, 0x8c, 0xa4,
        ],
        version_major: 4,
        version_minor: 4,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0xac, 0xa9, 0xcb, 0xfa, 0x97, 0x4b, 0x93, 0x36, 0x46, 0xba, 0xad, 0x65, 0x56, 0xa8,
            0x67, 0xec, 0xa9, 0xb8, 0x1c, 0xe6, 0x5d, 0x8a, 0xf3, 0x43, 0xa7, 0x84, 0x3f, 0x77,
            0x75, 0xb9, 0xff, 0xc8,
        ],
        version_major: 4,
        version_minor: 5,
        region: Region::NorthAmerica,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
    Metadata {
        sha256: [
            0x42, 0x24, 0x4b, 0x0c, 0x65, 0x08, 0x21, 0x51, 0x97, 0x51, 0xb7, 0xe7, 0x7a, 0xd1,
            0xd3, 0x22, 0x2a, 0x01, 0x25, 0xe7, 0x55, 0x86, 0xdf, 0x2b, 0x4e, 0x84, 0xba, 0x69,
            0x3b, 0x98, 0x09, 0xdc,
        ],
        version_major: 4,
        version_minor: 5,
        region: Region::Europe,
        known_bad: false,
        animation_jump_hook: None,
        patch_debug_uart: None,
    },
];

fn patch_debug_uart_na_30(bios: &mut Bios) {
    // At offset 0x6f0c the BIOS does:
    //
    //     lui     $at,0xa001
    //     jal     0xbfc06784
    //     sw      zero,-18000($at)
    //
    // In other wordss it stores 0 at 0xa000b9b0 which disables the
    // UART. We need to change this value to 1 to enable
    // it. Fortunately we can use $gp as SW base address since its
    // value at this point is equal to 0xa0010ff0.
    //
    // Credit to the No$ spec for documenting this hack.

    let mut asm = Assembler::from_base(0);

    asm.assemble(&[
        Li(AT, 1),
        Jal(Label::Absolute(0xbfc0_6784)),
        Sw(AT, GP, -18000 - 0xff0),
    ])
    .unwrap();

    let (mc, _) = asm.machine_code();

    assert!(mc.len() == 12);

    for (i, &b) in mc.iter().enumerate() {
        bios.rom[0x6f0c + i] = b;
    }
}
