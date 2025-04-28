use std::any::type_name;

/// Types of access supported by the PlayStation architecture
#[derive(PartialEq, Eq, Debug)]
pub enum AccessWidth {
    Byte = 1,
    HalfWord = 2,
    Word = 4,
}

/// rait representing the attributes of a primitive addressable
/// memory location.
pub trait Addressable {
    /// Retrieve the width of the access
    fn width() -> AccessWidth;
    /// Build an Addressable value from an u32. If the Addressable is 8 or 16bits wide the MSBs are
    /// discarded to fit.
    fn from_u32(i: u32) -> Self;
    /// Retrieve the value of the Addressable as an u32. If the Addressable is 8 or 16bits wide the
    /// MSBs are padded with 0s.
    fn as_u32(&self) -> u32;
    /// Retrieve the value of the Addressable as an u16. If the Addressable was 8 bit wide the MSBs
    /// are padded with 0s, if it was 32bit wide the MSBs are truncated.
    fn as_u16(&self) -> u16 {
        self.as_u32() as u16
    }
    /// Retrieve the value of the Addressable as an u8. If the Addressable was 16 or 32bit wide the
    /// MSBs are truncated.
    fn as_u8(&self) -> u8 {
        self.as_u32() as u8
    }
}

impl Addressable for u8 {
    fn width() -> AccessWidth {
        AccessWidth::Byte
    }

    fn from_u32(v: u32) -> u8 {
        v as u8
    }

    fn as_u32(&self) -> u32 {
        u32::from(*self)
    }
}

impl Addressable for u16 {
    fn width() -> AccessWidth {
        AccessWidth::HalfWord
    }

    fn from_u32(v: u32) -> u16 {
        v as u16
    }

    fn as_u32(&self) -> u32 {
        u32::from(*self)
    }
}

impl Addressable for u32 {
    fn width() -> AccessWidth {
        AccessWidth::Word
    }

    fn from_u32(v: u32) -> u32 {
        v
    }

    fn as_u32(&self) -> u32 {
        *self
    }
}
