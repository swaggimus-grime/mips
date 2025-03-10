
/// PSX typically supports these access sizes
#[derive(PartialEq, Eq, Debug)]
pub enum AccessWidth {
    Byte = 1,
    HalfWord = 2,
    Word = 4,
}


pub trait Addressable {
    fn width() -> AccessWidth;
    fn size() -> usize where Self: Sized { size_of::<Self>() }
}

impl Addressable for u8 {
    fn width() -> AccessWidth {  AccessWidth::Byte }
}

impl Addressable for u16 {
    fn width() -> AccessWidth {  AccessWidth::HalfWord }
}

impl Addressable for u32 {
    fn width() -> AccessWidth {  AccessWidth::Word }
}