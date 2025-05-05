use std::{fmt, ops};
use std::marker::PhantomData;
use flexbuffers::FlexbufferSerializer;
use serde::de::{SeqAccess, Visitor};
use serde::{de, Deserialize, Deserializer, Serialize, Serializer};
use serde::ser::SerializeTuple;

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct BoxSlice<T, const N: usize> {
    array: Box<[T; N]>,
}

impl<T, const N: usize> BoxSlice<T, N> {
    pub(crate) fn serialize(&self, p0: &mut FlexbufferSerializer) {
        todo!()
    }
}

impl<T, const N: usize> BoxSlice<T, N> {
    /// Creates a new instance of BoxArray from the given Vec. Panics if the length of the vector
    /// is not equal to `N`
    pub fn from_vec(v: Vec<T>) -> BoxSlice<T, N> {
        assert_eq!(
            v.len(),
            N,
            "Attempted to create a BoxArray from a Vec of the wrong size"
        );

        let boxed_slice = v.into_boxed_slice();

        let ptr = ::std::boxed::Box::into_raw(boxed_slice) as *mut [T; N];
        let array = unsafe { Box::from_raw(ptr) };

        BoxSlice { array }
    }
}

impl<T, const N: usize> ops::Deref for BoxSlice<T, N> {
    type Target = [T; N];

    fn deref(&self) -> &Self::Target {
        &self.array
    }
}

impl<T, const N: usize> ops::DerefMut for BoxSlice<T, N> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.array
    }
}

impl<T: Serialize, const N: usize> Serialize for BoxSlice<T, N> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut tup = serializer.serialize_tuple(N)?;

        for v in self.array.iter() {
            tup.serialize_element(v)?;
        }

        tup.end()
    }
}

impl<'de, T, const N: usize> Deserialize<'de> for BoxSlice<T, N>
where
    T: Deserialize<'de> + Copy,
{
    fn deserialize<D>(deserializer: D) -> std::result::Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct BoxArrayVisitor<T, const N: usize> {
            phantom: PhantomData<[T; N]>,
        }

        impl<'de, T, const N: usize> Visitor<'de> for BoxArrayVisitor<T, N>
        where
            T: Deserialize<'de> + Copy,
        {
            type Value = BoxSlice<T, N>;

            fn expecting(&self, f: &mut fmt::Formatter) -> fmt::Result {
                write!(f, "an array of {} elements", N)
            }

            fn visit_seq<S>(self, mut seq: S) -> std::result::Result<BoxSlice<T, N>, S::Error>
            where
                S: SeqAccess<'de>,
            {
                let mut v = Vec::with_capacity(N);

                for i in 0..N {
                    match seq.next_element()? {
                        Some(e) => v.push(e),
                        None => return Err(de::Error::invalid_length(i, &self)),
                    }
                }

                Ok(BoxSlice::from_vec(v))
            }
        }

        let visitor = BoxArrayVisitor {
            phantom: PhantomData,
        };
        deserializer.deserialize_tuple(N, visitor)
    }
}

/*
#[test]
fn boxarray_serialize() {
    let mut ba: BoxSlice<u8, 512> = BoxSlice::from_vec(vec![0; 512]);

    for (i, b) in ba.iter_mut().enumerate() {
        let i = i as u8;

        *b = i ^ (i << 1);
    }

    let mut fb = flexbuffers::FlexbufferSerializer::new();

    ba.serialize(&mut fb);

    let fbr = flexbuffers::Reader::get_root(fb.view()).unwrap();

    let ba_out = BoxSlice::deserialize(fbr).unwrap();

    assert_eq!(ba, ba_out)
}
*/
