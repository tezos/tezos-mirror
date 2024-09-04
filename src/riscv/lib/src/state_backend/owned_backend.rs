// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use super::{
    AllocatedOf, Elem, Layout, Location, ManagerAlloc, ManagerBase, ManagerDeserialise,
    ManagerRead, ManagerReadWrite, ManagerSerialise, ManagerWrite,
};
use serde::ser::SerializeTuple;
use std::{
    fmt,
    marker::PhantomData,
    mem::{self, MaybeUninit},
};

/// Manager that allows state binders to own the state storage
pub struct Owned;

impl Owned {
    /// Allocate regions for the given layout.
    pub fn allocate<L: Layout>() -> AllocatedOf<L, Self> {
        let places = L::placed();
        L::allocate(&mut Self, places.into_location())
    }
}

impl ManagerBase for Owned {
    type Region<E: Elem, const LEN: usize> = [E; LEN];

    type DynRegion<const LEN: usize> = Box<[u8; LEN]>;
}

impl ManagerAlloc for Owned {
    fn allocate_region<E: Elem, const LEN: usize>(
        &mut self,
        _loc: Location<[E; LEN]>,
    ) -> Self::Region<E, LEN> {
        unsafe { std::mem::zeroed() }
    }

    fn allocate_dyn_region<const LEN: usize>(
        &mut self,
        _loc: Location<[u8; LEN]>,
    ) -> Self::DynRegion<LEN> {
        unsafe {
            let layout = std::alloc::Layout::new::<[u8; LEN]>();
            let alloc = std::alloc::alloc_zeroed(layout);
            Box::from_raw(alloc.cast())
        }
    }
}

impl ManagerRead for Owned {
    fn region_read<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>, index: usize) -> E {
        region[index]
    }

    fn region_read_all<E: Elem, const LEN: usize>(region: &Self::Region<E, LEN>) -> Vec<E> {
        region.to_vec()
    }

    fn region_read_some<E: Elem, const LEN: usize>(
        region: &Self::Region<E, LEN>,
        offset: usize,
        buffer: &mut [E],
    ) {
        let slice = &region[offset..][..buffer.len()];
        buffer.copy_from_slice(slice)
    }

    fn dyn_region_read<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
    ) -> E {
        {
            assert!(address + mem::size_of::<E>() <= LEN);

            let mut result = unsafe { region.as_ptr().add(address).cast::<E>().read_unaligned() };
            result.from_stored_in_place();

            result
        }
    }

    fn dyn_region_read_all<E: Elem, const LEN: usize>(
        region: &Self::DynRegion<LEN>,
        address: usize,
        values: &mut [E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        unsafe {
            region
                .as_ptr()
                .add(address)
                .cast::<E>()
                .copy_to(values.as_mut_ptr(), values.len());
        }

        for elem in values.iter_mut() {
            elem.from_stored_in_place();
        }
    }
}

impl ManagerWrite for Owned {
    fn region_write<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) {
        region[index] = value;
    }

    fn region_write_all<E: Elem, const LEN: usize>(region: &mut Self::Region<E, LEN>, value: &[E]) {
        region.copy_from_slice(value)
    }

    fn region_write_some<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        buffer: &[E],
    ) {
        region[index..][..buffer.len()].copy_from_slice(buffer)
    }

    fn dyn_region_write<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        mut value: E,
    ) {
        assert!(address + mem::size_of_val(&value) <= LEN);

        value.to_stored_in_place();

        unsafe {
            region
                .as_mut_ptr()
                .add(address)
                .cast::<E>()
                .write_unaligned(value);
        }
    }

    fn dyn_region_write_all<E: Elem, const LEN: usize>(
        region: &mut Self::DynRegion<LEN>,
        address: usize,
        values: &[E],
    ) {
        assert!(address + mem::size_of_val(values) <= LEN);

        unsafe {
            let ptr = region.as_mut_ptr().add(address).cast::<E>();

            for (i, mut value) in values.iter().copied().enumerate() {
                value.to_stored_in_place();
                ptr.add(i).write_unaligned(value)
            }
        }
    }
}

impl ManagerReadWrite for Owned {
    fn region_replace<E: Elem, const LEN: usize>(
        region: &mut Self::Region<E, LEN>,
        index: usize,
        value: E,
    ) -> E {
        mem::replace(&mut region[index], value)
    }
}

impl ManagerSerialise for Owned {
    fn serialise_region<E: serde::Serialize + Elem, const LEN: usize, S: serde::Serializer>(
        region: &Self::Region<E, LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        // We're serialising this as a fixed-sized tuple because otherwise `bincode` would prefix
        // the length of this array, which is not needed.
        let mut serializer = serializer.serialize_tuple(LEN)?;

        for item in region.iter() {
            serializer.serialize_element(item)?;
        }

        serializer.end()
    }

    fn serialise_dyn_region<const LEN: usize, S: serde::Serializer>(
        region: &Self::DynRegion<LEN>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        serializer.serialize_bytes(region.as_slice())
    }
}

impl ManagerDeserialise for Owned {
    fn deserialise_region<
        'de,
        E: serde::de::Deserialize<'de> + Elem,
        const LEN: usize,
        D: serde::de::Deserializer<'de>,
    >(
        deserializer: D,
    ) -> Result<Self::Region<E, LEN>, D::Error> {
        struct Inner<E, const LEN: usize>(PhantomData<E>);

        impl<'de, E: serde::Deserialize<'de> + Sized, const LEN: usize> serde::de::Visitor<'de>
            for Inner<E, LEN>
        {
            type Value = [E; LEN];

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                write!(formatter, "{}", std::any::type_name::<Self::Value>())
            }

            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let mut values: [MaybeUninit<E>; LEN] =
                    std::array::from_fn(|_| MaybeUninit::uninit());

                for value in values.iter_mut() {
                    value.write(seq.next_element::<E>()?.ok_or_else(|| {
                        serde::de::Error::custom(format!(
                            "Not enough elements to construct {}",
                            std::any::type_name::<Self::Value>()
                        ))
                    })?);
                }

                // We can't use `std::mem::transmute` here because `[_; LEN]` does not have a fixed
                // size according to the compiler. I suspect this because `LEN` is a const generic
                // parameter.
                Ok(values.map(|value| {
                    // SAFETY: We've initialised all the elements in the array.
                    unsafe { value.assume_init() }
                }))
            }
        }

        deserializer.deserialize_tuple(LEN, Inner(PhantomData))
    }

    fn deserialise_dyn_region<'de, const LEN: usize, D: serde::de::Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Self::DynRegion<LEN>, D::Error> {
        let vec: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        vec.try_into()
            .map_err(|_err| serde::de::Error::custom("Dynamic region of mismatching length"))
    }
}

#[cfg(test)]
pub mod test_helpers {
    use super::*;
    use crate::state_backend::{Cell, Cells, DynCells};

    /// Ensure [`Cell`] can be serialised and deserialised in a consistent way.
    #[test]
    fn cell_serialise() {
        proptest::proptest!(|(value: u64)|{
            let cell: Cell<u64, Owned> = Cell::bind([value; 1]);
            let bytes = bincode::serialize(&cell).unwrap();

            let cell_after: Cell<u64, Owned> = bincode::deserialize(&bytes).unwrap();
            assert_eq!(cell.read(), cell_after.read());

            let bytes_after = bincode::serialize(&cell_after).unwrap();
            assert_eq!(bytes, bytes_after);
        });
    }

    /// Ensure [`Cells`] can be serialised and deserialised in a consistent way.
    #[test]
    fn cells_serialise() {
        proptest::proptest!(|(a: u64, b: u64, c: u64)|{
            let cell: Cells<u64, 3, Owned> = Cells::bind([a, b, c]);
            let bytes = bincode::serialize(&cell).unwrap();

            let cell_after: Cells<u64, 3, Owned> = bincode::deserialize(&bytes).unwrap();

            assert_eq!(cell.read_all(), cell_after.read_all());

            for i in 0..3 {
                assert_eq!(cell.read(i), cell_after.read(i));
            }

            let bytes_after = bincode::serialize(&cell_after).unwrap();
            assert_eq!(bytes, bytes_after);
        });
    }

    /// Ensure [`DynCells`] can be serialised and deserialised in a consistent way.
    #[test]
    fn dyn_cells_serialise() {
        proptest::proptest!(|(address in (0usize..120), value: u64)|{
            let mut cells: DynCells<128, Owned> = DynCells::bind(Box::new([0u8; 128]));
            cells.write(address, value);
            let bytes = bincode::serialize(&cells).unwrap();

            let cells_after: DynCells<128, Owned> = bincode::deserialize(&bytes).unwrap();
            for i in 0..128 {
                assert_eq!(cells.read::<u8>(i), cells_after.read::<u8>(i));
            }

            let bytes_after = bincode::serialize(&cells_after).unwrap();
            assert_eq!(bytes, bytes_after);
        });
    }
}
