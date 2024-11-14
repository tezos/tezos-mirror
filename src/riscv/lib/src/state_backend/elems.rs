// SPDX-FileCopyrightText: 2023-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Elements that may be stored using a [Backend]
pub trait Elem: Copy + 'static {
    /// Copy from `source` and convert to stored representation.
    fn store(&mut self, source: &Self);

    /// Convert to stored representation in place.
    fn to_stored_in_place(&mut self);

    /// Convert from stored representation in place.
    // The naming of this function trips Clippy.
    #[allow(clippy::wrong_self_convention)]
    fn from_stored_in_place(&mut self);

    /// Read a value from its stored representation.
    fn from_stored(source: &Self) -> Self;
}

macro_rules! impl_elem_prim {
    ( $x:ty ) => {
        impl Elem for $x {
            #[inline(always)]
            fn store(&mut self, source: &Self) {
                *self = source.to_le();
            }

            #[inline(always)]
            fn to_stored_in_place(&mut self) {
                *self = self.to_le();
            }

            #[inline(always)]
            fn from_stored_in_place(&mut self) {
                *self = Self::from_le(*self);
            }

            #[inline(always)]
            fn from_stored(source: &Self) -> Self {
                Self::from_le(*source)
            }
        }
    };
}

impl_elem_prim!(u8);
impl_elem_prim!(i8);
impl_elem_prim!(u16);
impl_elem_prim!(i16);
impl_elem_prim!(u32);
impl_elem_prim!(i32);
impl_elem_prim!(u64);
impl_elem_prim!(i64);
impl_elem_prim!(u128);
impl_elem_prim!(i128);

impl Elem for bool {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        *self = *source;
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {}

    #[inline(always)]
    fn from_stored_in_place(&mut self) {}

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        *source
    }
}

impl<E: Elem, const LEN: usize> Elem for [E; LEN] {
    #[inline(always)]
    fn store(&mut self, source: &Self) {
        self.copy_from_slice(source);

        // NOTE: This loop may be eliminated if [to_stored_in_place] is a no-op.
        for elem in self {
            elem.to_stored_in_place();
        }
    }

    #[inline(always)]
    fn to_stored_in_place(&mut self) {
        // NOTE: This loop may be eliminated if [to_stored_in_place] is a no-op.
        for elem in self {
            elem.to_stored_in_place();
        }
    }

    #[inline(always)]
    fn from_stored_in_place(&mut self) {
        // NOTE: This loop may be eliminated if [from_stored_in_place] is a no-op.
        for elem in self {
            elem.from_stored_in_place();
        }
    }

    #[inline(always)]
    fn from_stored(source: &Self) -> Self {
        let mut new = *source;

        // NOTE: This loop may be eliminated if [from_stored_in_place] is a no-op.
        for elem in new.iter_mut() {
            elem.from_stored_in_place();
        }

        new
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_bool_repr() {
        proptest::proptest!(|(val: u8)| {
            let bool_val: bool = unsafe {std::mem::transmute_copy(&val)};
            let truthy = val & 1 == 1;
            assert_eq!(bool_val, truthy);
        });
    }
}
