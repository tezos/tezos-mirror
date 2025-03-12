// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::ops::Range;

/// Like [`Default`] but uses an associated constant instead of a function to produce the default
/// value.
pub trait ConstDefault {
    /// Default value
    const DEFAULT: Self;
}

impl ConstDefault for () {
    const DEFAULT: Self = ();
}

impl ConstDefault for bool {
    const DEFAULT: Self = false;
}

impl<T: ConstDefault, const LEN: usize> ConstDefault for [T; LEN] {
    const DEFAULT: Self = [T::DEFAULT; LEN];
}

impl<T: ConstDefault> ConstDefault for Range<T> {
    const DEFAULT: Self = T::DEFAULT..T::DEFAULT;
}

macro_rules! impl_const_default_int {
    ( $t:ty ) => {
        impl ConstDefault for $t {
            const DEFAULT: Self = 0;
        }
    };
}

impl_const_default_int!(u8);
impl_const_default_int!(u16);
impl_const_default_int!(u32);
impl_const_default_int!(u64);

impl_const_default_int!(i8);
impl_const_default_int!(i16);
impl_const_default_int!(i32);
impl_const_default_int!(i64);
