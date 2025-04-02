// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

/// Return a boxed array without first allocating it on the stack
macro_rules! boxed_array {
    ($elem:expr; $n:expr) => {
        Box::try_from(vec![$elem; $n])
            .map_err(|_| {
                unreachable!("Converting a vector to an array of the same size always succeeds")
            })
            .unwrap()
    };
}

pub(crate) use boxed_array;

/// Create a boxed array from a function.
pub fn boxed_from_fn<T, const LEN: usize>(mut f: impl FnMut() -> T) -> Box<[T; LEN]> {
    let mut entries = Vec::with_capacity(LEN);
    entries.resize_with(LEN, &mut f);
    entries
        .into_boxed_slice()
        .try_into()
        .map_err(|_| unreachable!("Converting vec into boxed slice of same length always succeeds"))
        .unwrap()
}
