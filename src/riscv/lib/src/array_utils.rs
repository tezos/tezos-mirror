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
