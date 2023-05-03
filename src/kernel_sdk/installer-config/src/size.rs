// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
use tezos_smart_rollup_host::path::PATH_MAX_SIZE;

use crate::instr::{ConfigInstruction, MoveInstruction, RawBytes, RevealInstruction};

// https://stackoverflow.com/questions/53619695/calculating-maximum-value-of-a-set-of-constant-expressions-at-compile-time
const fn max(a: usize, b: usize) -> usize {
    [a, b][(a < b) as usize]
}

/// This trait is auxiliary one,
/// which is needed to estimate maximum possible size of config instruction,
/// in order to allocate buffer of statically known size,
/// which would fit one instruction.
pub trait EncodingSize {
    const MAX_SIZE: usize;
}

const MAX_SIZE_REF_PATH: usize = 1 + PATH_MAX_SIZE;

impl<'a> EncodingSize for RawBytes<'a> {
    const MAX_SIZE: usize = 4 + MAX_FILE_CHUNK_SIZE;
}

impl<'a> EncodingSize for MoveInstruction<'a> {
    const MAX_SIZE: usize = MAX_SIZE_REF_PATH * 2;
}

impl<'a> EncodingSize for RevealInstruction<'a> {
    const MAX_SIZE: usize = RawBytes::MAX_SIZE + MAX_SIZE_REF_PATH;
}

impl<'a> EncodingSize for ConfigInstruction<'a> {
    const MAX_SIZE: usize =
        1 + max(MoveInstruction::MAX_SIZE, RevealInstruction::MAX_SIZE);
}
