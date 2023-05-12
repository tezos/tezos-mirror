// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup_core::{MAX_FILE_CHUNK_SIZE, PREIMAGE_HASH_SIZE};
use tezos_smart_rollup_host::path::PATH_MAX_SIZE;

use super::{ConfigInstruction, MoveInstruction, RefBytes, RevealInstruction};

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

impl<'a> EncodingSize for RefBytes<'a> {
    const MAX_SIZE: usize = 4 + MAX_FILE_CHUNK_SIZE;
}

impl<Path> EncodingSize for MoveInstruction<Path> {
    const MAX_SIZE: usize = MAX_SIZE_REF_PATH * 2;
}

impl<Path, Bytes> EncodingSize for RevealInstruction<Path, Bytes> {
    // Hash size + path size
    const MAX_SIZE: usize = PREIMAGE_HASH_SIZE + MAX_SIZE_REF_PATH;
}

impl<Path, Bytes> EncodingSize for ConfigInstruction<Path, Bytes> {
    const MAX_SIZE: usize = 1 + max(
        MoveInstruction::<Path>::MAX_SIZE,
        RevealInstruction::<Path, Bytes>::MAX_SIZE,
    );
}
