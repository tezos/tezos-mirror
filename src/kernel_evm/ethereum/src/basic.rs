// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Basic Ethereum types for computation
//!
//! Many of the functions in this module (all the `one` and `zero`) can be made
//! constant, but the underlying library and functions we use are not constant.
//! TODO: <https://gitlab.com/tezos/tezos/-/milestones/114>
use primitive_types::U256;
use rlp::{Decodable, DecoderError, Encodable, Rlp};

/// The size of one 256 bit word. Size in bytes
pub const WORD_SIZE: usize = 32_usize;

/// Gas price newtype to wrap U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct GasPrice {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl GasPrice {
    /// Create a new gas price from serilizable u256
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new gas price from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for GasPrice {
    fn decode(decoder: &Rlp<'_>) -> Result<GasPrice, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for GasPrice {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}

/// Gas limit newtype to wrap U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct GasLimit {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl GasLimit {
    /// Translate to unsigned 64 bit (should be adequate for all calls at the moment)
    /// Error can only be overflow.
    pub fn to_u64(&self) -> Option<u64> {
        // Unfortunately, the `primitive_types` library doesn't implement u64 -> U256
        // conversion in `const`.
        let max_u64: U256 = U256::from(core::u64::MAX);

        if self.value <= max_u64 {
            Some(self.value.low_u64())
        } else {
            None
        }
    }

    /// Create a new gas limit from serilizable u256
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new gas limit from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for GasLimit {
    fn decode(decoder: &Rlp<'_>) -> Result<GasLimit, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for GasLimit {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}

/// Amount or value in Wei. Newtype wrapper for U256
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Wei {
    /// tezos_encoding doesn't support deriving reader and writer from newtypes so therefore this
    /// public field instead.
    pub value: U256,
}

impl Wei {
    /// Create a new value in Wei from serlizable type
    pub fn new(value: U256) -> Self {
        Self { value }
    }

    /// Create a new value in Wei from primitive type
    pub fn from_u256(value: U256) -> Self {
        Self { value }
    }

    /// Zero
    pub fn zero() -> Self {
        Self {
            value: U256::zero(),
        }
    }

    /// One
    pub fn one() -> Self {
        Self { value: U256::one() }
    }
}

impl Decodable for Wei {
    fn decode(decoder: &Rlp<'_>) -> Result<Wei, DecoderError> {
        Ok(Self {
            value: U256::decode(decoder)?,
        })
    }
}
impl Encodable for Wei {
    fn rlp_append(&self, s: &mut rlp::RlpStream) {
        s.append(&self.value);
    }
}
