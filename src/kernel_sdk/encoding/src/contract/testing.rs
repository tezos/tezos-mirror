// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arbitrary contract generation

use super::Contract;
use crypto::hash::ContractKt1Hash;
use crypto::hash::HashTrait;
use crypto::hash::HashType;
use proptest::prelude::*;

impl Contract {
    /// Randomly selected originated contract.
    pub fn arb_originated() -> BoxedStrategy<Contract> {
        any::<[u8; HashType::ContractKt1Hash.size()]>()
            .prop_map(|c| ContractKt1Hash::try_from_bytes(&c).unwrap())
            .prop_map(Contract::Originated)
            .boxed()
    }
}
