// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arbitrary contract generation

use super::Contract;
use crypto::hash::ContractKt1Hash;
use crypto::hash::HashType;
use proptest::prelude::*;

impl Contract {
    /// Randomly selected originated contract.
    pub fn arb_originated() -> BoxedStrategy<Contract> {
        any::<[u8; HashType::ContractKt1Hash.size()]>()
            .prop_map(Vec::from)
            .prop_map(ContractKt1Hash)
            .prop_map(Contract::Originated)
            .boxed()
    }
}
