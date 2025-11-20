// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Arbitrary string ticket generation.
use tezos_protocol::contract::Contract;

use super::StringTicket;
use num_traits::Zero;
use proptest::prelude::*;

impl StringTicket {
    /// Strategy for generating string tickets.
    pub fn arb() -> BoxedStrategy<StringTicket> {
        (
            Contract::arb_originated(),
            String::arbitrary(),
            // non-zero
            u64::arbitrary().prop_filter("zero tickets banned", |i| !i.is_zero()),
        )
            .prop_map(|(creator, contents, amount)| {
                StringTicket::new(creator, contents, amount).unwrap()
            })
            .boxed()
    }
}
