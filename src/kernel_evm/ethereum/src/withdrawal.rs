// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Withdrawals to layer 1 from the EVM kernel

use primitive_types::U256;
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_encoding::contract::Contract;

/// A single withdrawal from the rollup to an account on layer one.
#[derive(Debug, Eq, PartialEq)]
pub struct Withdrawal {
    /// The target address on layer one.
    pub target: Contract,
    /// The amount in wei we wish to transfer. This has to be
    /// translated into CTEZ or whatever currency is used for
    /// paying for L2XTZ.
    pub amount: U256,
}

impl Withdrawal {
    /// De-serialize a contract address from bytes (binary format).
    pub fn address_from_bytes(bytes: &[u8]) -> Option<Contract> {
        Some(Contract::nom_read(bytes).ok()?.1)
    }

    /// De-serialize a contract address from string given as bytes
    /// (use case). The bytes present the address in textual format.
    pub fn address_from_str(s: &str) -> Option<Contract> {
        Contract::from_b58check(s).ok()
    }
}
