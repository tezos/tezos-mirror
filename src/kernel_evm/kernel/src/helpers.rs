// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_ethereum::address::EthereumAddress;
use tezos_ethereum::eth_gen::OwnedHash;

pub fn address_to_hash(address: EthereumAddress) -> OwnedHash {
    let str: String = address.into();
    str.as_bytes().to_vec()
}
