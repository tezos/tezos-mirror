// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use clarity::Address;
use tezos_ethereum::eth_gen::OwnedHash;

pub fn address_to_hash(address: Address) -> OwnedHash {
    hex::encode(address.as_bytes()).as_bytes().to_vec()
}
