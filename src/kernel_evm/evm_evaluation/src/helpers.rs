// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use primitives::SpecId;

pub fn parse_and_get_cmp(data: &str) -> impl Fn(&u8, &u8) -> bool {
    if data.contains('>') {
        u8::gt
    } else if data.contains(">=") {
        u8::ge
    } else if data.contains('<') {
        u8::lt
    } else if data.contains("<=") {
        u8::le
    } else {
        // By default, if there is not cmp prefix then it's supposed
        // to be (strictly) the data itself.
        u8::eq
    }
}

pub fn purify_network(network: &str) -> String {
    let network = network.replace('<', "");
    let network = network.replace('>', "");
    network.replace('=', "")
}

pub fn network_to_specid(network: &str) -> SpecId {
    let actual_network = if network == "EIP150" {
        "Homestead"
    } else if network == "EIP158" {
        "Spurious"
    } else if network.contains("Constantinople") {
        "Petersburg"
    } else {
        network
    };
    SpecId::from(actual_network)
}
