// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#[repr(u8)]
pub enum SpecId {
    // Unwanted standalone fork support for Etherlink:
    // Frontier, Homestead, Tangerine, Spurious, ,Byzantium, Constantinople,
    // Petersburg, Istanbul, MuirGlacier, Berlin, London, Merge.
    Unsupported = 0,
    Shanghai = 1,
    Cancun = 2,
    Latest = 3,
}

impl From<&String> for SpecId {
    fn from(name: &String) -> Self {
        // Eliminate shortcuts
        let name = if name == "EIP150" {
            "Homestead"
        } else if name == "EIP158" {
            "Spurious"
        } else if name.contains("Constantinople") {
            "Petersburg"
        } else {
            name
        };
        match name {
            "Frontier" | "Homestead" | "Tangerine" | "Spurious" | "Byzantium"
            | "Constantinople" | "Petersburg" | "Istanbul" | "MuirGlacier" | "Berlin"
            | "London" | "Merge" => SpecId::Unsupported,
            "Shanghai" => SpecId::Shanghai,
            "Cancun" => SpecId::Cancun,
            _ => SpecId::Latest,
        }
    }
}
