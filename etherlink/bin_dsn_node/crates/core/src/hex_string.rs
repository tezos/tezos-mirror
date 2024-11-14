// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Deserializer, Serialize};

#[derive(Debug, Clone, PartialEq)]
pub struct HexString(pub Vec<u8>);

impl Serialize for HexString {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let hex = format!("0x{}", hex::encode(&self.0));
        serializer.serialize_str(&hex)
    }
}

impl<'de> Deserialize<'de> for HexString {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: String = Deserialize::deserialize(deserializer)?;
        if let Some(hex) = value.strip_prefix("0x") {
            let bytes = hex::decode(hex).map_err(serde::de::Error::custom)?;
            Ok(Self(bytes))
        } else {
            Err(serde::de::Error::custom(
                "Hex string is missing '0x' prefix",
            ))
        }
    }
}
