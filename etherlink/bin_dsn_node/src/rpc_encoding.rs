// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_derive::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
enum Param {
    #[serde(untagged)]
    #[serde(with = "hex")]
    Transaction(Vec<u8>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SendRawTransaction {
    jsonrpc: JsonRpcVersion,
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<u64>,
    method: EthMethod,
    params: Vec<Param>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct SendRawTransactionResult {
    #[serde(skip_serializing_if = "Option::is_none")]
    id: Option<u64>,
    jsonrpc: JsonRpcVersion,
    #[serde(with = "hex")]
    result: Vec<u8>,
}

#[derive(Debug, Clone)]
enum JsonRpcVersion {
    V2_0,
}

impl Serialize for JsonRpcVersion {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            JsonRpcVersion::V2_0 => "2.0".serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for JsonRpcVersion {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: Value = Deserialize::deserialize(deserializer)?;
        match value {
            Value::String(s) if s == "2.0" => Ok(JsonRpcVersion::V2_0),
            _ => Err(serde::de::Error::custom(
                "Only JSON-RPC version 2.0 supported",
            )),
        }
    }
}

#[derive(Debug, Clone)]
enum EthMethod {
    SendRawTransaction,
    Other(String),
}

impl Serialize for EthMethod {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            EthMethod::SendRawTransaction => "eth_sendRawTransaction".serialize(serializer),
            EthMethod::Other(s) => s.serialize(serializer),
        }
    }
}

impl<'de> Deserialize<'de> for EthMethod {
    fn deserialize<D>(deserializer: D) -> Result<EthMethod, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: Value = Deserialize::deserialize(deserializer)?;
        match value {
            Value::String(s) if s == "eth_sendRawTransaction" => Ok(EthMethod::SendRawTransaction),
            Value::String(s) => Ok(EthMethod::Other(s)),
            _ => Err(serde::de::Error::custom("Unsupported method")),
        }
    }
}

mod hex {
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(data: &Vec<u8>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let hex = format!("0x{}", hex::encode(data));
        serializer.serialize_str(&hex)
    }
    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let value: String = Deserialize::deserialize(deserializer)?;
        if let Some(hex) = value.strip_prefix("0x") {
            hex::decode(hex).map_err(serde::de::Error::custom)
        } else {
            Err(serde::de::Error::custom(
                "Hex string is missing '0x' prefix",
            ))
        }
    }
}
