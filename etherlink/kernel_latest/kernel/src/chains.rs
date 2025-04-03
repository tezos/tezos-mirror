// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_execution::configuration::EVMVersion;
use primitive_types::U256;

use crate::{
    configuration::CHAIN_ID, fees::MINIMUM_BASE_FEE_PER_GAS,
    tick_model::constants::MAXIMUM_GAS_LIMIT,
};

#[derive(Clone, Copy, Debug)]
pub enum ChainFamily {
    Evm,
    Michelson,
}

impl Default for ChainFamily {
    fn default() -> Self {
        Self::Evm
    }
}

impl From<String> for ChainFamily {
    fn from(value: String) -> Self {
        match value.as_str() {
            "Michelson" => Self::Michelson,
            "Evm" => Self::Evm,
            _ => Self::default(),
        }
    }
}

pub struct EvmChainConfig {
    pub chain_id: U256,
    pub limits: EvmLimits,
    pub evm_config: evm_execution::Config,
}

impl EvmChainConfig {
    pub fn create_config(
        chain_id: U256,
        limits: EvmLimits,
        evm_config: evm_execution::Config,
    ) -> Self {
        Self {
            chain_id,
            limits,
            evm_config,
        }
    }
}

pub struct MichelsonChainConfig {
    pub chain_id: U256,
}

impl MichelsonChainConfig {
    pub fn create_config(chain_id: U256) -> Self {
        Self { chain_id }
    }
}

#[allow(clippy::large_enum_variant)]
pub enum ChainConfig {
    Evm(EvmChainConfig),
    Michelson(MichelsonChainConfig),
}

impl ChainConfig {
    pub fn new_evm_config(
        chain_id: U256,
        limits: EvmLimits,
        evm_config: evm_execution::Config,
    ) -> Self {
        ChainConfig::Evm(EvmChainConfig::create_config(chain_id, limits, evm_config))
    }

    pub fn new_michelson_config(chain_id: U256) -> Self {
        ChainConfig::Michelson(MichelsonChainConfig::create_config(chain_id))
    }

    pub fn get_chain_family(&self) -> ChainFamily {
        match self {
            ChainConfig::Evm(_) => ChainFamily::Evm,
            ChainConfig::Michelson(_) => ChainFamily::Michelson,
        }
    }

    pub fn get_chain_id(&self) -> U256 {
        match self {
            ChainConfig::Evm(evm_chain_config) => evm_chain_config.chain_id,
            ChainConfig::Michelson(michelson_chain_config) => {
                michelson_chain_config.chain_id
            }
        }
    }
}

impl std::fmt::Display for ChainFamily {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Evm => write!(f, "EVM"),
            Self::Michelson => write!(f, "Michelson"),
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct EvmLimits {
    pub maximum_gas_limit: u64,
    pub minimum_base_fee_per_gas: U256,
}

impl Default for EvmLimits {
    fn default() -> Self {
        Self {
            maximum_gas_limit: MAXIMUM_GAS_LIMIT,
            minimum_base_fee_per_gas: MINIMUM_BASE_FEE_PER_GAS.into(),
        }
    }
}

impl std::fmt::Display for ChainConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ChainConfig::Evm(chain_config) => write!(
                f,
                "{{Chain id: {}, Chain family: {}, Limits: {:?}, VMConfig: {:?}}}",
                chain_config.chain_id,
                ChainFamily::Evm,
                chain_config.limits,
                chain_config.evm_config
            ),
            ChainConfig::Michelson(chain_config) => {
                write!(
                    f,
                    "{{Chain id: {}, Chain family: {}}}",
                    chain_config.chain_id,
                    ChainFamily::Michelson
                )
            }
        }
    }
}

impl Default for ChainConfig {
    fn default() -> Self {
        Self::Evm(EvmChainConfig::create_config(
            U256::from(CHAIN_ID),
            EvmLimits::default(),
            EVMVersion::to_config(&EVMVersion::default()),
        ))
    }
}

#[cfg(test)]
pub fn test_chain_config() -> ChainConfig {
    ChainConfig::new_evm_config(
        U256::from(CHAIN_ID),
        EvmLimits::default(),
        EVMVersion::current_test_config(),
    )
}
