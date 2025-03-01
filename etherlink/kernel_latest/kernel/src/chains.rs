// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    fees::MINIMUM_BASE_FEE_PER_GAS, tick_model::constants::MAXIMUM_GAS_LIMIT, CHAIN_ID,
};
use evm_execution::configuration::EVMVersion;
use primitive_types::U256;

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

#[derive(Debug)]
pub struct EvmChainConfig {
    chain_id: U256,
    limits: EvmLimits,
    evm_config: evm_execution::Config,
}

#[derive(Debug)]
pub struct MichelsonChainConfig {
    chain_id: U256,
}

pub enum ChainConfig {
    Evm(Box<EvmChainConfig>),
    Michelson(MichelsonChainConfig),
}

pub trait ChainConfigTrait: std::fmt::Debug {
    fn get_chain_id(&self) -> U256;

    fn fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        chain_family: ChainFamily,
    ) -> std::fmt::Result {
        write!(f, "{{Chain family: {}, {:?}}}", chain_family, self)
    }
}

impl ChainConfigTrait for EvmChainConfig {
    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }
}

impl EvmChainConfig {
    fn create_config(
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

    pub fn get_limits(&self) -> &EvmLimits {
        &self.limits
    }

    pub fn get_evm_config(&self) -> &evm_execution::Config {
        &self.evm_config
    }
}

impl ChainConfigTrait for MichelsonChainConfig {
    fn get_chain_id(&self) -> U256 {
        self.chain_id
    }
}

impl MichelsonChainConfig {
    pub fn create_config(chain_id: U256) -> Self {
        Self { chain_id }
    }
}

impl ChainConfig {
    pub fn new_evm_config(
        chain_id: U256,
        limits: EvmLimits,
        evm_config: evm_execution::Config,
    ) -> Self {
        ChainConfig::Evm(Box::new(EvmChainConfig::create_config(
            chain_id, limits, evm_config,
        )))
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
            ChainConfig::Evm(evm_chain_config) => evm_chain_config.get_chain_id(),
            ChainConfig::Michelson(michelson_chain_config) => {
                michelson_chain_config.get_chain_id()
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

impl Default for ChainConfig {
    fn default() -> Self {
        ChainConfig::Evm(Box::default())
    }
}

impl std::fmt::Display for ChainConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let chain_family = self.get_chain_family();
        match self {
            ChainConfig::Evm(evm_chain_config) => evm_chain_config.fmt(f, chain_family),
            ChainConfig::Michelson(michelson_chain_config) => {
                michelson_chain_config.fmt(f, chain_family)
            }
        }
    }
}

impl Default for EvmChainConfig {
    fn default() -> Self {
        Self::create_config(
            U256::from(CHAIN_ID),
            EvmLimits::default(),
            EVMVersion::to_config(&EVMVersion::default()),
        )
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
