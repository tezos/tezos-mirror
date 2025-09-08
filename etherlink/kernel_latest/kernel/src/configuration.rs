// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    blueprint_storage::DEFAULT_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS,
    chains::{ChainConfig, ChainFamily, EvmLimits},
    delayed_inbox::DelayedInbox,
    retrieve_minimum_base_fee_per_gas,
    storage::{
        dal_slots, enable_dal, evm_node_flag, is_enable_fa_bridge,
        max_blueprint_lookahead_in_seconds, read_admin, read_chain_family,
        read_delayed_transaction_bridge, read_kernel_governance,
        read_kernel_security_governance, read_maximum_allowed_ticks,
        read_or_set_maximum_gas_per_transaction, read_sequencer_governance, sequencer,
    },
    tick_model::constants::{MAXIMUM_GAS_LIMIT, MAX_ALLOWED_TICKS},
};
use evm_execution::{configuration::fetch_evm_configuration, read_ticketer};
use primitive_types::U256;
use tezos_crypto_rs::hash::{ChainId, ContractKt1Hash, HashTrait};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::public_key::PublicKey;

/// The chain id will need to be unique when the EVM rollup is deployed in
/// production.
pub const CHAIN_ID: u32 = 1337;

#[derive(Debug, Clone, Default)]
pub struct DalConfiguration {
    pub slot_indices: Vec<u8>,
}

pub enum ConfigurationMode {
    Proxy,
    Sequencer {
        delayed_bridge: ContractKt1Hash,
        delayed_inbox: Box<DelayedInbox>,
        sequencer: PublicKey,
        dal: Option<DalConfiguration>,
        evm_node_flag: bool,
        max_blueprint_lookahead_in_seconds: i64,
    },
}

impl std::fmt::Display for ConfigurationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigurationMode::Proxy => write!(f, "Proxy"),
            ConfigurationMode::Sequencer {
                delayed_bridge,
                delayed_inbox: _, // Ignoring delayed_inbox
                sequencer,
                dal,
                evm_node_flag,
                max_blueprint_lookahead_in_seconds,
            } => write!(
                f,
                "Sequencer {{ delayed_bridge: {delayed_bridge:?}, sequencer: {sequencer:?}, dal: {dal:?}, evm_node_flag: {evm_node_flag}, max_blueprints_lookahead_in_seconds: {max_blueprint_lookahead_in_seconds} }}"
            ),
        }
    }
}

pub struct Configuration {
    pub tezos_contracts: TezosContracts,
    pub mode: ConfigurationMode,
    pub maximum_allowed_ticks: u64,
    pub enable_fa_bridge: bool,
    pub garbage_collect_blocks: bool,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            tezos_contracts: TezosContracts::default(),
            mode: ConfigurationMode::Proxy,
            maximum_allowed_ticks: MAX_ALLOWED_TICKS,
            enable_fa_bridge: false,
            garbage_collect_blocks: false,
        }
    }
}

impl std::fmt::Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tezos Contracts: {}, Mode: {}, Enable FA Bridge: {}, Garbage collect blocks: {}",
            &self.tezos_contracts, &self.mode, &self.enable_fa_bridge, &self.garbage_collect_blocks
        )
    }
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct TezosContracts {
    pub ticketer: Option<ContractKt1Hash>,
    pub admin: Option<ContractKt1Hash>,
    pub sequencer_governance: Option<ContractKt1Hash>,
    pub kernel_governance: Option<ContractKt1Hash>,
    pub kernel_security_governance: Option<ContractKt1Hash>,
}

impl std::fmt::Display for TezosContracts {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let TezosContracts {
            ticketer,
            admin,
            sequencer_governance,
            kernel_governance,
            kernel_security_governance,
        } = self;
        write!(
            f,
            "Ticketer is {ticketer:?}. Administrator is {admin:?}. Sequencer governance is {sequencer_governance:?}. Kernel governance is {kernel_governance:?}. Kernel security governance is {kernel_security_governance:?}."
        )
    }
}
fn contains(contract: &Option<ContractKt1Hash>, expected: &ContractKt1Hash) -> bool {
    contract.as_ref() == Some(expected)
}

impl TezosContracts {
    pub fn is_admin(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.admin, contract)
    }
    pub fn is_sequencer_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.sequencer_governance, contract)
    }
    pub fn is_ticketer(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.ticketer, contract)
    }
    pub fn is_kernel_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.kernel_governance, contract)
    }

    pub fn is_kernel_security_governance(&self, contract: &ContractKt1Hash) -> bool {
        contains(&self.kernel_security_governance, contract)
    }
}

fn fetch_tezos_contracts(host: &mut impl Runtime) -> TezosContracts {
    // 1. Fetch the kernel's ticketer, returns `None` if it is badly
    //    encoded or absent.
    let ticketer = read_ticketer(host);
    // 2. Fetch the kernel's administrator, returns `None` if it is badly
    //    encoded or absent.
    let admin = read_admin(host);
    // 3. Fetch the sequencer governance, returns `None` if it is badly
    //    encoded or absent.
    let sequencer_governance = read_sequencer_governance(host);
    // 4. Fetch the kernel_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_governance = read_kernel_governance(host);
    // 5. Fetch the kernel_security_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_security_governance = read_kernel_security_governance(host);

    TezosContracts {
        ticketer,
        admin,
        sequencer_governance,
        kernel_governance,
        kernel_security_governance,
    }
}

pub fn fetch_evm_limits(host: &mut impl Runtime) -> EvmLimits {
    let maximum_gas_limit =
        read_or_set_maximum_gas_per_transaction(host).unwrap_or(MAXIMUM_GAS_LIMIT);

    let minimum_base_fee_per_gas = retrieve_minimum_base_fee_per_gas(host);

    EvmLimits {
        maximum_gas_limit,
        minimum_base_fee_per_gas,
    }
}

fn fetch_dal_configuration<Host: Runtime>(host: &mut Host) -> Option<DalConfiguration> {
    let enable_dal = enable_dal(host).unwrap_or(false);
    if enable_dal {
        let slot_indices: Vec<u8> = dal_slots(host).unwrap_or(None)?;
        Some(DalConfiguration { slot_indices })
    } else {
        None
    }
}

fn fetch_evm_chain_configuration<Host: Runtime>(
    host: &mut Host,
    chain_id: U256,
) -> ChainConfig {
    let evm_limits = fetch_evm_limits(host);
    let evm_configuration = fetch_evm_configuration(host);
    ChainConfig::new_evm_config(chain_id, evm_limits, evm_configuration)
}

fn fetch_michelson_chain_configuration<Host: Runtime>(
    _host: &mut Host,
    chain_id: ChainId,
) -> ChainConfig {
    ChainConfig::new_michelson_config(chain_id)
}

pub fn fetch_chain_configuration<Host: Runtime>(
    host: &mut Host,
    chain_id: U256,
) -> ChainConfig {
    // if the info is not in durable storage, we must not fail, but treat it as EVM
    let chain_family = read_chain_family(host, chain_id).unwrap_or_default();
    match chain_family {
        ChainFamily::Evm => fetch_evm_chain_configuration(host, chain_id),
        ChainFamily::Michelson => {
            // Tezos-compatible chain ids have only 4 bytes.
            let chain_id_low_bytes = chain_id.low_u32();

            if chain_id != chain_id_low_bytes.into() {
                log!(host, Error, "Configured chain family is Michelson but chain id does not fit on 4 bytes; falling back to EVM chain family.");
                return fetch_evm_chain_configuration(host, chain_id);
            }

            match ChainId::try_from_bytes(&chain_id_low_bytes.to_le_bytes()) {
                Err(_) => {
                    // This is unexpected, any u32 should be decodable as a chain id
                    log!(host, Error, "Configured chain family is Michelson and the chain id fits on 4 bytes but converting to ChainId failed; falling back to EVM chain family.");
                    fetch_evm_chain_configuration(host, chain_id)
                }
                Ok(chain_id) => fetch_michelson_chain_configuration(host, chain_id),
            }
        }
    }
}

pub fn fetch_configuration<Host: Runtime>(host: &mut Host) -> Configuration {
    let tezos_contracts = fetch_tezos_contracts(host);
    let maximum_allowed_ticks =
        read_maximum_allowed_ticks(host).unwrap_or(MAX_ALLOWED_TICKS);
    let sequencer = sequencer(host).unwrap_or_default();
    let enable_fa_bridge = is_enable_fa_bridge(host).unwrap_or_default();
    let dal: Option<DalConfiguration> = fetch_dal_configuration(host);
    let evm_node_flag = evm_node_flag(host).unwrap_or(false);
    match sequencer {
        Some(sequencer) => {
            let delayed_bridge = read_delayed_transaction_bridge(host)
                // The sequencer must declare a delayed transaction bridge. This
                // default value is only to facilitate the testing.
                .unwrap_or_else(|| {
                    ContractKt1Hash::from_base58_check(
                        "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT",
                    )
                    .unwrap()
                });
            // Default to 5 minutes.
            let max_blueprint_lookahead_in_seconds =
                max_blueprint_lookahead_in_seconds(host)
                    .unwrap_or(DEFAULT_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS);
            match DelayedInbox::new(host) {
                Ok(delayed_inbox) => Configuration {
                    tezos_contracts,
                    mode: ConfigurationMode::Sequencer {
                        delayed_bridge,
                        delayed_inbox: Box::new(delayed_inbox),
                        sequencer,
                        dal,
                        evm_node_flag,
                        max_blueprint_lookahead_in_seconds,
                    },
                    maximum_allowed_ticks,
                    enable_fa_bridge,
                    garbage_collect_blocks: !evm_node_flag,
                },
                Err(err) => {
                    log!(host, Fatal, "The kernel failed to created the delayed inbox, reverting configuration to proxy ({:?})", err);
                    Configuration::default()
                }
            }
        }
        None => Configuration {
            tezos_contracts,
            mode: ConfigurationMode::Proxy,
            maximum_allowed_ticks,
            enable_fa_bridge,
            garbage_collect_blocks: false,
        },
    }
}
