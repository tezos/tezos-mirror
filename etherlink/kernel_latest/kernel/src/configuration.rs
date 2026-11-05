// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    blueprint_storage::DEFAULT_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS,
    chains::{DebugFeatures, EvmLimits, ExperimentalFeatures, TezosXChainConfig},
    delayed_inbox::DelayedInbox,
    retrieve_minimum_base_fee_per_gas,
    storage::{
        dal_slots, enable_dal, is_enable_fa_bridge, max_blueprint_lookahead_in_seconds,
        read_admin, read_delayed_transaction_bridge, read_evm_chain_id,
        read_kernel_governance, read_kernel_security_governance,
        read_maximum_allowed_ticks, read_michelson_runtime_chain_id,
        read_or_set_maximum_gas_per_transaction, read_sequencer_governance, sequencer,
        store_evm_chain_id, store_michelson_runtime_chain_id, ADMIN_KEY,
        DELAYED_BRIDGE_KEY, KERNEL_GOVERNANCE_KEY, KERNEL_SECURITY_GOVERNANCE_KEY,
    },
    tick_model::constants::{MAXIMUM_GAS_LIMIT, MAX_ALLOWED_TICKS},
};
use primitive_types::U256;
use revm_etherlink::storage::{read_ticketer, version::read_or_set_evm_version};
use tezos_crypto_rs::{
    blake2b,
    hash::{ChainId, ContractKt1Hash},
};
use tezos_evm_logging::{log, Level::*};
use tezos_evm_runtime::runtime::evm_node_flag;
use tezos_evm_runtime::runtime_keyspaces::BlueprintKeyspaces;
use tezos_smart_rollup_encoding::public_key::PublicKey;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::{Key, KeySpace, KeySpaceError};

/// The chain id will need to be unique when the EVM rollup is deployed in
/// production.
pub const EVM_CHAIN_ID: u32 = 1337;

#[derive(Debug, Clone, Default)]
pub struct DalConfiguration {
    pub slot_indices: Vec<u8>,
}

pub struct SequencerConfig {
    pub delayed_bridge: ContractKt1Hash,
    pub delayed_inbox: Box<DelayedInbox>,
    pub sequencer: PublicKey,
    pub dal: Option<DalConfiguration>,
    pub max_blueprint_lookahead_in_seconds: i64,
}

pub enum ConfigurationMode {
    Proxy,
    Sequencer(SequencerConfig),
}

impl std::fmt::Display for SequencerConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SequencerConfig {
            delayed_bridge,
            delayed_inbox: _, // Ignoring delayed_inbox
            sequencer,
            dal,
            max_blueprint_lookahead_in_seconds,
        } = self;
        write!(
            f,
            "Sequencer {{ delayed_bridge: {delayed_bridge:?}, sequencer: {sequencer:?}, dal: {dal:?}, max_blueprints_lookahead_in_seconds: {max_blueprint_lookahead_in_seconds} }}"
        )
    }
}

impl std::fmt::Display for ConfigurationMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigurationMode::Proxy => write!(f, "Proxy"),
            ConfigurationMode::Sequencer(seq) => write!(f, "{seq}"),
        }
    }
}

/// Configuration shared by every mode (proxy and sequencer alike).
pub struct CommonConfig {
    pub tezos_contracts: TezosContracts,
    pub maximum_allowed_ticks: u64,
    pub enable_fa_bridge: bool,
    pub evm_node_flag: bool,
}

impl Default for CommonConfig {
    fn default() -> Self {
        Self {
            tezos_contracts: TezosContracts::default(),
            maximum_allowed_ticks: MAX_ALLOWED_TICKS,
            enable_fa_bridge: false,
            evm_node_flag: false,
        }
    }
}

pub struct Configuration {
    pub common: CommonConfig,
    pub mode: ConfigurationMode,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            common: CommonConfig::default(),
            mode: ConfigurationMode::Proxy,
        }
    }
}

impl std::fmt::Display for Configuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Tezos Contracts: {}, Mode: {}, Enable FA Bridge: {}",
            &self.common.tezos_contracts, &self.mode, &self.common.enable_fa_bridge
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

// The `/evm` contracts are read through the raw host; the `/base` ones go
// through the keyspace handle.
fn read_and_log_b58<T, E: std::fmt::Display>(
    result: Result<Option<T>, E>,
    key: &Key,
) -> Option<T> {
    match result {
        Ok(Some(contract)) => Some(contract),
        Ok(None) => {
            log!(Info, "No contract provided at key {}", key);
            None
        }
        Err(err) => {
            log!(
                Error,
                "Failed to read contract from storage at key {}: {}",
                key,
                err
            );
            None
        }
    }
}

fn fetch_tezos_contracts(host: &impl StorageV1, base: &impl KeySpace) -> TezosContracts {
    // 1. Fetch the kernel's ticketer, returns `None` if it is badly
    //    encoded or absent.
    let ticketer = read_ticketer(host);
    // 2. Fetch the kernel's administrator, returns `None` if it is badly
    //    encoded or absent.
    let admin = read_and_log_b58(read_admin(base), &ADMIN_KEY);
    // 3. Fetch the sequencer governance, returns `None` if it is badly
    //    encoded or absent.
    let sequencer_governance = read_sequencer_governance(host);
    // 4. Fetch the kernel_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_governance =
        read_and_log_b58(read_kernel_governance(base), &KERNEL_GOVERNANCE_KEY);
    // 5. Fetch the kernel_security_governance contract, returns `None` if it is badly
    //    encoded or absent.
    let kernel_security_governance = read_and_log_b58(
        read_kernel_security_governance(base),
        &KERNEL_SECURITY_GOVERNANCE_KEY,
    );

    TezosContracts {
        ticketer,
        admin,
        sequencer_governance,
        kernel_governance,
        kernel_security_governance,
    }
}

/// Reads the EVM limits from `evm_world_state`, the `/evm/world_state`
/// keyspace.
///
/// When the maximum gas per transaction is absent, the default is written
/// there, and a failed write is ignored.
///
/// When the minimum base fee per gas is absent, the default is written there,
/// and a failed write is only logged.
pub fn fetch_evm_limits(evm_world_state: &mut impl KeySpace) -> EvmLimits {
    let maximum_gas_limit = read_or_set_maximum_gas_per_transaction(evm_world_state)
        .unwrap_or(MAXIMUM_GAS_LIMIT);

    let minimum_base_fee_per_gas = retrieve_minimum_base_fee_per_gas(evm_world_state);

    EvmLimits {
        maximum_gas_limit,
        minimum_base_fee_per_gas,
    }
}

fn fetch_dal_configuration(
    base: &impl KeySpace,
    is_evm_node: bool,
) -> Option<DalConfiguration> {
    if enable_dal(base, is_evm_node) {
        let slot_indices: Vec<u8> = dal_slots(base)?;
        Some(DalConfiguration { slot_indices })
    } else {
        None
    }
}

/// Reads the EVM chain id from `evm_world_state`, the `/evm/world_state`
/// keyspace. When it is absent, writes the default there and returns it.
/// Errors when that write fails.
pub fn fetch_evm_chain_id(
    evm_world_state: &mut impl KeySpace,
) -> Result<U256, KeySpaceError> {
    match read_evm_chain_id(evm_world_state) {
        Some(evm_chain_id) => Ok(evm_chain_id),
        None => {
            log!(
                Info,
                "No EVM chain id in storage, falling back to the default."
            );
            let evm_chain_id = U256::from(EVM_CHAIN_ID);
            store_evm_chain_id(evm_world_state, evm_chain_id)?;
            Ok(evm_chain_id)
        }
    }
}

/// Derive the Michelson runtime chain ID from the EVM chain ID by
/// hashing the EVM chain ID bytes with Blake2B-256 and taking the
/// first 4 bytes.  This follows the same pattern as Tezos L1's
/// `Chain_id.of_block_hash` (hash + truncate to 4 bytes).
fn derive_michelson_chain_id(evm_chain_id: U256) -> ChainId {
    let mut evm_chain_id_bytes = [0u8; 32];
    evm_chain_id.to_little_endian(&mut evm_chain_id_bytes);
    let hash = blake2b::digest_256(&evm_chain_id_bytes);
    ChainId::from(<[u8; 4]>::try_from(&hash[..4]).unwrap())
}

fn fetch_michelson_runtime_chain_id(
    tez_world_state: &mut impl KeySpace,
    evm_chain_id: U256,
) -> Result<ChainId, KeySpaceError> {
    match read_michelson_runtime_chain_id(tez_world_state)? {
        Some(michelson_chain_id) => Ok(michelson_chain_id),
        None => {
            let michelson_chain_id = derive_michelson_chain_id(evm_chain_id);
            store_michelson_runtime_chain_id(tez_world_state, &michelson_chain_id)?;
            Ok(michelson_chain_id)
        }
    }
}

/// Reads the chain configuration of the TezosX kernel.
///
/// The EVM chain id is read from the `/evm/world_state` keyspace of `bk`.
/// When it is absent, the default is written there. Errors when that write
/// fails.
///
/// The Michelson chain id is read from the `/tez/world_state` keyspace of
/// `bk`. When it is absent, the id is derived from the EVM chain id and
/// written there. Errors when that read or write fails.
///
/// The minimum base fee per gas is read from the `/evm/world_state` keyspace
/// of `bk`. When it is absent, the default is written there, and a failed
/// write is only logged.
///
/// The maximum gas per transaction is read from the `/evm/world_state`
/// keyspace of `bk`. When it is absent, the default is written there, and a
/// failed write is ignored.
pub fn fetch_tezosx_configuration(
    host: &mut impl StorageV1,
    bk: &mut BlueprintKeyspaces<impl KeySpace>,
) -> Result<TezosXChainConfig, KeySpaceError> {
    // Read both runtime chain ids from storage. The EVM chain id falls back to
    // the default and is persisted on first use; the Michelson runtime chain id
    // is derived from it and persisted if absent.
    let evm_chain_id = fetch_evm_chain_id(bk.evm_world_state_mut())?;
    let limits = fetch_evm_limits(bk.evm_world_state_mut());
    let spec_id = read_or_set_evm_version(host).into();
    let experimental_features = ExperimentalFeatures::read_from_storage(bk.base());
    let debug_features = DebugFeatures::read_from_storage(bk.base());
    let michelson_chain_id =
        fetch_michelson_runtime_chain_id(bk.tez_world_state_mut(), evm_chain_id)?;
    Ok(TezosXChainConfig::create_config(
        evm_chain_id,
        limits,
        spec_id,
        experimental_features,
        debug_features,
        michelson_chain_id,
    ))
}

pub fn fetch_common_config(host: &impl StorageV1, base: &impl KeySpace) -> CommonConfig {
    let tezos_contracts = fetch_tezos_contracts(host, base);
    let maximum_allowed_ticks =
        read_maximum_allowed_ticks(base).unwrap_or(MAX_ALLOWED_TICKS);
    let enable_fa_bridge = is_enable_fa_bridge(base);
    let evm_node_flag = evm_node_flag(base);
    CommonConfig {
        tezos_contracts,
        maximum_allowed_ticks,
        enable_fa_bridge,
        evm_node_flag,
    }
}

pub fn fetch_configuration(host: &impl StorageV1, base: &impl KeySpace) -> Configuration {
    let sequencer = sequencer(host).unwrap_or_default();
    let common = fetch_common_config(host, base);
    let dal: Option<DalConfiguration> =
        fetch_dal_configuration(base, common.evm_node_flag);
    match sequencer {
        Some(sequencer) => {
            let delayed_bridge = read_and_log_b58(
                read_delayed_transaction_bridge(base),
                &DELAYED_BRIDGE_KEY,
            )
            // The sequencer must declare a delayed transaction bridge. This
            // default value is only to facilitate the testing.
            .unwrap_or_else(|| {
                ContractKt1Hash::from_base58_check("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT")
                    .unwrap()
            });
            // Default to 5 minutes.
            let max_blueprint_lookahead_in_seconds =
                max_blueprint_lookahead_in_seconds(base)
                    .unwrap_or(DEFAULT_MAX_BLUEPRINT_LOOKAHEAD_IN_SECONDS);
            match DelayedInbox::from_base(base) {
                Ok(delayed_inbox) => Configuration {
                    common,
                    mode: ConfigurationMode::Sequencer(SequencerConfig {
                        delayed_bridge,
                        delayed_inbox: Box::new(delayed_inbox),
                        sequencer,
                        dal,
                        max_blueprint_lookahead_in_seconds,
                    }),
                },
                Err(err) => {
                    log!(Fatal, "The kernel failed to created the delayed inbox, reverting configuration to proxy ({:?})", err);
                    Configuration::default()
                }
            }
        }
        None => Configuration {
            common,
            mode: ConfigurationMode::Proxy,
        },
    }
}
