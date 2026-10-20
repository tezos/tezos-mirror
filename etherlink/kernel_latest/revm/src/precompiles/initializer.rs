// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, KECCAK_EMPTY},
    state::Bytecode,
};
use tezos_smart_rollup_keyspace::KeySpace;

use crate::error::EvmDbError;

use crate::{
    precompiles::constants::{
        ALIAS_FORWARDER_PRECOMPILE_ADDRESS, ALIAS_FORWARDER_SOL_CONTRACT,
        FA_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
        RESCUE_GUARD_ADDRESS, RESCUE_GUARD_CONTRACT, XTZ_BRIDGE_SOL_ADDR,
        XTZ_BRIDGE_SOL_CONTRACT,
    },
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
};

use super::constants::PredeployedContract;

pub fn init_precompile_bytecodes(
    eth_accounts: &'_ mut impl KeySpace,
    tezosx_enabled: bool,
) -> Result<(), EvmDbError> {
    init_precompile_bytecode(
        eth_accounts,
        &Address::ZERO,
        &INTERNAL_FORWARDER_SOL_CONTRACT,
    )?;
    init_precompile_bytecode(
        eth_accounts,
        &XTZ_BRIDGE_SOL_ADDR,
        &XTZ_BRIDGE_SOL_CONTRACT,
    )?;
    init_precompile_bytecode(eth_accounts, &FA_BRIDGE_SOL_ADDR, &FA_BRIDGE_SOL_CONTRACT)?;
    if tezosx_enabled {
        init_precompile_bytecode(
            eth_accounts,
            &ALIAS_FORWARDER_PRECOMPILE_ADDRESS,
            &ALIAS_FORWARDER_SOL_CONTRACT,
        )?;
        // RFC-26051: the deployment guard prevents the deployment of rescue
        // contracts on Etherlink.
        init_precompile_bytecode(
            eth_accounts,
            &RESCUE_GUARD_ADDRESS,
            &RESCUE_GUARD_CONTRACT,
        )?;
    }
    Ok(())
}

fn init_precompile_bytecode(
    eth_accounts: &'_ mut impl KeySpace,
    addr: &Address,
    predeployed: &'static PredeployedContract,
) -> Result<(), EvmDbError> {
    let mut created_account = StorageAccount::from_address(addr)?;
    let update = created_account.info_update(eth_accounts)?;
    let previous_code_hash = update.code_hash();

    if previous_code_hash == predeployed.code_hash {
        return Ok(());
    }

    update
        .with_code(
            Bytecode::new_legacy(Bytes::from_static(predeployed.code)),
            predeployed.code_hash,
        )
        .apply()?;

    if previous_code_hash != KECCAK_EMPTY {
        CodeStorage::delete(eth_accounts, &previous_code_hash)?;
    }

    Ok(())
}
