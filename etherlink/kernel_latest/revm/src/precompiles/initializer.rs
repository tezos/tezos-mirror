// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, KECCAK_EMPTY},
    state::Bytecode,
};
use tezos_smart_rollup_host::storage::StorageV1;

use crate::error::EvmDbError;

use crate::{
    precompiles::constants::{
        ALIAS_FORWARDER_PRECOMPILE_ADDRESS, ALIAS_FORWARDER_SOL_CONTRACT,
        FA12_WRAPPER_SOL_ADDR, FA12_WRAPPER_SOL_CONTRACT, FA_BRIDGE_SOL_ADDR,
        FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT, XTZ_BRIDGE_SOL_ADDR,
        XTZ_BRIDGE_SOL_CONTRACT,
    },
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
};

use super::constants::PredeployedContract;

pub fn init_precompile_bytecodes(
    host: &'_ mut impl StorageV1,
    tezosx_enabled: bool,
) -> Result<(), EvmDbError> {
    init_precompile_bytecode(host, &Address::ZERO, &INTERNAL_FORWARDER_SOL_CONTRACT)?;
    init_precompile_bytecode(host, &XTZ_BRIDGE_SOL_ADDR, &XTZ_BRIDGE_SOL_CONTRACT)?;
    init_precompile_bytecode(host, &FA_BRIDGE_SOL_ADDR, &FA_BRIDGE_SOL_CONTRACT)?;
    if tezosx_enabled {
        init_precompile_bytecode(
            host,
            &ALIAS_FORWARDER_PRECOMPILE_ADDRESS,
            &ALIAS_FORWARDER_SOL_CONTRACT,
        )?;
        init_precompile_bytecode(
            host,
            &FA12_WRAPPER_SOL_ADDR,
            &FA12_WRAPPER_SOL_CONTRACT,
        )?;
    }
    Ok(())
}

fn init_precompile_bytecode(
    host: &'_ mut impl StorageV1,
    addr: &Address,
    predeployed: &'static PredeployedContract,
) -> Result<(), EvmDbError> {
    let mut created_account = StorageAccount::from_address(addr)?;
    let mut account_info = created_account.info(host)?;

    if account_info.code_hash == predeployed.code_hash {
        return Ok(());
    }

    if account_info.code_hash != KECCAK_EMPTY {
        CodeStorage::delete(host, &account_info.code_hash)?;
    }

    let code = Bytecode::new_legacy(Bytes::from_static(predeployed.code));
    account_info.code_hash = predeployed.code_hash;
    created_account.set_info(host, account_info)?;
    CodeStorage::add(
        host,
        code.original_byte_slice(),
        Some(predeployed.code_hash),
    )?;

    Ok(())
}
