// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{Address, Bytes, KECCAK_EMPTY},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;

use crate::{
    custom,
    precompiles::constants::{
        FA_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_ADDR, WITHDRAWAL_SOL_CONTRACT,
    },
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
    Error,
};

use super::constants::PredeployedContract;

pub fn init_precompile_bytecodes<Host: Runtime>(host: &'_ mut Host) -> Result<(), Error> {
    init_precompile_bytecode(host, &Address::ZERO, &INTERNAL_FORWARDER_SOL_CONTRACT)?;
    init_precompile_bytecode(host, &WITHDRAWAL_SOL_ADDR, &WITHDRAWAL_SOL_CONTRACT)?;
    init_precompile_bytecode(host, &FA_BRIDGE_SOL_ADDR, &FA_BRIDGE_SOL_CONTRACT)
}

fn init_precompile_bytecode<Host: Runtime>(
    host: &'_ mut Host,
    addr: &Address,
    predeployed: &'static PredeployedContract,
) -> Result<(), Error> {
    let mut created_account = StorageAccount::from_address(addr)?;
    let mut account_info = created_account.info(host).map_err(custom)?;

    if account_info.code_hash == predeployed.code_hash {
        return Ok(());
    }

    if account_info.code_hash != KECCAK_EMPTY {
        CodeStorage::delete(host, &account_info.code_hash)?;
    }

    let code = Bytecode::new_legacy(Bytes::from_static(predeployed.code));
    account_info.code_hash = predeployed.code_hash;
    created_account
        .set_info(host, account_info)
        .map_err(custom)?;
    CodeStorage::add(
        host,
        code.original_byte_slice(),
        Some(predeployed.code_hash),
    )?;

    Ok(())
}
