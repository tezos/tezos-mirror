// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{hex::FromHex, Address, Bytes, FixedBytes, KECCAK_EMPTY},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;

use crate::{
    custom,
    helpers::storage::bytes_hash,
    precompiles::constants::{
        FA_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_CONTRACT, INTERNAL_FORWARDER_SOL_CONTRACT,
        WITHDRAWAL_SOL_ADDR, WITHDRAWAL_SOL_CONTRACT,
    },
    storage::{code::CodeStorage, world_state_handler::StorageAccount},
    Error,
};

use super::constants::{
    FA_BRIDGE_SOL_CODE_HASH, INTERNAL_FORWARDER_SOL_CODE_HASH, WITHDRAWAL_SOL_CODE_HASH,
};

pub fn init_precompile_bytecodes<Host: Runtime>(host: &'_ mut Host) -> Result<(), Error> {
    init_precompile_bytecode(
        host,
        &Address::ZERO,
        INTERNAL_FORWARDER_SOL_CONTRACT,
        &INTERNAL_FORWARDER_SOL_CODE_HASH,
    )?;
    init_precompile_bytecode(
        host,
        &WITHDRAWAL_SOL_ADDR,
        WITHDRAWAL_SOL_CONTRACT,
        &WITHDRAWAL_SOL_CODE_HASH,
    )?;
    init_precompile_bytecode(
        host,
        &FA_BRIDGE_SOL_ADDR,
        FA_BRIDGE_SOL_CONTRACT,
        &FA_BRIDGE_SOL_CODE_HASH,
    )?;

    Ok(())
}

fn init_precompile_bytecode<Host: Runtime>(
    host: &'_ mut Host,
    addr: &Address,
    hex_bytes: &str,
    code_hash: &FixedBytes<32>,
) -> Result<(), Error> {
    let mut created_account = StorageAccount::from_address(addr)?;
    let mut account_info = created_account.info(host).map_err(custom)?;

    if account_info.code_hash == *code_hash {
        return Ok(());
    }

    if account_info.code_hash != KECCAK_EMPTY {
        CodeStorage::delete(host, &account_info.code_hash)?;
    }

    let code = Bytecode::new_legacy(Bytes::from_hex(hex_bytes).map_err(custom)?);
    let code_hash = bytes_hash(code.original_byte_slice());
    account_info.code_hash = code_hash;
    created_account
        .set_info(host, account_info)
        .map_err(custom)?;
    CodeStorage::add(host, code.original_byte_slice(), Some(code_hash))?;
    Ok(())
}
