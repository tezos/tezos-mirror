// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{hex::FromHex, Address, Bytes, KECCAK_EMPTY},
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
    storage::{
        code::CodeStorage,
        world_state_handler::{account_path, WorldStateHandler},
    },
    Error,
};

pub fn init_precompile_bytecodes<'a, Host: Runtime>(
    host: &'a mut Host,
    world_state_handler: &'a mut WorldStateHandler,
) -> Result<(), Error> {
    init_precompile_bytecode(
        host,
        world_state_handler,
        &Address::ZERO,
        INTERNAL_FORWARDER_SOL_CONTRACT,
    )?;
    init_precompile_bytecode(
        host,
        world_state_handler,
        &WITHDRAWAL_SOL_ADDR,
        WITHDRAWAL_SOL_CONTRACT,
    )?;
    init_precompile_bytecode(
        host,
        world_state_handler,
        &FA_BRIDGE_SOL_ADDR,
        FA_BRIDGE_SOL_CONTRACT,
    )
}

fn init_precompile_bytecode<'a, Host: Runtime>(
    host: &'a mut Host,
    world_state_handler: &'a mut WorldStateHandler,
    addr: &Address,
    hex_bytes: &str,
) -> Result<(), Error> {
    let mut created_account = world_state_handler
        .get_or_create(host, &account_path(addr).map_err(custom)?)
        .map_err(custom)?;
    let mut account_info = created_account.info(host).map_err(custom)?;
    if account_info.code_hash != KECCAK_EMPTY {
        return Ok(());
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
