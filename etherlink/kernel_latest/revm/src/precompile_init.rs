// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    primitives::{hex::FromHex, Address, Bytes},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;

use crate::{
    constants::{
        FA_WITHDRAWAL_SOL_ADDR, FA_WITHDRAWAL_SOL_CONTRACT, WITHDRAWAL_SOL_ADDR,
        WITHDRAWAL_SOL_CONTRACT,
    },
    custom,
    world_state_handler::{account_path, WorldStateHandler},
    Error,
};

pub fn init_precompile_bytecodes<'a, Host: Runtime>(
    host: &'a mut Host,
    world_state_handler: &'a mut WorldStateHandler,
) -> Result<(), Error> {
    init_precompile_bytecode(
        host,
        world_state_handler,
        &WITHDRAWAL_SOL_ADDR,
        WITHDRAWAL_SOL_CONTRACT,
    )?;
    init_precompile_bytecode(
        host,
        world_state_handler,
        &FA_WITHDRAWAL_SOL_ADDR,
        FA_WITHDRAWAL_SOL_CONTRACT,
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
    if created_account.code_exists(host).map_err(custom)? {
        return Ok(());
    }
    let code = Some(Bytecode::new_legacy(
        Bytes::from_hex(hex_bytes).map_err(custom)?,
    ));
    created_account.set_code(host, code).map_err(custom)?;
    Ok(())
}
