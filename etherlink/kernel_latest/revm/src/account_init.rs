use revm::{
    primitives::{hex::FromHex, Bytes},
    state::Bytecode,
};
use tezos_evm_runtime::runtime::Runtime;

use crate::{
    constants::{WITHDRAWAL_SOL_ADDR, WITHDRAWAL_SOL_CONTRACT},
    custom,
    world_state_handler::{account_path, WorldStateHandler},
    Error,
};

pub fn init_withdrawal_account<'a, Host: Runtime>(
    host: &'a mut Host,
    world_state_handler: &'a mut WorldStateHandler,
) -> Result<(), Error> {
    let mut created_account = world_state_handler
        .get_or_create(host, &account_path(&WITHDRAWAL_SOL_ADDR).map_err(custom)?)
        .map_err(custom)?;
    if created_account.code_exists(host).map_err(custom)? {
        return Ok(());
    }
    let code = Some(Bytecode::new_legacy(
        Bytes::from_hex(WITHDRAWAL_SOL_CONTRACT).map_err(custom)?,
    ));
    created_account.set_code(host, code).map_err(custom)?;
    Ok(())
}
