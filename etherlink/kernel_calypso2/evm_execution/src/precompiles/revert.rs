// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

/// Implements a precompiled contract that always revert. It can be useful
/// if we want to replace the implementation of a precompiled contract by
/// a version that always revert, e.g. to prevent reetrancy on withdrawals.
use evm::{Context, Transfer};
use tezos_evm_runtime::runtime::Runtime;

use crate::{handler::EvmHandler, EthereumError};

use super::PrecompileOutcome;

pub fn revert_precompile<Host: Runtime>(
    _handler: &mut EvmHandler<Host>,
    _input: &[u8],
    _context: &Context,
    _is_static: bool,
    _transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    Ok(crate::precompiles::PrecompileOutcome {
        exit_status: evm::ExitReason::Revert(evm::ExitRevert::Reverted),
        withdrawals: vec![],
        output: vec![],
        estimated_ticks: 0,
    })
}
