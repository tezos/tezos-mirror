// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Echo kernel for the RISC-V PVM
//!
//! This kernel replicates the behaviour of the Wasm echo kernel
//! `proto_alpha/lib_protocol/test/integration/wasm_kernel/echo.wast`:
//!
//! - Reads inbox messages
//! - For external messages: strips the header and writes the message to the outbox
//! - For internal transfer messages: extracts the payload and writes it to the outbox

use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::host::WasmHost;
use tezos_smart_rollup::inbox::InboxMessage;
use tezos_smart_rollup::inbox::InternalInboxMessage;
use tezos_smart_rollup::inbox::Transfer;
use tezos_smart_rollup::michelson::MichelsonBytes;
use tezos_smart_rollup::prelude::debug_msg;

#[entrypoint::main]
pub fn entry<Host>(host: &mut Host)
where
    Host: WasmHost,
{
    while let Some(msg) = host.read_input().expect("Failed to read input") {
        let (_, parsed_msg) = InboxMessage::<MichelsonBytes>::parse(msg.as_ref())
            .expect("Failed to parse inbox message");

        match parsed_msg {
            InboxMessage::External(payload) => {
                if let Err(e) = host.write_output(payload) {
                    debug_msg!(host, "Failed to write external message to outbox: {e:?}\n");
                }
            }

            InboxMessage::Internal(InternalInboxMessage::Transfer(Transfer {
                payload, ..
            })) => {
                if let Err(e) = host.write_output(&payload.0) {
                    debug_msg!(host, "Failed to write internal transfer to outbox: {e:?}\n");
                }
            }

            other => {
                debug_msg!(host, "Other inbox message: {other:#?}\n");
            }
        }
    }
}
