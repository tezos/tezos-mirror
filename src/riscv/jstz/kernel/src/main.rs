// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::sync::Once;
use tezos_crypto_rs::hash::ContractKt1Hash;
use tezos_smart_rollup::prelude::Runtime;
use tezos_smart_rollup::{entrypoint, storage::path::RefPath};

#[entrypoint::main]
#[cfg_attr(
    feature = "static-inbox",
    entrypoint::runtime(static_inbox = "$INBOX_FILE")
)]
pub fn entry(host: &mut impl Runtime) {
    // We need to setup the ticketer (bridge address that funds Jstz) for Jstz to not panic.
    {
        static ONCE: Once = Once::new();

        ONCE.call_once(|| {
            let ticketer =
                ContractKt1Hash::from_base58_check("KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn").unwrap();

            const TICKETER: RefPath = RefPath::assert_from(b"/ticketer");
            host.store_write(&TICKETER, &bincode::serialize(&ticketer).unwrap(), 0)
                .unwrap();
        });
    }

    // Delegate to Jstz kernel
    jstz_kernel::entry(host);
}
