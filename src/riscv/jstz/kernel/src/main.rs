// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use std::sync::Once;

use jstz_crypto::hash::Hash;
use jstz_crypto::smart_function_hash::SmartFunctionHash;
use jstz_kernel::TICKETER;
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::prelude::Runtime;

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
            let config = bincode::config::legacy();

            let ticketer =
                SmartFunctionHash::from_base58("KT1HbQepzV1nVGg8QVznG7z4RcHseD5kwqBn").unwrap();

            host.store_write(
                &TICKETER,
                bincode::encode_to_vec(ticketer, config).unwrap().as_slice(),
                0,
            )
            .unwrap();
        });
    }

    // Delegate to Jstz kernel
    jstz_kernel::entry(host);

    // Forcibly reset the garbage collector, to prevent the slowdown over time.
    //
    // It's safe to do so at this point, as jstz does not maintain references to any part of boa
    // between runs.
    boa_gc::gc_reset();
}
