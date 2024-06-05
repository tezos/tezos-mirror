// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::prelude::Runtime;

#[entrypoint::main]
#[cfg_attr(
    feature = "static-inbox",
    entrypoint::runtime(static_inbox = "$INBOX_FILE")
)]
pub fn entry(host: &mut impl Runtime) {
    jstz_kernel::entry(host);
}
