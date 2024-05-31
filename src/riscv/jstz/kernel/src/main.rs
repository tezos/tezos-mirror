// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::host::Runtime;

#[tezos_smart_rollup::entrypoint]
pub fn entry(host: &mut (impl Runtime + 'static)) {
    jstz_kernel::entry(host)
}
