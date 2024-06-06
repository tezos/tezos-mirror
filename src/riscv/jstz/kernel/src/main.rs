// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup::host::Runtime;

#[entrypoint::main]
pub fn entry(host: &mut impl Runtime) {
    #[cfg(not(feature = "static-inbox"))]
    jstz_kernel::entry(host);
    #[cfg(feature = "static-inbox")]
    entry::entry(host);
}

#[cfg(feature = "static-inbox")]
mod entry {
    use std::cell::RefCell;
    use std::thread_local;
    use tezos_smart_rollup::entrypoint::internal::StaticInbox;
    use tezos_smart_rollup::prelude::*;

    const INPUT: &str = include_str!(std::env!("INBOX_FILE"));

    thread_local! {
        static STATIC_INBOX: RefCell<StaticInbox> = RefCell::new(StaticInbox::new_from_json(INPUT));
    }

    pub fn entry(host: &mut impl Runtime) {
        STATIC_INBOX.with_borrow_mut(|inbox| {
            let mut host = inbox.wrap_runtime(host);

            jstz_kernel::entry(&mut host);
        });
    }
}
