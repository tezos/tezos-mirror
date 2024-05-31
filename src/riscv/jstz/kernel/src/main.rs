// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use tezos_smart_rollup::host::Runtime;

#[cfg(feature = "static-inbox")]
mod static_inbox;

#[tezos_smart_rollup::entrypoint]
pub fn entry(host: &mut (impl Runtime + 'static)) {
    #[cfg(not(feature = "static-inbox"))]
    jstz_kernel::entry(host);
    #[cfg(feature = "static-inbox")]
    entry::entry(host);
}

#[cfg(feature = "static-inbox")]
mod entry {
    use crate::static_inbox::{StaticInbox, StaticInputHost};
    use tezos_smart_rollup::prelude::*;

    pub fn entry<R: Runtime + 'static>(host: &mut R) {
        let mut inbox = StaticInbox {};
        let mut host = inbox.wrap_runtime(host);

        // jstz unfortunately requires a static lifetime on its runtime.
        // - but we also know that it can't escape the enclosing scope here.
        //
        // So we have to extend the lifetime to 'static, and rely on the above.
        let host = (&mut host) as *mut StaticInputHost<'_, R>;
        let host: *mut StaticInputHost<'static, R> = unsafe { std::mem::transmute(host) };
        let host = unsafe { host.as_mut() }.unwrap();

        jstz_kernel::entry(host);
    }
}
