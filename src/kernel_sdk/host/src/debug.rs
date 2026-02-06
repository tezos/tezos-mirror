// SPDX-FileCopyrightText: 2026 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! All hosts provide a debug capability: the ability to send messages to the outside
//! world, without recording the message in the PVM state itself.

use tezos_smart_rollup_core::SmartRollupCore;

/// Core capability required to expose the SDK's debug functionality for a given host.
pub trait HostDebug {
    /// Write message to debug log.
    fn write_debug(&self, msg: &str);
}

impl<Host: SmartRollupCore> HostDebug for Host {
    fn write_debug(&self, msg: &str) {
        // SAFETY: msg corresponds to a valid pointer of bytes for the given length
        unsafe { SmartRollupCore::write_debug(self, msg.as_ptr(), msg.len()) };
    }
}
