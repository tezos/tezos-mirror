// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#[cfg(feature = "native-kernel")]
#[doc(hidden)]
pub fn kernel_entrypoint_fn(user_kernel_fn: fn(&mut tezos_smart_rollup_mock::MockHost)) {
    let mut host = tezos_smart_rollup_mock::MockHost::default();

    tezos_smart_rollup_utils::native_cli::apply_cli_opts(&mut host);

    while !host.should_quit() {
        // TODO #6727: Capture and recover panics.
        user_kernel_fn(&mut host);
    }
}
