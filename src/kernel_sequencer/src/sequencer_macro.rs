// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Derive `kernel_run` entrypoint.
///
/// ```no_run
/// # use tezos_smart_rollup_host::runtime::Runtime;
/// use kernel_sequencer::sequencer_kernel_entry;
///
/// fn run<Host: Runtime>(host: &mut Host) {
///   host.write_debug("Hello Kernel");
/// }
///
/// sequencer_kernel_entry!(run);
/// ```
#[macro_export]
macro_rules! sequencer_kernel_entry {
    ($kernel_run: expr, $filter_behavior: expr) => {
        /// The `kernel_run` function is called by the wasm host at regular intervals.
        #[cfg(target_arch = "wasm32")]
        #[no_mangle]
        pub extern "C" fn kernel_run() {
            use tezos_smart_rollup_core::rollup_host::RollupHost;
            let host = unsafe { RollupHost::new() }; // Runtime from the tezos sdk
            let mut host = $crate::sequencer_runtime::SequencerRuntime::new(host, $filter_behavior); // create a sequencer runtime that use the RollupHost runtime
            $kernel_run(&mut host)
        }
    };
    ($kernel_run: expr) => {
        sequencer_kernel_entry!($kernel_run, $crate::routing::FilterBehavior::AllowAll);
    };
}
