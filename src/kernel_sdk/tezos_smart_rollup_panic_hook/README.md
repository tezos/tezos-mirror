# Tezos Smart Rollup Kernel SDK - panic hook

The panic hook may be used to capture kernel panics, and write them to the debug log. Execution is then aborted, to ensure no further issues could arise.

To disable execution abortion (which prevents panic unwinding) disable the `abort` feature flag.

To prevent writing the panic info to the debug log (which requires an allocator for formatting) disable the `debug` feature flag.
