Entrypoint definition for Tezos Smart Rollup kernels.

A kernel must expose an `extern "C" fn kernel_run();` entrypoint, which is called on a loop
by the runtime.  The kernel *yields* to the runtime by returning out of
`kernel_run`.

There is a limit on how many computation ticks a kernel may perform per entry. It is
called a number of times per non-empty level.  The kernel must take care not to perform
arbitrarily long computations, to avoid breaching the computation limit.
