Provides *debug log* which can be written to, but does not affect the host state.

The result of writing to the debug log is *implementation specific* - it may, for
example, be written to a log file, or to `stdout` etc.

The `debug_msg` macro supports inline formatting:

```no_run
extern crate alloc;
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::debug::HostDebug;

fn log_runtime(host: &impl HostDebug) {
  debug_msg!(host, "Simple constant string");

  debug_msg!(host, "A format {} with argument {}", "test", 5);
}
```