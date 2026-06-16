Mock runtime provides a host that can used in integration & unit testing of kernels for Tezos Smart Rollups.

To do so, [`MockHost`] implements the [`SmartRollupCore`] trait - allowing
it to be passed as an argument to any function, where the argument is
required to implement either `SmartRollupCore` or one of the 'safe api' traits available in
[tezos_smart_rollup_host].

# Example

Take a simple kernel that will count the number of times it is called, rebooting
at most 500 times per level.

```
use tezos_smart_rollup_mock::MockHost;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::wasm::WasmHost;

const COUNTER_PATH: RefPath = RefPath::assert_from(b"/counter");
const COUNTER_SIZE: usize = core::mem::size_of::<u32>();

// Kernel entrypoint, to count the number of times called.
// After 1000 calls at a given level, will restart at the
// next level.
fn count_calls<Host>(host: &mut Host)
where
  Host: StorageV1 + WasmHost
{
  let mut counter = read_counter(host);

  counter += 1;

  // A kernel can reboot up to 1000 times per level.
  if counter % 1000 != 0 {
    host.mark_for_reboot().expect("Marking for reboot failed.");
  }

  host.store_write(&COUNTER_PATH, &counter.to_le_bytes(), 0)
      .expect("Failed to write counter to storage.")
}

// Reads `COUNTER_PATH` as `u32` in little-endian.
// - returns 0 if counter does not exist.
fn read_counter(host: &impl StorageV1) -> u32 {
  host.store_read(&COUNTER_PATH, 0, COUNTER_SIZE)
      .map(|bytes| bytes.try_into().unwrap_or_default())
      .map(u32::from_le_bytes)
      .unwrap_or_default()
}

// Run using mock host - every `run_level` should increment
// counter by 500.
let mut host = MockHost::default();
assert_eq!(0, read_counter(&host));

host.run_level(count_calls);
assert_eq!(1000, read_counter(&host));

host.run_level(count_calls);
assert_eq!(2000, read_counter(&host));
```

[`SmartRollupCore`]: tezos_smart_rollup_core::smart_rollup_core::SmartRollupCore
