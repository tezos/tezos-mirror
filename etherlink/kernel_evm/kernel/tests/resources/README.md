# Kernel test ressources

## debug_kernel.wasm

The kernel `debug_kernel.wasm` contains the code to a simple debug kernel that says hello. It's a useful ressource that can be used to test a kernel upgrade with the `wasm-debugger` for instance.

To generate it you will need to look at the code of the test `should_run_debug_kernel` that can be found at `src/lib_scoru_wasm/test/test_wasm_pvm.ml`.

It contains a small kernel in WAT:

```
(module
 (import "smart_rollup_core" "write_debug"
         (func $write_debug (param i32 i32)))
 ;; Durable keys
 (data (i32.const 100) "hello")
 (memory 1)
 (export "mem" (memory 0))
 (func (export "kernel_run")
       (local $hello_address i32)
       (local $hello_length i32)

       (local.set $hello_address (i32.const 100))
       (local.set $hello_length (i32.const 5))

       (call $write_debug (local.get $hello_address) (local.get $hello_length))
       (nop)
       )
 )
```

To convert it to a WASM file, use `wat2wasm`:

```
> wat2wasm debug_kernel.wat -o debug_kernel.wasm
```

To generate the associated preimages hashes:

```
> smart-rollup-installer get-reveal-installer --output debug_kernel_installer.wasm --upgrade-to debug_kernel.wasm --preimages-dir preimages
```

## failed_migration.wasm

The kernel `failed_migration.wasm` contains the code of a kernel that:
1. Bumps the storage version to trigger a migration.
2. Fails during the migration step
This triggers a fallback mechanism to the kernel before the upgrade.

Because the `STORAGE_VERSION` is hardcoded in `failed_migration.wasm`, this blob
needs to be updated everytime we bump the `STORAGE_VERSION` to trigger the migration.

Example:
- Bump the storage version:
```
diff --git a/etherlink/kernel_evm/kernel/src/storage.rs b/etherlink/kernel_evm/kernel/src/storage.rs
index 560c736546..a230a84234 100644
--- a/etherlink/kernel_evm/kernel/src/storage.rs
+++ b/etherlink/kernel_evm/kernel/src/storage.rs
@@ -31,7 +31,7 @@ use tezos_ethereum::wei::Wei;

 use primitive_types::{H160, H256, U256};

-pub const STORAGE_VERSION: u64 = 8;
+pub const STORAGE_VERSION: u64 = 9;
 pub const STORAGE_VERSION_PATH: RefPath = RefPath::assert_from(b"/evm/storage_version");

 const KERNEL_VERSION_PATH: RefPath = RefPath::assert_from(b"/evm/kernel_version");
```
- Fail during the migration:
```
diff --git a/etherlink/kernel_evm/kernel/src/migration.rs b/etherlink/kernel_evm/kernel/src/migration.rs
index a7473e084e..faccb18ff9 100644
--- a/etherlink/kernel_evm/kernel/src/migration.rs
+++ b/etherlink/kernel_evm/kernel/src/migration.rs
@@ -45,6 +45,7 @@ fn migration<Host: Runtime>(host: &mut Host) -> anyhow::Result<MigrationStatus>
     let current_version = read_storage_version(host)?;
     if STORAGE_VERSION == current_version + 1 {
         // MIGRATION CODE - START
+        anyhow::bail!("Erreur");
         allow_path_not_found(
             host.store_delete(&RefPath::assert_from(b"/evm/blueprints/last")),
         )?;
```
- Compile the kernel and moves it to `failed_migration.wasm`:
```
$ make -f etherlink.mk evm_kernel.wasm
$ cp evm_kernel.wasm etherlink/kernel_evm/kernel/tests/resources/failed_migration.wasm
```
- Commit the modified `failed_migration.wasm` and restore other changes:
```
$ git add etherlink/kernel_evm/kernel/tests/resources/failed_migration.wasm
$ git commit -m "Bump failed_migration"
$ git restore etherlink
```

## ghostnet_evm_kernel.wasm

The kernel `ghostnet_evm_kernel.wasm` is a compiled version of the latest
released kernel described in [CHANGES_KERNEL.md](../../../../CHANGES_KERNEL.md).
It is used for migration tests with the current kernel.

It can be reproduced easily with (from the root of the repository):
```
make -f kernels.ml evm_kernel.wasm
cp evm_kernel.wasm etherlink/kernel_evm/kernel/tests/resources/ghostnet_evm_kernel.wasm
```
