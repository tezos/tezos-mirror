# Kernel test ressources

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
