// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Module containing the types and functions exposed to OCaml.

use crate::{
    bindings::{self, end_span, init_spans},
    host::Host,
    runtime::{self, InputsBuffer, RunStatus},
    types::{ContextHash, EvmTree, OCamlString, OpenTelemetryScope, SmartRollupAddress},
};
use log::trace;
use ocaml::{Error, List, Pointer, Value};
use std::sync::OnceLock;
use std::{
    collections::BTreeMap,
    sync::{Arc, RwLock, RwLockReadGuard},
};
use wasmer::sys::{Features, NativeEngineExt, Target};
use wasmer::{Engine, Module, Store};
use wasmer_compiler_cranelift::Cranelift;

static TRACE_HOST_FUNS: OnceLock<bool> = OnceLock::new();

fn set_host_tracing(trace_host_funs: bool) {
    TRACE_HOST_FUNS.set(trace_host_funs);
}

pub fn trace_host_funs_enabled() -> bool {
    *(TRACE_HOST_FUNS.get().unwrap_or(&false))
}

pub struct Kernel(Module);

impl Kernel {
    pub fn new(engine: &Engine, code: &[u8]) -> Result<Kernel, Error> {
        let store = Store::new(engine.clone());
        let module = Module::new(&store, code)?;
        Ok(Kernel(module))
    }

    pub fn as_ref(&self) -> &Module {
        &self.0
    }
}

pub struct KernelsCache(Arc<RwLock<BTreeMap<[u8; 32], Arc<Kernel>>>>);

impl KernelsCache {
    pub fn new() -> Self {
        KernelsCache(Arc::new(RwLock::new(BTreeMap::new())))
    }

    pub fn miss(&self, hash: &ContextHash) -> bool {
        !self.0.read().unwrap().contains_key(hash.as_bytes())
    }

    pub fn get<'a>(&'a self, hash: &ContextHash) -> Arc<Kernel> {
        self.0.read().unwrap().get(hash.as_bytes()).unwrap().clone()
    }

    pub fn insert(&mut self, hash: &ContextHash, kernel: Kernel) {
        self.0
            .write()
            .unwrap()
            .insert(hash.as_bytes().to_owned(), Arc::new(kernel));
    }

    pub fn load(
        &mut self,
        engine: &Engine,
        evm_tree: &EvmTree,
    ) -> Result<(Arc<Kernel>, bool), Error> {
        const KERNEL_PATH: &'static str = "/kernel/boot.wasm";
        let hash = bindings::store_get_hash(evm_tree, &KERNEL_PATH)?;

        let missed = self.miss(&hash);

        if missed {
            trace!("KernelsCache::load cache miss");
            let code = bindings::read_value(&evm_tree, KERNEL_PATH)?;
            let kernel = Kernel::new(engine, code.as_bytes())?;
            let _previous = self.insert(&hash, kernel);
        }

        Ok((self.get(&hash), missed))
    }
}

#[ocaml::sig]
pub struct Context {
    engine: Engine,
    kernels_cache: KernelsCache,
}

ocaml::custom!(Context);

impl Context {
    pub fn new() -> Self {
        // Every field is listed on purpose, and as a struct literal rather
        // than through the `Features` setters. Two reasons. A literal without
        // `..` fails to compile when wasmer adds a proposal, which forces a
        // decision on it instead of inheriting whatever `Features::new()`
        // defaults to -- and that default moves between releases, 7.2.1 having
        // turned `extended_const` on. The setters, meanwhile, are not
        // independent: `reference_types(true)` also enables `bulk_memory`, and
        // `bulk_memory(false)` also disables `reference_types`, so a chain's
        // outcome depends on the order it is written in.
        //
        // `bulk_memory` is enabled: the Octez WebAssembly interpreter
        // implements it (`MemoryCopy`, `MemoryFill`, `MemoryInit`, `DataDrop`
        // in src/lib_webassembly), rustc emits `memory.copy` and `memory.fill`
        // for wasm32-unknown-unknown by default, and every Etherlink kernel
        // therefore contains them.
        let features = Features {
            bulk_memory: true,
            exceptions: false,
            extended_const: false,
            memory64: false,
            module_linking: false,
            multi_memory: false,
            multi_value: false,
            reference_types: true,
            relaxed_simd: false,
            simd: true,
            tail_call: false,
            threads: false,
            wide_arithmetic: false,
        };

        let config = Cranelift::new();
        let engine = NativeEngineExt::new(Box::new(config), Target::default(), features);

        Context {
            engine,
            kernels_cache: KernelsCache::new(),
        }
    }
}

#[ocaml::func]
#[ocaml::sig("unit -> unit")]
pub fn wasm_runtime_logger_init() {
    env_logger::init();
}

#[ocaml::func]
#[ocaml::sig("unit -> context")]
pub fn wasm_runtime_new_context() -> Pointer<Context> {
    Pointer::alloc_custom(Context::new())
}

#[ocaml::func]
#[ocaml::sig(
    "context -> string -> string option -> bool -> string -> Wasm_runtime_callbacks.scope -> bool -> Irmin_context.tree -> bytes -> int32 -> string list -> Irmin_context.tree"
)]
pub fn wasm_runtime_run(
    mut ctxt: Pointer<Context>,
    preimages_dir: OCamlString,
    preimages_endpoint: Option<OCamlString>,
    native_execution: bool,
    entrypoint: OCamlString,
    otel_scope: OpenTelemetryScope,
    trace_host_funs: bool,
    mut tree: EvmTree,
    rollup_address: SmartRollupAddress,
    level: u32,
    inputs: List<OCamlString>,
) -> Result<EvmTree, Error> {
    let ctxt = ctxt.as_mut();
    let mut inputs_buffer = InputsBuffer::new(level, inputs.into_vec());
    set_host_tracing(trace_host_funs);
    init_spans(&otel_scope, "wasm_runtime_run")?;
    loop {
        let host = Host::new(
            &tree,
            rollup_address,
            inputs_buffer,
            preimages_dir.clone(),
            preimages_endpoint.clone(),
        );
        let mut runtime = runtime::load_runtime(
            &ctxt.engine,
            &mut ctxt.kernels_cache,
            host,
            entrypoint.as_str(),
            native_execution,
        )?;

        match runtime.run()? {
            RunStatus::Done(evm_tree) => {
                end_span()?;
                return Ok(evm_tree);
            }
            RunStatus::PendingKernelUpgrade(new_tree, remaining_inputs) => {
                tree = new_tree;
                inputs_buffer = remaining_inputs;
            }
        }
    }
}

#[ocaml::func]
#[ocaml::sig("context -> Irmin_context.tree -> bool")]
pub fn wasm_runtime_preload_kernel(
    mut ctxt: Pointer<Context>,
    tree: EvmTree,
) -> Result<bool, Error> {
    let ctxt = ctxt.as_mut();
    let (_kernel, loaded) = ctxt.kernels_cache.load(&ctxt.engine, &tree)?;
    Ok(loaded)
}

#[cfg(test)]
mod tests {
    use super::*;

    // Modules in binary form on purpose: this exercises the Wasmer validator
    // directly rather than `wat`, whose support for a proposal is independent
    // from the features the engine enables.

    // (module (global i32 (i32.const 3)))
    const CONST_GLOBAL: &[u8] = b"\x00asm\x01\x00\x00\x00\x06\x06\x01\x7f\x00\x41\x03\x0b";

    // (module (global i32 (i32.add (i32.const 1) (i32.const 2))))
    const EXTENDED_CONST_GLOBAL: &[u8] =
        b"\x00asm\x01\x00\x00\x00\x06\x09\x01\x7f\x00\x41\x01\x41\x02\x6a\x0b";

    // (module (memory 1) (func (memory.copy (i32.const 0) (i32.const 1) (i32.const 2))))
    const BULK_MEMORY: &[u8] = b"\x00asm\x01\x00\x00\x00\x01\x04\x01\x60\x00\x00\x03\x02\x01\x00\x05\x03\x01\x00\x01\x0a\x0e\x01\x0c\x00\x41\x00\x41\x01\x41\x02\xfc\x0a\x00\x00\x0b";

    fn compiles(code: &[u8]) -> bool {
        let ctxt = Context::new();
        Kernel::new(&ctxt.engine, code).is_ok()
    }

    /// The proposals this runtime accepts must match the ones the Octez
    /// WebAssembly interpreter implements: a module accepted here but rejected
    /// there makes the two disagree on whether a kernel is valid, and one
    /// rejected here but accepted there stops the node executing kernels at
    /// all. Both directions have gone wrong, so both are pinned.
    #[test]
    fn wasm_proposals() {
        assert!(compiles(CONST_GLOBAL), "plain constant global initialiser");
        // Accepted: the interpreter implements bulk memory, and rustc emits
        // memory.copy for wasm32-unknown-unknown in every kernel.
        assert!(compiles(BULK_MEMORY), "bulk memory instruction");
        // Rejected: the interpreter does not implement extended const.
        assert!(
            !compiles(EXTENDED_CONST_GLOBAL),
            "extended constant expression must be rejected"
        );
    }
}
