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
use wasmer::{Engine, Features, Module, NativeEngineExt, Store, Target};
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
        let mut features = Features::new();
        features
            .bulk_memory(false)
            .memory64(false)
            .module_linking(false)
            .multi_memory(false)
            .multi_value(false)
            .reference_types(true)
            .simd(true)
            .tail_call(false)
            .threads(false);

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
