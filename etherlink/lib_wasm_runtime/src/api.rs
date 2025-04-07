// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Module containing the types and functions exposed to OCaml.

use crate::{
    bindings,
    host::Host,
    runtime::{self, InputsBuffer, RunStatus},
    types::{ContextHash, EvmTree, OCamlString, SmartRollupAddress},
};
use log::trace;
use ocaml::{Error, List, Pointer, Value};
use std::collections::BTreeMap;
use wasmer::{Engine, Features, Module, NativeEngineExt, Store, Target};
use wasmer_compiler_cranelift::Cranelift;

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

pub struct KernelsCache(BTreeMap<[u8; 32], Kernel>);

impl KernelsCache {
    pub fn new() -> Self {
        KernelsCache(BTreeMap::new())
    }

    pub fn miss(&self, hash: &ContextHash) -> bool {
        !self.0.contains_key(hash.as_bytes())
    }

    pub fn get(&self, hash: &ContextHash) -> &Kernel {
        self.0.get(hash.as_bytes()).unwrap()
    }
    pub fn insert(&mut self, hash: &ContextHash, kernel: Kernel) {
        self.0.insert(hash.as_bytes().to_owned(), kernel);
    }

    pub fn load(&mut self, engine: &Engine, evm_tree: &EvmTree) -> Result<&Kernel, Error> {
        const KERNEL_PATH: &'static str = "/kernel/boot.wasm";
        let hash = bindings::store_get_hash(evm_tree, &KERNEL_PATH)?;

        if self.miss(&hash) {
            trace!("KernelsCache::load cache miss");
            let code = bindings::read_value(&evm_tree, KERNEL_PATH)?;
            let kernel = Kernel::new(engine, code.as_bytes())?;
            let _previous = self.insert(&hash, kernel);
        }

        Ok(self.get(&hash))
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
    "context -> string -> string option -> bool -> string -> Irmin_context.tree -> bytes -> int32 -> string list -> Irmin_context.tree"
)]
pub fn wasm_runtime_run(
    mut ctxt: Pointer<Context>,
    preimages_dir: OCamlString,
    preimages_endpoint: Option<OCamlString>,
    native_execution: bool,
    entrypoint: OCamlString,
    mut tree: EvmTree,
    rollup_address: SmartRollupAddress,
    level: u32,
    inputs: List<OCamlString>,
) -> Result<EvmTree, Error> {
    let ctxt = ctxt.as_mut();
    let mut inputs_buffer = InputsBuffer::new(level, inputs.into_vec());

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
            RunStatus::Done(evm_tree) => return Ok(evm_tree),
            RunStatus::PendingKernelUpgrade(new_tree, remaining_inputs) => {
                tree = new_tree;
                inputs_buffer = remaining_inputs;
            }
        }
    }
}

#[ocaml::func]
#[ocaml::sig("context -> Irmin_context.tree -> unit")]
pub fn wasm_runtime_preload_kernel(mut ctxt: Pointer<Context>, tree: EvmTree) -> Result<(), Error> {
    let ctxt = ctxt.as_mut();
    let _kernel = ctxt.kernels_cache.load(&ctxt.engine, &tree)?;
    Ok(())
}
