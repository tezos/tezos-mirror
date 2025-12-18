// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Abstraction layer hiding how a kernel is concretely executed.

mod env;
mod host_funcs;

use std::{cell::RefCell, rc::Rc};

pub use crate::host::{Host, InputsBuffer};
use crate::{
    api::{Kernel, KernelsCache},
    bindings::{self, end_span, start_span},
    constants::KERNEL,
    host::RuntimeVersion,
    types::{ContextHash, EvmTree},
};
pub use env::Env;
use log::trace;
use ocaml::Error;
use runtime_ebisu::internal_runtime::InternalRuntime as EbisuInternalRuntime;
use runtime_farfadet::internal_runtime::InternalRuntime as FarfadetInternalRuntime;
use wasmer::{Engine, Function, FunctionEnv, Instance, Store};

pub enum RunStatus {
    Done(EvmTree),
    PendingKernelUpgrade(EvmTree, InputsBuffer),
}

pub trait Runtime {
    fn host(&self) -> &Host;

    fn mut_host(&mut self) -> &mut Host;

    fn call(&mut self) -> Result<(), Error>;

    fn run(&mut self) -> Result<RunStatus, Error> {
        // If the initial state was computed by the WASM PVM, then the reboot flag is set (because
        // the first “reboot” is used to collect the shared inbox messages to fill the inputs
        // buffer. We don’t need this in our case, but in order to remain compatible, (1) we assume
        // the reboot flag can be set and we remove it, and (2) we will create it once the
        // computation is over.
        let _ = self.mut_host().reboot_requested()?;

        loop {
            start_span("run")?;

            self.call()?;

            end_span()?;

            if self.host().needs_kernel_reload() {
                return Ok(RunStatus::PendingKernelUpgrade(
                    self.host().tree().clone(),
                    self.host().inputs_buffer().clone(),
                ));
            }

            if !self.mut_host().reboot_requested()? {
                // We do that to be compatible with the WASM PVM, which creates it in the collect
                // stage.
                self.mut_host().create_reboot_flag()?;
                return Ok(RunStatus::Done(self.host().tree().clone()));
            }
        }
    }
}

struct WasmRuntime {
    store: Store,
    env: FunctionEnv<Env>,
    entrypoint: Function,
}

impl WasmRuntime {
    pub fn new(
        engine: &Engine,
        host: Host,
        kernel: &Kernel,
        entrypoint: &str,
    ) -> Result<Self, Error> {
        let mut store = Store::new(engine.clone());
        let engine_env = Env::new(host);
        let env = FunctionEnv::new(&mut store, engine_env);
        let imports = host_funcs::imports(&mut store, &env);
        let instance = Instance::new(&mut store, kernel.as_ref(), &imports)?;
        let memory = instance.exports.get_memory("memory")?.clone();
        env.as_mut(&mut store).set_memory(&memory);
        let entrypoint = instance.exports.get_function(entrypoint)?.clone();

        Ok(Self {
            store,
            env,
            entrypoint,
        })
    }
}

impl Runtime for WasmRuntime {
    fn host(&self) -> &Host {
        self.env.as_ref(&self.store).host()
    }

    fn mut_host(&mut self) -> &mut Host {
        self.env.as_mut(&mut self.store).mut_host()
    }

    fn call(&mut self) -> Result<(), Error> {
        trace!("WasmRuntime::call()");
        let _result = self.entrypoint.call(&mut self.store, &[])?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
enum NativeKernel {
    Bifrost,
    Calypso,
    Calypso2,
    Dionysus,
    DionysusR1,
    Ebisu,
    Farfadet,
    FarfadetR1,
}

impl NativeKernel {
    fn runtime_version(&self) -> RuntimeVersion {
        match self {
            Self::Bifrost => RuntimeVersion::V0,
            Self::Calypso => RuntimeVersion::V1,
            Self::Calypso2 => RuntimeVersion::V1,
            Self::Dionysus => RuntimeVersion::V1,
            Self::DionysusR1 => RuntimeVersion::V1,
            Self::Ebisu => RuntimeVersion::V1,
            Self::Farfadet => RuntimeVersion::V1,
            Self::FarfadetR1 => RuntimeVersion::V1,
        }
    }
}

const BIFROST_ROOT_HASH_HEX: &'static str =
    "7ff257e4f6ddb11766ec2266857c8fc75bd00e73230a7b598fec2bd9a68b6908";
const CALYPSO_ROOT_HASH_HEX: &'static str =
    "96114bf7a28e617a3788d8554aa24711b4b11f9c54cd0b12c00bc358beb814a7";
const CALYPSO2_ROOT_HASH_HEX: &'static str =
    "7b42577597504d6a705cdd56e59c770125223a0ffda471d70b463a2dc2d5f84f";
const DIONYSUS_ROOT_HASH_HEX: &'static str =
    "2214b77edf321b0ed41cc3a1028934299c4b94e0687b06e5239cc0b4eb31417f";
const DIONYSUS_R1_ROOT_HASH_HEX: &'static str =
    "1f533dbc6404cf6b05c8df6b6b879f96299fb0d6b661d26152ce3297bc22d550";
const EBISU_ROOT_HASH_HEX: &'static str =
    "8eb91f4b0955a02d394565b31cf806a3d281f1f1d7fed709f0b7af29c7b53996";
const FARFADET_ROOT_HASH_HEX: &'static str =
    "002ce9946e42dad48751bf81120f26d44bd86aa2e386e9015a0c2e8b0aefb89c";
const FARFADET_R1_ROOT_HASH_HEX: &'static str =
    "7a5e3327696fb0869c8de96888ad672acfaebb6d6ff23f43df75ae535fae1e5f";

impl NativeKernel {
    fn of_root_hash(root_hash: &ContextHash) -> Option<NativeKernel> {
        let root_hash_hex = hex::encode(root_hash.as_bytes());
        trace!("kernel root hash {root_hash_hex}");

        match root_hash_hex.as_str() {
            BIFROST_ROOT_HASH_HEX => Some(NativeKernel::Bifrost),
            CALYPSO_ROOT_HASH_HEX => Some(NativeKernel::Calypso),
            CALYPSO2_ROOT_HASH_HEX => Some(NativeKernel::Calypso2),
            DIONYSUS_ROOT_HASH_HEX => Some(NativeKernel::Dionysus),
            DIONYSUS_R1_ROOT_HASH_HEX => Some(NativeKernel::DionysusR1),
            EBISU_ROOT_HASH_HEX => Some(NativeKernel::Ebisu),
            FARFADET_ROOT_HASH_HEX => Some(NativeKernel::Farfadet),
            FARFADET_R1_ROOT_HASH_HEX => Some(NativeKernel::FarfadetR1),
            _ => None,
        }
    }
}

pub struct NativeRuntime {
    variant: NativeKernel,
    entrypoint: String,
    host: Host,
}

impl Runtime for NativeRuntime {
    fn host(&self) -> &Host {
        &self.host
    }

    fn mut_host(&mut self) -> &mut Host {
        &mut self.host
    }

    fn call(&mut self) -> Result<(), Error> {
        match (self.entrypoint.as_str(), self.variant) {
            ("kernel_run", NativeKernel::Bifrost) => {
                trace!("bifrost::kernel_loop");
                kernel_bifrost::kernel_loop(self.mut_host());
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Bifrost) => {
                trace!("bifrost::populate_delayed_inbox");
                kernel_bifrost::evm_node_entrypoint::populate_delayed_inbox(self.mut_host());
                Ok(())
            }
            ("kernel_run", NativeKernel::Calypso) => {
                trace!("calypso::kernel_loop");
                kernel_calypso::kernel_loop(self.mut_host());
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Calypso) => {
                trace!("calypso::populate_delayed_inbox");
                kernel_calypso::evm_node_entrypoint::populate_delayed_inbox(self.mut_host());
                Ok(())
            }
            ("kernel_run", NativeKernel::Calypso2) => {
                trace!("calypso2::kernel_loop");
                kernel_calypso2::kernel_loop(self.mut_host());
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Calypso2) => {
                trace!("calypso2::populate_delayed_inbox");
                kernel_calypso2::evm_node_entrypoint::populate_delayed_inbox(self.mut_host());
                Ok(())
            }
            ("kernel_run", NativeKernel::Dionysus) => {
                trace!("dionysus::kernel_loop");
                kernel_dionysus::kernel_loop(self.mut_host());
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Dionysus) => {
                trace!("dionysus::populate_delayed_inbox");
                kernel_dionysus::evm_node_entrypoint::populate_delayed_inbox(self.mut_host());
                Ok(())
            }
            ("kernel_run", NativeKernel::DionysusR1) => {
                trace!("dionysus_r1::kernel_loop");
                kernel_dionysus_r1::kernel_loop(self.mut_host());
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::DionysusR1) => {
                trace!("dionysus_r1::populate_delayed_inbox");
                kernel_dionysus_r1::evm_node_entrypoint::populate_delayed_inbox(self.mut_host());
                Ok(())
            }
            ("kernel_run", NativeKernel::Ebisu) => {
                trace!("ebisu::kernel_loop");
                let hasher = self.host().hasher();
                kernel_ebisu::kernel(self.mut_host(), hasher);
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Ebisu) => {
                trace!("ebisu::populate_delayed_inbox");
                let hasher = self.host().hasher();
                kernel_ebisu::evm_node_entrypoint::populate_delayed_inbox_with_durable_storage(
                    self.mut_host(),
                    hasher,
                );
                Ok(())
            }
            ("kernel_run", NativeKernel::Farfadet) => {
                trace!("farfadet::kernel_loop");
                let hasher = self.host().hasher();
                kernel_farfadet::kernel(self.mut_host(), hasher);
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::Farfadet) => {
                trace!("farfadet::populate_delayed_inbox");
                let hasher = self.host().hasher();
                kernel_farfadet::evm_node_entrypoint::populate_delayed_inbox_with_durable_storage(
                    self.mut_host(),
                    hasher,
                );
                Ok(())
            }
            ("single_tx_execution", NativeKernel::Farfadet) => {
                trace!("farfadet::single_tx_execution");
                let hasher = self.host().hasher();
                kernel_farfadet::evm_node_entrypoint::single_tx_execution_fn(
                    self.mut_host(),
                    hasher,
                );
                Ok(())
            }
            ("assemble_block", NativeKernel::Farfadet) => {
                trace!("farfadet::assemble_block");
                let hasher = self.host().hasher();
                kernel_farfadet::evm_node_entrypoint::assemble_block_fn(self.mut_host(), hasher);
                Ok(())
            }
            ("kernel_run", NativeKernel::FarfadetR1) => {
                trace!("farfadet_r1::kernel_loop");
                let hasher = self.host().hasher();
                kernel_farfadet_r1::kernel(self.mut_host(), hasher);
                Ok(())
            }
            ("populate_delayed_inbox", NativeKernel::FarfadetR1) => {
                trace!("farfadet_r1::populate_delayed_inbox");
                let hasher = self.host().hasher();
                kernel_farfadet_r1::evm_node_entrypoint::populate_delayed_inbox_with_durable_storage(
                    self.mut_host(),
                    hasher,
                );
                Ok(())
            }
            ("single_tx_execution", NativeKernel::FarfadetR1) => {
                trace!("farfadet_r1::single_tx_execution");
                let hasher = self.host().hasher();
                kernel_farfadet_r1::evm_node_entrypoint::single_tx_execution_fn(
                    self.mut_host(),
                    hasher,
                );
                Ok(())
            }
            ("assemble_block", NativeKernel::FarfadetR1) => {
                trace!("farfadet_r1::assemble_block");
                let hasher = self.host().hasher();
                kernel_farfadet_r1::evm_node_entrypoint::assemble_block_fn(self.mut_host(), hasher);
                Ok(())
            }
            (missing_entrypoint, _) => todo!("entrypoint {missing_entrypoint} not covered yet"),
        }
    }
}
/// Returns an appropriate runtime that can be used to execute the current kernel.
pub fn load_runtime(
    engine: &Engine,
    kernels_cache: &mut KernelsCache,
    mut host: Host,
    entrypoint: &str,
    native_execution: bool,
) -> Result<Box<dyn Runtime>, Error> {
    let root_hash = bindings::store_get_hash(&host.tree(), KERNEL)?;
    match NativeKernel::of_root_hash(&root_hash) {
        Some(kernel) if native_execution => {
            host.version = kernel.runtime_version();
            Ok(Box::new(NativeRuntime {
                variant: kernel,
                host,
                entrypoint: entrypoint.to_owned(),
            }))
        }
        Some(_) | None => {
            let (kernel, _) = kernels_cache.load(&engine, &host.tree())?;
            let runtime = WasmRuntime::new(&engine, host, &kernel, entrypoint)?;
            Ok(Box::new(runtime))
        }
    }
}
