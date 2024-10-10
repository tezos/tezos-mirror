// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Abstraction layer hiding how a kernel is concretely executed.

mod env;
mod host_funcs;

pub use crate::host::{Host, InputsBuffer};
use crate::{
    api::{Kernel, KernelsCache},
    types::EvmTree,
};
pub use env::Env;
use ocaml::Error;
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

        // We don’t modify the reboot counter, so the kernel will never print the `Kernel
        // Invocation` header. We print it ourselves instead.
        self.host()
            .write_debug("------------------ Kernel Invocation ------------------\n".as_bytes());

        loop {
            self.call()?;

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
        let _result = self.entrypoint.call(&mut self.store, &[])?;
        Ok(())
    }
}

/// Returns an appropriate runtime that can be used to execute the current kernel.
pub fn load_runtime(
    engine: &Engine,
    kernels_cache: &mut KernelsCache,
    host: Host,
    entrypoint: &str,
) -> Result<Box<dyn Runtime>, Error> {
    let kernel = kernels_cache.load(&engine, host.tree())?;
    let runtime = WasmRuntime::new(&engine, host, kernel, entrypoint)?;
    Ok(Box::new(runtime))
}
