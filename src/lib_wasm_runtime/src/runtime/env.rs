// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! State manipulated by the host functions (for kernels executed with Wasmer).

use wasmer::Memory;

use crate::host::Host;

pub struct Env {
    memory: Option<Memory>,
    host: Host,
}

// `Env` implementing `Send` is a requirement from Wasmer to wrap it in a `FunctionEnv`.
// This way, instances created by Wasmer are thread-safe out of the box.
//
// It turns out, we don’t need to be thread-safe because
//   - Wasmer executes WASM programs in the thread it is started
//   - We don’t have a multi-threaded kernel
//
// Sadly, there is not an alternative version of `FunctionEnv` that reflects this use case.
// Instead, we declare `Env` thread, and it is safe as long as no threads are thrown in the mix.
//
// Note: The only reason why `Env` is not thread safe is because `EvmTree` is a wrapper around an
// OCaml `Value`. But this `Value` has no reason to be garbaged collected because it always comes
// from an OCaml function calling our Rust WASM Runtime, and keeping a reference to this EVM tree
// until the WASM runtime returns.
unsafe impl Send for Env {}

impl Env {
    pub fn new(host: Host) -> Self {
        Env { memory: None, host }
    }

    pub fn host(&self) -> &Host {
        &self.host
    }

    pub fn mut_host(&mut self) -> &mut Host {
        &mut self.host
    }

    pub fn set_memory(&mut self, memory: &Memory) {
        self.memory = Some(memory.clone())
    }

    pub fn memory(&self) -> &Memory {
        self.memory.as_ref().unwrap()
    }
}
