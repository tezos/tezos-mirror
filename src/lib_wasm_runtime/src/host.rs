// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Low-level interactions with the persistent state manipulated by the kernel
//! (starting with the durable storage).

use std::collections::VecDeque;

use ocaml::Error;

use crate::{
    bindings,
    types::{EvmTree, OCamlString, SmartRollupAddress},
};

#[derive(Clone)]
pub struct InputsBuffer {
    inputs: VecDeque<Vec<u8>>,
    level: u32,
    next_message: u32,
}

pub struct Input {
    pub level: u32,
    pub index: u32,
    pub payload: Vec<u8>,
}

impl InputsBuffer {
    pub fn new(level: u32, inputs: Vec<OCamlString>) -> Self {
        InputsBuffer {
            inputs: inputs
                .into_iter()
                .map(|x| x.as_bytes().to_owned())
                .collect(),
            level,
            next_message: 0,
        }
    }

    pub fn next_input(&mut self) -> Option<Input> {
        self.inputs.pop_front().map(|payload| {
            let res = Input {
                level: self.level,
                index: self.next_message,
                payload,
            };
            self.next_message += 1;
            res
        })
    }
}

pub struct Host {
    inputs_buffer: InputsBuffer,
    tree: EvmTree,
    rollup_address: SmartRollupAddress,
    needs_kernel_reload: bool,
    preimages_dir: OCamlString,
    preimages_endpoint: Option<OCamlString>,
}

impl Host {
    pub fn new(
        tree: &EvmTree,
        rollup_address: SmartRollupAddress,
        inputs_buffer: InputsBuffer,
        preimages_dir: OCamlString,
        preimages_endpoint: Option<OCamlString>,
    ) -> Self {
        Host {
            inputs_buffer,
            tree: tree.clone(),
            rollup_address,
            needs_kernel_reload: false,
            preimages_dir,
            preimages_endpoint,
        }
    }

    pub fn preimages_dir(&self) -> &str {
        self.preimages_dir.as_str()
    }

    pub fn preimages_endpoint(&self) -> Option<&str> {
        self.preimages_endpoint.as_ref().map(|s| s.as_str())
    }

    pub fn request_kernel_reload(&mut self) {
        self.needs_kernel_reload = true;
    }

    pub fn needs_kernel_reload(&self) -> bool {
        self.needs_kernel_reload
    }

    pub fn rollup_address(&self) -> &SmartRollupAddress {
        &self.rollup_address
    }

    pub fn tree(&self) -> &EvmTree {
        &self.tree
    }

    pub fn set_tree(&mut self, evm_tree: EvmTree) {
        self.tree = evm_tree;
    }

    pub fn write_debug(&self, msg: &[u8]) {
        print!("{}", String::from_utf8_lossy(msg))
    }

    pub fn next_input(&mut self) -> Option<Input> {
        self.inputs_buffer.next_input()
    }

    pub fn inputs_buffer(&self) -> &InputsBuffer {
        &self.inputs_buffer
    }

    pub fn reboot_requested(&mut self) -> Result<bool, Error> {
        let (reboot, evm_tree) = bindings::check_reboot_flag(&self.tree)?;
        self.tree = evm_tree;

        Ok(reboot)
    }

    pub fn create_reboot_flag(&mut self) -> Result<(), Error> {
        let (evm_tree, _) = bindings::store_write(&self.tree, "/kernel/env/reboot", 0, &[])?;
        self.tree = evm_tree;

        Ok(())
    }
}
