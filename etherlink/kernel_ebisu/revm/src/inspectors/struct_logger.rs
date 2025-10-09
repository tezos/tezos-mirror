// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    database::EtherlinkVMDB,
    helpers::rlp::{append_option_canonical, append_u16_le, append_u64_le},
    Error,
};

use revm::{
    context::{ContextTr, JournalTr},
    inspector::inspectors::GasInspector,
    interpreter::{
        interpreter::ReturnDataImpl,
        interpreter_types::{Jumps, LoopControl, MemoryTr, StackTr},
        CallInputs, CallOutcome, CreateInputs, CreateOutcome, InstructionResult,
        Interpreter, InterpreterTypes,
    },
    primitives::{Address, Bytes, B256},
    state::AccountInfo,
    Database, Inspector,
};
use rlp::{Encodable, RlpStream};
use tezos_evm_logging::{log, tracing::instrument, Level::Debug};
use tezos_evm_runtime::runtime::Runtime;

use super::{
    storage::{
        store_return_value, store_struct_log, store_trace_failed, store_trace_gas,
    },
    StructStack,
};

#[derive(Debug)]
pub struct StructLoggerInput {
    pub config: StructLoggerConfig,
    pub transaction_hash: Option<B256>,
}

#[derive(Debug)]
pub struct StructLoggerConfig {
    pub enable_memory: bool,
    pub enable_return_data: bool,
    pub disable_stack: bool,
    pub disable_storage: bool,
}

#[derive(Debug)]
pub struct StorageMapItem {
    pub address: Address,
    pub index: B256,
    pub value: B256,
}

impl Encodable for StorageMapItem {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(3);
        stream.append(&primitive_types::H160(*self.address.0));
        stream.append(&primitive_types::H256(self.index.0));
        stream.append(&primitive_types::H256(self.value.0));
    }
}

#[derive(Debug)]
pub struct StructLog {
    pub pc: u64,
    pub opcode: u8,
    pub gas: u64,
    pub gas_cost: u64,
    pub depth: u16,
    pub error: Option<Vec<u8>>,
    pub stack: Option<Vec<B256>>,
    pub return_data: Option<Vec<u8>>,
    pub memory: Option<Vec<u8>>,
    pub storage: Option<Vec<StorageMapItem>>,
}

impl StructLog {
    #[allow(clippy::too_many_arguments)]
    pub fn minimal_structure(
        pc: u64,
        opcode: u8,
        gas: u64,
        depth: u16,
        stack: Option<Vec<B256>>,
        return_data: Option<Vec<u8>>,
        memory: Option<Vec<u8>>,
        storage: Option<Vec<StorageMapItem>>,
    ) -> Self {
        StructLog {
            pc,
            opcode,
            gas,
            gas_cost: 0,
            depth,
            error: None,
            stack,
            return_data,
            memory,
            storage,
        }
    }

    pub fn complete(self, depth: u16, gas_cost: u64, error: Option<Vec<u8>>) -> Self {
        StructLog {
            depth,
            gas_cost,
            error,
            ..self
        }
    }

    pub fn store(&self, host: &mut impl Runtime, transaction_hash: &Option<B256>) {
        store_struct_log(host, self, transaction_hash)
            .inspect_err(|err| {
                log!(host, Debug, "Storing call trace failed with: {err:?}")
            })
            .ok();
    }
}

impl Encodable for StructLog {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(10);
        append_u64_le(stream, &self.pc);
        stream.append(&self.opcode);
        append_u64_le(stream, &self.gas);
        append_u64_le(stream, &self.gas_cost);
        append_u16_le(stream, &self.depth);
        stream.append(&self.error);
        append_option_canonical(stream, &self.stack, |s, l| {
            let l: Vec<primitive_types::H256> = l
                .iter()
                .map(|stack_element| primitive_types::H256(stack_element.0))
                .collect();
            s.append_list(&l)
        });
        stream.append(&self.return_data);
        stream.append(&self.memory);
        append_option_canonical(stream, &self.storage, |s, l| s.append_list(l));
    }
}

pub struct StructLogger {
    config: StructLoggerConfig,
    pub transaction_hash: Option<B256>,
    struct_log: Option<StructLog>,
    execution_address: Address,
    gas_inspector: GasInspector,
}

impl StructLogger {
    pub fn new(config: StructLoggerConfig, transaction_hash: Option<B256>) -> Self {
        Self {
            config,
            transaction_hash,
            struct_log: None,
            execution_address: Address::ZERO,
            gas_inspector: GasInspector::new(),
        }
    }

    #[instrument(skip_all)]
    pub fn store_outcome<Host: Runtime>(
        host: &mut Host,
        is_success: bool,
        output: Option<&Bytes>,
        gas_used: u64,
        transaction_hash: Option<B256>,
    ) -> Result<(), Error> {
        store_trace_failed(host, is_success, &transaction_hash)?;
        store_trace_gas(host, gas_used, &transaction_hash)?;
        if let Some(return_value) = output {
            store_return_value(host, return_value, &transaction_hash)?;
        };
        Ok(())
    }
}

impl<'a, Host, CTX, INTR> Inspector<CTX, INTR> for StructLogger
where
    Host: Runtime + 'a,
    CTX: ContextTr<Db = EtherlinkVMDB<'a, Host>>,
    INTR: InterpreterTypes<
        Stack: StackTr + StructStack,
        ReturnData = ReturnDataImpl,
        Memory: MemoryTr,
    >,
{
    fn initialize_interp(&mut self, interp: &mut Interpreter<INTR>, _: &mut CTX) {
        self.gas_inspector.initialize_interp(&interp.gas);
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        self.gas_inspector.step(&interp.gas);
        let depth = context.journal_mut().depth() as u16 + 1;
        let opcode = interp.bytecode.opcode();
        let is_sstore = opcode == 0x55;

        let struct_log = StructLog::minimal_structure(
            interp.bytecode.pc() as u64,
            opcode,
            interp.gas.remaining(),
            depth,
            if !self.config.disable_stack {
                Some(interp.stack.to_structured_stack())
            } else {
                None
            },
            if self.config.enable_return_data {
                Some(interp.return_data.0.to_vec())
            } else {
                None
            },
            if self.config.enable_memory {
                Some(interp.memory.slice(0..interp.memory.size()).to_vec())
            } else {
                None
            },
            if !self.config.disable_storage {
                let storage = if !is_sstore {
                    // Do not return [None], otherwise field will look like:
                    // "storage" : null, <-- this should happen if disable_storage = true
                    // instead of:
                    // "storage" : [],
                    vec![]
                } else {
                    let address = self.execution_address;
                    let mut stack = interp.stack.to_structured_stack();
                    let index = stack.pop().unwrap_or_default();
                    let value = stack.pop().unwrap_or_default();
                    let storage_map_item = StorageMapItem {
                        address,
                        index,
                        value,
                    };
                    vec![storage_map_item]
                };
                Some(storage)
            } else {
                None
            },
        );

        self.struct_log = Some(struct_log);
    }

    fn step_end(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        let depth = context.journal_mut().depth() as u16;
        if let Some(struct_log) = std::mem::take(&mut self.struct_log) {
            self.gas_inspector.step_end(&mut interp.gas);
            let error = interp
                .bytecode
                .action()
                .as_ref()
                .and_then(|a| a.instruction_result())
                .filter(|ir| {
                    !matches!(
                        ir,
                        InstructionResult::Stop
                            | InstructionResult::Return
                            | InstructionResult::SelfDestruct
                    )
                })
                .map(|ir| format!("{ir:?}").as_bytes().to_vec());

            let struct_log =
                struct_log.complete(depth, self.gas_inspector.last_gas_cost(), error);
            struct_log.store(context.db_mut().host, &self.transaction_hash);
        }
    }

    fn call(&mut self, _: &mut CTX, inputs: &mut CallInputs) -> Option<CallOutcome> {
        self.execution_address = inputs.bytecode_address;
        // NB: Always return [None] or else the result of the call will be overriden.
        None
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        if let Ok(Some(AccountInfo { nonce, .. })) = context.db_mut().basic(inputs.caller)
        {
            self.execution_address = inputs.created_address(nonce);
        }
        // NB: Always return [None] or else the result of the create will be overriden.
        None
    }

    fn call_end(&mut self, _: &mut CTX, _: &CallInputs, outcome: &mut CallOutcome) {
        self.gas_inspector.call_end(outcome);
    }

    fn create_end(&mut self, _: &mut CTX, _: &CreateInputs, outcome: &mut CreateOutcome) {
        self.gas_inspector.create_end(outcome);
    }
}
