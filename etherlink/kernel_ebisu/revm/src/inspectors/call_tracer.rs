// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::storage::store_call_trace;
use crate::{
    database::EtherlinkVMDB,
    helpers::rlp::{
        append_address, append_option_address, append_option_canonical,
        append_option_u64_le, append_u16_le, append_u256_le, append_u64_le,
    },
    precompiles::provider::EtherlinkPrecompiles,
};

use revm::{
    context::{ContextTr, CreateScheme, JournalTr, Transaction},
    interpreter::{
        gas::calculate_initial_tx_gas_for_tx, interpreter::ReturnDataImpl,
        interpreter_types::StackTr, CallInputs, CallOutcome, CallScheme, CreateInputs,
        CreateOutcome, Gas, InitialAndFloorGas, InstructionResult, InterpreterResult,
        InterpreterTypes,
    },
    primitives::{hardfork::SpecId, hash_map::HashMap, Address, Bytes, Log, B256, U256},
    Inspector,
};
use rlp::{Encodable, RlpStream};
use std::ops::Range;
use tezos_ethereum::Log as RlpLog;
use tezos_evm_logging::{log, Level::Debug};
use tezos_evm_runtime::runtime::Runtime;

#[derive(Debug)]
pub struct CallTracerConfig {
    pub only_top_call: bool,
    pub with_logs: bool,
}

#[derive(Debug)]
pub struct CallTracerInput {
    pub config: CallTracerConfig,
    pub transaction_hash: Option<B256>,
}

#[derive(Debug)]
pub struct CallTrace {
    type_: Vec<u8>,
    from: Address,
    /// `to` will be the created contract address if type is CREATE / CREATE2.
    to: Option<Address>,
    value: U256,
    /// `gas` will be [None] if no gas limit was provided.
    gas: Option<u64>,
    gas_used: u64,
    input: Vec<u8>,
    /// `output` will also be used in revert reason, if there's any.
    output: Option<Vec<u8>>,
    error: Option<Vec<u8>>,
    logs: Option<Vec<Log>>,
    /// `depth` is helpful to reconstruct the tree of call on the EVM node's side.
    depth: u16,
}

impl Encodable for CallTrace {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(11);
        stream.append(&self.type_);
        append_address(stream, &self.from);
        append_option_address(stream, &self.to);
        append_u256_le(stream, &self.value);
        append_option_u64_le(stream, &self.gas);
        append_u64_le(stream, &self.gas_used);
        stream.append(&self.input);
        stream.append(&self.output);
        stream.append(&self.error);
        let logs = self.logs.as_ref().map(|logs| {
            logs.iter()
                .map(|Log { address, data }| {
                    let topics = data
                        .topics()
                        .iter()
                        .map(|topic| primitive_types::H256(topic.0))
                        .collect();
                    RlpLog {
                        address: primitive_types::H160(*address.0),
                        topics,
                        data: data.data.to_vec(),
                    }
                })
                .collect::<Vec<RlpLog>>()
        });
        append_option_canonical(stream, &logs, |s, logs| s.append_list(logs));
        append_u16_le(stream, &self.depth);
    }
}

impl CallTrace {
    pub fn new_minimal_trace(
        type_: Vec<u8>,
        from: Address,
        value: U256,
        input: Vec<u8>,
        depth: u16,
    ) -> Self {
        Self {
            type_,
            from,
            value,
            gas_used: 0,
            input,
            to: None,
            gas: None,
            output: None,
            error: None,
            logs: None,
            depth,
        }
    }

    pub fn add_to(&mut self, to: Option<Address>) {
        self.to = to;
    }

    pub fn add_gas(&mut self, gas: Option<u64>) {
        self.gas = gas;
    }

    pub fn add_gas_used(&mut self, gas_used: u64) {
        self.gas_used = gas_used;
    }

    pub fn add_output(&mut self, output: Option<Vec<u8>>) {
        self.output = output;
    }

    pub fn add_error(&mut self, error: Option<Vec<u8>>) {
        self.error = error;
    }

    fn add_error_from_instruction_result(
        &mut self,
        instruction_result: &InstructionResult,
    ) {
        match instruction_result {
            InstructionResult::Stop
            | InstructionResult::Return
            | InstructionResult::SelfDestruct => (),
            InstructionResult::Revert => {
                // NB:
                // Strong dependency towards:
                // `etherlink/bin_node/lib_dev/encodings/tracer_types.ml`
                // We need to return "Reverted" so the `revertReason`
                // can be replaced by whatever the revert function outputs.
                self.add_error(Some("Reverted".into()))
            }
            instruction_result_error => {
                self.add_error(Some(format!("{instruction_result_error:?}").into()))
            }
        }
    }

    fn add_logs(&mut self, logs: Option<Vec<Log>>) {
        self.logs = logs;
    }

    pub fn store(&self, host: &mut impl Runtime, transaction_hash: &Option<B256>) {
        store_call_trace(host, self, transaction_hash)
            .inspect_err(|err| {
                log!(host, Debug, "Storing call trace failed with: {err:?}")
            })
            .ok();
    }
}

pub struct CallTracer {
    config: CallTracerConfig,
    precompiles: EtherlinkPrecompiles,
    call_trace: HashMap<u16, CallTrace>,
    pub transaction_hash: Option<B256>,
    initial_gas: u64,
    spec_id: SpecId,
}

impl CallTracer {
    pub fn new(
        config: CallTracerConfig,
        precompiles: EtherlinkPrecompiles,
        spec_id: SpecId,
        transaction_hash: Option<B256>,
    ) -> Self {
        Self {
            config,
            precompiles,
            call_trace: HashMap::with_capacity(1),
            transaction_hash,
            initial_gas: 0,
            spec_id,
        }
    }

    #[inline]
    pub fn tx_hash(&self) -> Option<B256> {
        self.transaction_hash
    }

    #[inline]
    fn set_call_trace(&mut self, depth: u16, call_trace: CallTrace) {
        self.call_trace.insert(depth, call_trace);
    }

    #[inline]
    fn set_initial_gas(&mut self, tx: impl Transaction) {
        let InitialAndFloorGas { initial_gas, .. } =
            calculate_initial_tx_gas_for_tx(tx, self.spec_id);
        self.initial_gas = initial_gas;
    }

    #[inline]
    fn clear(&mut self, depth: &u16) {
        self.call_trace.remove(depth);
    }

    fn end_transaction_layer<
        'a,
        Host: Runtime + 'a,
        CTX: ContextTr<Db = EtherlinkVMDB<'a, Host>>,
    >(
        &mut self,
        context: &mut CTX,
        gas_spent: u64,
        output: &Bytes,
        instruction_result: &InstructionResult,
    ) {
        let depth = context.journal().depth() as u16;

        if self.config.only_top_call && depth > 0 {
            return;
        }

        if let Some(call_trace) = self.call_trace.get_mut(&depth) {
            if self.config.with_logs {
                call_trace.add_logs(Some(context.journal_mut().take_logs()));
            }
            call_trace.add_gas_used(gas_spent + self.initial_gas);
            call_trace.add_output(Some(output.to_vec()));
            call_trace.add_error_from_instruction_result(instruction_result);
            call_trace.store(context.db_mut().host, &self.transaction_hash);

            self.clear(&depth);
        }
    }
}

impl<'a, Host, CTX, INTR> Inspector<CTX, INTR> for CallTracer
where
    Host: Runtime + 'a,
    CTX: ContextTr<Db = EtherlinkVMDB<'a, Host>>,
    INTR: InterpreterTypes<Stack: StackTr, ReturnData = ReturnDataImpl>,
{
    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        let depth = context.journal().depth() as u16;

        if self.config.only_top_call && depth > 0 {
            return None;
        }

        self.set_initial_gas(context.tx());

        let (type_, from) = match inputs.scheme {
            CallScheme::Call => ("CALL", inputs.caller),
            CallScheme::StaticCall => ("STATICCALL", inputs.caller),
            CallScheme::DelegateCall => ("DELEGATECALL", inputs.target_address),
            CallScheme::CallCode => ("CALLCODE", inputs.target_address),
        };

        let call_data = inputs.input.bytes(context);

        let mut call_trace = CallTrace::new_minimal_trace(
            type_.into(),
            from,
            inputs.value.get(),
            call_data.to_vec(),
            depth,
        );

        call_trace.add_to(Some(inputs.bytecode_address));
        call_trace.add_gas(Some(inputs.gas_limit + self.initial_gas));

        self.set_call_trace(depth, call_trace);

        if let Some(precompile) = self
            .precompiles
            .builtins
            .precompiles
            .get(&inputs.bytecode_address)
        {
            // Hack-ish behavior. In case the invoked address is a precompile we need to
            // pre-simulate its result because the `call_end` hook is never called when a
            // precompile contract is called.

            let memory_offset = Range { start: 0, end: 0 }; // Ignored.
            let mut outcome = match precompile.execute(&call_data, inputs.gas_limit) {
                Ok(result) => CallOutcome {
                    result: InterpreterResult {
                        result: InstructionResult::Return,
                        output: result.bytes,
                        gas: Gas::new_spent(result.gas_used),
                    },
                    memory_offset,
                },
                Err(_) => CallOutcome {
                    result: InterpreterResult {
                        result: InstructionResult::PrecompileError,
                        // No return data, indicates a precompile contract error.
                        output: Bytes::new(),
                        gas: Gas::new_spent(inputs.gas_limit),
                    },
                    memory_offset,
                },
            };

            <CallTracer as Inspector<CTX, INTR>>::call_end(
                self,
                context,
                inputs,
                &mut outcome,
            );

            return None;
        }

        // NB: Always return [None] or else the result of the call will be overriden.
        None
    }

    fn call_end(&mut self, context: &mut CTX, _: &CallInputs, outcome: &mut CallOutcome) {
        self.end_transaction_layer(
            context,
            outcome.gas().spent(),
            outcome.output(),
            outcome.instruction_result(),
        );
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        let depth = context.journal().depth() as u16;

        if self.config.only_top_call && depth > 0 {
            return None;
        }

        self.set_initial_gas(context.tx());

        let (type_, from) = match inputs.scheme {
            CreateScheme::Create => ("CREATE", inputs.caller),
            CreateScheme::Create2 { .. } => ("CREATE2", inputs.caller),
            // Impossible case on Etherlink:
            CreateScheme::Custom { .. } => ("CUSTOM", inputs.caller),
        };

        let mut call_trace = CallTrace::new_minimal_trace(
            type_.into(),
            from,
            inputs.value,
            inputs.init_code.to_vec(),
            depth,
        );

        call_trace.add_gas(Some(inputs.gas_limit + self.initial_gas));

        self.set_call_trace(depth, call_trace);

        // NB: Always return [None] or else the result of the create will be overriden.
        None
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        _: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        let depth = context.journal().depth() as u16;
        if let Some(call_trace) = self.call_trace.get_mut(&depth) {
            call_trace.add_to(outcome.address);
        }
        self.end_transaction_layer(
            context,
            outcome.gas().spent(),
            outcome.output(),
            outcome.instruction_result(),
        );
    }
}
