// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use super::{
    rlp_helpers::{
        append_address, append_option_address, append_option_canonical,
        append_option_u64_le, append_u16_le, append_u256_le, append_u64_le,
    },
    storage::flush_call_traces,
    HasHost,
};

use revm::{
    context::{ContextTr, CreateScheme, Transaction},
    interpreter::{
        gas::calculate_initial_tx_gas_for_tx, interpreter::ReturnDataImpl, CallInputs,
        CallOutcome, CallScheme, CreateInputs, CreateOutcome, InitialAndFloorGas,
        InstructionResult, InterpreterTypes,
    },
    primitives::{hardfork::SpecId, Address, Bytes, Log, B256, U256},
    Inspector,
};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use tezos_ethereum::{
    rlp_helpers::{check_list, decode_field, decode_option, next},
    Log as RlpLog,
};
use tezos_evm_logging::{log, Level::Debug};
use tezos_smart_rollup_host::storage::StorageV1;

const CALL_TRACER_CONFIG_SIZE: usize = 3;

#[derive(Debug, Clone, Copy)]
pub struct CallTracerConfig {
    pub only_top_call: bool,
    pub with_logs: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct CallTracerInput {
    pub config: CallTracerConfig,
    pub transaction_hash: Option<B256>,
}

impl Decodable for CallTracerInput {
    fn decode(decoder: &Rlp) -> Result<Self, DecoderError> {
        let mut it = decoder.iter();
        check_list(decoder, CALL_TRACER_CONFIG_SIZE)?;

        let transaction_hash: Option<primitive_types::H256> =
            decode_option(&next(&mut it)?, "transaction_hash")?;
        let only_top_call = decode_field(&next(&mut it)?, "only_top_call")?;
        let with_logs = decode_field(&next(&mut it)?, "with_logs")?;

        Ok(CallTracerInput {
            transaction_hash: transaction_hash.map(|h| B256::from_slice(&h.0)),
            config: CallTracerConfig {
                only_top_call,
                with_logs,
            },
        })
    }
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
    /// Intrinsic gas of the transaction this frame belongs to, captured when
    /// the frame opens (an inner cross-runtime leg has its own TxEnv, so a
    /// tracer-global value would be overwritten mid-crossing). Added to
    /// `gas_used` at close; not part of the encoded trace.
    initial_gas: u64,
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
            initial_gas: 0,
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

    pub fn add_logs(&mut self, logs: Option<Vec<Log>>) {
        self.logs = logs;
    }
}

#[derive(Debug)]
pub struct CallTracer {
    config: CallTracerConfig,
    /// Stack of currently-open call frames: `call`/`create` push a frame on
    /// entry and `call_end`/`create_end` pop it on exit. The frame's entry
    /// depth is its position in this stack, so no separate depth counter is
    /// needed.
    call_trace: Vec<CallTrace>,
    /// Traces buffered in memory, flushed to storage once at the end of
    /// each transaction (when the frame stack empties).  RLP encoding is
    /// deferred to flush time so the buffer remains readable.
    pending_traces: Vec<CallTrace>,
    pub(crate) transaction_hash: Option<B256>,
    spec_id: SpecId,
}

impl CallTracer {
    pub fn new(
        config: CallTracerConfig,
        spec_id: SpecId,
        transaction_hash: Option<B256>,
    ) -> Self {
        Self {
            config,
            call_trace: Vec::with_capacity(1),
            pending_traces: Vec::new(),
            transaction_hash,
            spec_id,
        }
    }

    #[inline]
    fn initial_gas(&self, tx: impl Transaction) -> u64 {
        let InitialAndFloorGas { initial_gas, .. } =
            calculate_initial_tx_gas_for_tx(tx, self.spec_id);
        initial_gas
    }

    fn end_transaction_layer<CTX>(
        &mut self,
        context: &mut CTX,
        gas_spent: u64,
        output: &Bytes,
        instruction_result: &InstructionResult,
    ) where
        CTX: ContextTr<Db: HasHost<H: StorageV1>>,
    {
        // Leaving a frame: pop it off the stack.
        if let Some(mut call_trace) = self.call_trace.pop() {
            // In `only_top_call` mode nested frames are still pushed to keep
            // the stack in sync, but only the top-level frame is reported.
            if !(self.config.only_top_call && call_trace.depth > 0) {
                let initial_gas = call_trace.initial_gas;
                call_trace.add_gas_used(gas_spent + initial_gas);
                call_trace.add_output(Some(output.to_vec()));
                call_trace.add_error_from_instruction_result(instruction_result);

                self.pending_traces.push(call_trace);
            }
        }

        // Once the frame stack empties (end of top-level transaction), flush
        // all buffered traces to storage in a single batch operation.
        if self.call_trace.is_empty() && !self.pending_traces.is_empty() {
            let traces = std::mem::take(&mut self.pending_traces);
            flush_call_traces(
                context.db_mut().as_host_mut(),
                &traces,
                &self.transaction_hash,
            )
            .inspect_err(|err| {
                log!(Debug, "Flushing call traces failed with: {err:?}");
            })
            .ok();
        }
    }
}

impl<CTX, INTR> Inspector<CTX, INTR> for CallTracer
where
    CTX: ContextTr<Db: HasHost<H: StorageV1>>,
    INTR: InterpreterTypes<ReturnData = ReturnDataImpl>,
{
    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        // Entering a frame: its depth is the current stack height, before it
        // is pushed.
        let depth = self.call_trace.len() as u16;

        let initial_gas = self.initial_gas(context.tx());

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

        call_trace.initial_gas = initial_gas;
        call_trace.add_to(Some(inputs.bytecode_address));
        call_trace.add_gas(Some(inputs.gas_limit + initial_gas));

        self.call_trace.push(call_trace);

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
        // Entering a frame: its depth is the current stack height, before it
        // is pushed.
        let depth = self.call_trace.len() as u16;

        let initial_gas = self.initial_gas(context.tx());

        let (type_, from) = match inputs.scheme() {
            CreateScheme::Create => ("CREATE", inputs.caller()),
            CreateScheme::Create2 { .. } => ("CREATE2", inputs.caller()),
            // Impossible case on Etherlink:
            CreateScheme::Custom { .. } => ("CUSTOM", inputs.caller()),
        };

        let mut call_trace = CallTrace::new_minimal_trace(
            type_.into(),
            from,
            inputs.value(),
            inputs.init_code().to_vec(),
            depth,
        );

        call_trace.initial_gas = initial_gas;
        call_trace.add_gas(Some(inputs.gas_limit() + initial_gas));

        self.call_trace.push(call_trace);

        // NB: Always return [None] or else the result of the create will be overriden.
        None
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        _: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        // The frame being left is on top of the stack: record the created
        // address on it before `end_transaction_layer` pops it.
        if let Some(call_trace) = self.call_trace.last_mut() {
            call_trace.add_to(outcome.address);
        }
        self.end_transaction_layer(
            context,
            outcome.gas().spent(),
            outcome.output(),
            outcome.instruction_result(),
        );
    }

    fn log(&mut self, _context: &mut CTX, log: Log) {
        if !self.config.with_logs {
            return;
        }

        // The frame that emitted the LOG opcode is the one currently
        // executing, i.e. the frame on top of the stack.
        if let Some(t) = self.call_trace.last_mut() {
            t.logs.get_or_insert_with(Vec::new).push(log);
        }
    }
}
