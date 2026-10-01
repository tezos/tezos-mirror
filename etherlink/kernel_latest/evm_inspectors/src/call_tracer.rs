// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::error::InspectorError;

use super::{
    rlp_helpers::{
        append_address, append_option_address, append_option_canonical,
        append_option_u64_le, append_u16_le, append_u256_le, append_u64_le,
    },
    storage::flush_call_traces,
};

use revm::{
    context::{result::ExecutionResult, ContextTr, CreateScheme, Transaction},
    interpreter::{
        gas::calculate_initial_tx_gas_for_tx, interpreter::ReturnDataImpl, CallInputs,
        CallOutcome, CallScheme, CreateInputs, CreateOutcome, InitialAndFloorGas,
        InstructionResult, InterpreterTypes,
    },
    primitives::{hardfork::SpecId, Address, Bytes, Log, B256, U256},
    Inspector,
};
use rlp::{Decodable, DecoderError, Encodable, Rlp, RlpStream};
use tezos_ethereum::rlp_helpers::{check_list, decode_field, decode_option, next};
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

/// A log captured on a frame, carrying geth's `position`: the number of
/// the enclosing frame's sub-calls that had completed when the log fired.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallTraceLog {
    pub log: Log,
    pub position: u64,
}

impl Encodable for CallTraceLog {
    fn rlp_append(&self, stream: &mut RlpStream) {
        stream.begin_list(4);
        append_address(stream, &self.log.address);
        let topics: Vec<primitive_types::H256> = self
            .log
            .data
            .topics()
            .iter()
            .map(|topic| primitive_types::H256(topic.0))
            .collect();
        stream.append_list(&topics);
        stream.append(&self.log.data.data.to_vec());
        append_u64_le(stream, &self.position);
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
    logs: Option<Vec<CallTraceLog>>,
    /// `depth` is helpful to reconstruct the tree of call on the EVM node's side.
    depth: u16,
    /// Intrinsic gas of the traced transaction, captured when the frame
    /// opens. Zero on every frame but the top-level one, which alone
    /// accounts intrinsic gas as geth's callTracer does. Added to
    /// `gas_used` at close; not part of the encoded trace.
    initial_gas: u64,
    /// Sub-calls of this frame that have completed — the length geth's
    /// `calls` array would have. Stamped on each log as its `position`;
    /// not part of the encoded trace.
    completed_calls: u64,
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
        append_option_canonical(stream, &self.logs, |s, logs| s.append_list(logs));
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
            completed_calls: 0,
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

    pub fn add_logs(&mut self, logs: Option<Vec<CallTraceLog>>) {
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
    /// Traces buffered in memory, flushed to storage in one batch by
    /// [`CallTracer::finalize`].  RLP encoding is deferred to flush time
    /// so the buffer remains readable.
    pending_traces: Vec<CallTrace>,
    /// Whether a smart contract was called during the transaction lifespan
    saw_call: bool,
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
            saw_call: false,
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

    fn end_transaction_layer(
        &mut self,
        gas_spent: u64,
        output: &Bytes,
        instruction_result: &InstructionResult,
    ) {
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

                // Where geth appends the finished child to its parent's
                // `calls` array. Unreported frames are not appended, so
                // `only_top_call` keeps every position at 0, as geth does.
                if let Some(parent) = self.call_trace.last_mut() {
                    parent.completed_calls += 1;
                }
            }
        }
    }

    /// Flush the buffered traces to storage in one batch.
    pub fn finalize<Host>(
        &mut self,
        host: &mut Host,
        _result: &ExecutionResult,
    ) -> Result<(), InspectorError>
    where
        Host: StorageV1,
    {
        if !self.pending_traces.is_empty() {
            let traces = std::mem::take(&mut self.pending_traces);
            flush_call_traces(host, &traces, &self.transaction_hash)
                .inspect_err(|err| {
                    log!(Debug, "Flushing call traces failed with: {err:?}");
                })
                .ok();
        }

        Ok(())
    }

    pub fn inject_log(&mut self, log: Log) {
        if !self.config.with_logs {
            return;
        }

        // The frame that emitted the LOG opcode is the one currently
        // executing, i.e. the frame on top of the stack.
        if let Some(t) = self.call_trace.last_mut() {
            let position = t.completed_calls;
            t.logs
                .get_or_insert_with(Vec::new)
                .push(CallTraceLog { log, position });
        }
    }

    /// Open the top-level frame mirroring a fake EVM transaction's
    /// envelope: a `CALL` with `from == to == caller` (the originator
    /// alias), no value, empty input, and no intrinsic gas.
    pub fn fake_top_level_call(&mut self, caller: Address, gas_limit: u64) {
        // Entering the top-level frame: the stack must be empty, so its
        // depth is 0.
        assert!(self.call_trace.is_empty());
        let depth = self.call_trace.len() as u16;

        let mut call_trace = CallTrace::new_minimal_trace(
            b"CALL".to_vec(),
            caller,
            U256::ZERO,
            Vec::new(),
            depth,
        );

        call_trace.add_to(Some(caller));
        call_trace.add_gas(Some(gas_limit));

        self.saw_call = false;
        self.call_trace.push(call_trace);
    }

    /// Close the frame opened by [`CallTracer::fake_top_level_call`]. When
    /// nothing nested below it, the mirrored operation dispatched no EVM
    /// call: the frame is dropped without recording anything.
    pub fn fake_top_level_call_end(&mut self, gas_spent: u64, status: bool) {
        if !self.saw_call {
            self.call_trace.pop();
            return;
        }

        self.end_transaction_layer(
            gas_spent,
            &Bytes::new(),
            &if status {
                InstructionResult::Return
            } else {
                InstructionResult::Revert
            },
        );
    }
}

impl<CTX, INTR> Inspector<CTX, INTR> for CallTracer
where
    CTX: ContextTr,
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
        self.saw_call = true;

        // Only the top-level frame accounts the transaction's intrinsic gas.
        let initial_gas = if depth == 0 {
            self.initial_gas(context.tx())
        } else {
            0
        };

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

    fn call_end(&mut self, _: &mut CTX, _: &CallInputs, outcome: &mut CallOutcome) {
        self.end_transaction_layer(
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
        self.saw_call = true;

        // Only the top-level frame accounts the transaction's intrinsic gas.
        let initial_gas = if depth == 0 {
            self.initial_gas(context.tx())
        } else {
            0
        };

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

    fn create_end(&mut self, _: &mut CTX, _: &CreateInputs, outcome: &mut CreateOutcome) {
        // The frame being left is on top of the stack: record the created
        // address on it before `end_transaction_layer` pops it.
        if let Some(call_trace) = self.call_trace.last_mut() {
            call_trace.add_to(outcome.address);
        }
        self.end_transaction_layer(
            outcome.gas().spent(),
            outcome.output(),
            outcome.instruction_result(),
        );
    }

    // At-emission delivery. Deliberately not `log`: revm replays a resolved precompile frame's
    // journal logs through that hook, which would duplicate nested logs onto the caller's frame.
    // Precompile logs arrive via `inject_log`.
    fn log_full(
        &mut self,
        _interpreter: &mut revm::interpreter::Interpreter<INTR>,
        _context: &mut CTX,
        log: Log,
    ) {
        self.inject_log(log);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use revm::primitives::LogData;

    fn tracer(only_top_call: bool) -> CallTracer {
        CallTracer::new(
            CallTracerConfig {
                only_top_call,
                with_logs: true,
            },
            SpecId::default(),
            None,
        )
    }

    /// Enter a frame, as the `call` inspector hook does.
    fn enter(tracer: &mut CallTracer) {
        let depth = tracer.call_trace.len() as u16;
        tracer.call_trace.push(CallTrace::new_minimal_trace(
            b"CALL".to_vec(),
            Address::ZERO,
            U256::ZERO,
            Vec::new(),
            depth,
        ));
    }

    /// Leave the innermost frame, as the `call_end` inspector hook does.
    fn leave(tracer: &mut CallTracer) {
        tracer.end_transaction_layer(0, &Bytes::new(), &InstructionResult::Return);
    }

    fn some_log() -> Log {
        Log {
            address: Address::ZERO,
            data: LogData::new_unchecked(vec![B256::ZERO], Bytes::new()),
        }
    }

    /// Positions recorded on the reported frame at `depth`.
    fn positions(tracer: &CallTracer, depth: u16) -> Vec<u64> {
        tracer
            .pending_traces
            .iter()
            .find(|trace| trace.depth == depth)
            .expect("frame should have been reported")
            .logs
            .iter()
            .flatten()
            .map(|log| log.position)
            .collect()
    }

    #[test]
    fn position_counts_completed_sub_calls() {
        let mut tracer = tracer(false);
        enter(&mut tracer);
        tracer.inject_log(some_log());
        enter(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);

        assert_eq!(positions(&tracer, 0), vec![0, 1]);
        assert_eq!(positions(&tracer, 1), vec![0]);
    }

    #[test]
    fn position_counts_a_sub_call_that_emitted_nothing() {
        let mut tracer = tracer(false);
        enter(&mut tracer);
        enter(&mut tracer);
        leave(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);

        // A silent sub-call counts — the point of recording at emission time.
        assert_eq!(positions(&tracer, 0), vec![1]);
    }

    #[test]
    fn only_top_call_keeps_positions_at_zero() {
        let mut tracer = tracer(true);
        enter(&mut tracer);
        enter(&mut tracer);
        leave(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);

        assert_eq!(tracer.pending_traces.len(), 1);
        assert_eq!(positions(&tracer, 0), vec![0]);
    }

    #[test]
    fn encoding_carries_the_log_position() {
        let mut trace = CallTrace::new_minimal_trace(
            b"CALL".to_vec(),
            Address::from([25; 20]),
            U256::from(251197),
            vec![0x00, 0x01, 0x02],
            2,
        );
        trace.add_to(Some(Address::from([25; 20])));
        trace.add_gas(Some(5000));
        trace.add_gas_used(5000);
        trace.add_output(Some(vec![0x00, 0x01, 0x02]));
        trace.add_error(Some(vec![0x00, 0x01, 0x02]));
        trace.add_logs(Some(vec![CallTraceLog {
            log: Log {
                address: Address::from([25; 20]),
                data: LogData::new_unchecked(
                    vec![B256::from([25; 32]), B256::from([13; 32])],
                    Bytes::from_static(&[0x00, 0x01, 0x02]),
                ),
            },
            position: 1,
        }]));

        // Decoded back by `test_decoding_rlp_log_position` in
        // `etherlink/bin_node/test/test_call_tracer_algo.ml`.
        assert_eq!(
            hex::encode(rlp::encode(&trace)),
            "f8e18443414c4c941919191919191919191919191919191919191919d5\
             941919191919191919191919191919191919191919a03dd50300000000\
             00000000000000000000000000000000000000000000000000c9888813\
             00000000000088881300000000000083000102c483000102c483000102\
             f86af868f866941919191919191919191919191919191919191919f842\
             a019191919191919191919191919191919191919191919191919191919\
             19191919a00d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d\
             0d0d0d0d0d0d0d0d83000102880100000000000000820200"
        );
    }

    /// Distinct from [`some_log`] under (address, topics, data).
    fn other_log() -> Log {
        Log {
            address: Address::ZERO,
            data: LogData::new_unchecked(vec![B256::from([1; 32])], Bytes::new()),
        }
    }

    // The two tests below run the same three frames and emit the same
    // sequence of logs — the very same log at a frame and at its parent,
    // with a distinct one in between — for opposite interleavings. The
    // receipt is identical; only positions recorded at emission tell the
    // two apart.

    #[test]
    fn position_orders_identical_logs_inner_first() {
        let mut tracer = tracer(false);
        enter(&mut tracer); // run()
        enter(&mut tracer); // inner()
        tracer.inject_log(some_log());
        enter(&mut tracer); // ping()
        tracer.inject_log(other_log());
        leave(&mut tracer);
        leave(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);

        assert_eq!(positions(&tracer, 0), vec![1]);
        assert_eq!(positions(&tracer, 1), vec![0]);
        assert_eq!(positions(&tracer, 2), vec![0]);
    }

    #[test]
    fn position_orders_identical_logs_outer_first() {
        let mut tracer = tracer(false);
        enter(&mut tracer); // run()
        tracer.inject_log(some_log());
        enter(&mut tracer); // inner()
        enter(&mut tracer); // ping()
        tracer.inject_log(other_log());
        leave(&mut tracer);
        tracer.inject_log(some_log());
        leave(&mut tracer);
        leave(&mut tracer);

        assert_eq!(positions(&tracer, 0), vec![0]);
        assert_eq!(positions(&tracer, 1), vec![1]);
        assert_eq!(positions(&tracer, 2), vec![0]);
    }
}
