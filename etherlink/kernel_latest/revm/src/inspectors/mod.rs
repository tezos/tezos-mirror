// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::precompiles::provider::EtherlinkPrecompiles;
use call_tracer::{CallTracer, CallTracerInput};
use revm::{
    context::ContextTr,
    interpreter::{
        interpreter::ReturnDataImpl, CallInputs, CallOutcome, CreateInputs,
        CreateOutcome, Interpreter, InterpreterTypes, Stack,
    },
    primitives::{hardfork::SpecId, Address, Log, B256, U256},
    Inspector,
};
use struct_logger::{StructLogger, StructLoggerInput};
use tezos_smart_rollup_host::storage::StorageV1;

pub trait HasHost {
    type H;

    fn as_host_mut(&mut self) -> &mut Self::H;
}

pub mod call_tracer;
pub mod storage;
pub mod struct_logger;

pub const CALL_TRACER_CONFIG_PREFIX: u8 = 0x01;

#[derive(Debug, Clone, Copy)]
pub enum TracerInput {
    CallTracer(CallTracerInput),
    StructLogger(StructLoggerInput),
}

impl TracerInput {
    pub fn tracer(&self, precompiles: EtherlinkPrecompiles, spec_id: SpecId) -> Tracer {
        match self {
            TracerInput::CallTracer(CallTracerInput {
                config,
                transaction_hash,
            }) => Tracer::CallTracer(CallTracer::new(
                *config,
                precompiles,
                spec_id,
                *transaction_hash,
            )),
            TracerInput::StructLogger(StructLoggerInput {
                config,
                transaction_hash,
            }) => Tracer::StructLogger(StructLogger::new(*config, *transaction_hash)),
        }
    }
}

impl TracerInput {
    pub fn tx_hash(&self) -> Option<B256> {
        match self {
            TracerInput::StructLogger(input) => input.transaction_hash,
            TracerInput::CallTracer(input) => input.transaction_hash,
        }
    }
}

pub enum Tracer {
    CallTracer(CallTracer),
    StructLogger(StructLogger),
}

impl Tracer {
    pub fn is_struct_logger(&self) -> bool {
        matches!(self, Tracer::StructLogger(_))
    }

    pub fn transaction_hash(&self) -> Option<B256> {
        match self {
            Tracer::CallTracer(CallTracer {
                transaction_hash, ..
            }) => *transaction_hash,
            Tracer::StructLogger(StructLogger {
                transaction_hash, ..
            }) => *transaction_hash,
        }
    }
}

impl<CTX, INTR> Inspector<CTX, INTR> for Tracer
where
    CTX: ContextTr<Db: HasHost<H: StorageV1>>,
    INTR: InterpreterTypes<Stack = Stack, ReturnData = ReturnDataImpl>,
{
    fn initialize_interp(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Tracer::CallTracer(t) => t.initialize_interp(interp, context),
            Tracer::StructLogger(t) => t.initialize_interp(interp, context),
        }
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Tracer::CallTracer(t) => t.step(interp, context),
            Tracer::StructLogger(t) => t.step(interp, context),
        }
    }

    fn step_end(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Tracer::CallTracer(t) => t.step_end(interp, context),
            Tracer::StructLogger(t) => t.step_end(interp, context),
        }
    }

    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        match self {
            Tracer::CallTracer(t) => {
                <CallTracer as Inspector<CTX, INTR>>::call(t, context, inputs)
            }
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::call(t, context, inputs)
            }
        }
    }

    fn call_end(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
        outcome: &mut CallOutcome,
    ) {
        match self {
            Tracer::CallTracer(t) => <CallTracer as Inspector<CTX, INTR>>::call_end(
                t, context, inputs, outcome,
            ),
            Tracer::StructLogger(t) => <StructLogger as Inspector<CTX, INTR>>::call_end(
                t, context, inputs, outcome,
            ),
        }
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        match self {
            Tracer::CallTracer(t) => {
                <CallTracer as Inspector<CTX, INTR>>::create(t, context, inputs)
            }
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::create(t, context, inputs)
            }
        }
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        inputs: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        match self {
            Tracer::CallTracer(t) => <CallTracer as Inspector<CTX, INTR>>::create_end(
                t, context, inputs, outcome,
            ),
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::create_end(
                    t, context, inputs, outcome,
                )
            }
        }
    }

    #[inline]
    fn log(&mut self, context: &mut CTX, log: Log) {
        match self {
            Tracer::CallTracer(t) => {
                <CallTracer as Inspector<CTX, INTR>>::log(t, context, log)
            }
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::log(t, context, log)
            }
        }
    }

    fn log_full(
        &mut self,
        interpreter: &mut Interpreter<INTR>,
        context: &mut CTX,
        log: Log,
    ) {
        match self {
            Tracer::CallTracer(t) => <CallTracer as Inspector<CTX, INTR>>::log_full(
                t,
                interpreter,
                context,
                log,
            ),
            Tracer::StructLogger(t) => <StructLogger as Inspector<CTX, INTR>>::log_full(
                t,
                interpreter,
                context,
                log,
            ),
        }
    }

    #[inline]
    fn selfdestruct(&mut self, contract: Address, target: Address, value: U256) {
        match self {
            Tracer::CallTracer(t) => <CallTracer as Inspector<CTX, INTR>>::selfdestruct(
                t, contract, target, value,
            ),
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::selfdestruct(
                    t, contract, target, value,
                )
            }
        }
    }
}

pub fn get_tracer_configuration(
    tx_hash_target: B256,
    tracer_input: Option<TracerInput>,
) -> Option<TracerInput> {
    match tracer_input {
        Some(tracer_input) => match tracer_input.tx_hash() {
            None => {
                // If there is no transaction hash, we still provide
                // the configuration to trace all transactions
                Some(tracer_input)
            }
            Some(input_hash) => {
                // If there is a transaction hash in the input
                // we only trace if the current transaction hash
                // matches the transaction hash from the input
                if input_hash == tx_hash_target {
                    Some(tracer_input)
                } else {
                    None
                }
            }
        },
        None => None,
    }
}
