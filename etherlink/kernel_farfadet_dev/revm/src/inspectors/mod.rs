// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    database::EtherlinkVMDB, precompiles::provider::EtherlinkPrecompiles,
    EVMInnerContext, Error,
};
use call_tracer::{CallTracer, CallTracerInput};
use noop::NoInspector;
use revm::{
    context::{
        result::{EVMError, ExecutionResult, HaltReason},
        ContextSetters, ContextTr, Evm, JournalTr,
    },
    handler::{
        instructions::EthInstructions, EthFrame, EvmTr, EvmTrError, FrameResult, FrameTr,
        Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::{EthInterpreter, ReturnDataImpl},
        interpreter_action::FrameInit,
        interpreter_types::StackTr,
        CallInputs, CallOutcome, CreateInputs, CreateOutcome, Interpreter,
        InterpreterTypes, Stack,
    },
    primitives::B256,
    state::EvmState,
    ExecuteCommitEvm, ExecuteEvm, InspectEvm, Inspector,
};
use struct_logger::{StructLogger, StructLoggerInput};
use tezos_evm_runtime::runtime::Runtime;

pub mod call_tracer;
pub mod noop;
pub mod storage;
pub mod struct_logger;

pub const CALL_TRACER_CONFIG_PREFIX: u8 = 0x01;

pub type EvmInspection<'a, Host> = Evm<
    EVMInnerContext<'a, Host>,
    EtherlinkInspector,
    EthInstructions<EthInterpreter, EVMInnerContext<'a, Host>>,
    EtherlinkPrecompiles,
    EthFrame<EthInterpreter>,
>;

pub struct EtherlinkEvmInspector<'a, Host: Runtime> {
    inner: EvmInspection<'a, Host>,
}

impl<'a, Host: Runtime> ExecuteEvm for EtherlinkEvmInspector<'a, Host> {
    type ExecutionResult = ExecutionResult;
    type State = EvmState;
    type Error = EVMError<Error>;
    type Tx = <EVMInnerContext<'a, Host> as ContextTr>::Tx;
    type Block = <EVMInnerContext<'a, Host> as ContextTr>::Block;

    fn set_block(&mut self, block: Self::Block) {
        self.inner.set_block(block);
    }

    fn transact_one(
        &mut self,
        tx: Self::Tx,
    ) -> Result<Self::ExecutionResult, Self::Error> {
        self.inner.transact_one(tx)
    }

    fn finalize(&mut self) -> Self::State {
        self.inner.finalize()
    }

    fn replay(
        &mut self,
    ) -> Result<
        revm::context::result::ExecResultAndState<Self::ExecutionResult, Self::State>,
        Self::Error,
    > {
        self.inner.replay()
    }
}

impl<'a, Host: Runtime> ExecuteCommitEvm for EtherlinkEvmInspector<'a, Host> {
    fn commit(&mut self, state: Self::State) {
        self.inner.commit(state);
    }
}

#[derive(Debug)]
pub struct EtherlinkHandler<CTX, ERROR, FRAME> {
    _phantom: core::marker::PhantomData<(CTX, ERROR, FRAME)>,
}

impl<EVM, ERROR, FRAME> Handler for EtherlinkHandler<EVM, ERROR, FRAME>
where
    EVM: EvmTr<Context: ContextTr<Journal: JournalTr<State = EvmState>>, Frame = FRAME>,
    ERROR: EvmTrError<EVM>,
    FRAME: FrameTr<FrameResult = FrameResult, FrameInit = FrameInit>,
{
    type Evm = EVM;
    type Error = ERROR;
    type HaltReason = HaltReason;
}

impl<CTX, ERROR, FRAME> Default for EtherlinkHandler<CTX, ERROR, FRAME> {
    fn default() -> Self {
        Self {
            _phantom: core::marker::PhantomData,
        }
    }
}

impl<EVM, ERROR> InspectorHandler
    for EtherlinkHandler<EVM, ERROR, EthFrame<EthInterpreter>>
where
    EVM: revm::inspector::InspectorEvmTr<
        Context: ContextTr<Journal: JournalTr<State = EvmState>>,
        Frame = EthFrame<EthInterpreter>,
        Inspector: Inspector<<<Self as Handler>::Evm as EvmTr>::Context, EthInterpreter>,
    >,
    ERROR: EvmTrError<EVM>,
{
    type IT = EthInterpreter;
}

impl<Host: Runtime> InspectEvm for EtherlinkEvmInspector<'_, Host> {
    type Inspector = EtherlinkInspector;

    fn set_inspector(&mut self, inspector: Self::Inspector) {
        self.inner.inspector = inspector;
    }

    fn inspect_one_tx(
        &mut self,
        tx: Self::Tx,
    ) -> Result<Self::ExecutionResult, Self::Error> {
        self.inner.set_tx(tx);
        EtherlinkHandler::default().inspect_run(&mut self.inner)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TracerInput {
    NoOp,
    CallTracer(CallTracerInput),
    StructLogger(StructLoggerInput),
}

impl TracerInput {
    pub fn tx_hash(&self) -> Option<B256> {
        match self {
            TracerInput::StructLogger(input) => input.transaction_hash,
            TracerInput::CallTracer(input) => input.transaction_hash,
            TracerInput::NoOp => None,
        }
    }
}

pub enum EtherlinkInspector {
    NoOp(NoInspector),
    CallTracer(Box<CallTracer>),
    StructLogger(Box<StructLogger>),
}

impl EtherlinkInspector {
    pub fn is_struct_logger(&self) -> bool {
        matches!(self, EtherlinkInspector::StructLogger(_))
    }

    pub fn get_transaction_hash(&self) -> Option<B256> {
        match self {
            EtherlinkInspector::NoOp(_) => None,
            EtherlinkInspector::CallTracer(call_tracer) => call_tracer.transaction_hash,
            EtherlinkInspector::StructLogger(struct_logger) => {
                struct_logger.transaction_hash
            }
        }
    }
}

impl Default for EtherlinkInspector {
    fn default() -> Self {
        Self::NoOp(NoInspector)
    }
}

pub trait StructStack {
    fn to_structured_stack(&self) -> Vec<B256>;
}

impl StructStack for Stack {
    fn to_structured_stack(&self) -> Vec<B256> {
        let stack: Vec<B256> = self
            .data()
            .iter()
            .map(|e| B256::from_slice(e.to_be_bytes::<32>().as_slice()))
            .collect();
        stack
    }
}

impl<'a, Host, CTX, INTR> Inspector<CTX, INTR> for EtherlinkInspector
where
    Host: Runtime + 'a,
    CTX: ContextTr<Db = EtherlinkVMDB<'a, Host>>,
    INTR: InterpreterTypes<Stack: StackTr + StructStack, ReturnData = ReturnDataImpl>,
{
    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        match self {
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::call(no_inspector, context, inputs)
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::call(call_tracer, context, inputs)
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::call(
                    struct_logger,
                    context,
                    inputs,
                )
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
            Self::NoOp(no_inspector) => <NoInspector as Inspector<CTX, INTR>>::call_end(
                no_inspector,
                context,
                inputs,
                outcome,
            ),
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::call_end(
                    call_tracer,
                    context,
                    inputs,
                    outcome,
                )
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::call_end(
                    struct_logger,
                    context,
                    inputs,
                    outcome,
                )
            }
        }
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        match self {
            Self::NoOp(no_inspector) => <NoInspector as Inspector<CTX, INTR>>::create(
                no_inspector,
                context,
                inputs,
            ),
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::create(call_tracer, context, inputs)
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::create(
                    struct_logger,
                    context,
                    inputs,
                )
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
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::create_end(
                    no_inspector,
                    context,
                    inputs,
                    outcome,
                )
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::create_end(
                    call_tracer,
                    context,
                    inputs,
                    outcome,
                )
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::create_end(
                    struct_logger,
                    context,
                    inputs,
                    outcome,
                )
            }
        }
    }

    fn initialize_interp(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::initialize_interp(
                    no_inspector,
                    interp,
                    context,
                )
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::initialize_interp(
                    call_tracer,
                    interp,
                    context,
                )
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::initialize_interp(
                    struct_logger,
                    interp,
                    context,
                )
            }
        }
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Self::NoOp(no_inspector) => {
                <NoInspector as Inspector<CTX, INTR>>::step(no_inspector, interp, context)
            }
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::step(call_tracer, interp, context)
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::step(
                    struct_logger,
                    interp,
                    context,
                )
            }
        }
    }

    fn step_end(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        match self {
            Self::NoOp(no_inspector) => <NoInspector as Inspector<CTX, INTR>>::step_end(
                no_inspector,
                interp,
                context,
            ),
            Self::CallTracer(call_tracer) => {
                <CallTracer as Inspector<CTX, INTR>>::step_end(
                    call_tracer,
                    interp,
                    context,
                )
            }
            Self::StructLogger(struct_logger) => {
                <StructLogger as Inspector<CTX, INTR>>::step_end(
                    struct_logger,
                    interp,
                    context,
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
