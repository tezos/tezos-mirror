// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{precompiles::provider::EtherlinkPrecompiles, EVMInnerContext, Error};
use call_tracer::CallTracerInput;
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
    interpreter::{interpreter::EthInterpreter, interpreter_action::FrameInit, Stack},
    primitives::B256,
    state::EvmState,
    ExecuteCommitEvm, ExecuteEvm, InspectEvm, Inspector,
};
use struct_logger::StructLoggerInput;
use tezos_evm_runtime::runtime::Runtime;
use tezosx_interfaces::Registry;

pub mod call_tracer;
pub mod storage;
pub mod struct_logger;

pub const CALL_TRACER_CONFIG_PREFIX: u8 = 0x01;

pub type EvmInspection<'a, Host, INSP, R> = Evm<
    EVMInnerContext<'a, Host, R>,
    INSP,
    EthInstructions<EthInterpreter, EVMInnerContext<'a, Host, R>>,
    EtherlinkPrecompiles,
    EthFrame<EthInterpreter>,
>;

pub struct EtherlinkEvmInspector<
    'a,
    Host: Runtime,
    R: Registry,
    INSP: EtherlinkInspector<'a, Host, R>,
> {
    inner: EvmInspection<'a, Host, INSP, R>,
}

impl<'a, Host: Runtime, R: Registry, INSP: EtherlinkInspector<'a, Host, R>> ExecuteEvm
    for EtherlinkEvmInspector<'a, Host, R, INSP>
{
    type ExecutionResult = ExecutionResult;
    type State = EvmState;
    type Error = EVMError<Error>;
    type Tx = <EVMInnerContext<'a, Host, R> as ContextTr>::Tx;
    type Block = <EVMInnerContext<'a, Host, R> as ContextTr>::Block;

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

impl<'a, Host: Runtime, R: Registry, INSP: EtherlinkInspector<'a, Host, R>>
    ExecuteCommitEvm for EtherlinkEvmInspector<'a, Host, R, INSP>
{
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

impl<'a, Host: Runtime, R: Registry, INSP: EtherlinkInspector<'a, Host, R>> InspectEvm
    for EtherlinkEvmInspector<'a, Host, R, INSP>
{
    type Inspector = INSP;

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
    CallTracer(CallTracerInput),
    StructLogger(StructLoggerInput),
}

impl TracerInput {
    pub fn tx_hash(&self) -> Option<B256> {
        match self {
            TracerInput::StructLogger(input) => input.transaction_hash,
            TracerInput::CallTracer(input) => input.transaction_hash,
        }
    }
}

pub trait EtherlinkInspector<'a, Host: Runtime + 'a, R: Registry + 'a>:
    Inspector<EVMInnerContext<'a, Host, R>>
{
    fn is_struct_logger(&self) -> bool;
    fn get_transaction_hash(&self) -> Option<B256>;
}

impl<'a, Host: Runtime + 'a, R: Registry + 'a> EtherlinkInspector<'a, Host, R>
    for Box<dyn EtherlinkInspector<'a, Host, R>>
{
    fn is_struct_logger(&self) -> bool {
        self.as_ref().is_struct_logger()
    }

    fn get_transaction_hash(&self) -> Option<B256> {
        self.as_ref().get_transaction_hash()
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
