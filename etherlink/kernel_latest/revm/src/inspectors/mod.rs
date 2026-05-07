// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    error::EvmDbError, precompiles::provider::EtherlinkPrecompiles, EVMInnerContext,
};
use call_tracer::CallTracerInput;
use revm::{
    context::{
        result::{EVMError, ExecutionResult, HaltReason},
        ContextSetters, ContextTr, Evm, JournalTr,
    },
    context_interface::{Cfg, LocalContextTr, Transaction},
    handler::{
        execution::create_init_frame, instructions::EthInstructions, EthFrame, EvmTr,
        EvmTrError, FrameResult, FrameTr, Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::EthInterpreter,
        interpreter_action::{FrameInit, FrameInput},
        SharedMemory, Stack,
    },
    primitives::B256,
    state::{Bytecode, EvmState},
    ExecuteCommitEvm, ExecuteEvm, InspectEvm, Inspector,
};
use struct_logger::StructLoggerInput;
use tezos_smart_rollup_host::storage::StorageV1;
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

pub struct EtherlinkEvmInspector<'a, Host, R, INSP>
where
    Host: StorageV1,
    R: Registry,
    INSP: EtherlinkInspector<'a, Host, R>,
{
    inner: EvmInspection<'a, Host, INSP, R>,
}

impl<'a, Host, R, INSP> ExecuteEvm for EtherlinkEvmInspector<'a, Host, R, INSP>
where
    Host: StorageV1,
    R: Registry,
    INSP: EtherlinkInspector<'a, Host, R>,
{
    type ExecutionResult = ExecutionResult;
    type State = EvmState;
    type Error = EVMError<EvmDbError>;
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

impl<'a, Host, R, INSP> ExecuteCommitEvm for EtherlinkEvmInspector<'a, Host, R, INSP>
where
    Host: StorageV1,
    R: Registry,
    INSP: EtherlinkInspector<'a, Host, R>,
{
    fn commit(&mut self, state: Self::State) {
        self.inner.commit(state);
    }
}

#[derive(Debug)]
pub struct EtherlinkHandler<CTX, ERROR, FRAME> {
    /// When `true`, [`Self::first_frame_input`] sets `is_static = true`
    /// on the top-level frame so REVM enforces strict `STATICCALL`
    /// semantics. Used by [`TransactionOrigin::CrossRuntimeStatic`](crate::TransactionOrigin::CrossRuntimeStatic).
    is_static_top_frame: bool,
    _phantom: core::marker::PhantomData<(CTX, ERROR, FRAME)>,
}

impl<CTX, ERROR, FRAME> EtherlinkHandler<CTX, ERROR, FRAME> {
    /// Build a handler that runs the top-level frame in static mode
    /// (see the field doc on [`Self::is_static_top_frame`]).
    pub fn new_static() -> Self {
        Self {
            is_static_top_frame: true,
            _phantom: core::marker::PhantomData,
        }
    }
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

    /// Flip `is_static = true` on the top-level call frame when built
    /// via [`Self::new_static`]. The rest of the body is a verbatim
    /// copy of the default in `revm-handler` 17.0.0 (load bytecode +
    /// EIP-7702 resolution + `create_init_frame`) because Rust does
    /// not let us reuse the default while overriding it. **Re-sync
    /// this body when bumping REVM to 38** — the upstream default
    /// will likely have drifted.
    fn first_frame_input(
        &mut self,
        evm: &mut Self::Evm,
        gas_limit: u64,
    ) -> Result<FrameInit, Self::Error> {
        let ctx = evm.ctx_mut();
        let mut memory =
            SharedMemory::new_with_buffer(ctx.local().shared_memory_buffer().clone());
        memory.set_memory_limit(ctx.cfg().memory_limit());

        let (tx, journal) = ctx.tx_journal_mut();
        let bytecode = if let Some(&to) = tx.kind().to() {
            let account = &journal.load_account_with_code(to)?.info;
            if let Some(delegated_address) =
                account.code.as_ref().and_then(Bytecode::eip7702_address)
            {
                let account = &journal.load_account_with_code(delegated_address)?.info;
                Some((
                    account.code.clone().unwrap_or_default(),
                    account.code_hash(),
                ))
            } else {
                Some((
                    account.code.clone().unwrap_or_default(),
                    account.code_hash(),
                ))
            }
        } else {
            None
        };

        let mut frame_input = create_init_frame(tx, bytecode, gas_limit);

        if self.is_static_top_frame {
            // `Create` is incompatible with static (code write); only
            // the `Call` shape carries the flag.
            if let FrameInput::Call(ref mut call_inputs) = frame_input {
                call_inputs.is_static = true;
            }
        }

        Ok(FrameInit {
            depth: 0,
            memory,
            frame_input,
        })
    }
}

impl<CTX, ERROR, FRAME> Default for EtherlinkHandler<CTX, ERROR, FRAME> {
    fn default() -> Self {
        Self {
            is_static_top_frame: false,
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

impl<'a, Host, R, INSP> InspectEvm for EtherlinkEvmInspector<'a, Host, R, INSP>
where
    Host: StorageV1,
    R: Registry,
    INSP: EtherlinkInspector<'a, Host, R>,
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

pub trait EtherlinkInspector<'a, Host, R>:
    Inspector<EVMInnerContext<'a, Host, R>>
where
    Host: StorageV1 + 'a,
    R: Registry + 'a,
{
    fn is_struct_logger(&self) -> bool;
    fn get_transaction_hash(&self) -> Option<B256>;
}

impl<'a, Host, R> EtherlinkInspector<'a, Host, R>
    for Box<dyn EtherlinkInspector<'a, Host, R>>
where
    Host: StorageV1 + 'a,
    R: Registry + 'a,
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
