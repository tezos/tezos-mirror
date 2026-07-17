// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::{
    database::EtherlinkVMDB, precompiles::provider::EtherlinkPrecompiles, EVMInnerContext,
};
use call_tracer::{CallTracer, CallTracerInput};
use revm::{
    context::{result::HaltReason, ContextTr, Evm, JournalTr},
    context_interface::{Cfg, LocalContextTr, Transaction},
    handler::{
        execution::create_init_frame, instructions::EthInstructions, EthFrame, EvmTr,
        EvmTrError, FrameResult, FrameTr, Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::{EthInterpreter, ReturnDataImpl},
        interpreter_action::{FrameInit, FrameInput},
        CallInputs, CallOutcome, CreateInputs, CreateOutcome, Interpreter,
        InterpreterTypes, SharedMemory, Stack,
    },
    primitives::{hardfork::SpecId, Address, Log, B256, U256},
    state::{Bytecode, EvmState},
    Inspector,
};
use struct_logger::{StructLogger, StructLoggerInput};
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

#[derive(Debug)]
pub struct EtherlinkHandler<CTX, ERROR, FRAME> {
    /// When `true`, [`Self::first_frame_input`] sets `is_static = true`
    /// on the top-level frame so REVM enforces strict `STATICCALL`
    /// semantics. Used by [`TransactionOrigin::CrossRuntimeStatic`](crate::TransactionOrigin::CrossRuntimeStatic).
    is_static_top_frame: bool,
    call_depth: usize,
    _phantom: core::marker::PhantomData<(CTX, ERROR, FRAME)>,
}

impl<CTX, ERROR, FRAME> EtherlinkHandler<CTX, ERROR, FRAME> {
    /// Build a handler that runs the top-level frame in static mode
    /// (see the field doc on [`Self::is_static_top_frame`]).
    pub fn new_static() -> Self {
        Self {
            is_static_top_frame: true,
            call_depth: 0,
            _phantom: core::marker::PhantomData,
        }
    }

    pub fn with_call_depth(mut self, depth: usize) -> Self {
        self.call_depth = depth;
        self
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
            depth: self.call_depth,
            memory,
            frame_input,
        })
    }
}

impl<CTX, ERROR, FRAME> Default for EtherlinkHandler<CTX, ERROR, FRAME> {
    fn default() -> Self {
        Self {
            is_static_top_frame: false,
            call_depth: 0,
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

impl<'a, Host, R, CTX, INTR> Inspector<CTX, INTR> for Tracer
where
    Host: StorageV1 + 'a,
    R: Registry + 'a,
    CTX: ContextTr<Db = EtherlinkVMDB<'a, Host, R>>,
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
