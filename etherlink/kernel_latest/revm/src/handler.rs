// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::{
        journaled_state::account::JournaledAccountTr, result::HaltReason, ContextTr,
        JournalTr,
    },
    context_interface::{Cfg, LocalContextTr, Transaction},
    handler::{
        execution::create_init_frame,
        pre_execution::validate_against_state_and_deduct_caller, EthFrame, EvmTr,
        EvmTrError, FrameResult, FrameTr, Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::EthInterpreter, interpreter_action::FrameInit, CallScheme,
        FrameInput, GasTracker, InitialAndFloorGas, SharedMemory,
    },
    state::EvmState,
    Inspector,
};

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

    /// Validate the caller against its state and deduct the transaction costs.
    /// Contrary to vanilla REVM, bumping the caller nonce only happens when
    /// the call is top-level (depth is 0). Cross-runtime calls should not have
    /// nonce bump.
    fn validate_against_state_and_deduct_caller(
        &self,
        evm: &mut Self::Evm,
        _init_and_floor_gas: &mut InitialAndFloorGas,
    ) -> Result<(), Self::Error> {
        let caller_address = evm.ctx().tx().caller();
        let nonce_before = evm
            .ctx()
            .journal_mut()
            .load_account(caller_address)?
            .info
            .nonce;
        // Note: revm's validate_env rejects a tx nonce of u64::MAX (EIP-2681).
        validate_against_state_and_deduct_caller::<_, Self::Error>(evm.ctx())?;
        if self.call_depth != 0 {
            evm.ctx()
                .journal_mut()
                .load_account_with_code_mut(caller_address)?
                .data
                .set_nonce(nonce_before);
        }
        Ok(())
    }

    /// Upstream default, plus the `call_depth` seed and the static flag of
    /// [`Self::new_static`] on the top-level call frame.
    fn first_frame_input(
        &mut self,
        evm: &mut Self::Evm,
        gas: &mut GasTracker,
    ) -> Result<Option<FrameInit>, Self::Error> {
        let ctx = evm.ctx_mut();
        let mut memory =
            SharedMemory::new_with_buffer(ctx.local().shared_memory_buffer().clone());
        memory.set_memory_limit(ctx.cfg().memory_limit());

        // Note: an Err here leaves the revm pre-execution checkpoint open.
        let Some(mut frame_input) = create_init_frame(ctx, gas)? else {
            // Note: EIP-2780 only (Amsterdam); revm then reverts its checkpoint.
            return Ok(None);
        };

        if self.is_static_top_frame {
            // `Create` is incompatible with static (code write); only
            // the `Call` shape carries the flag.
            if let FrameInput::Call(ref mut call_inputs) = frame_input {
                call_inputs.is_static = true;
                // Rewrite the scheme so inspectors render the frame as
                // STATICCALL, like an outgoing view crossing. revm never
                // consults a Call-shaped input's scheme after frame
                // creation; `is_static` above carries the semantics.
                call_inputs.scheme = CallScheme::StaticCall;
            }
        }

        Ok(Some(FrameInit {
            depth: self.call_depth,
            memory,
            frame_input,
        }))
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
