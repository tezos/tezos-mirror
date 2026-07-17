// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::{result::HaltReason, ContextTr, JournalTr},
    context_interface::{Cfg, LocalContextTr, Transaction},
    handler::{
        execution::create_init_frame, EthFrame, EvmTr, EvmTrError, FrameResult, FrameTr,
        Handler,
    },
    inspector::InspectorHandler,
    interpreter::{
        interpreter::EthInterpreter, interpreter_action::FrameInit, FrameInput,
        SharedMemory,
    },
    state::{Bytecode, EvmState},
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
