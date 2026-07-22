// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

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
pub mod error;
pub mod rlp_helpers;
pub mod storage;
pub mod struct_logger;

pub const CALL_TRACER_CONFIG_PREFIX: u8 = 0x01;

#[derive(Debug, Clone, Copy)]
pub enum TracerInput {
    CallTracer(CallTracerInput),
    StructLogger(StructLoggerInput),
}

impl TracerInput {
    pub fn tracer(&self, spec_id: SpecId) -> Tracer {
        match self {
            TracerInput::CallTracer(CallTracerInput {
                config,
                transaction_hash,
            }) => {
                Tracer::CallTracer(CallTracer::new(*config, spec_id, *transaction_hash))
            }
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

/// Access to the [`Tracer`] owned by a journal, for [`TracerInspector`].
///
/// [`Self::take_tracer`] and [`Self::restore_tracer`] come as a pair: a
/// caller detaching the tracer is expected to re-attach it once done.
pub trait TracerContainer {
    /// Detach the journal's tracer, if any.
    fn take_tracer(&mut self) -> Option<Box<Tracer>>;

    /// Re-attach a tracer detached with [`Self::take_tracer`].
    fn restore_tracer(&mut self, tracer: Option<Box<Tracer>>);
}

/// Stateless [`Inspector`] adapter for a journal-owned [`Tracer`].
///
/// The tracer must be reachable from the journal so it can follow a
/// transaction across execution contexts: a cross-runtime call spawns a
/// fresh EVM context which only receives the journal. Rather than sharing
/// the tracer between the journal and the inspector slot of the `Evm`
/// context, the journal is the tracer's single owner and this zero-sized
/// handle fills the inspector slot. Every hook briefly takes the tracer out
/// of the journal — through the `&mut CTX` the hook already receives — runs
/// it, and puts it back. Detaching the tracer while it runs keeps unique
/// borrows sound: the tracer's own hooks receive the full `&mut CTX` and
/// must not be able to reach themselves through it.
///
/// See [`TracerInspector::selfdestruct`] for the one hook this handle
/// cannot route to the journal-owned tracer.
#[derive(Debug, Clone, Copy, Default)]
pub struct TracerInspector;

impl TracerInspector {
    fn with_tracer<CTX, F>(&mut self, context: &mut CTX, f: F)
    where
        CTX: ContextTr<Db: HasHost<H: StorageV1>, Journal: TracerContainer>,
        F: FnOnce(&mut Tracer, &mut CTX),
    {
        let Some(mut tracer) = context.journal_mut().take_tracer() else {
            return;
        };
        f(&mut tracer, context);
        context.journal_mut().restore_tracer(Some(tracer));
    }

    fn bind_tracer<CTX, T, F>(&mut self, context: &mut CTX, f: F) -> Option<T>
    where
        CTX: ContextTr<Db: HasHost<H: StorageV1>, Journal: TracerContainer>,
        F: FnOnce(&mut Tracer, &mut CTX) -> Option<T>,
    {
        let mut tracer = context.journal_mut().take_tracer()?;
        let outcome = f(&mut tracer, context);
        context.journal_mut().restore_tracer(Some(tracer));
        outcome
    }
}

macro_rules! dispatch {
    ($method:ident, $tracer:expr, $($arg:expr),* $(,)?) => {
        match &mut *$tracer {
            Tracer::CallTracer(t) => {
                <CallTracer as Inspector<CTX, INTR>>::$method(t, $($arg),*)
            }
            Tracer::StructLogger(t) => {
                <StructLogger as Inspector<CTX, INTR>>::$method(t, $($arg),*)
            }
        }
    };
}

impl<CTX, INTR> Inspector<CTX, INTR> for TracerInspector
where
    CTX: ContextTr<Db: HasHost<H: StorageV1>, Journal: TracerContainer>,
    INTR: InterpreterTypes<Stack = Stack, ReturnData = ReturnDataImpl>,
{
    fn initialize_interp(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(initialize_interp, tracer, interp, context)
        });
    }

    fn step(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(step, tracer, interp, context)
        });
    }

    fn step_end(&mut self, interp: &mut Interpreter<INTR>, context: &mut CTX) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(step_end, tracer, interp, context)
        });
    }

    fn call(
        &mut self,
        context: &mut CTX,
        inputs: &mut CallInputs,
    ) -> Option<CallOutcome> {
        self.bind_tracer(context, |tracer, context| {
            dispatch!(call, tracer, context, inputs)
        })
    }

    fn call_end(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
        outcome: &mut CallOutcome,
    ) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(call_end, tracer, context, inputs, outcome)
        });
    }

    fn create(
        &mut self,
        context: &mut CTX,
        inputs: &mut CreateInputs,
    ) -> Option<CreateOutcome> {
        self.bind_tracer(context, |tracer, context| {
            dispatch!(create, tracer, context, inputs)
        })
    }

    fn create_end(
        &mut self,
        context: &mut CTX,
        inputs: &CreateInputs,
        outcome: &mut CreateOutcome,
    ) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(create_end, tracer, context, inputs, outcome)
        });
    }

    #[inline]
    fn log(&mut self, context: &mut CTX, log: Log) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(log, tracer, context, log)
        });
    }

    fn log_full(
        &mut self,
        interpreter: &mut Interpreter<INTR>,
        context: &mut CTX,
        log: Log,
    ) {
        self.with_tracer(context, |tracer, context| {
            dispatch!(log_full, tracer, interpreter, context, log)
        });
    }

    /// Deliberately a no-op.
    ///
    /// `selfdestruct` is the only [`Inspector`] hook without a context
    /// parameter, so the journal-owned tracer cannot be reached from here.
    /// This is only sound because neither supported tracer
    /// ([`CallTracer`], [`StructLogger`]) implements `selfdestruct`: the
    /// current tracers do not self-destruct either, so there is nothing to
    /// route to. If either ever needs this hook, the journal-owned-tracer
    /// design breaks — the tracer would have to be reachable without a
    /// context, and this override would then need to forward to it.
    fn selfdestruct(&mut self, _contract: Address, _target: Address, _value: U256) {}
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
