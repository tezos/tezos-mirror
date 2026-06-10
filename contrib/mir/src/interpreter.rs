// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Michelson interpreter definitions. Most functions are defined on
//! [Instruction] and [ContractScript], see there for more.

use checked::Checked;
use cryptoxide::hashing::{blake2b_256, keccak256, sha256, sha3_256, sha512};
use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use num_traits::{Signed, ToPrimitive, Zero};
use std::cmp::min;
use std::collections::BTreeMap;
use std::ops::{Shl, Shr};
use std::rc::Rc;
use tezos_crypto_rs::hash::OperationHash;
use tezos_crypto_rs::{
    blake2b::digest_160, hash::ContractKt1Hash, CryptoError, PublicKeySignatureVerifier,
    PublicKeyWithHash,
};
use typed_arena::Arena;

use crate::ast::big_map::{dump_big_map_updates, BigMap, LazyStorageError};
use crate::ast::*;
#[cfg(feature = "bls")]
use crate::bls;
use crate::context::{CtxTrait, TypecheckingCtx};
use crate::gas::{interpret_cost, CompareError, CostOverflow, OutOfGas};
use crate::interpreter::interpret_cost::SigCostError;
use crate::lexer::Prim;
use crate::serializer::DecodeError;
use crate::stack::*;
use crate::typechecker::{typecheck_contract_address, typecheck_value, TcError};

/// Errors possible during interpretation.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum InterpretError<'a> {
    /// Interpreter ran out of gas.
    #[error("Gas_exhaustion")]
    OutOfGas,
    /// Cryptographic error.
    #[error(transparent)]
    CryptoError(#[from] CryptoError),
    /// When performing mutez arithmetic, an overflow occurred.
    #[error("mutez overflow")]
    MutezOverflow,
    /// During a type conversion, a negative mutez was found.
    #[error("negative mutez")]
    NegativeMutez,
    /// Interpreter reached a `FAILWITH` instruction.
    #[error("failed with: {1:?} of type {0:?}")]
    FailedWith(Type, TypedValue<'a>),
    /// Encountered an argument outside of the bounds defined in the documentation. We keep the message prompted by the octez implementaiton.
    #[error("Overflow")]
    Overflow,
    /// An error occurred when working with `big_map` storage.
    #[error("lazy storage error: {0}")]
    LazyStorageError(#[from] LazyStorageError),
    /// Looking up a contract's view (for `VIEW`) failed because the
    /// surrounding context could not produce the view's storage —
    /// host I/O, decoded-code corruption, balance overflow, or an
    /// in-memory encoding failure. Distinct from "view not found",
    /// which is signalled by `Ok(None)` and pushed as `V::Option(None)`
    /// without raising an error.
    #[error("view lookup error: {0}")]
    ViewLookupError(#[from] crate::context::LookupViewError),
    /// Error when encoding serialized data
    ///
    /// Wrapped in `Rc` because [`tezos_data_encoding::enc::BinError`]
    /// does not implement [`Clone`] (its `IOError` variant carries a
    /// non-Clone `std::io::Error`), and this enum derives `Clone`.
    #[error("encoding error: {0}")]
    EncodeError(Rc<tezos_data_encoding::enc::BinError>),
    /// Error when decoding serialized data
    #[error(transparent)]
    DecodeError(#[from] DecodeError),
    /// Error when typechecking unserialized data
    #[error(transparent)]
    TcError(#[from] TcError),
    /// A cost-helper overflowed while computing a gas cost (the helpers do
    /// pure cost arithmetic; gas is charged separately at the call site).
    /// Distinct from [`InterpretError::OutOfGas`] (a direct `Gas::consume`
    /// exhaustion) and kept structure-preserving rather than flattened.
    #[error(transparent)]
    CostOverflow(#[from] CostOverflow),
    /// Comparison-cost failure (cost computation, or an attempt to compare
    /// incomparable values).
    #[error(transparent)]
    CompareError(#[from] CompareError),
    /// A stack access went out of bounds inside the interpreter. Stays a
    /// structured variant resolved at the right depth: stack errors that
    /// spawn in the interpreter propagate directly via `?` instead of
    /// detouring through `TcError`.
    #[error(transparent)]
    StackOob(#[from] StackOob),
    #[allow(missing_docs)]
    #[error(transparent)]
    EnshrinedViewDispatch(#[from] EnshrinedViewDispatchError),
    /// Internal invariant violation reachable only on a typechecker bug or
    /// malformed input — analogous to [`TcError::InternalError`] but raised
    /// from the interpreter side, so the value lands directly in
    /// `InterpretError` without detouring through `TcError`.
    #[error("internal interpreter error: {0}")]
    InternalError(InterpretInvariant),
}

/// `FAILWITH` embeds its popped argument directly in
/// `InterpretError::FailedWith(_, TypedValue<'a>)`. A deep runtime-built
/// value -- e.g. a `LOOP`-built right-comb of `PAIR` cells -- would
/// otherwise overflow the kernel's ~1 MiB Rust stack on the auto-derived
/// `TypedValue` destructor when the error is dropped (L2-1446). Draining
/// here, at the last drop point, lets the kernel's `#[error]` formatter
/// observe the value's original structural shape (matching L1 protocol
/// observability and the TZT `expectation` matcher) before the children
/// are flattened to unit sentinels by [`drain_deep_typed_value`].
///
/// The `Drop` impl forbids partial-move destructuring of `InterpretError`
/// anywhere in the workspace. Existing match sites all consume the error
/// by *reference*: `tzt/expectation.rs::unify_interpreter_error` takes
/// `err: &InterpretError`, and the kernel's
/// `tezosx-tezos-runtime::view::classify_interpret_error` matches on
/// `&e`. Both use match-ergonomics auto-borrow on the inner fields, so
/// `Drop` is irrelevant to their soundness.
///
/// Scope note: this `Drop` closes the *drop-time* overflow on a deep
/// `FailedWith` payload. The *observation-time* overflow via the auto-
/// derived `Debug` walk on `TypedValue` (reached by the kernel's
/// `BadRequest(format!("{e:?}"))` and the `#[error("…{1:?}…")]` derive)
/// is independent and is closed by !21988 (iterative `Debug for
/// TypedValue`, Linear L2-1436). The two MRs are conjugate: this MR
/// makes the drop safe; !21988 makes the observation safe.
impl<'a> Drop for InterpretError<'a> {
    fn drop(&mut self) {
        // Only `FailedWith` currently carries a `TypedValue` payload that
        // can be deep at runtime. If a future variant gains an owned
        // `TypedValue` / `Rc<TypedValue>` reachable from attacker input
        // (e.g. a richer FAILWITH-with-context variant, or a Closure-
        // returning error), extend this match — otherwise its
        // auto-derived destructor would re-introduce the L2-1446
        // overflow on `Drop` of the `Err`.
        if let InterpretError::FailedWith(_, ref mut v) = self {
            crate::ast::drain_deep_typed_value(v);
        }
    }
}

/// Categorises invariant violations reachable only on a typechecker bug or
/// malformed Micheline that bypassed typechecking. Mirrors
/// [`crate::typechecker::TcInvariant`] on the interpreter side: variants
/// either have no payload (single-site invariants where the tag alone
/// identifies the broken precondition) or carry typed, equality-stable
/// fields (`&'static str`) that preserve forensic context without the
/// brittleness of formatted strings.
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum InterpretInvariant {
    #[error("attempted to pop from an empty value stack")]
    EmptyValueStackPop,
    #[error("type mismatch when popping the value stack: expected {expected}")]
    TypeMismatchOnPop { expected: &'static str },
    #[error("type mismatch on the top of the value stack: expected {expected}")]
    TypeMismatchOnTop { expected: &'static str },
    #[error("type mismatch on a typed value: expected {expected}")]
    TypeMismatch { expected: &'static str },
    #[error("unreachable interpreter state reached")]
    UnreachableState,
    #[error("script result was not a `pair operation_list storage`")]
    ExpectedPairResult,
    #[error("script result's operation field was not `list operation`")]
    ExpectedListOperation,
    #[error("script result's operation list contained a non-operation element")]
    ExpectedOperationElement,
}

#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum EnshrinedViewDispatchError {
    #[error("enshrined view dispatch: could not resolve caller alias in the peer runtime")]
    AliasResolution,
    #[error("enshrined view dispatch: gas-budget translation overflow")]
    BudgetOverflow,
    /// Destination string supplied by the Michelson caller could not be
    /// parsed into a valid URL. Caller-side mistake — routes to 400.
    #[error("enshrined view dispatch: invalid destination {destination:?}")]
    InvalidDestination { destination: String },
    /// Kernel-side request construction failure (header assembly, body
    /// construction). Routes to 500.
    #[error("enshrined view dispatch: could not build dispatch request")]
    DispatchSetup,
    #[error("enshrined view dispatch: unclassifiable peer-runtime response: status {status}")]
    UnclassifiableResponse { status: u16 },
}

impl<'a> From<OutOfGas> for InterpretError<'a> {
    fn from(_: OutOfGas) -> Self {
        InterpretError::OutOfGas
    }
}

impl<'a> From<tezos_data_encoding::enc::BinError> for InterpretError<'a> {
    fn from(err: tezos_data_encoding::enc::BinError) -> Self {
        InterpretError::EncodeError(Rc::new(err))
    }
}

impl<'a> From<SigCostError> for InterpretError<'a> {
    fn from(e: SigCostError) -> Self {
        match e {
            SigCostError::Crypto(err) => err.into(),
            SigCostError::OutOfGas(err) => err.into(),
        }
    }
}

/// Errors possible when interpreting a full contract script.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ContractInterpretError<'a> {
    /// Failed to typecheck the provided input as the type expected by the
    /// script.
    #[error("failed typechecking input: {0}")]
    TcError(#[from] crate::typechecker::TcError),
    /// Failed during the interpretation of the script code.
    #[error("runtime failure while running the script: {0}")]
    InterpretError(InterpretError<'a>),
    /// Failed during lazy storage manipulation
    #[error(transparent)]
    LazyStorageError(#[from] LazyStorageError),
}

impl<'a> From<InterpretError<'a>> for ContractInterpretError<'a> {
    fn from(x: InterpretError<'a>) -> Self {
        Self::InterpretError(x)
    }
}

impl<'a> ContractScript<'a> {
    /// Interpret a typechecked contract script using the provided parameter and
    /// storage. Parameter and storage are given as `Micheline`, as this
    /// allows ensuring they satisfy the types expected by the script.
    pub fn interpret(
        &self,
        ctx: &mut impl CtxTrait<'a>,
        arena: &'a Arena<Micheline<'a>>,
        parameter: Micheline<'a>,
        entrypoint: &Entrypoint,
        storage_in: &Micheline<'a>,
    ) -> Result<(impl Iterator<Item = OperationInfo<'a>>, TypedValue<'a>), ContractInterpretError<'a>>
    {
        let wrapped_parameter = self.wrap_parameter(arena, parameter, entrypoint, ctx)?;
        let mut storage = self.typecheck_storage(ctx, storage_in)?;
        let mut started_with_map_ids = vec![];
        storage.view_big_map_ids(&mut started_with_map_ids);

        let tc_val = TypedValue::new_pair(wrapped_parameter, storage);
        let mut stack = stk![tc_val];
        // The interpreter has already drained the restored caller stack
        // and the embedded `FailedWith` value on the error path (see
        // `fn interpret`), so the `?` propagation here is safe regardless
        // of how deep the runtime-built values were (L2-1446).
        self.code.interpret(ctx, arena, &mut stack)?;

        use TypedValue as V;

        let result = TypedValue::unwrap_rc(stack.pop().ok_or(InterpretError::InternalError(
            InterpretInvariant::EmptyValueStackPop,
        ))?);
        let (operation_list, storage) = match result {
            V::Pair(operation_list, storage) => (operation_list, storage),
            _ => {
                return Err(
                    InterpretError::InternalError(InterpretInvariant::ExpectedPairResult).into(),
                )
            }
        };
        let mut operation_list = TypedValue::unwrap_rc(operation_list);
        let mut storage = TypedValue::unwrap_rc(storage);
        // Handle storage big_maps (those big_maps are definitive and will be stored in the durable_storage)
        let mut storage_big_maps = vec![];
        storage.view_big_maps_mut(&mut storage_big_maps);
        let lazy_storage = *ctx.lazy_storage();
        dump_big_map_updates(
            lazy_storage,
            &started_with_map_ids,
            &mut storage_big_maps,
            false,
        )?;
        // Handle big_maps that appears in the operation list, those big_maps are temporary and it depends to
        // the internal operation to determine what to do with it
        let mut operations_big_maps = vec![];
        operation_list.view_big_maps_mut(&mut operations_big_maps);
        dump_big_map_updates(lazy_storage, &[], &mut operations_big_maps, true)?;

        let vec = match operation_list {
            V::List(vec) => vec,
            _ => {
                return Err(
                    InterpretError::InternalError(InterpretInvariant::ExpectedListOperation).into(),
                )
            }
        };

        let mut ops = Vec::with_capacity(vec.len());
        for op in vec {
            match TypedValue::unwrap_rc(op) {
                V::Operation(op) => ops.push(*op),
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::ExpectedOperationElement,
                    )
                    .into())
                }
            }
        }
        Ok((ops.into_iter(), storage))
    }

    /// Wrap the input in the entrypoint with the given name.
    pub fn wrap_parameter(
        &self,
        arena: &'a Arena<Micheline<'a>>,
        parameter: Micheline<'a>,
        entrypoint: &Entrypoint,
        ctx: &mut impl CtxTrait<'a>,
    ) -> Result<TypedValue<'a>, TcError> {
        let parsed_annotation = FieldAnnotation::from_string(entrypoint.to_string());
        if let Some((annotation_path, annotation_type)) = self.annotations.get(&parsed_annotation) {
            typecheck_value(&parameter, ctx, annotation_type)?;
            let mut result = parameter;
            for direction in annotation_path.iter().rev() {
                match direction {
                    Direction::Left => {
                        result = Micheline::prim1(arena, Prim::Left, result, ctx.gas())?;
                    }
                    Direction::Right => {
                        result = Micheline::prim1(arena, Prim::Right, result, ctx.gas())?;
                    }
                }
            }
            Ok(typecheck_value(&result, ctx, &self.parameter)?)
        } else {
            Err(TcError::NoSuchEntrypoint(entrypoint.clone()))
        }
    }

    /// Typechecks the given storage value against the expected storage type of the contract.
    ///
    /// # Arguments
    ///
    /// * `ctx` - The context in which the typechecking is performed.
    /// * `storage` - The storage value to be typechecked.
    ///
    /// # Returns
    ///
    /// Returns a `Result` containing the typechecked `TypedValue` if successful, or a `TcError` if the typechecking fails.
    pub fn typecheck_storage(
        &self,
        ctx: &mut impl TypecheckingCtx<'a>,
        storage: &Micheline<'a>,
    ) -> Result<TypedValue<'a>, TcError> {
        typecheck_value(storage, ctx, &self.storage)
    }
}

impl<'a> Instruction<'a> {
    /// Interpret the instruction with the given `Ctx` and input stack. Note the
    /// interpreter assumes the instruction can execute on the provided stack,
    /// otherwise this function will panic.
    ///
    /// # Panics
    ///
    /// When the instruction can't be executed on the provided stack.
    pub fn interpret(
        &self,
        ctx: &mut impl CtxTrait<'a>,
        arena: &'a Arena<Micheline<'a>>,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        // Unwrap a top-level `Seq` so its body forms the outermost block,
        // matching the recursive `interpret_one(Seq) -> interpret(body)`:
        // both charge exactly one `INTERPRET_RET` (L1 `KNil`). Wrapping the
        // `Seq` in a one-element slice would charge a second `INTERPRET_RET`
        // for the synthetic outer block, over-charging every contract call.
        match self {
            Self::Seq(body) => interpret(body, ctx, arena, stack),
            _ => interpret(std::slice::from_ref(self), ctx, arena, stack),
        }
    }
}

/// Reference to a block of instructions. Arena owned blocks are passed
/// by borrow (no copy); Rc owned blocks (from EXEC, View) hold their Rc
/// so the worklist can keep them alive across iterations. `'a` is the
/// arena lifetime; `'b` is the borrow lifetime of the caller's input
/// slice and may be shorter than `'a`.
#[derive(Clone)]
enum CodeRef<'a, 'b> {
    Borrowed(&'b [Instruction<'a>]),
    Owned(Rc<[Instruction<'a>]>),
}

/// Worklist frame for the iterative interpreter driver. Each variant
/// either kicks the next instruction in a block or finalizes the side
/// effects of a previously opened control flow shape.
enum InterpFrame<'a, 'b> {
    NextInstr {
        block: CodeRef<'a, 'b>,
        idx: usize,
    },
    AfterDip {
        protected: crate::stack::Stack<Rc<TypedValue<'a>>>,
        opt_height: Option<u16>,
    },
    LoopBody {
        body: CodeRef<'a, 'b>,
    },
    LoopLeftBody {
        body: CodeRef<'a, 'b>,
    },
    IterList {
        body: CodeRef<'a, 'b>,
        remaining: std::vec::IntoIter<Rc<TypedValue<'a>>>,
    },
    IterSet {
        body: CodeRef<'a, 'b>,
        remaining: std::collections::btree_set::IntoIter<Rc<TypedValue<'a>>>,
    },
    IterMap {
        body: CodeRef<'a, 'b>,
        remaining: std::collections::btree_map::IntoIter<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>,
    },
    MapListAccum {
        body: CodeRef<'a, 'b>,
        remaining: std::vec::IntoIter<Rc<TypedValue<'a>>>,
        acc: Vec<Rc<TypedValue<'a>>>,
    },
    MapOptionAfter,
    MapMapAccum {
        body: CodeRef<'a, 'b>,
        remaining: std::collections::btree_map::IntoIter<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>,
        acc: BTreeMap<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>,
        current_key: Option<Rc<TypedValue<'a>>>,
    },
    /// EXEC body just finished on the top sub stack; pop sub stack,
    /// take its single result, push to the new top stack.
    AfterExec,
    AfterView {
        saved_self_address: AddressHash,
        saved_sender: AddressHash,
        saved_amount: i64,
        saved_balance: i64,
    },
}

/// What a per instruction step yields. Non recursive arms produce Done;
/// control flow arms produce Open variants that the driver expands.
/// Nested control-flow bodies are carried as [CodeRef]: borrowed straight
/// from the (arena-lived) AST when the parent block is itself borrowed, and
/// only cloned into a fresh `Rc` when the parent is `Owned` (a runtime
/// lambda/view body). EXEC/View bodies are always `Rc` (runtime values).
enum StepResult<'a, 'b> {
    Done,
    OpenBlock(CodeRef<'a, 'b>),
    OpenDip {
        body: CodeRef<'a, 'b>,
        protected: crate::stack::Stack<Rc<TypedValue<'a>>>,
        opt_height: Option<u16>,
    },
    OpenLoop(CodeRef<'a, 'b>),
    OpenLoopLeft(CodeRef<'a, 'b>),
    OpenIterList {
        body: CodeRef<'a, 'b>,
        iter: std::vec::IntoIter<Rc<TypedValue<'a>>>,
    },
    OpenIterSet {
        body: CodeRef<'a, 'b>,
        iter: std::collections::btree_set::IntoIter<Rc<TypedValue<'a>>>,
    },
    OpenIterMap {
        body: CodeRef<'a, 'b>,
        iter: std::collections::btree_map::IntoIter<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>,
    },
    OpenMapList {
        body: CodeRef<'a, 'b>,
        iter: std::vec::IntoIter<Rc<TypedValue<'a>>>,
    },
    OpenMapOption(CodeRef<'a, 'b>),
    OpenMapMap {
        body: CodeRef<'a, 'b>,
        iter: std::collections::btree_map::IntoIter<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>,
        first_key: Rc<TypedValue<'a>>,
    },
    /// EXEC: enter a lambda body on a fresh sub stack containing args.
    OpenExec {
        code: Rc<[Instruction<'a>]>,
        initial: IStack<'a>,
    },
    /// View: enter a typechecked view body on a fresh sub stack plus the
    /// view context to install (the driver restores the saved one on exit).
    OpenView {
        code: Rc<[Instruction<'a>]>,
        initial: IStack<'a>,
        new_self_address: AddressHash,
        new_sender: AddressHash,
        new_amount: i64,
        new_balance: i64,
    },
}

// Counts `body_to_owned` calls so tests can assert that an AST-borrowed
// program clones zero nested bodies. Per-thread; tests reset it at entry.
#[cfg(test)]
thread_local! {
    static BODY_CLONES: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
}

/// Clone a nested body (Vec inside an Instruction variant) into a fresh
/// Rc so its CodeRef::Owned can outlive the parent frame. Only used when
/// the parent block is `Rc` owned (a runtime lambda/view body), where a
/// borrowed slice would not have a valid lifetime across worklist
/// iterations; bodies borrowed from the (arena-lived) AST are kept as
/// `CodeRef::Borrowed` and never reach here.
fn body_to_owned<'a>(body: &[Instruction<'a>]) -> Rc<[Instruction<'a>]> {
    #[cfg(test)]
    BODY_CLONES.with(|c| c.set(c.get() + 1));
    body.to_vec().into_boxed_slice().into()
}

/// Iterative driver: walks every nested block from one explicit worklist
/// and a stack of active IStacks instead of recursing through
/// `interpret_one`. Each call uses the caller's stack as the bottom of
/// the stack of stacks (moved in via `mem::take`, restored on exit).
fn interpret<'a, 'b>(
    ast: &'b [Instruction<'a>],
    ctx: &mut impl CtxTrait<'a>,
    arena: &'a Arena<Micheline<'a>>,
    stack: &mut IStack<'a>,
) -> Result<(), InterpretError<'a>> {
    let initial = std::mem::take(stack);
    let mut stacks: Vec<IStack<'a>> = vec![initial];
    let mut frames: Vec<InterpFrame<'a, 'b>> = vec![InterpFrame::NextInstr {
        block: CodeRef::Borrowed(ast),
        idx: 0,
    }];

    let outcome = run_interp_driver(ctx, arena, &mut stacks, &mut frames);

    // On error the driver `?`-returned before any pending `AfterView`
    // frame could restore the saved view context. The recursive
    // interpreter restored the context unconditionally before
    // propagating a view-body error, so mirror that here. See
    // `restore_pending_view_context`.
    if outcome.is_err() {
        restore_pending_view_context(ctx, &frames);
        // A pending control-flow frame may itself hold a deep runtime-built
        // value (a `DIP`-protected comb, or the not-yet-consumed elements /
        // accumulator of an `ITER`/`MAP` over a deep collection). Drain those
        // before `frames` is dropped, for the same reason the sub-stacks below
        // are drained. On success `frames` is empty, so this never runs.
        drain_value_frames(frames);
    }

    // Restore the caller's stack from the bottom of the stack of stacks, and
    // drain any EXEC/View sub-stacks left above it on the error path before
    // they are dropped. `pop()` would hand the caller an inner sub-stack, so
    // take the bottom (the caller's own) explicitly. A leftover sub-stack may
    // hold a deep runtime-built value (e.g. a LOOP-built PAIR comb); draining
    // it iteratively keeps the recursive `Rc<TypedValue>` destructor from
    // overflowing the kernel's ~1 MiB Rust stack (L2-1446). On success there is
    // exactly one entry, so the drain loop is a no-op.
    let mut stacks = stacks.into_iter();
    let mut bottom = stacks.next().ok_or(InterpretError::InternalError(
        InterpretInvariant::UnreachableState,
    ))?;
    for mut sub in stacks {
        drain_value_stack(&mut sub);
    }
    // On the error-unwind path the caller's restored stack may still carry a
    // deep value pushed before the error (e.g. `PUSH int 0 ; LOOP { PAIR } ;
    // DUP ; FAILWITH` — DUP leaves a deep comb on the stack, FAILWITH pops
    // the other copy). Drain in place here so every caller of
    // `Instruction::interpret` / `fn interpret` (including the EVM gateway's
    // per-instruction view loop in `tezosx-tezos-runtime/src/view.rs`) is
    // protected without needing per-call-site boilerplate, AND the stack
    // shape the caller observes (length and atomic values) is preserved —
    // only the deep composite spine inside any leftover value is severed.
    if outcome.is_err() {
        drain_value_stack_in_place(&mut bottom);
    }
    *stack = bottom;
    outcome
}

/// Reinstall the view context saved by the outermost pending `AfterView`
/// frame, used on the error-unwind path. Frames are pushed innermost-last,
/// so the *first* `AfterView` in `frames` carries the context that was
/// live before any view was entered — restoring it returns the context to
/// its pre-view state, matching the recursive interpreter which restored
/// each level as it unwound. A no-op when no view is pending.
fn restore_pending_view_context<'a>(
    ctx: &mut impl CtxTrait<'a>,
    frames: &[InterpFrame<'a, '_>],
) {
    if let Some(InterpFrame::AfterView {
        saved_self_address,
        saved_sender,
        saved_amount,
        saved_balance,
    }) = frames
        .iter()
        .find(|f| matches!(f, InterpFrame::AfterView { .. }))
    {
        ctx.set_view_context(
            saved_self_address.clone(),
            saved_sender.clone(),
            *saved_amount,
            *saved_balance,
        );
    }
}

/// Drain every value left on a value stack iteratively before it is dropped,
/// so a deep runtime-built value (e.g. a `LOOP`-built `PAIR` comb) does not
/// overflow the kernel's ~1 MiB Rust stack via the recursive `Rc<TypedValue>`
/// destructor. Uniquely-owned values are flattened with
/// [`crate::ast::drain_deep_typed_value`]; shared values are left to their
/// other owners -- only their `Rc` refcount is decremented, which does not
/// recurse. L2-1446.
fn drain_value_stack(stack: &mut IStack<'_>) {
    while let Some(rc) = stack.pop() {
        if let Ok(mut v) = Rc::try_unwrap(rc) {
            crate::ast::drain_deep_typed_value(&mut v);
        }
    }
}

/// In-place sibling of [`drain_value_stack`] for value stacks that the
/// caller still observes after the drain runs. Walks the stack twice:
///
/// 1. First pass: every `Rc<TypedValue>` that is *aliased* on the same
///    stack (e.g. via `DUP` on a runtime-built deep value, leaving two
///    `Rc::clone` siblings of the same inner tree at refcount ≥ 2) is
///    replaced in place by a shallow `Rc<TypedValue::Unit>` sentinel.
///    Each replacement decrements the original `Rc`'s strong count; once
///    only one alias remains on the stack the surviving slot becomes
///    uniquely-owned.
/// 2. Second pass: every now-uniquely-owned `Rc<TypedValue>` is drained
///    in place via [`crate::ast::drain_deep_typed_value`], flattening the
///    deep composite spine while preserving the variant tag.
///
/// Atomic variants (`Int`, `Nat`, `Unit`, …) survive both passes
/// untouched — `drain_deep_typed_value` is a no-op on them, and they are
/// never aliased deeply enough to trigger the sentinel replacement (the
/// recursive `Rc<TypedValue>` destructor only overflows on deep
/// composites). The first-pass sentinel replacement loses the original
/// value at *aliased* slots; this is the documented in-place-drain
/// contract for error-path teardown: callers on the Err path must not
/// rely on aliased slots preserving their original deep content. The
/// existing `error_inside_exec_preserves_caller_stack` test (which uses
/// distinct atomic markers, never aliased deep values) is unaffected.
fn drain_value_stack_in_place(stack: &mut IStack<'_>) {
    // Pass 1: sentinel-replace every aliased Rc so the survivor becomes
    // unique. Atomic values are sometimes aliased too (e.g. a constant
    // pushed via `DUP`), but their shared destructor is shallow — the
    // replacement is harmless on them and necessary on the deep case
    // because we cannot distinguish "shared atom" from "shared deep" at
    // this layer without a recursive walk.
    for i in 0..stack.len() {
        if let Ok(rc) = stack.get_mut(i) {
            if Rc::strong_count(rc) > 1 {
                *rc = Rc::new(TypedValue::Unit);
            }
        }
    }
    // Pass 2: every Rc is now uniquely-owned on this stack (modulo
    // external aliases the kernel cannot enumerate, which the caller's
    // own Drop will handle). Drain the composite spine.
    for i in 0..stack.len() {
        if let Ok(rc) = stack.get_mut(i) {
            if let Some(v) = Rc::get_mut(rc) {
                crate::ast::drain_deep_typed_value(v);
            }
        }
    }
}

/// Drain every `Rc<TypedValue>` still held by the control-flow worklist before
/// the frames are dropped, mirroring [`drain_value_stack`]. On the error-unwind
/// path a frame may carry a deep runtime-built value -- a `DIP`-protected comb,
/// or the not-yet-consumed elements / accumulator of an `ITER`/`MAP` over a
/// deep collection -- whose recursive `Rc<TypedValue>` destructor would
/// overflow the kernel's ~1 MiB Rust stack. Control-only frames (`NextInstr`,
/// `LoopBody`, `AfterExec`, `AfterView`, ...) hold no values and are skipped.
/// L2-1446.
fn drain_value_frames(frames: Vec<InterpFrame<'_, '_>>) {
    let mut pending: Vec<Rc<TypedValue<'_>>> = Vec::new();
    for frame in frames {
        match frame {
            InterpFrame::AfterDip { protected, .. } => pending.extend(protected),
            InterpFrame::IterList { remaining, .. } => pending.extend(remaining),
            InterpFrame::IterSet { remaining, .. } => pending.extend(remaining),
            InterpFrame::IterMap { remaining, .. } => {
                for (k, v) in remaining {
                    pending.push(k);
                    pending.push(v);
                }
            }
            InterpFrame::MapListAccum {
                remaining, acc, ..
            } => {
                pending.extend(remaining);
                pending.extend(acc);
            }
            InterpFrame::MapMapAccum {
                remaining,
                acc,
                current_key,
                ..
            } => {
                for (k, v) in remaining {
                    pending.push(k);
                    pending.push(v);
                }
                for (k, v) in acc {
                    pending.push(k);
                    pending.push(v);
                }
                pending.extend(current_key);
            }
            InterpFrame::NextInstr { .. }
            | InterpFrame::LoopBody { .. }
            | InterpFrame::LoopLeftBody { .. }
            | InterpFrame::MapOptionAfter
            | InterpFrame::AfterExec
            | InterpFrame::AfterView { .. } => {}
        }
    }
    for rc in pending {
        if let Ok(mut v) = Rc::try_unwrap(rc) {
            crate::ast::drain_deep_typed_value(&mut v);
        }
    }
}

/// Borrow the active (top) sub stack. The stack of stacks always has at
/// least the caller stack at the bottom, so an empty stack of stacks is a
/// driver invariant violation rather than a Michelson-level error.
fn active_stack_mut<'a, 'c>(
    stacks: &'c mut [IStack<'a>],
) -> Result<&'c mut IStack<'a>, InterpretError<'a>> {
    stacks.last_mut().ok_or(InterpretError::InternalError(
        InterpretInvariant::UnreachableState,
    ))
}

/// Pop a value off a sub stack, turning an empty stack into the structured
/// `EmptyValueStackPop` invariant (the recursive interpreter relied on
/// typechecking having ensured the value was present).
fn pop_value<'a>(stk: &mut IStack<'a>) -> Result<Rc<TypedValue<'a>>, InterpretError<'a>> {
    stk.pop().ok_or(InterpretError::InternalError(
        InterpretInvariant::EmptyValueStackPop,
    ))
}

/// Gas-charging wrapper for [`Vec::push`] on the interpreter's frame
/// worklist. Replaces every `frames.push(...)` site in the iterative
/// driver so each pending control-flow frame is paid for in Michelson
/// gas. MIR holds the worklist on the heap; L1's OCaml interpreter holds
/// the equivalent on the runtime stack and benefits from automatic
/// tail-call elimination — for L1 the per-frame cost is ≈ 0, for MIR it
/// is `O(InterpFrame size)`. Charging here converts that latent host-
/// memory consumption into a gas-priced resource so a divergent
/// `LAMBDA_REC int int { EXEC }` (or any other unbounded recursion
/// through `IF`/`LOOP`/`ITER`/`MAP`/`VIEW`/`DIP`) hits a clean
/// `OutOfGas` instead of allocating gigabytes and aborting the WASM
/// module with `unreachable`.
fn push_frame<'a, 'b>(
    frames: &mut Vec<InterpFrame<'a, 'b>>,
    ctx: &mut impl CtxTrait<'a>,
    frame: InterpFrame<'a, 'b>,
) -> Result<(), OutOfGas> {
    ctx.gas().consume(interpret_cost::FRAME_PUSH)?;
    frames.push(frame);
    Ok(())
}

/// Gas-charging wrapper for [`Vec::push`] on the per-EXEC/per-VIEW
/// substack worklist. Each push materializes a fresh `IStack`
/// (`Vec<TypedValue>` with its initial entries — lambda+arg for
/// `EXEC`, view-arg for `VIEW`), so the per-push memory footprint is
/// larger than a single [`InterpFrame`]; the gas charge is sized
/// accordingly via [`interpret_cost::STACK_PUSH`]. Together with
/// [`push_frame`] this bounds the iterative driver's heap by the
/// caller's Michelson gas budget.
fn push_stack<'a>(
    stacks: &mut Vec<IStack<'a>>,
    ctx: &mut impl CtxTrait<'a>,
    stack: IStack<'a>,
) -> Result<(), OutOfGas> {
    ctx.gas().consume(interpret_cost::STACK_PUSH)?;
    stacks.push(stack);
    Ok(())
}

/// Run consecutive `StepResult::Done` instructions of `instrs` in place,
/// advancing `*idx` until either the block is exhausted (charging
/// `INTERPRET_RET`) or a non-`Done` step breaks the run.
///
/// Returns `Some(step)` for the breaking step — the caller re-pushes the
/// parent `NextInstr` at the advanced `idx` and dispatches `step` through
/// `handle_step` — or `None` when the block ran out.
///
/// `mk_body` materializes nested control-flow bodies into a [`CodeRef`]:
/// `CodeRef::Borrowed` for arena-borrowed blocks (zero-copy) or a cloning
/// closure for owned `Rc` blocks. Each call site passes a distinct closure
/// type, so the compiler monomorphises one copy of this function per
/// `CodeRef` arm — the borrowed hot path keeps its sub-bodies zero-copy
/// with no runtime dispatch.
fn run_done_until<'a, 'b, 'c>(
    instrs: &'c [Instruction<'a>],
    idx: &mut usize,
    stack: &mut IStack<'a>,
    ctx: &mut impl CtxTrait<'a>,
    arena: &'a Arena<Micheline<'a>>,
    mk_body: impl Copy + Fn(&'c [Instruction<'a>]) -> CodeRef<'a, 'b>,
) -> Result<Option<StepResult<'a, 'b>>, InterpretError<'a>> {
    loop {
        if *idx >= instrs.len() {
            ctx.gas().consume(interpret_cost::INTERPRET_RET)?;
            return Ok(None);
        }
        let step = interpret_step(&instrs[*idx], ctx, arena, stack, mk_body)?;
        *idx += 1;
        match step {
            StepResult::Done => continue,
            other => return Ok(Some(other)),
        }
    }
}

fn run_interp_driver<'a, 'b>(
    ctx: &mut impl CtxTrait<'a>,
    arena: &'a Arena<Micheline<'a>>,
    stacks: &mut Vec<IStack<'a>>,
    frames: &mut Vec<InterpFrame<'a, 'b>>,
) -> Result<(), InterpretError<'a>> {
    while let Some(frame) = frames.pop() {
        match frame {
            InterpFrame::NextInstr { block, mut idx } => {
                // Fast path: stay in this frame while consecutive
                // instructions return `StepResult::Done` (the common
                // case — `PUSH`, `DROP`, `DUP`, `INT`, `NAT`, `ADD`,
                // arithmetic, comparisons, etc.). Only push/pop a new
                // worklist frame when control flow actually opens a
                // sub-block (`Open*` arms). The active sub-stack is
                // hoisted once per block — it only changes when
                // `OpenExec` / `OpenView` runs, and those `break` out
                // of the inner loop before `handle_step` mutates
                // `stacks`.
                //
                // L2-1579: pre-fix, the driver popped + re-pushed the
                // parent `NextInstr` every instruction and called
                // `handle_step` to dispatch `StepResult::Done` into a
                // no-op match arm. For arithmetic-heavy blocks (the
                // int/nat benchmark profile) the per-instruction
                // worklist + sub-stack lookup + `StepResult` round
                // trip dominated the actual instruction work, which
                // is what !21984 regressed. The inner-loop form
                // (`run_done_until`) pays worklist + `handle_step`
                // cost only once per control-flow event.
                //
                // The `CodeRef::{Borrowed, Owned}` split survives: a
                // borrowed block keeps sub-bodies arena-borrowed
                // (zero-copy), an owned block clones via
                // `body_to_owned`. Each arm hands `run_done_until` a
                // distinct `mk_body` closure, so it is monomorphised
                // per arm — same codegen as two hand-inlined loops.
                let stack = active_stack_mut(stacks)?;
                let pending = match &block {
                    CodeRef::Borrowed(s) => {
                        run_done_until(s, &mut idx, stack, ctx, arena, CodeRef::Borrowed)?
                    }
                    CodeRef::Owned(rc) => run_done_until(rc, &mut idx, stack, ctx, arena, |b| {
                        CodeRef::Owned(body_to_owned(b))
                    })?,
                };
                if let Some(step) = pending {
                    frames.push(InterpFrame::NextInstr { block, idx });
                    handle_step(step, ctx, stacks, frames)?;
                }
            }
            InterpFrame::AfterDip {
                protected,
                opt_height,
            } => {
                let stk = active_stack_mut(stacks)?;
                ctx.gas()
                    .consume(interpret_cost::undip(opt_height.unwrap_or(1))?)?;
                let mut protected = protected;
                stk.append(&mut protected);
            }
            InterpFrame::LoopBody { body } => {
                let stk = active_stack_mut(stacks)?;
                ctx.gas().consume(interpret_cost::LOOP)?;
                let cond = match TypedValue::unwrap_rc(pop_value(stk)?) {
                    TypedValue::Bool(b) => b,
                    _ => {
                        return Err(InterpretError::InternalError(
                            InterpretInvariant::TypeMismatchOnPop {
                                expected: "TypedValue::Bool",
                            },
                        ))
                    }
                };
                if cond {
                    frames.push(InterpFrame::LoopBody {
                        body: body.clone(),
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                } else {
                    ctx.gas().consume(interpret_cost::LOOP_EXIT)?;
                }
            }
            InterpFrame::LoopLeftBody { body } => {
                let stk = active_stack_mut(stacks)?;
                ctx.gas().consume(interpret_cost::LOOP)?;
                let or = match TypedValue::unwrap_rc(pop_value(stk)?) {
                    TypedValue::Or(or) => or,
                    _ => {
                        return Err(InterpretError::InternalError(
                            InterpretInvariant::TypeMismatchOnPop {
                                expected: "TypedValue::Or",
                            },
                        ))
                    }
                };
                match or {
                    Or::Left(x) => {
                        stk.push(x);
                        frames.push(InterpFrame::LoopLeftBody {
                            body: body.clone(),
                        });
                        frames.push(InterpFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    Or::Right(x) => {
                        stk.push(x);
                        ctx.gas().consume(interpret_cost::LOOP_EXIT)?;
                    }
                }
            }
            InterpFrame::IterList {
                body,
                mut remaining,
            } => {
                if let Some(elem) = remaining.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    active_stack_mut(stacks)?.push(elem);
                    frames.push(InterpFrame::IterList {
                        body: body.clone(),
                        remaining,
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                }
            }
            InterpFrame::IterSet {
                body,
                mut remaining,
            } => {
                if let Some(elem) = remaining.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    active_stack_mut(stacks)?.push(elem);
                    frames.push(InterpFrame::IterSet {
                        body: body.clone(),
                        remaining,
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                }
            }
            InterpFrame::IterMap {
                body,
                mut remaining,
            } => {
                if let Some((k, v)) = remaining.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    active_stack_mut(stacks)?.push(TypedValue::Pair(k, v));
                    frames.push(InterpFrame::IterMap {
                        body: body.clone(),
                        remaining,
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                }
            }
            InterpFrame::MapListAccum {
                body,
                mut remaining,
                mut acc,
            } => {
                // The body just finished; its result is on top of the stack.
                let stk = active_stack_mut(stacks)?;
                let result = pop_value(stk)?;
                acc.push(result);
                if let Some(elem) = remaining.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    stk.push(elem);
                    frames.push(InterpFrame::MapListAccum {
                        body: body.clone(),
                        remaining,
                        acc,
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                } else {
                    stk.push(TypedValue::List(crate::ast::MichelsonList::from(acc)));
                }
            }
            InterpFrame::MapOptionAfter => {
                let stk = active_stack_mut(stacks)?;
                let result = pop_value(stk)?;
                stk.push(TypedValue::new_option(Some(TypedValue::unwrap_rc(result))));
            }
            InterpFrame::MapMapAccum {
                body,
                mut remaining,
                mut acc,
                current_key,
            } => {
                // Body just finished; pop the resulting value and pair it
                // with the key we pushed.
                let stk = active_stack_mut(stacks)?;
                let new_val = pop_value(stk)?;
                let prev_key = current_key.ok_or(InterpretError::InternalError(
                    InterpretInvariant::UnreachableState,
                ))?;
                acc.insert(prev_key, new_val);
                if let Some((k, v)) = remaining.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    stk.push(TypedValue::Pair(k.clone(), v));
                    frames.push(InterpFrame::MapMapAccum {
                        body: body.clone(),
                        remaining,
                        acc,
                        current_key: Some(k),
                    });
                    frames.push(InterpFrame::NextInstr {
                        block: body,
                        idx: 0,
                    });
                } else {
                    stk.push(TypedValue::Map(acc));
                }
            }
            InterpFrame::AfterExec => {
                let mut sub = stacks.pop().ok_or(InterpretError::InternalError(
                    InterpretInvariant::UnreachableState,
                ))?;
                let result = pop_value(&mut sub)?;
                active_stack_mut(stacks)?.push(result);
            }
            InterpFrame::AfterView {
                saved_self_address,
                saved_sender,
                saved_amount,
                saved_balance,
            } => {
                let mut sub = stacks.pop().ok_or(InterpretError::InternalError(
                    InterpretInvariant::UnreachableState,
                ))?;
                ctx.set_view_context(
                    saved_self_address,
                    saved_sender,
                    saved_amount,
                    saved_balance,
                );
                let result = pop_value(&mut sub)?;
                active_stack_mut(stacks)?
                    .push(TypedValue::new_option(Some(TypedValue::unwrap_rc(result))));
            }
        }
    }
    Ok(())
}

fn handle_step<'a, 'b>(
    step: StepResult<'a, 'b>,
    ctx: &mut impl CtxTrait<'a>,
    stacks: &mut Vec<IStack<'a>>,
    frames: &mut Vec<InterpFrame<'a, 'b>>,
) -> Result<(), InterpretError<'a>> {
    match step {
        StepResult::Done => {}
        StepResult::OpenBlock(body) => {
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: body,
                idx: 0,
            })?;
        }
        StepResult::OpenDip {
            body,
            protected,
            opt_height,
        } => {
            push_frame(frames, ctx, InterpFrame::AfterDip {
                protected,
                opt_height,
            })?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: body,
                idx: 0,
            })?;
        }
        StepResult::OpenLoop(body) => {
            push_frame(frames, ctx, InterpFrame::LoopBody { body })?;
        }
        StepResult::OpenLoopLeft(body) => {
            push_frame(frames, ctx, InterpFrame::LoopLeftBody { body })?;
        }
        StepResult::OpenIterList { body, iter } => {
            push_frame(frames, ctx, InterpFrame::IterList {
                body,
                remaining: iter,
            })?;
        }
        StepResult::OpenIterSet { body, iter } => {
            push_frame(frames, ctx, InterpFrame::IterSet {
                body,
                remaining: iter,
            })?;
        }
        StepResult::OpenIterMap { body, iter } => {
            push_frame(frames, ctx, InterpFrame::IterMap {
                body,
                remaining: iter,
            })?;
        }
        StepResult::OpenMapList { body, iter } => {
            // First element was already pushed by interpret_step; run the
            // body and then collect from MapListAccum.
            push_frame(frames, ctx, InterpFrame::MapListAccum {
                body: body.clone(),
                remaining: iter,
                acc: Vec::new(),
            })?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: body,
                idx: 0,
            })?;
        }
        StepResult::OpenMapOption(body) => {
            push_frame(frames, ctx, InterpFrame::MapOptionAfter)?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: body,
                idx: 0,
            })?;
        }
        StepResult::OpenMapMap {
            body,
            iter,
            first_key,
        } => {
            push_frame(frames, ctx, InterpFrame::MapMapAccum {
                body: body.clone(),
                remaining: iter,
                acc: BTreeMap::new(),
                current_key: Some(first_key),
            })?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: body,
                idx: 0,
            })?;
        }
        StepResult::OpenExec { code, initial } => {
            push_stack(stacks, ctx, initial)?;
            push_frame(frames, ctx, InterpFrame::AfterExec)?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: CodeRef::Owned(code),
                idx: 0,
            })?;
        }
        StepResult::OpenView {
            code,
            initial,
            new_self_address,
            new_sender,
            new_amount,
            new_balance,
        } => {
            let saved_self_address = ctx.self_address().clone();
            let saved_sender = ctx.sender().clone();
            let saved_amount = ctx.amount();
            let saved_balance = ctx.balance();
            // Queue the `AfterView` restore frame first so that, if any
            // subsequent `push_*` returns `OutOfGas`, the unwind path's
            // `restore_pending_view_context` (which scans `frames` for the
            // first `AfterView`) sees the pre-VIEW save and rolls `ctx`
            // back to it. Mutating `ctx` first and then pushing would
            // leave a window where `ctx` is in the inner-view state
            // without any restore frame on the worklist.
            push_frame(frames, ctx, InterpFrame::AfterView {
                saved_self_address,
                saved_sender,
                saved_amount,
                saved_balance,
            })?;
            ctx.set_view_context(new_self_address, new_sender, new_amount, new_balance);
            push_stack(stacks, ctx, initial)?;
            push_frame(frames, ctx, InterpFrame::NextInstr {
                block: CodeRef::Owned(code),
                idx: 0,
            })?;
        }
    }
    Ok(())
}

/// Per instruction dispatcher used by the iterative driver. Recursive
/// instructions return an `Open` variant carrying the body and any state
/// the driver needs; everything else delegates to `interpret_one` for
/// the side effects on the active stack.
fn interpret_step<'a, 'b, 'c>(
    i: &'c Instruction<'a>,
    ctx: &mut impl CtxTrait<'a>,
    arena: &'a Arena<Micheline<'a>>,
    stack: &mut IStack<'a>,
    // Materializes a nested control-flow body into a [CodeRef]. The driver
    // passes `CodeRef::Borrowed` when this instruction comes from a borrowed
    // (arena-lived) block — keeping nested bodies zero-copy — and a cloning
    // closure when it comes from an owned `Rc` block (see the `NextInstr`
    // arm of `run_interp_driver`).
    mk_body: impl Fn(&'c [Instruction<'a>]) -> CodeRef<'a, 'b>,
) -> Result<StepResult<'a, 'b>, InterpretError<'a>> {
    use Instruction as I;
    use TypedValue as V;

    // Panic-free counterparts of the `pop!`/`top_mut!` macros used in
    // `interpret_one`: invariant violations (typechecking should have
    // ruled them out) surface as `InterpretError::InternalError` rather
    // than panicking.
    macro_rules! pop_v {
        ($p:path) => {{
            match TypedValue::unwrap_rc(pop_value(stack)?) {
                #[allow(unused_parens)]
                $p(i) => i,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnPop {
                            expected: stringify!($p),
                        },
                    ))
                }
            }
        }};
    }

    match i {
        I::Seq(body) => Ok(StepResult::OpenBlock(mk_body(body))),
        I::Dip(opt_height, body) => {
            ctx.gas().consume(interpret_cost::dip(*opt_height)?)?;
            let protected_height: u16 = opt_height.unwrap_or(1);
            let protected = stack.split_off(protected_height as usize);
            Ok(StepResult::OpenDip {
                body: mk_body(body),
                protected,
                opt_height: *opt_height,
            })
        }
        I::If(t, f) => {
            ctx.gas().consume(interpret_cost::IF)?;
            let body = if pop_v!(V::Bool) { t } else { f };
            Ok(StepResult::OpenBlock(mk_body(body)))
        }
        I::IfNone(when_none, when_some) => {
            ctx.gas().consume(interpret_cost::IF_NONE)?;
            let body = match pop_v!(V::Option) {
                Some(x) => {
                    stack.push(x);
                    when_some
                }
                None => when_none,
            };
            Ok(StepResult::OpenBlock(mk_body(body)))
        }
        I::IfCons(when_cons, when_nil) => {
            ctx.gas().consume(interpret_cost::IF_CONS)?;
            let lst = match Rc::make_mut(stack.get_mut(0)?) {
                V::List(lst) => lst,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnTop {
                            expected: "V::List",
                        },
                    ))
                }
            };
            let body = match lst.uncons() {
                Some(x) => {
                    stack.push(x);
                    when_cons
                }
                None => {
                    let _ = pop_value(stack)?;
                    when_nil
                }
            };
            Ok(StepResult::OpenBlock(mk_body(body)))
        }
        I::IfLeft(when_left, when_right) => {
            ctx.gas().consume(interpret_cost::IF_LEFT)?;
            let or = pop_v!(V::Or);
            let body = match or {
                Or::Left(x) => {
                    stack.push(x);
                    when_left
                }
                Or::Right(x) => {
                    stack.push(x);
                    when_right
                }
            };
            Ok(StepResult::OpenBlock(mk_body(body)))
        }
        I::Loop(body) => {
            ctx.gas().consume(interpret_cost::LOOP_ENTER)?;
            Ok(StepResult::OpenLoop(mk_body(body)))
        }
        I::LoopLeft(body) => {
            ctx.gas().consume(interpret_cost::LOOP_LEFT_ENTER)?;
            Ok(StepResult::OpenLoopLeft(mk_body(body)))
        }
        I::Iter(overload, body) => {
            ctx.gas().consume(interpret_cost::ITER)?;
            match overload {
                overloads::Iter::List => {
                    let lst = pop_v!(V::List);
                    Ok(StepResult::OpenIterList {
                        body: mk_body(body),
                        iter: Vec::from(lst).into_iter(),
                    })
                }
                overloads::Iter::Set => {
                    let set = pop_v!(V::Set);
                    Ok(StepResult::OpenIterSet {
                        body: mk_body(body),
                        iter: set.into_iter(),
                    })
                }
                overloads::Iter::Map => {
                    let map = pop_v!(V::Map);
                    Ok(StepResult::OpenIterMap {
                        body: mk_body(body),
                        iter: map.into_iter(),
                    })
                }
            }
        }
        I::Map(overload, body) => match overload {
            overloads::Map::List => {
                ctx.gas().consume(interpret_cost::MAP_LIST)?;
                let list = pop_v!(V::List);
                let mut iter = Vec::from(list).into_iter();
                if let Some(first) = iter.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    stack.push(first);
                    Ok(StepResult::OpenMapList {
                        body: mk_body(body),
                        iter,
                    })
                } else {
                    stack.push(V::List(crate::ast::MichelsonList::default()));
                    Ok(StepResult::Done)
                }
            }
            overloads::Map::Option => {
                ctx.gas().consume(interpret_cost::MAP_OPTION)?;
                let option = pop_v!(V::Option);
                match option {
                    Some(elem) => {
                        ctx.gas().consume(interpret_cost::PUSH)?;
                        stack.push(elem);
                        Ok(StepResult::OpenMapOption(mk_body(body)))
                    }
                    None => {
                        stack.push(V::new_option(None));
                        Ok(StepResult::Done)
                    }
                }
            }
            overloads::Map::Map => {
                ctx.gas().consume(interpret_cost::MAP_MAP)?;
                let map = pop_v!(V::Map);
                // Iterate the map's entries directly: unlike L1, which folds the
                // map into a list first, MIR walks the `BTreeMap` in place, so
                // there is no O(n) materialisation step to charge for.
                let mut iter = map.into_iter();
                if let Some((k, v)) = iter.next() {
                    ctx.gas().consume(interpret_cost::PUSH)?;
                    stack.push(TypedValue::Pair(k.clone(), v));
                    Ok(StepResult::OpenMapMap {
                        body: mk_body(body),
                        iter,
                        first_key: k,
                    })
                } else {
                    stack.push(V::Map(BTreeMap::new()));
                    Ok(StepResult::Done)
                }
            }
        },
        I::Exec => {
            ctx.gas().consume(interpret_cost::EXEC)?;
            let mut arg = TypedValue::unwrap_rc(pop_value(stack)?);
            let mut closure = pop_v!(V::Lambda);
            // Inline APPLY unwrapping (constant work, not stack recursive).
            loop {
                match closure {
                    Closure::Lambda(lam) => match lam {
                        Lambda::LambdaRec {
                            in_ty,
                            out_ty,
                            micheline_code,
                            code,
                        } => {
                            let code_clone = Rc::clone(&code);
                            // Recursive lambdas put themselves on top of
                            // the body's stack so the body can EXEC again.
                            let initial = stk![
                                V::Lambda(Closure::Lambda(Lambda::LambdaRec {
                                    in_ty,
                                    out_ty,
                                    micheline_code,
                                    code,
                                })),
                                arg
                            ];
                            return Ok(StepResult::OpenExec {
                                code: code_clone,
                                initial,
                            });
                        }
                        Lambda::Lambda { code, .. } => {
                            let initial = stk![arg];
                            return Ok(StepResult::OpenExec { code, initial });
                        }
                    },
                    Closure::Apply {
                        arg_val,
                        closure: inner,
                        ..
                    } => {
                        ctx.gas().consume(interpret_cost::PAIR)?;
                        arg = V::new_pair(*arg_val, arg);
                        closure = *inner;
                    }
                }
            }
        }
        I::IView { name, return_type } => {
            let input = TypedValue::unwrap_rc(pop_value(stack)?);
            let Address {
                hash,
                entrypoint: _,
            } = pop_v!(V::Address);

            ctx.gas().consume(interpret_cost::VIEW)?;

            let kt1 = match hash {
                AddressHash::Kt1(kt1) => kt1,
                _ => {
                    stack.push(V::Option(None));
                    return Ok(StepResult::Done);
                }
            };

            if let Some(result) =
                ctx.try_dispatch_enshrined_view(&kt1, name, &input, return_type, arena)
            {
                stack.push(V::new_option(result?));
                return Ok(StepResult::Done);
            }

            let Some((view, view_storage_ty, view_storage, view_balance)) =
                ctx.lookup_view_storage_balance(&kt1, name, arena)?
            else {
                stack.push(V::Option(None));
                return Ok(StepResult::Done);
            };

            let storage_ty = view_storage_ty.parse_ty(ctx.gas())?;
            let storage_ty_mich = storage_ty.into_micheline_optimized_legacy(arena, ctx.gas())?;
            let storage = Micheline::decode_raw(arena, &view_storage, ctx.gas())??
                .typecheck_value(ctx, &storage_ty_mich)?;
            let input_type = view.input_type.parse_ty(ctx.gas())?;
            let output_type = view.output_type.parse_ty(ctx.gas())?;

            if let Micheline::Seq(instrs) = view.code {
                if let Ok(code) = crate::typechecker::typecheck_view(
                    instrs,
                    ctx.gas(),
                    Type::Pair(PairBox::new(input_type, storage_ty)),
                    output_type,
                ) {
                    let initial = stk![TypedValue::new_pair(input, storage)];
                    return Ok(StepResult::OpenView {
                        code,
                        initial,
                        new_self_address: AddressHash::Kt1(kt1),
                        new_sender: ctx.self_address().clone(),
                        new_amount: 0,
                        new_balance: view_balance,
                    });
                }
            }
            stack.push(V::Option(None));
            Ok(StepResult::Done)
        }
        // Non recursive arms: delegate to the side effect only function.
        _ => {
            interpret_one(i, ctx, arena, stack)?;
            Ok(StepResult::Done)
        }
    }
}

fn interpret_one<'a>(
    i: &Instruction<'a>,
    ctx: &mut impl CtxTrait<'a>,
    arena: &'a Arena<Micheline<'a>>,
    stack: &mut IStack<'a>,
) -> Result<(), InterpretError<'a>> {
    use Instruction as I;
    use TypedValue as V;

    // Two macros: `pop_rc!` keeps the `Rc<TypedValue>`, `pop!` calls
    // `unwrap_rc` on it. Prefer `pop_rc!` for read-only / forwarding
    // access (no risk of deep-cloning a shared collection); use `pop!`
    // only when the value is consumed by value.
    //
    // Usage:
    // `pop_rc!()` pops the top element from the stack, propagating
    //   `InterpretError::InternalError(EmptyValueStackPop)` on `?` if empty.
    // `pop!()` pops and `unwrap_rc`s the top element, propagating the same
    //   error on `?`.
    // `pop!(T::Foo)` pops, `unwrap_rc`s, then matches `T::Foo(x)` returning
    //   `x`; on type mismatch propagates
    //   `InterpretError::InternalError(TypeMismatchOnPop { expected })`.
    // `pop!(T::Bar, x, y, z)` pops, `unwrap_rc`s, matches `T::Bar(x, y, z)`
    //   and binds the fields in the surrounding scope (statement form).
    //
    // `top_mut!(T::Foo)` borrows the top of the stack as `&mut Foo`'s inner
    //   payload, propagating `StackOob` on empty and
    //   `InterpretError::InternalError(TypeMismatchOnTop { expected })` on
    //   variant mismatch.
    macro_rules! pop_rc {
        () => {{
            stack.pop().ok_or(InterpretError::InternalError(
                InterpretInvariant::EmptyValueStackPop,
            ))?
        }};
    }

    macro_rules! pop {
        () => {{
            TypedValue::unwrap_rc(pop_rc!())
        }};
        ($p:path) => {{
            match pop!() {
                #[allow(unused_parens)]
                $p(i) => i,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnPop {
                            expected: stringify!($p),
                        },
                    ))
                }
            }
        }};
        ($p:path, $( $a:ident ),+) => {
            #[allow(unused_parens)]
            let ($($a),*) = match pop!() {
                $p($($a),*) => ($($a),*),
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnPop {
                            expected: stringify!($p),
                        },
                    ))
                }
            };
        };
    }

    // `pop_ref!(x, Foo)` pops the stack and binds `x` as a borrow into
    // `V::Foo`'s payload, without cloning. Expands to two `let`s so the
    // intermediate `Rc<TypedValue>` lives in the caller's scope and `x`
    // stays a valid reference.
    macro_rules! pop_ref {
        ($var:ident, $ctor:tt) => {
            let __pop_ref_rc = pop_rc!();
            let $var = match &*__pop_ref_rc {
                V::$ctor(x) => x,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnPop {
                            expected: concat!("V::", stringify!($ctor)),
                        },
                    ))
                }
            };
        };
    }

    macro_rules! top_mut {
        ($p:path) => {{
            let top_rc = stack.get_mut(0)?;
            match Rc::make_mut(top_rc) {
                #[allow(unused_parens)]
                $p(i) => i,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnTop {
                            expected: stringify!($p),
                        },
                    ))
                }
            }
        }};
    }

    match i {
        I::Add(overload) => match overload {
            overloads::Add::IntInt => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Nat(sum));
            }
            overloads::Add::IntNat => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + BigInt::from(o2);
                stack.push(V::Int(sum));
            }
            overloads::Add::NatInt => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = BigInt::from(o1) + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::MutezMutez => {
                let o1 = pop!(V::Mutez);
                let o2 = pop!(V::Mutez);
                ctx.gas().consume(interpret_cost::ADD_TEZ)?;
                let sum = o1.checked_add(o2).ok_or(InterpretError::Overflow)?;
                stack.push(V::Mutez(sum));
            }
            #[cfg(feature = "bls")]
            overloads::Add::Bls12381Fr => {
                let o1 = pop!(V::Bls12381Fr);
                let o2 = pop!(V::Bls12381Fr);
                ctx.gas().consume(interpret_cost::ADD_BLS_FR)?;
                stack.push(V::Bls12381Fr(o1 + o2));
            }
            #[cfg(feature = "bls")]
            overloads::Add::Bls12381G1 => {
                let o1 = pop!(V::Bls12381G1);
                let o2 = pop!(V::Bls12381G1);
                ctx.gas().consume(interpret_cost::ADD_BLS_G1)?;
                stack.push(V::new_bls12381_g1(o1.as_ref() + o2.as_ref()));
            }
            #[cfg(feature = "bls")]
            overloads::Add::Bls12381G2 => {
                let o1 = pop!(V::Bls12381G2);
                let o2 = pop!(V::Bls12381G2);
                ctx.gas().consume(interpret_cost::ADD_BLS_G2)?;
                stack.push(V::new_bls12381_g2(o1.as_ref() + o2.as_ref()));
            }
            overloads::Add::IntTimestamp => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Timestamp);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                stack.push(V::Timestamp(o1 + o2));
            }
            overloads::Add::TimestampInt => {
                let o1 = pop!(V::Timestamp);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::add_num(&o1, &o2)?)?;
                stack.push(V::Timestamp(o1 + o2));
            }
        },
        I::Sub(overload) => match overload {
            overloads::Sub::IntInt => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                let diff = o1 - o2;
                stack.push(V::Int(diff));
            }
            overloads::Sub::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                let diff = BigInt::from(o1) - BigInt::from(o2);
                stack.push(V::Int(diff));
            }
            overloads::Sub::IntNat => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                let diff = o1 - BigInt::from(o2);
                stack.push(V::Int(diff));
            }
            overloads::Sub::NatInt => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                let diff = BigInt::from(o1) - o2;
                stack.push(V::Int(diff));
            }
            overloads::Sub::TimestampInt => {
                let o1 = pop!(V::Timestamp);
                let o2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                stack.push(V::Timestamp(o1 - o2));
            }
            overloads::Sub::TimestampTimestamp => {
                let o1 = pop!(V::Timestamp);
                let o2 = pop!(V::Timestamp);
                ctx.gas().consume(interpret_cost::sub_num(&o1, &o2)?)?;
                stack.push(V::Int(o1 - o2));
            }
        },
        I::Mul(overload) => match overload {
            overloads::Mul::NatNat => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * x2;
                stack.push(V::Nat(res));
            }
            overloads::Mul::NatInt => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = BigInt::from(x1) * x2;
                stack.push(V::Int(res));
            }
            overloads::Mul::IntNat => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * BigInt::from(x2);
                stack.push(V::Int(res));
            }
            overloads::Mul::IntInt => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * x2;
                stack.push(V::Int(res));
            }
            overloads::Mul::MutezNat => {
                ctx.gas().consume(interpret_cost::MUL_TEZ_NAT)?;
                let x1 = pop!(V::Mutez);
                let x2 = i64::try_from(pop!(V::Nat)).map_err(|_| InterpretError::Overflow)?;
                let res = x1.checked_mul(x2).ok_or(InterpretError::Overflow)?;
                stack.push(V::Mutez(res));
            }
            overloads::Mul::NatMutez => {
                ctx.gas().consume(interpret_cost::MUL_NAT_TEZ)?;
                let x1 = i64::try_from(pop!(V::Nat)).map_err(|_| InterpretError::Overflow)?;
                let x2 = pop!(V::Mutez);
                let res = x1.checked_mul(x2).ok_or(InterpretError::Overflow)?;
                stack.push(V::Mutez(res));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::Bls12381G1Bls12381Fr => {
                ctx.gas().consume(interpret_cost::MUL_BLS_G1)?;
                let x1 = pop!(V::Bls12381G1);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::new_bls12381_g1(x1.as_ref() * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::Bls12381G2Bls12381Fr => {
                ctx.gas().consume(interpret_cost::MUL_BLS_G2)?;
                let x1 = pop!(V::Bls12381G2);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::new_bls12381_g2(x1.as_ref() * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::Bls12381FrBls12381Fr => {
                ctx.gas().consume(interpret_cost::MUL_BLS_FR)?;
                let x1 = pop!(V::Bls12381Fr);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::NatBls12381Fr => {
                let nat = pop!(V::Nat);
                ctx.gas()
                    .consume(interpret_cost::mul_bls_fr_big_int(&nat)?)?;
                let x1 = bls::Fr::from_big_int(&nat.into());
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::IntBls12381Fr => {
                let int = pop!(V::Int);
                ctx.gas()
                    .consume(interpret_cost::mul_bls_fr_big_int(&int)?)?;
                let x1 = bls::Fr::from_big_int(&int);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::Bls12381FrNat => {
                let x1 = pop!(V::Bls12381Fr);
                let nat = pop!(V::Nat);
                ctx.gas()
                    .consume(interpret_cost::mul_bls_fr_big_int(&nat)?)?;
                let x2 = bls::Fr::from_big_int(&nat.into());
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            #[cfg(feature = "bls")]
            overloads::Mul::Bls12381FrInt => {
                let x1 = pop!(V::Bls12381Fr);
                let int = pop!(V::Int);
                ctx.gas()
                    .consume(interpret_cost::mul_bls_fr_big_int(&int)?)?;
                let x2 = bls::Fr::from_big_int(&int);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
        },
        I::EDiv(overload) => match overload {
            overloads::EDiv::NatNat => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::ediv_nat(&x1, &x2)?)?;
                if x2 == BigUint::zero() {
                    stack.push(V::Option(None));
                } else {
                    let (quotient, remainder) = BigUint::div_rem(&x1, &x2);
                    stack.push(V::new_option(Some(V::new_pair(
                        V::Nat(quotient),
                        V::Nat(remainder),
                    ))));
                }
            }
            overloads::EDiv::NatInt => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::ediv_int(&x1, &x2)?)?;
                if x2 == BigInt::zero() {
                    stack.push(V::Option(None));
                } else {
                    let (quotient, remainder) = BigUint::div_rem(&x1, x2.magnitude());
                    if x2.sign() == Sign::Plus {
                        stack.push(V::new_option(Some(V::new_pair(
                            V::Int(BigInt::from_biguint(Sign::Plus, quotient)),
                            V::Nat(remainder),
                        ))));
                    } else {
                        stack.push(V::new_option(Some(V::new_pair(
                            V::Int(BigInt::from_biguint(Sign::Minus, quotient)),
                            V::Nat(remainder),
                        ))));
                    }
                }
            }
            overloads::EDiv::IntNat => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::ediv_int(&x1, &x2)?)?;
                if x2 == BigUint::zero() {
                    stack.push(V::Option(None));
                } else {
                    let (quotient, remainder) = BigUint::div_rem(x1.magnitude(), &x2);
                    if x1.sign() != Sign::Minus {
                        stack.push(V::new_option(Some(V::new_pair(
                            V::Int(BigInt::from_biguint(Sign::Plus, quotient)),
                            V::Nat(remainder),
                        ))));
                    } else {
                        stack.push(V::new_option(Some(if remainder > BigUint::zero() {
                            V::new_pair(
                                V::Int(BigInt::from_biguint(Sign::Minus, quotient) - 1),
                                V::Nat(x2 - remainder),
                            )
                        } else {
                            V::new_pair(
                                V::Int(BigInt::from_biguint(Sign::Minus, quotient)),
                                V::Nat(remainder),
                            )
                        })));
                    }
                }
            }
            overloads::EDiv::IntInt => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Int);
                ctx.gas().consume(interpret_cost::ediv_int(&x1, &x2)?)?;
                if x2.sign() == Sign::NoSign {
                    stack.push(V::Option(None));
                } else {
                    let (quotient, remainder) = BigUint::div_rem(x1.magnitude(), x2.magnitude());
                    match (x1.sign(), x2.sign()) {
                        (Sign::Minus, Sign::Minus) => {
                            stack.push(V::new_option(Some(if remainder > BigUint::zero() {
                                V::new_pair(
                                    V::Int(BigInt::from_biguint(Sign::Plus, quotient) + 1),
                                    V::Nat(x2.magnitude() - remainder),
                                )
                            } else {
                                V::new_pair(
                                    V::Int(BigInt::from_biguint(Sign::Plus, quotient)),
                                    V::Nat(remainder),
                                )
                            })));
                        }
                        (Sign::Minus, Sign::Plus) => {
                            stack.push(V::new_option(Some(if remainder > BigUint::zero() {
                                V::new_pair(
                                    V::Int(BigInt::from_biguint(Sign::Minus, quotient) - 1),
                                    V::Nat(x2.magnitude() - remainder),
                                )
                            } else {
                                V::new_pair(
                                    V::Int(BigInt::from_biguint(Sign::Minus, quotient)),
                                    V::Nat(remainder),
                                )
                            })));
                        }
                        (_, Sign::Plus) => {
                            stack.push(V::new_option(Some(V::new_pair(
                                V::Int(BigInt::from_biguint(Sign::Plus, quotient)),
                                V::Nat(remainder),
                            ))));
                        }
                        (_, Sign::Minus) => {
                            stack.push(V::new_option(Some(V::new_pair(
                                V::Int(BigInt::from_biguint(Sign::Minus, quotient)),
                                V::Nat(remainder),
                            ))));
                        }
                        _ => stack.push(V::Option(None)),
                    }
                }
            }
            overloads::EDiv::MutezNat => {
                let x1 = pop!(V::Mutez);
                let x2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::EDIV_TEZ_NAT)?;
                if x2 == BigUint::zero() {
                    stack.push(V::Option(None));
                } else {
                    let x2_i64 = x2.to_i64();
                    match x2_i64 {
                        Some(x2_i64) => {
                            let (quotient, remainder) = Integer::div_rem(&x1, &x2_i64);
                            stack.push(V::new_option(Some(V::new_pair(
                                V::Mutez(quotient),
                                V::Mutez(remainder),
                            ))));
                        }
                        _ => {
                            stack.push(V::new_option(Some(V::new_pair(V::Mutez(0), V::Mutez(x1)))));
                        }
                    }
                }
            }
            overloads::EDiv::MutezMutez => {
                let x1 = pop!(V::Mutez);
                let x2 = pop!(V::Mutez);
                ctx.gas().consume(interpret_cost::EDIV_TEZ_TEZ)?;
                if x2 == 0 {
                    stack.push(V::Option(None));
                } else {
                    let (quotient, remainder) = Integer::div_rem(&x1, &x2);
                    stack.push(V::new_option(Some(V::new_pair(
                        V::Nat(BigUint::try_from(quotient).unwrap()), // cannot fail because `x1` and `x2` are of Mutez type and thus non-negative
                        V::Mutez(remainder),
                    ))));
                }
            }
        },
        I::Neg(overload) => match overload {
            overloads::Neg::Nat => {
                let v = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::neg_int(&v)?)?;
                stack.push(V::Int(BigInt::from_biguint(Sign::Minus, v)));
            }
            overloads::Neg::Int => {
                let v = pop!(V::Int);
                ctx.gas().consume(interpret_cost::neg_int(&v)?)?;
                stack.push(V::Int(-v));
            }
            #[cfg(feature = "bls")]
            overloads::Neg::Bls12381G1 => {
                ctx.gas().consume(interpret_cost::NEG_G1)?;
                let v = top_mut!(V::Bls12381G1).as_mut();
                *v = -(v as &bls::G1);
            }
            #[cfg(feature = "bls")]
            overloads::Neg::Bls12381G2 => {
                ctx.gas().consume(interpret_cost::NEG_G2)?;
                let v = top_mut!(V::Bls12381G2).as_mut();
                *v = -(v as &bls::G2);
            }
            #[cfg(feature = "bls")]
            overloads::Neg::Bls12381Fr => {
                ctx.gas().consume(interpret_cost::NEG_FR)?;
                let v = top_mut!(V::Bls12381Fr);
                *v = -(v as &bls::Fr);
            }
        },
        I::Lsl(overload) => match overload {
            overloads::Lsl::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);

                if o2 > BigUint::from(256u16) {
                    return Err(InterpretError::Overflow);
                }

                let o2_usize = o2.to_usize().ok_or(InterpretError::Overflow)?;
                ctx.gas().consume(interpret_cost::lsl_nat(&o1)?)?;
                stack.push(V::Nat(o1.shl(o2_usize)));
            }
            overloads::Lsl::Bytes => {
                let o1 = pop!(V::Bytes);
                let o2 = pop!(V::Nat);

                if o2 > BigUint::from(64000u16) {
                    return Err(InterpretError::Overflow);
                }

                let o2_usize = o2.to_usize().ok_or(InterpretError::Overflow)?;
                ctx.gas()
                    .consume(interpret_cost::lsl_bytes(&o1, &o2_usize)?)?;

                let byte_shifts = o2_usize / 8;
                let bit_shifts = o2_usize % 8;

                let left_pad = bit_shifts > 0;
                let mut result: Vec<u8> =
                    vec![0; o1.len() + byte_shifts + if left_pad { 1 } else { 0 }];
                let mut carry = 0u8;

                for (i, &byte) in o1.iter().enumerate().rev() {
                    result[i + if left_pad { 1 } else { 0 }] = (byte << bit_shifts) | carry;
                    if left_pad {
                        carry = byte >> (8 - bit_shifts);
                    }
                }

                if left_pad {
                    result[0] = carry;
                }

                stack.push(V::Bytes(result));
            }
        },
        I::Lsr(overload) => match overload {
            overloads::Lsr::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);

                if o2 > BigUint::from(256u16) {
                    return Err(InterpretError::Overflow);
                }

                let o2_usize = o2.to_usize().ok_or(InterpretError::Overflow)?;
                ctx.gas().consume(interpret_cost::lsr_nat(&o1)?)?;
                stack.push(V::Nat(o1.shr(o2_usize)));
            }
            overloads::Lsr::Bytes => {
                let o1 = pop!(V::Bytes);
                let o2 = pop!(V::Nat);

                // L1 deliberately leaves `LSR bytes` unbounded on the shift
                // count: any count too large to address the bit width of `o1`
                // shifts the operand out completely and yields empty bytes
                // (see `Script_bytes.bytes_lsr` in
                // `src/proto_alpha/lib_protocol/script_bytes.ml`). Saturating
                // an out-of-`usize` count to `usize::MAX` produces empty bytes
                // the same way L1 does, instead of trapping with `Overflow`:
                // `byte_shifts` below clamps to `o1.len()`, so the result is
                // an empty vector.
                let o2_usize = o2.to_usize().unwrap_or(usize::MAX);
                ctx.gas()
                    .consume(interpret_cost::lsr_bytes(&o1, &o2_usize)?)?;

                let byte_shifts = min(o2_usize / 8, o1.len());
                let bit_shifts = o2_usize % 8;

                let need_carry = bit_shifts > 0;
                let mut result: Vec<u8> = vec![0; o1.len() - byte_shifts];
                let mut carry = 0u8;

                for (i, byte) in o1.iter().enumerate() {
                    if i >= o1.len() - byte_shifts {
                        break;
                    }
                    result[i] = (byte >> bit_shifts) | carry;
                    if need_carry {
                        carry = byte << (8 - bit_shifts);
                    }
                }

                stack.push(V::Bytes(result));
            }
        },
        I::SubMutez => {
            ctx.gas().consume(interpret_cost::SUB_MUTEZ)?;
            let v1 = pop!(V::Mutez);
            let v2 = pop!(V::Mutez);
            if v1 >= v2 {
                stack.push(V::new_option(Some(V::Mutez(v1 - v2))));
            } else {
                stack.push(V::Option(None));
            }
        }
        I::And(overload) => match overload {
            overloads::And::Bool => {
                let o1 = pop!(V::Bool);
                let o2 = top_mut!(V::Bool);
                ctx.gas().consume(interpret_cost::AND_BOOL)?;
                *o2 &= o1;
            }
            overloads::And::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = top_mut!(V::Nat);
                ctx.gas().consume(interpret_cost::and_num(&o1, o2)?)?;
                *o2 &= o1;
            }
            overloads::And::IntNat => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::and_num(&o1, &o2)?)?;
                let res = BigUint::try_from(o1 & BigInt::from(o2))
                    // safe, `neg` & `pos` = `pos`
                    .unwrap();
                stack.push(V::Nat(res));
            }
            overloads::And::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = top_mut!(V::Bytes);
                ctx.gas().consume(interpret_cost::and_bytes(&o1, o2)?)?;

                // The resulting vector length is the smallest length among the
                // operands, so to reuse memory we put the smallest vector to
                // the result (`o2`).
                if o1.len() < o2.len() {
                    std::mem::swap(&mut o1, o2)
                }
                for (b1, b2) in std::iter::zip(o1.into_iter().rev(), o2.iter_mut().rev()) {
                    *b2 &= b1;
                }
            }
        },
        I::Or(overload) => match overload {
            overloads::Or::Bool => {
                let o1 = pop!(V::Bool);
                let o2 = top_mut!(V::Bool);
                ctx.gas().consume(interpret_cost::OR_BOOL)?;
                *o2 |= o1;
            }
            overloads::Or::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = top_mut!(V::Nat);
                ctx.gas().consume(interpret_cost::or_num(&o1, o2)?)?;
                *o2 |= o1;
            }
            overloads::Or::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = top_mut!(V::Bytes);
                ctx.gas().consume(interpret_cost::or_bytes(&o1, o2)?)?;

                // The resulting vector length is the largest length among the
                // operands, so to reuse memory we put the largest vector to
                // the result (`o2`).
                if o1.len() > o2.len() {
                    std::mem::swap(&mut o1, o2)
                }
                for (b1, b2) in std::iter::zip(o1.into_iter().rev(), o2.iter_mut().rev()) {
                    *b2 |= b1;
                }
            }
        },
        I::Xor(overloads) => match overloads {
            overloads::Xor::Bool => {
                let o1 = pop!(V::Bool);
                let o2 = top_mut!(V::Bool);
                ctx.gas().consume(interpret_cost::XOR_BOOL)?;
                *o2 ^= o1;
            }
            overloads::Xor::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = top_mut!(V::Nat);
                ctx.gas().consume(interpret_cost::xor_nat(&o1, o2)?)?;
                *o2 ^= o1;
            }
            overloads::Xor::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = top_mut!(V::Bytes);

                // The resulting vector length is the largest length among the
                // operands, so to reuse memory we put the largest vector to
                // the result (`o2`).
                if o1.len() > o2.len() {
                    std::mem::swap(&mut o1, o2)
                }
                for (b1, b2) in std::iter::zip(o1.into_iter().rev(), o2.iter_mut().rev()) {
                    *b2 ^= b1;
                }
            }
        },
        I::Not(overload) => match overload {
            overloads::Not::Bool => {
                let o = top_mut!(V::Bool);
                ctx.gas().consume(interpret_cost::NOT_BOOL)?;
                *o = !*o;
            }
            overloads::Not::Int => {
                let o = pop!(V::Int);
                ctx.gas().consume(interpret_cost::not_num(&o)?)?;
                stack.push(V::Int(!o));
            }
            overloads::Not::Nat => {
                let o = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::not_num(&o)?)?;
                stack.push(V::Int(!BigInt::from(o)))
            }
            overloads::Not::Bytes => {
                let o = top_mut!(V::Bytes);
                ctx.gas().consume(interpret_cost::not_bytes(o)?)?;
                for b in o.iter_mut() {
                    *b = !*b
                }
            }
        },
        // Control-flow / sub-stack instructions are handled by the iterative
        // driver (interpret_step returns StepResult::Open*). interpret_one is
        // only reached via the driver's `_` fallthrough for non-control-flow
        // ops, so seeing one here is a driver invariant violation.
        I::Seq(..) | I::Dip(..) | I::If(..) | I::IfNone(..) | I::IfCons(..)
        | I::IfLeft(..) | I::Loop(..) | I::LoopLeft(..) | I::Iter(..)
        | I::Map(..) | I::Exec | I::IView { .. } => Err(InterpretError::InternalError(
            InterpretInvariant::UnreachableState,
        ))?,
        I::Drop(opt_height) => {
            ctx.gas().consume(interpret_cost::drop(*opt_height)?)?;
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            stack.drop_top(drop_height)?;
        }
        I::Dup(opt_height) => {
            ctx.gas().consume(interpret_cost::dup(*opt_height)?)?;
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            stack.push(stack.get(dup_height - 1)?.clone());
        }
        I::Dig(dig_height) => {
            ctx.gas().consume(interpret_cost::dig(*dig_height)?)?;
            if *dig_height > 0 {
                let e = stack.remove(*dig_height as usize)?;
                stack.push(e);
            }
        }
        I::Dug(dug_height) => {
            ctx.gas().consume(interpret_cost::dug(*dug_height)?)?;
            if *dug_height > 0 {
                let e = pop_rc!();
                stack.insert(*dug_height as usize, e)?;
            }
        }
        I::Gt => {
            ctx.gas().consume(interpret_cost::GT)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_positive()));
        }
        I::Ge => {
            ctx.gas().consume(interpret_cost::GE)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_negative()));
        }
        I::Eq => {
            ctx.gas().consume(interpret_cost::EQ)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_zero()));
        }
        I::Neq => {
            ctx.gas().consume(interpret_cost::NEQ)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_zero()));
        }
        I::Le => {
            ctx.gas().consume(interpret_cost::LE)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_positive()));
        }
        I::Lt => {
            ctx.gas().consume(interpret_cost::LT)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_negative()));
        }
        I::Abs => {
            let i = pop!(V::Int);
            ctx.gas().consume(interpret_cost::abs(&i)?)?;
            stack.push(V::Nat(i.into_parts().1));
        }
        I::IsNat => {
            let i = pop!(V::Int);
            ctx.gas().consume(interpret_cost::ISNAT)?;
            stack.push(V::new_option(i.try_into().ok().map(V::Nat)));
        }
        I::Int(overload) => match overload {
            overloads::Int::Nat => {
                let i = pop!(V::Nat);
                ctx.gas().consume(interpret_cost::INT_NAT)?;
                stack.push(V::Int(i.into()));
            }
            #[cfg(feature = "bls")]
            overloads::Int::Bls12381Fr => {
                pop_ref!(i, Bls12381Fr);
                ctx.gas().consume(interpret_cost::INT_BLS_FR)?;
                stack.push(V::Int(i.to_big_int()))
            }
            overloads::Int::Bytes => {
                pop_ref!(i, Bytes);
                ctx.gas().consume(interpret_cost::int_bytes(i.len())?)?;
                stack.push(V::Int(BigInt::from_signed_bytes_be(i)))
            }
        },
        I::Nat => {
            pop_ref!(i, Bytes);
            ctx.gas().consume(interpret_cost::int_bytes(i.len())?)?;
            stack.push(V::Nat(BigUint::from_bytes_be(i)))
        }
        I::Bytes(overload) => match overload {
            overloads::Bytes::Nat => {
                pop_ref!(i, Nat);
                ctx.gas().consume(interpret_cost::bytes_nat(i)?)?;
                stack.push(V::Bytes(if i.is_zero() {
                    Vec::new() // empty
                } else {
                    i.to_bytes_be()
                }));
            }
            overloads::Bytes::Int => {
                pop_ref!(i, Int);
                ctx.gas().consume(interpret_cost::bytes_int(i)?)?;
                stack.push(V::Bytes(if i.is_zero() {
                    Vec::new() // empty
                } else {
                    i.to_signed_bytes_be()
                }));
            }
        },
        I::Push(v) => {
            ctx.gas().consume(interpret_cost::PUSH)?;
            stack.push(v.clone());
        }
        I::Swap => {
            ctx.gas().consume(interpret_cost::SWAP)?;
            stack.swap(0, 1)?;
        }
        I::Failwith(ty) => {
            let x = pop!();
            return Err(InterpretError::FailedWith(ty.clone(), x));
        }
        I::Never => {
            return Err(InterpretError::InternalError(
                InterpretInvariant::UnreachableState,
            ))
        }
        I::Unit => {
            ctx.gas().consume(interpret_cost::UNIT)?;
            stack.push(V::Unit);
        }
        I::Car => {
            ctx.gas().consume(interpret_cost::CAR)?;
            pop!(V::Pair, l, _r);
            stack.push(l);
        }
        I::Cdr => {
            ctx.gas().consume(interpret_cost::CDR)?;
            pop!(V::Pair, _l, r);
            stack.push(r);
        }
        I::Pair => {
            ctx.gas().consume(interpret_cost::PAIR)?;
            let l = pop_rc!();
            let r = pop_rc!();
            stack.push(V::new_pair_rc(l, r));
        }
        I::PairN(n) => {
            ctx.gas().consume(interpret_cost::pair_n(*n as usize)?)?;
            let res = stack
                .drain_top(*n as usize)?
                .rev()
                .reduce(|acc, e| V::new_pair_rc(e, acc).into())
                .ok_or(InterpretError::InternalError(
                    InterpretInvariant::UnreachableState,
                ))?;
            stack.push(res);
        }
        I::Unpair => {
            ctx.gas().consume(interpret_cost::UNPAIR)?;
            pop!(V::Pair, l, r);
            stack.push(r);
            stack.push(l);
        }
        I::UnpairN(n) => {
            ctx.gas().consume(interpret_cost::unpair_n(*n as usize)?)?;
            // Iterative descent: walk the right-comb spine, recording each
            // `l` in `lefts` (post-order push: deepest pair value goes first,
            // then `lefts` in reverse so `l_0` ends on top). Mirrors the
            // recursive `fn fill` behaviour bit-for-bit but does not grow a
            // Rust frame per nesting level -- a depth-N right-comb on the
            // stack with `UNPAIR (N+1)` would otherwise overflow the kernel's
            // ~1 MiB Rust stack around N ≈ 100. L2-1434 in-scope item.
            let mut cur = pop_rc!();
            let total = *n as usize;
            stack.reserve(total);
            let mut lefts: Vec<Rc<TypedValue<'a>>> = Vec::with_capacity(total.saturating_sub(1));
            for _ in 0..(n - 1) {
                let next = match cur.as_ref() {
                    V::Pair(l, r) => {
                        lefts.push(l.clone());
                        r.clone()
                    }
                    _ => {
                        return Err(InterpretError::InternalError(
                            InterpretInvariant::TypeMismatch {
                                expected: "V::Pair",
                            },
                        ))
                    }
                };
                cur = next;
            }
            stack.push(cur);
            for l in lefts.into_iter().rev() {
                stack.push(l);
            }
        }
        I::ISome => {
            ctx.gas().consume(interpret_cost::SOME)?;
            let v = pop_rc!();
            stack.push(V::new_option_rc(Some(v)));
        }
        I::None => {
            ctx.gas().consume(interpret_cost::NONE)?;
            stack.push(V::new_option(None));
        }
        I::Compare => {
            let l = pop_rc!();
            let r = pop_rc!();
            ctx.gas().consume(interpret_cost::compare(&l, &r)?)?;
            let cmp = l
                .partial_cmp(&r)
                .ok_or(InterpretError::CompareError(CompareError::Incomparable))?
                as i8;
            stack.push(V::Int(cmp.into()));
        }
        I::Amount => {
            ctx.gas().consume(interpret_cost::AMOUNT)?;
            stack.push(V::Mutez(ctx.amount()));
        }
        I::Nil => {
            ctx.gas().consume(interpret_cost::NIL)?;
            stack.push(V::List(MichelsonList::new()));
        }
        I::Cons => {
            ctx.gas().consume(interpret_cost::CONS)?;
            let elt = pop_rc!();
            let mut lst = pop!(V::List);
            // NB: this is slightly better than lists on average, but needs to
            // be benchmarked.
            lst.cons(elt);
            stack.push(V::List(lst));
        }
        I::Concat(overload) => match overload {
            overloads::Concat::TwoStrings => {
                let mut s1 = pop!(V::String);
                let s2 = pop!(V::String);
                ctx.gas()
                    .consume(interpret_cost::concat_string_pair(s1.len(), s2.len())?)?;
                s1.push_str(&s2);
                stack.push(V::String(s1));
            }
            overloads::Concat::TwoBytes => {
                let mut bs1 = pop!(V::Bytes);
                let bs2 = pop!(V::Bytes);
                ctx.gas()
                    .consume(interpret_cost::concat_bytes_pair(bs1.len(), bs2.len())?)?;
                bs1.extend_from_slice(&bs2);
                stack.push(V::Bytes(bs1))
            }
            overloads::Concat::ListOfStrings => {
                pop_ref!(list, List);
                ctx.gas()
                    .consume(interpret_cost::concat_list_precheck(list.len())?)?;

                let mut total_len = Checked::zero();
                for val in list {
                    let s = match val.as_ref() {
                        V::String(s) => s,
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::String",
                                },
                            ))
                        }
                    };
                    total_len += s.len()
                }
                ctx.gas()
                    .consume(interpret_cost::concat_string_list(total_len)?)?;

                let mut result = String::with_capacity(total_len.ok_or(OutOfGas)?);
                for val in list {
                    let s = match val.as_ref() {
                        V::String(s) => s,
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::String",
                                },
                            ))
                        }
                    };
                    result.push_str(s);
                }
                stack.push(V::String(result))
            }
            overloads::Concat::ListOfBytes => {
                pop_ref!(list, List);
                ctx.gas()
                    .consume(interpret_cost::concat_list_precheck(list.len())?)?;

                let mut total_len = Checked::zero();
                for val in list {
                    let bs = match val.as_ref() {
                        V::Bytes(bs) => bs,
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::Bytes",
                                },
                            ))
                        }
                    };
                    total_len += bs.len()
                }
                ctx.gas()
                    .consume(interpret_cost::concat_bytes_list(total_len)?)?;

                let mut result = Vec::with_capacity(total_len.ok_or(OutOfGas)?);
                for val in list {
                    let bs = match val.as_ref() {
                        V::Bytes(bs) => bs,
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::Bytes",
                                },
                            ))
                        }
                    };
                    result.extend_from_slice(bs);
                }
                stack.push(V::Bytes(result))
            }
        },
        I::EmptySet => {
            use std::collections::BTreeSet;
            ctx.gas().consume(interpret_cost::EMPTY_SET)?;
            stack.push(V::Set(BTreeSet::new()))
        }
        I::EmptyMap => {
            use std::collections::BTreeMap;
            ctx.gas().consume(interpret_cost::EMPTY_MAP)?;
            stack.push(V::Map(BTreeMap::new()))
        }
        I::EmptyBigMap(kty, vty) => {
            ctx.gas().consume(interpret_cost::EMPTY_BIG_MAP)?;
            stack.push(V::BigMap(BigMap::empty(kty.clone(), vty.clone())))
        }
        I::Mem(overload) => match overload {
            overloads::Mem::Set => {
                let key_rc = pop_rc!();
                pop_ref!(set, Set);
                ctx.gas()
                    .consume(interpret_cost::set_mem(&key_rc, set.len())?)?;
                let result = set.contains(&*key_rc);
                stack.push(V::Bool(result));
            }
            overloads::Mem::Map => {
                let key_rc = pop_rc!();
                pop_ref!(map, Map);
                ctx.gas()
                    .consume(interpret_cost::map_mem(&key_rc, map.len())?)?;
                let result = map.contains_key(&*key_rc);
                stack.push(V::Bool(result));
            }
            overloads::Mem::BigMap => {
                let key_rc = pop_rc!();
                pop_ref!(map, BigMap);
                let len = map.len_for_gas();
                // the protocol deliberately uses map costs for the overlay
                ctx.gas().consume(interpret_cost::map_mem(&key_rc, len)?)?;
                let result = map.mem(&key_rc, *ctx.lazy_storage())?;
                stack.push(V::Bool(result));
            }
        },
        I::Get(overload) => match overload {
            overloads::Get::Map => {
                let key_rc = pop_rc!();
                pop_ref!(map, Map);
                ctx.gas()
                    .consume(interpret_cost::map_get(&key_rc, map.len())?)?;
                let result = map.get(&*key_rc);
                stack.push(V::new_option_rc(result.cloned()));
            }
            overloads::Get::BigMap => {
                let key_rc = pop_rc!();
                pop_ref!(map, BigMap);
                let len = map.len_for_gas();
                // the protocol deliberately uses map costs for the overlay
                ctx.gas().consume(interpret_cost::map_get(&key_rc, len)?)?;
                let result = map.get(arena, &key_rc, *ctx.lazy_storage())?;
                stack.push(V::new_option(result));
            }
        },
        I::GetN(n) => {
            ctx.gas().consume(interpret_cost::get_n(*n as usize)?)?;
            let comb_rc = pop_rc!();
            let field = get_nth_field_rc(*n, &comb_rc)?;
            stack.push(field);
        }
        I::Update(overload) => match overload {
            overloads::Update::Set => {
                let key_rc = pop_rc!();
                let new_present = pop!(V::Bool);
                let set = top_mut!(V::Set);
                ctx.gas()
                    .consume(interpret_cost::set_update(&key_rc, set.len())?)?;
                if new_present {
                    set.insert(key_rc)
                } else {
                    set.remove(&*key_rc)
                };
            }
            overloads::Update::Map => {
                let key_rc = pop_rc!();
                let opt_new_val = pop!(V::Option);
                let map = top_mut!(V::Map);
                ctx.gas()
                    .consume(interpret_cost::map_update(&key_rc, map.len())?)?;
                match opt_new_val {
                    None => map.remove(&*key_rc),
                    Some(val) => map.insert(key_rc, val),
                };
            }
            overloads::Update::BigMap => {
                let key = pop!();
                let opt_new_val = pop!(V::Option).map(TypedValue::unwrap_rc);
                let map = top_mut!(V::BigMap);
                let len = map.len_for_gas();
                // the protocol deliberately uses map costs for the overlay
                ctx.gas().consume(interpret_cost::map_update(&key, len)?)?;
                map.update(key, opt_new_val);
            }
        },
        I::GetAndUpdate(overload) => match overload {
            overloads::GetAndUpdate::Map => {
                let key_rc = pop_rc!();
                let opt_new_val = pop!(V::Option);
                let map = top_mut!(V::Map);
                ctx.gas()
                    .consume(interpret_cost::map_get_and_update(&key_rc, map.len())?)?;
                let opt_old_val = match opt_new_val {
                    None => map.remove(&*key_rc),
                    Some(val) => map.insert(key_rc, val),
                };
                stack.push(V::new_option_rc(opt_old_val));
            }
            overloads::GetAndUpdate::BigMap => {
                let key = pop!();
                let opt_new_val = pop!(V::Option).map(TypedValue::unwrap_rc);
                let map = top_mut!(V::BigMap);
                let len = map.len_for_gas();
                // the protocol deliberately uses map costs for the overlay
                ctx.gas()
                    .consume(interpret_cost::map_get_and_update(&key, len)?)?;
                let opt_old_val = map.get(arena, &key, *ctx.lazy_storage())?;
                map.update(key, opt_new_val);
                stack.push(V::new_option(opt_old_val));
            }
        },
        I::Size(overload) => {
            macro_rules! run_size {
                ($ctor:tt, $gas:ident) => {{
                    pop_ref!(e, $ctor);
                    ctx.gas().consume(interpret_cost::$gas)?;
                    let res = e.len();
                    stack.push(V::Nat(res.into()));
                }};
            }
            match overload {
                overloads::Size::String => run_size!(String, SIZE_STRING),
                overloads::Size::Bytes => run_size!(Bytes, SIZE_BYTES),
                overloads::Size::List => run_size!(List, SIZE_LIST),
                overloads::Size::Set => run_size!(Set, SIZE_SET),
                overloads::Size::Map => run_size!(Map, SIZE_MAP),
            }
        }
        I::UpdateN(n) => {
            ctx.gas().consume(interpret_cost::update_n(*n as usize)?)?;
            let new_val = pop!();
            let field = get_nth_field_ref(*n, Rc::make_mut(stack.get_mut(0)?))?;
            *field = new_val;
        }
        I::ChainId => {
            ctx.gas().consume(interpret_cost::CHAIN_ID)?;
            stack.push(V::ChainId(ctx.chain_id()));
        }
        I::ISelf(entrypoint) => {
            ctx.gas().consume(interpret_cost::SELF)?;
            stack.push(V::Contract(Address {
                hash: ctx.self_address(),
                entrypoint: entrypoint.clone(),
            }));
        }
        I::Pack => {
            ctx.gas().consume(interpret_cost::PACK)?;
            let v = pop!();
            let arena = Arena::new();
            let mich = v.into_micheline_optimized_legacy(&arena, ctx.gas())?;
            let encoded = mich.encode_for_pack()??;
            stack.push(V::Bytes(encoded));
        }
        I::Unpack(ty) => {
            let bytes = pop!(V::Bytes);
            ctx.gas()
                .consume(interpret_cost::unpack(bytes.as_slice())?)?;
            let mut try_unpack = || -> Option<TypedValue> {
                let mich = Micheline::decode_packed(arena, &bytes, ctx.gas())
                    .ok()?
                    .ok()?;
                crate::interpreter::typecheck_value(&mich, ctx, ty).ok()
            };
            stack.push(V::new_option(try_unpack()));
        }
        I::CheckSignature => {
            let key = pop!(V::Key);
            let sig = pop!(V::Signature);
            let msg = pop!(V::Bytes);
            ctx.gas()
                .consume(interpret_cost::check_signature(&key, &msg)?)?;
            stack.push(V::Bool(key.verify_signature(&sig, &msg).unwrap_or(false)));
        }
        I::TransferTokens => {
            let param = pop!();
            let mutez_amount = pop!(V::Mutez);
            let contract_address = pop!(V::Contract);
            let counter = ctx.operation_counter();
            ctx.gas().consume(interpret_cost::TRANSFER_TOKENS)?;
            stack.push(V::new_operation(
                Operation::TransferTokens(TransferTokens {
                    param,
                    amount: mutez_amount,
                    destination_address: contract_address,
                }),
                counter,
            ));
        }
        I::SetDelegate => {
            let opt_keyhash = pop!(V::Option)
                .map(|kh| match TypedValue::unwrap_rc(kh) {
                    V::KeyHash(k) => Ok(k),
                    _ => Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch {
                            expected: "V::KeyHash",
                        },
                    )),
                })
                .transpose()?;
            let counter: u128 = ctx.operation_counter();
            ctx.gas().consume(interpret_cost::SET_DELEGATE)?;
            stack.push(V::new_operation(
                Operation::SetDelegate(SetDelegate(opt_keyhash)),
                counter,
            ))
        }
        I::Address => {
            ctx.gas().consume(interpret_cost::ADDRESS)?;
            let address = pop!(V::Contract);
            stack.push(V::Address(address));
        }
        I::Slice(overload) => {
            fn validate_bounds(
                offset: BigUint,
                length: BigUint,
                actual_length: usize,
            ) -> Option<std::ops::Range<usize>> {
                // If `offset` or `offset + length` are greater than `usize::MAX`, `SLICE` will return `None`.
                // But in reality, slicing a string of length greater than `usize::MAX` would
                // exhaust the gas before execution gets here.
                let start: usize = offset.try_into().ok()?;
                let end: usize = start.checked_add(length.try_into().ok()?)?;

                // `str::get` performs bounds checks, but Michelson's bounds checks
                // are stricter than rust's.
                // E.g. rust allows `String::from("").get(0..0)`, but michelson doesn't.
                // For that reason, we have to perform this additional check here.
                if start >= actual_length {
                    None
                } else {
                    Some(start..end)
                }
            }

            let offset = pop!(V::Nat);
            let length = pop!(V::Nat);
            let result = match overload {
                overloads::Slice::String => {
                    pop_ref!(str, String);
                    ctx.gas().consume(interpret_cost::slice(str.len())?)?;
                    validate_bounds(offset, length, str.len())
                        .and_then(|range| str.get(range))
                        .map(|str| V::String(str.to_string()))
                }
                overloads::Slice::Bytes => {
                    pop_ref!(bytes, Bytes);
                    ctx.gas().consume(interpret_cost::slice(bytes.len())?)?;
                    validate_bounds(offset, length, bytes.len())
                        .and_then(|range| bytes.get(range))
                        .map(|bytes| V::Bytes(bytes.to_owned()))
                }
            };
            stack.push(V::new_option(result));
        }
        I::Left => {
            ctx.gas().consume(interpret_cost::LEFT)?;
            let left = pop_rc!();
            stack.push(V::new_or_rc(Or::Left(left)));
        }
        I::Right => {
            ctx.gas().consume(interpret_cost::RIGHT)?;
            let right = pop_rc!();
            stack.push(V::new_or_rc(Or::Right(right)));
        }
        I::Lambda(lam) => {
            ctx.gas().consume(interpret_cost::LAMBDA)?;
            stack.push(V::Lambda(Closure::Lambda(lam.clone())));
        }
        I::Apply { arg_ty } => {
            let arg_val = pop!();
            let closure = pop!(V::Lambda);
            ctx.gas().consume(interpret_cost::APPLY)?;
            stack.push(V::Lambda(Closure::Apply {
                arg_ty: arg_ty.clone(),
                arg_val: Box::new(arg_val),
                closure: Box::new(closure),
            }))
        }
        I::HashKey => {
            ctx.gas().consume(interpret_cost::HASH_KEY)?;
            let key = pop!(V::Key);
            stack.push(TypedValue::KeyHash(key.pk_hash()))
        }
        I::Ticket(content_type) => {
            let content = pop!();
            let amount = pop!(V::Nat);
            ctx.gas().consume(interpret_cost::TICKET)?;
            if amount.is_zero() {
                // If the amount is zero, then we push a None value
                // as per the specified instruction behavior.
                stack.push(V::new_option(None));
            } else {
                let ticket = Ticket {
                    ticketer: ctx.self_address(),
                    content_type: content_type.clone(),
                    content,
                    amount,
                };
                stack.push(V::new_option(Some(V::new_ticket(ticket))));
            }
        }
        I::ReadTicket => {
            ctx.gas().consume(interpret_cost::READ_TICKET)?;
            let top = stack.get(0)?.as_ref();
            let ticket = match top {
                V::Ticket(t) => t,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatchOnTop {
                            expected: "V::Ticket",
                        },
                    ))
                }
            };
            stack.push(unwrap_ticket(ticket.as_ref().clone()));
        }
        I::SplitTicket => {
            let ticket = *pop!(V::Ticket);
            pop!(V::Pair, amount_left, amount_right);
            let amount_left = match TypedValue::unwrap_rc(amount_left) {
                V::Nat(n) => n,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch { expected: "V::Nat" },
                    ))
                }
            };
            let amount_right = match TypedValue::unwrap_rc(amount_right) {
                V::Nat(n) => n,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch { expected: "V::Nat" },
                    ))
                }
            };

            ctx.gas()
                .consume(interpret_cost::split_ticket(&amount_left, &amount_right)?)?;
            if amount_left.clone() + amount_right.clone() == ticket.amount
                && amount_left.gt(&BigUint::zero())
                && amount_right.gt(&BigUint::zero())
            {
                stack.push(V::new_option(Some(V::new_pair(
                    V::new_ticket(Ticket {
                        amount: amount_left,
                        ..ticket.clone()
                    }),
                    V::new_ticket(Ticket {
                        amount: amount_right,
                        ..ticket
                    }),
                ))));
            } else {
                stack.push(V::new_option(None));
            }
        }
        I::JoinTickets => {
            pop!(V::Pair, ticket_left, ticket_right);
            let mut ticket_left = match TypedValue::unwrap_rc(ticket_left) {
                V::Ticket(t) => t,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch {
                            expected: "V::Ticket",
                        },
                    ))
                }
            };
            let ticket_right = match TypedValue::unwrap_rc(ticket_right) {
                V::Ticket(t) => t,
                _ => {
                    return Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch {
                            expected: "V::Ticket",
                        },
                    ))
                }
            };
            ctx.gas()
                .consume(interpret_cost::join_tickets(&ticket_left, &ticket_right)?)?;
            if ticket_left.content == ticket_right.content
                && ticket_left.ticketer == ticket_right.ticketer
            {
                ticket_left.amount += ticket_right.amount;
                stack.push(V::new_option(Some(TypedValue::Ticket(ticket_left))));
            } else {
                stack.push(V::new_option(None));
            }
        }
        I::Blake2b => {
            let msg = top_mut!(V::Bytes);
            ctx.gas().consume(interpret_cost::blake2b(msg)?)?;
            *msg = blake2b_256(msg).to_vec();
        }
        I::Keccak => {
            let msg = top_mut!(V::Bytes);
            ctx.gas().consume(interpret_cost::keccak(msg)?)?;
            *msg = keccak256(msg).to_vec();
        }
        I::Sha256 => {
            let msg = top_mut!(V::Bytes);
            ctx.gas().consume(interpret_cost::sha256(msg)?)?;
            *msg = sha256(msg).to_vec();
        }
        I::Sha3 => {
            let msg = top_mut!(V::Bytes);
            ctx.gas().consume(interpret_cost::sha3(msg)?)?;
            *msg = sha3_256(msg).to_vec();
        }
        I::Sha512 => {
            let msg = top_mut!(V::Bytes);
            ctx.gas().consume(interpret_cost::sha512(msg)?)?;
            *msg = sha512(msg).to_vec();
        }
        I::Balance => {
            ctx.gas().consume(interpret_cost::BALANCE)?;
            stack.push(V::Mutez(ctx.balance()));
        }
        I::Contract(typ, ep) => {
            ctx.gas().consume(interpret_cost::CONTRACT)?;
            let address = pop!(V::Address);
            stack.push(TypedValue::new_option(
                typecheck_contract_address(ctx, address, ep.clone(), typ)
                    .ok()
                    .map(TypedValue::Contract),
            ));
        }
        I::Level => {
            ctx.gas().consume(interpret_cost::LEVEL)?;
            stack.push(TypedValue::Nat(ctx.level()));
        }
        I::MinBlockTime => {
            ctx.gas().consume(interpret_cost::MIN_BLOCK_TIME)?;
            stack.push(TypedValue::Nat(ctx.min_block_time()));
        }
        I::SelfAddress => {
            ctx.gas().consume(interpret_cost::SELF_ADDRESS)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.self_address(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Sender => {
            ctx.gas().consume(interpret_cost::SENDER)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.sender(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Source => {
            ctx.gas().consume(interpret_cost::SOURCE)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.source().into(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Now => {
            ctx.gas().consume(interpret_cost::NOW)?;
            stack.push(TypedValue::Timestamp(ctx.now()));
        }
        I::ImplicitAccount => {
            ctx.gas().consume(interpret_cost::IMPLICIT_ACCOUNT)?;
            let keyhash = pop!(V::KeyHash);
            stack.push(TypedValue::Contract(Address {
                hash: AddressHash::Implicit(keyhash),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::IsImplicitAccount => {
            ctx.gas().consume(interpret_cost::IS_IMPLICIT_ACCOUNT)?;
            let addr = pop!(V::Address);
            stack.push(TypedValue::new_option(match addr.hash {
                AddressHash::Implicit(keyhash) => Some(TypedValue::KeyHash(keyhash)),
                _ => None,
            }));
        }
        I::VotingPower => {
            ctx.gas().consume(interpret_cost::VOTING_POWER)?;
            let keyhash = pop!(V::KeyHash);
            stack.push(TypedValue::Nat(ctx.voting_power(&keyhash)))
        }
        I::TotalVotingPower => {
            ctx.gas().consume(interpret_cost::TOTAL_VOTING_POWER)?;
            stack.push(TypedValue::Nat(ctx.total_voting_power()))
        }
        I::Emit { tag, arg_ty } => {
            let counter: u128 = ctx.operation_counter();
            let emit_val = pop!();
            ctx.gas().consume(interpret_cost::EMIT)?;
            stack.push(TypedValue::new_operation(
                Operation::Emit(Emit {
                    tag: tag.clone(),
                    value: emit_val,
                    arg_ty: arg_ty.clone(),
                }),
                counter,
            ))
        }
        #[cfg(feature = "bls")]
        I::PairingCheck => {
            pop_ref!(list, List);
            ctx.gas()
                .consume(interpret_cost::pairing_check(list.len())?)?;
            let pairs: Vec<(&bls::G1, &bls::G2)> = list
                .iter()
                .map(|elt| -> Result<_, InterpretError<'a>> {
                    let (g1, g2) = match elt.as_ref() {
                        V::Pair(g1, g2) => (g1, g2),
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::Pair(V::Bls12381G1, V::Bls12381G2)",
                                },
                            ))
                        }
                    };
                    let g1 = match g1.as_ref() {
                        V::Bls12381G1(g) => g.as_ref(),
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::Bls12381G1",
                                },
                            ))
                        }
                    };
                    let g2 = match g2.as_ref() {
                        V::Bls12381G2(g) => g.as_ref(),
                        _ => {
                            return Err(InterpretError::InternalError(
                                InterpretInvariant::TypeMismatch {
                                    expected: "V::Bls12381G2",
                                },
                            ))
                        }
                    };
                    Ok((g1, g2))
                })
                .collect::<Result<_, _>>()?;
            let res = bls::pairing::pairing_check(pairs.into_iter());
            stack.push(V::Bool(res));
        }
        I::CreateContract(cs, micheline) => {
            ctx.gas().consume(interpret_cost::CREATE_CONTRACT)?;
            let counter: u128 = ctx.operation_counter();
            let opt_keyhash = pop!(V::Option)
                .map(|keyhash| match TypedValue::unwrap_rc(keyhash) {
                    V::KeyHash(k) => Ok(k),
                    _ => Err(InterpretError::InternalError(
                        InterpretInvariant::TypeMismatch {
                            expected: "V::KeyHash",
                        },
                    )),
                })
                .transpose()?;
            let amount = pop!(V::Mutez);
            let storage = pop!();
            let origination_counter = ctx.origination_counter();
            let address = compute_contract_address(ctx.operation_group_hash(), origination_counter);
            stack.push(TypedValue::Address(Address {
                hash: AddressHash::Kt1(address.clone()),
                entrypoint: Entrypoint::default(),
            }));
            stack.push(TypedValue::new_operation(
                Operation::CreateContract(CreateContract {
                    delegate: opt_keyhash,
                    amount,
                    storage,
                    code: cs.clone(), // This clone is cheap since it is an Rc.
                    micheline_code: micheline,
                    address,
                }),
                counter,
            ))
        }
    }
    Ok(())
}

/// Computes the contract address based on the operation group hash and an origination index.
///
/// # Arguments
///
/// * `operation_group_hash` - A 32-byte array representing the hash of the operation group.
/// * `o_index` - A 32-bit unsigned integer representing the origination index.
///
/// # Returns
///
/// Returns a `ContractKt1Hash` struct containing the computed contract address.
pub fn compute_contract_address(
    operation_group_hash: &OperationHash,
    o_index: u32,
) -> ContractKt1Hash {
    let mut input: [u8; 36] = [0; 36];
    input[..32].copy_from_slice(operation_group_hash.as_ref());
    // append bytes representing o_index
    input[32..36].copy_from_slice(&o_index.to_be_bytes());
    ContractKt1Hash::from(digest_160(&input))
}

fn get_nth_field_ref<'a, 'b>(
    mut m: u16,
    mut val: &'a mut TypedValue<'b>,
) -> Result<&'a mut TypedValue<'b>, InterpretError<'b>> {
    use TypedValue as V;
    loop {
        match (m, val) {
            (0, val_) => break Ok(val_),
            (1, V::Pair(l, _)) => {
                break Ok(Rc::make_mut(l));
            }

            (_, V::Pair(_, r)) => {
                val = Rc::make_mut(r);
                m -= 2;
            }
            _ => {
                break Err(InterpretError::InternalError(
                    InterpretInvariant::TypeMismatch {
                        expected: "V::Pair",
                    },
                ))
            }
        }
    }
}

/// Walk the comb by reference (no `Rc::make_mut`) and return the `Rc`
/// of the nth field. Refcount-only operation: extracting a field from a
/// shared comb no longer deep-clones the whole pair tree.
///
/// The Michelson `GET n` indexing on a right-nested pair tree
/// `Pair(f0, Pair(f1, Pair(f2, ...)))` is:
///   - even `n`: the suffix subtree starting at depth `n/2`
///     (`n=0` is the whole comb, `n=2` is `Pair(f1, ...)`, etc.)
///   - odd  `n`: the field at depth `(n-1)/2`
///     (`n=1` is `f0`, `n=3` is `f1`, etc.)
/// Descending into the right child consumes two indices at once: the
/// "suffix at this level" (even) and the "field at this level" (odd),
/// hence `m -= 2`.
fn get_nth_field_rc<'b>(
    mut m: u16,
    mut val: &Rc<TypedValue<'b>>,
) -> Result<Rc<TypedValue<'b>>, InterpretError<'b>> {
    use TypedValue as V;
    loop {
        match (m, &**val) {
            (0, _) => return Ok(val.clone()),
            (1, V::Pair(l, _)) => return Ok(l.clone()),
            (_, V::Pair(_, r)) => {
                val = r;
                m -= 2;
            }
            _ => {
                return Err(InterpretError::InternalError(
                    InterpretInvariant::TypeMismatch {
                        expected: "V::Pair",
                    },
                ))
            }
        }
    }
}

#[cfg(test)]
mod interpreter_tests {
    use std::collections::{BTreeMap, BTreeSet, HashMap};
    use std::rc::Rc;

    use super::*;
    use super::{Lambda, Or};
    use crate::ast::big_map::{InMemoryLazyStorage, LazyStorage, LazyStorageBulkUpdate};
    use crate::ast::michelson_address as addr;
    use crate::ast::or::Or::Left;
    #[cfg(feature = "bls")]
    use crate::bls;
    use crate::context::{Ctx, TypecheckingCtx};
    use crate::gas::Gas;
    use chrono::DateTime;
    use num_bigint::BigUint;
    use tezos_crypto_rs::public_key::PublicKey;
    use tezos_data_encoding::nom::NomReader;
    use Instruction::*;
    use Option::None;
    use TypedValue as V;

    #[track_caller]
    fn mk_0x(hex: &str) -> TypedValue {
        V::Bytes(hex::decode(hex).unwrap_or_else(|e| panic!("Invalid hex: {e}")))
    }

    fn interpret<'a>(
        ast: &[Instruction<'a>],
        ctx: &mut impl CtxTrait<'a>,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        let temp = Box::leak(Box::default());
        super::interpret(ast, ctx, temp, stack)
    }

    fn interpret_one<'a>(
        i: &Instruction<'a>,
        ctx: &mut impl CtxTrait<'a>,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        let temp = Box::leak(Box::default());
        super::interpret_one(i, ctx, temp, stack)
    }

    fn rc_set<'a, I>(values: I) -> BTreeSet<Rc<TypedValue<'a>>>
    where
        I: IntoIterator<Item = TypedValue<'a>>,
    {
        values.into_iter().map(Rc::new).collect()
    }

    fn rc_map<'a, I>(values: I) -> BTreeMap<Rc<TypedValue<'a>>, Rc<TypedValue<'a>>>
    where
        I: IntoIterator<Item = (TypedValue<'a>, TypedValue<'a>)>,
    {
        values
            .into_iter()
            .map(|(key, value)| (Rc::new(key), Rc::new(value)))
            .collect()
    }

    #[test]
    fn test_add() {
        let mut stack = stk![V::nat(10), V::nat(20)];
        let expected_stack = stk![V::nat(30)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::NatNat), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    /// Regression (gas parity): `ContractScript::interpret` runs
    /// `self.code.interpret(..)` and `self.code` is always a `Seq`. The
    /// public `Instruction::interpret` unwraps a top-level `Seq` before
    /// handing it to the iterative driver, so a contract's outer block
    /// charges exactly one `INTERPRET_RET` (L1 `KNil`) — the same as running
    /// the body as a bare block, and the same as the pre-iterative
    /// `interpret_one(Seq) -> interpret(body)`. Without the unwrap the driver
    /// would charge a second `INTERPRET_RET` for a synthetic wrapper block.
    #[test]
    fn top_level_seq_matches_block_gas() {
        let body = vec![Push(Rc::new(V::int(1))), Drop(None)];

        // Production shape: `Seq(body).interpret(..)`.
        let arena_a: &Arena<Micheline> = Box::leak(Box::default());
        let mut ctx_a = Ctx::default();
        let mut stack_a: IStack = Stack::new();
        Seq(body.clone())
            .interpret(&mut ctx_a, arena_a, &mut stack_a)
            .unwrap();
        let gas_seq = Gas::default().milligas().unwrap() - ctx_a.gas().milligas().unwrap();

        // The same instructions run as a single block must cost the same.
        let mut ctx_b = Ctx::default();
        let mut stack_b: IStack = Stack::new();
        interpret(&body, &mut ctx_b, &mut stack_b).unwrap();
        let gas_block = Gas::default().milligas().unwrap() - ctx_b.gas().milligas().unwrap();

        assert_eq!(
            gas_seq, gas_block,
            "a top-level Seq must not charge an extra INTERPRET_RET vs the same block"
        );
    }

    /// Regression (error-unwind): when an error fires inside an EXEC/View
    /// sub-computation the driver returns before the pending `AfterExec`
    /// frame can pop the sub-stack, so several `IStack`s remain. `interpret`
    /// must hand the caller back its own (bottom) stack, not an inner
    /// sub-stack. Here an identical FAILWITH leaves the caller's stack
    /// untouched both at top level (a) and when reached from inside EXEC (b).
    #[test]
    fn error_inside_exec_preserves_caller_stack() {
        let marker = V::int(42);

        // (a) Top-level FAILWITH: the caller's bottom stack is preserved.
        let mut top = stk![marker.clone(), V::int(7)];
        let r1 = interpret(&[Failwith(Type::Int)], &mut Ctx::default(), &mut top);
        assert!(matches!(r1, Err(InterpretError::FailedWith(..))));
        assert_eq!(top, stk![marker.clone()]);

        // (b) Same FAILWITH reached from inside an EXEC sub-computation: the
        // caller's bottom stack (just `marker`, after EXEC consumed the
        // lambda and its argument) is restored, not the inner sub-stack.
        let mut nested = stk![
            marker.clone(),
            V::Lambda(Closure::Lambda(Lambda::Lambda {
                micheline_code: Micheline::Seq(&[]),
                code: vec![Failwith(Type::Int)].into(),
            })),
            V::int(7)
        ];
        let r2 = interpret(&[Exec], &mut Ctx::default(), &mut nested);
        assert!(matches!(r2, Err(InterpretError::FailedWith(..))));
        assert_eq!(nested, stk![marker]);
    }

    /// L2-1446: a deep runtime-built value (e.g. a `LOOP`/`PAIR`-built comb)
    /// left on a value stack at interpreter teardown must be flattened
    /// iteratively, not recursed through the `Rc<TypedValue>` destructor. This
    /// exercises `drain_value_stack`, which the driver (for EXEC/View
    /// sub-stacks) and `ContractScript::interpret` (for the caller stack) call
    /// on their error paths. Runs on a 1 MiB worker thread matching the kernel
    /// budget; without the iterative drain it overflows.
    #[test]
    fn drain_deep_value_stack_does_not_overflow() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut deep = V::int(0);
                for _ in 0..DEPTH {
                    deep = V::new_pair(V::int(0), deep);
                }
                let mut stack = stk![deep];
                drain_value_stack(&mut stack);
                assert!(stack.pop().is_none());
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Regression for round-2 BLOCKER (shared-Rc evasion of
    /// `drain_value_stack_in_place`): `DUP` of a deep value bumps the Rc
    /// refcount; both copies on the stack see `Rc::get_mut == None`. Pre-
    /// fix the in-place drain skipped both; the last one to drop hit
    /// refcount=1 and recursed through the deep spine -> SIGABRT. Post-
    /// fix the in-place drain has a sentinel-replacement first pass that
    /// breaks aliased Rcs so the second pass can drain the survivor.
    /// Builds the shape directly via shared `Rc::clone` (matches the
    /// runtime shape of `PUSH ; LOOP { PAIR } ; DUP ; PUSH 42 ; FAILWITH`).
    #[test]
    fn dup_deep_value_then_failwith_does_not_overflow_on_drop() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut deep = V::int(0);
                for _ in 0..DEPTH {
                    deep = V::new_pair(V::int(0), deep);
                }
                let shared = Rc::new(deep);
                // Two Rc::clone siblings (refcount=2) plus a shallow marker
                // on top — matches the runtime shape of `PUSH ; LOOP { PAIR }
                // ; DUP ; PUSH 1 ; FAILWITH`. FAILWITH pops the marker, the
                // shared deep value stays on bottom twice.
                let mut stack: IStack = Stack::new();
                stack.push(Rc::clone(&shared));
                stack.push(shared);
                stack.push(Rc::new(V::int(42)));
                let outcome = interpret(&[Failwith(Type::Int)], &mut Ctx::default(), &mut stack);
                assert!(matches!(outcome, Err(InterpretError::FailedWith(..))));
                drop(outcome);
                drop(stack);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Regression: on the error path the caller's restored stack may still
    /// carry a deep runtime-built value pushed before the error -- e.g. a
    /// `LOOP`-built right-comb DUP'd onto the stack and then a separate
    /// FAILWITH on the duplicate copy leaves the original on `bottom`. Pre-
    /// fix, only `ContractScript::interpret` drained the caller stack via
    /// `drain_value_stack` (pop-based), so any direct caller of
    /// `Instruction::interpret` / `fn interpret` -- notably the EVM
    /// gateway's per-instruction view loop in
    /// `etherlink/.../tezosx-tezos-runtime/src/view.rs` -- inherited a stack
    /// whose drop overflowed the kernel ~1 MiB Rust stack. Now the in-place
    /// drain runs in `fn interpret` so every caller is covered AND the
    /// existing "Err preserves caller stack" contract is honored: the stack
    /// length and atomic values are unchanged; only the deep composite spine
    /// inside any leftover value is severed.
    #[test]
    fn interpret_err_drains_leftover_caller_stack_in_place() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut deep = V::int(0);
                for _ in 0..DEPTH {
                    deep = V::new_pair(V::int(0), deep);
                }
                let marker = V::int(42);
                // deep at bottom, marker on top. FAILWITH pops marker; deep
                // stays on the bottom across the unwind. Without the
                // bottom-stack drain in `fn interpret`, dropping the caller's
                // `stack` here would overflow.
                let mut stack: IStack = stk![deep, marker.clone()];
                let outcome = interpret(&[Failwith(Type::Int)], &mut Ctx::default(), &mut stack);
                assert!(matches!(outcome, Err(InterpretError::FailedWith(..))));
                // In-place drain preserves stack length and atomic values;
                // the marker is still observable post-Err. The deep value
                // is structurally flattened; only its variant tag remains.
                assert_eq!(stack.len(), 1, "bottom stack length preserved post-Err");
                drop(outcome);
                drop(stack);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Regression: `FAILWITH` of a deep runtime-built value embeds the value
    /// in `InterpretError::FailedWith(_, TypedValue<'a>)`. Pre-fix the
    /// resulting `Err` propagates through `fn interpret`'s leftover-stack /
    /// frame drain — both of which target the value *stack*, not the error
    /// variant — and lands at the caller's `drop` site with the deep value
    /// still recursive. The auto-derived `TypedValue` destructor overflows
    /// the kernel's ~1 MiB Rust stack. Drain the embedded value at the
    /// driver-error boundary so `Err` propagation is safe for any caller
    /// (L2-1446 + the multi-agent review BLOCKER).
    #[test]
    fn failwith_with_deep_value_does_not_overflow_on_drop() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut deep = V::int(0);
                for _ in 0..DEPTH {
                    deep = V::new_pair(V::int(0), deep);
                }
                let mut stack: IStack = stk![deep];
                let outcome = interpret(&[Failwith(Type::Int)], &mut Ctx::default(), &mut stack);
                assert!(matches!(outcome, Err(InterpretError::FailedWith(..))));
                drop(outcome);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Companion to [`drain_deep_value_stack_does_not_overflow`] for the
    /// control-flow worklist: every `InterpFrame` variant that owns runtime
    /// values is given a deep one, then `drain_value_frames` must flatten them
    /// all without the recursive `Rc<TypedValue>` destructor overflowing the
    /// ~1 MiB worker stack. This is the value carried on the error-unwind path
    /// by a `DIP`-protected comb or an in-flight `ITER`/`MAP` (L2-1446).
    #[test]
    fn drain_deep_value_frames_does_not_overflow() {
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let deep = || {
                    let mut d = V::int(0);
                    for _ in 0..DEPTH {
                        d = V::new_pair(V::int(0), d);
                    }
                    Rc::new(d)
                };
                let empty: &[Instruction] = &[];
                let body = || CodeRef::Borrowed(empty);
                let frames: Vec<InterpFrame> = vec![
                    InterpFrame::AfterDip {
                        protected: stk![(*deep()).clone()],
                        opt_height: None,
                    },
                    InterpFrame::IterList {
                        body: body(),
                        remaining: vec![deep()].into_iter(),
                    },
                    InterpFrame::IterSet {
                        body: body(),
                        remaining: BTreeSet::from([deep()]).into_iter(),
                    },
                    InterpFrame::IterMap {
                        body: body(),
                        remaining: BTreeMap::from([(Rc::new(V::int(0)), deep())]).into_iter(),
                    },
                    InterpFrame::MapListAccum {
                        body: body(),
                        remaining: vec![deep()].into_iter(),
                        acc: vec![deep()],
                    },
                    InterpFrame::MapMapAccum {
                        body: body(),
                        remaining: BTreeMap::from([(Rc::new(V::int(0)), deep())]).into_iter(),
                        acc: BTreeMap::from([(Rc::new(V::int(0)), deep())]),
                        current_key: Some(deep()),
                    },
                ];
                drain_value_frames(frames);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// Zero-copy bodies: a control-flow body borrowed from the (arena-lived)
    /// AST is never deep-cloned by the driver, even across loop iterations.
    /// Before this optimization, `body_to_owned` ran once per opened nested
    /// block — so an ITER body containing a nested IF cloned the IF branch on
    /// every iteration. Now AST-borrowed bodies stay `CodeRef::Borrowed`, and
    /// `body_to_owned` runs only for runtime `Rc` bodies (EXEC/View). The
    /// `BODY_CLONES` counter (cfg(test)) asserts both directions.
    #[test]
    fn ast_borrowed_bodies_are_not_cloned() {
        use crate::parser::test_helpers::parse;

        let clones = || BODY_CLONES.with(|c| c.get());
        let reset = || BODY_CLONES.with(|c| c.set(0));

        // (a) An ITER over a 3-element list whose body holds a nested IF: all
        // bodies are borrowed straight from the AST, so zero clones happen,
        // including across the three iterations.
        let iter_prog = parse(
            "{ NIL int ; PUSH int 3 ; CONS ; PUSH int 2 ; CONS ; PUSH int 1 ; CONS ; \
               ITER { DROP ; PUSH bool True ; IF {} {} } }",
        )
        .unwrap()
        .typecheck_instruction(&mut Gas::default(), None, &[])
        .unwrap();
        let arena = Arena::new();
        let mut stack: IStack = Stack::new();
        reset();
        iter_prog
            .interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        assert_eq!(clones(), 0, "AST-borrowed bodies must not be cloned");

        // (b) The same nested IF inside a LAMBDA body run via EXEC: the lambda
        // code is a runtime `Rc`, so its nested control-flow IS cloned. This
        // guards against the counter being trivially zero.
        let exec_prog = parse(
            "{ LAMBDA int int { PUSH bool True ; IF {} {} } ; PUSH int 5 ; EXEC ; DROP }",
        )
        .unwrap()
        .typecheck_instruction(&mut Gas::default(), None, &[])
        .unwrap();
        let arena2 = Arena::new();
        let mut stack2: IStack = Stack::new();
        reset();
        exec_prog
            .interpret(&mut Ctx::default(), &arena2, &mut stack2)
            .unwrap();
        assert!(
            clones() > 0,
            "a nested body inside an owned (EXEC) lambda is still cloned"
        );
    }

    /// Residual scope of the optimization (L2-1442): a loop *inside an EXEC'd
    /// lambda* runs on an owned `Rc` body, so its nested control-flow is still
    /// deep-cloned once per iteration — a borrow into the lambda `Rc` cannot
    /// outlive the worklist frame. This pins that boundary: two extra ITER
    /// iterations inside the lambda add exactly two nested-IF clones, whereas
    /// the AST-borrowed case (above) is always zero regardless of iterations.
    #[test]
    fn owned_lambda_loop_clones_scale_per_iteration() {
        use crate::parser::test_helpers::parse;

        let run = |prog_src: &str| -> usize {
            let prog = parse(prog_src)
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[])
                .unwrap();
            let arena = Arena::new();
            let mut stack: IStack = Stack::new();
            BODY_CLONES.with(|c| c.set(0));
            prog.interpret(&mut Ctx::default(), &arena, &mut stack)
                .unwrap();
            BODY_CLONES.with(|c| c.get())
        };

        // The lambda iterates its input list, running a nested IF per element.
        let lam = "LAMBDA (list int) unit { ITER { DROP ; PUSH bool True ; IF {} {} } ; UNIT }";
        let c2 = run(&format!(
            "{{ {lam} ; NIL int ; PUSH int 2 ; CONS ; PUSH int 1 ; CONS ; EXEC ; DROP }}"
        ));
        let c4 = run(&format!(
            "{{ {lam} ; NIL int ; PUSH int 4 ; CONS ; PUSH int 3 ; CONS ; \
                PUSH int 2 ; CONS ; PUSH int 1 ; CONS ; EXEC ; DROP }}"
        ));

        assert!(c2 > 0, "owned lambda body clones nested control-flow");
        assert_eq!(
            c4 - c2,
            2,
            "each extra ITER iteration inside the owned lambda adds one nested-IF clone"
        );
    }

    mod sub {
        use super::*;

        #[track_caller]
        fn test_sub(
            overload: overloads::Sub,
            input1: TypedValue,
            input2: TypedValue,
            output: TypedValue,
        ) {
            let mut stack = stk![input2, input1];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Sub(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        macro_rules! test {
            ($name:ident, $overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    test_sub(overloads::Sub::$overload, $i1, $i2, $out);
                }
            };
        }

        test!(
            NatNatPos,
            NatNat,
            V::nat(54263),
            V::nat(5034),
            V::int(49229),
        );

        test!(
            NatNatNeg,
            NatNat,
            V::nat(5034),
            V::nat(54263),
            V::int(-49229),
        );

        test!(NatNatZero, NatNat, V::nat(43872), V::nat(43872), V::int(0),);

        test!(
            NatPosInt,
            NatInt,
            V::nat(99131),
            V::int(11012),
            V::int(88119),
        );

        test!(
            NatNegInt,
            NatInt,
            V::nat(99131),
            V::int(-11012),
            V::int(110143),
        );

        test!(
            PosIntNat,
            IntNat,
            V::int(11012),
            V::nat(99131),
            V::int(-88119),
        );

        test!(
            NegIntNat,
            IntNat,
            V::int(-11012),
            V::nat(99131),
            V::int(-110143),
        );

        test!(NegIntNegInt, IntInt, V::int(-10), V::int(-30), V::int(20),);

        test!(NegIntPosInt, IntInt, V::int(-10), V::int(30), V::int(-40),);

        test!(PosIntNegInt, IntInt, V::int(10), V::int(-30), V::int(40),);

        test!(PosIntPosInt, IntInt, V::int(10), V::int(30), V::int(-20),);
    }

    #[test]
    #[cfg(feature = "bls")]
    fn test_add_bls12_381_fr() {
        let mut stack = stk![
            V::Bls12381Fr(bls::Fr::one()),
            V::Bls12381Fr(bls::Fr::zero())
        ];
        let expected_stack = stk![V::Bls12381Fr(bls::Fr::one())];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::Bls12381Fr), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    #[cfg(feature = "bls")]
    fn test_add_bls12_381_g1() {
        let mut stack = stk![
            V::new_bls12381_g1(bls::G1::one()),
            V::new_bls12381_g1(bls::G1::zero())
        ];
        let expected_stack = stk![V::new_bls12381_g1(bls::G1::one())];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::Bls12381G1), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    #[cfg(feature = "bls")]
    fn test_add_bls12_381_g2() {
        let mut stack = stk![
            V::new_bls12381_g2(bls::G2::one()),
            V::new_bls12381_g2(bls::G2::zero())
        ];
        let expected_stack = stk![V::new_bls12381_g2(bls::G2::one())];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::Bls12381G2), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_add_mutez() {
        let mut stack = stk![V::Mutez(2i64.pow(62)), V::Mutez(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::MutezMutez), &mut ctx, &mut stack).is_ok());
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap() - 45
        );
        assert_eq!(stack, stk![V::Mutez(2i64.pow(62) + 20)]);
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![V::Mutez(2i64.pow(62)), V::Mutez(2i64.pow(62))]
            ),
            Err(InterpretError::Overflow)
        );
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![
                    V::Mutez((2u64.pow(63) - 1).try_into().unwrap()),
                    V::Mutez(1)
                ]
            ),
            Err(InterpretError::Overflow)
        );
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![
                    V::Mutez(1),
                    V::Mutez((2u64.pow(63) - 1).try_into().unwrap())
                ]
            ),
            Err(InterpretError::Overflow)
        );
    }

    #[test]
    fn test_add_timestamp_int() {
        let str_o1: &str = "1979-02-21T17:13:37+01:00";
        let int_o1 = DateTime::parse_from_rfc3339(str_o1)
            .unwrap()
            .timestamp()
            .into();
        let str_result: &str = "2000-02-21T17:14:37+01:00";
        let int_result = DateTime::parse_from_rfc3339(str_result)
            .unwrap()
            .timestamp()
            .into();
        let mut stack = stk![V::Int("662688060".parse().unwrap()), V::Timestamp(int_o1),];
        let expected_stack = stk![V::Timestamp(int_result)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::TimestampInt), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_add_int_timestamp() {
        let str_o2: &str = "1972-02-23T19:31:04+01:00";
        let int_o2 = DateTime::parse_from_rfc3339(str_o2)
            .unwrap()
            .timestamp()
            .into();
        let str_result: &str = "1973-01-30T03:08:24+01:00";
        let int_result = DateTime::parse_from_rfc3339(str_result)
            .unwrap()
            .timestamp()
            .into();
        let mut stack = stk![V::Timestamp(int_o2), V::Int("29489840".parse().unwrap()),];
        let expected_stack = stk![V::Timestamp(int_result)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::IntTimestamp), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_sub_timestamp_int() {
        let str_o1: &str = "2000-02-21T17:14:37+01:00";
        let int_o1 = DateTime::parse_from_rfc3339(str_o1)
            .unwrap()
            .timestamp()
            .into();
        let str_result: &str = "1979-02-21T17:13:37+01:00";
        let int_result = DateTime::parse_from_rfc3339(str_result)
            .unwrap()
            .timestamp()
            .into();
        let mut stack = stk![V::Int("662688060".parse().unwrap()), V::Timestamp(int_o1),];
        let expected_stack = stk![V::Timestamp(int_result)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Sub(overloads::Sub::TimestampInt), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_sub_timestamp_timestamp() {
        let str_o1: &str = "1973-01-30T03:08:24+01:00";
        let int_o1 = DateTime::parse_from_rfc3339(str_o1)
            .unwrap()
            .timestamp()
            .into();
        let str_o2: &str = "1972-02-23T19:31:04+01:00";
        let int_o2 = DateTime::parse_from_rfc3339(str_o2)
            .unwrap()
            .timestamp()
            .into();
        let mut stack = stk![V::Timestamp(int_o2), V::Timestamp(int_o1),];
        let expected_stack = stk![V::Int("29489840".parse().unwrap())];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Sub(overloads::Sub::TimestampTimestamp),
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    mod logic {
        use super::*;

        #[track_caller]
        fn check(instr: Instruction, val1: TypedValue, val2: TypedValue, expected: TypedValue) {
            let mut stack = stk![val2, val1];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&instr, &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, stk![expected]);
        }

        #[test]
        fn and() {
            use overloads as o;

            check(
                And(o::And::Bool),
                V::Bool(false),
                V::Bool(false),
                V::Bool(false),
            );
            check(
                And(o::And::Bool),
                V::Bool(false),
                V::Bool(true),
                V::Bool(false),
            );
            check(
                And(o::And::Bool),
                V::Bool(true),
                V::Bool(false),
                V::Bool(false),
            );
            check(
                And(o::And::Bool),
                V::Bool(true),
                V::Bool(true),
                V::Bool(true),
            );

            // 101 & 110 = 100
            check(And(o::And::NatNat), V::nat(5), V::nat(6), V::nat(4));
            // same
            check(And(o::And::IntNat), V::int(5), V::nat(6), V::nat(4));
            // 1100 & 1111 = 1100
            check(And(o::And::IntNat), V::int(-4), V::nat(15), V::nat(12));
            // 1011 & 0110 = 0010
            check(And(o::And::IntNat), V::int(-5), V::nat(6), V::nat(2));
            // small neg int & large nat
            check(And(o::And::IntNat), V::int(-8), V::nat(1003), V::nat(1000));
            // large neg int & small nat
            check(And(o::And::IntNat), V::int(-1001), V::nat(12), V::nat(4));
            // large nats (several "digits" in BigUint)
            check(
                And(o::And::NatNat),
                V::Nat("123456789123456789123456789123456789".parse().unwrap()),
                V::Nat("234567891234567890123456789123456789".parse().unwrap()),
                V::Nat("26042851674400450614930174457700117".parse().unwrap()),
            );
            // large int & nat (several "digits" in big number)
            check(
                And(o::And::IntNat),
                V::Int("-123456789123456789123456789123456789".parse().unwrap()),
                V::Nat("234567891234567890123456789123456789".parse().unwrap()),
                V::Nat("208525039560167439508526614665756673".parse().unwrap()),
            );

            check(And(o::And::Bytes), mk_0x("05"), mk_0x("06"), mk_0x("04"));
            check(And(o::And::Bytes), mk_0x(""), mk_0x("f00f"), mk_0x(""));
            check(
                And(o::And::Bytes),
                mk_0x("f00f"),
                mk_0x("1234"),
                mk_0x("1004"),
            );
            check(And(o::And::Bytes), mk_0x("f0"), mk_0x("1234"), mk_0x("30"));
            check(
                And(o::And::Bytes),
                mk_0x("f00ff0"),
                mk_0x("1234"),
                mk_0x("0230"),
            );
        }

        #[test]
        fn or() {
            use overloads as o;

            check(
                Or(o::Or::Bool),
                V::Bool(false),
                V::Bool(false),
                V::Bool(false),
            );
            check(
                Or(o::Or::Bool),
                V::Bool(false),
                V::Bool(true),
                V::Bool(true),
            );
            check(
                Or(o::Or::Bool),
                V::Bool(true),
                V::Bool(false),
                V::Bool(true),
            );
            check(Or(o::Or::Bool), V::Bool(true), V::Bool(true), V::Bool(true));

            // 101 & 110 = 111
            check(Or(o::Or::Nat), V::nat(5), V::nat(6), V::nat(7));
            // large numbers (several "digits" in BigInt)
            check(
                Or(o::Or::Nat),
                V::Nat("123456789123456789123456789123456789".parse().unwrap()),
                V::Nat("234567891234567890123456789123456789".parse().unwrap()),
                V::Nat("331981828683624228631983403789213461".parse().unwrap()),
            );

            check(Or(o::Or::Bytes), mk_0x("05"), mk_0x("06"), mk_0x("07"));
            check(Or(o::Or::Bytes), mk_0x(""), mk_0x("f00f"), mk_0x("f00f"));
            check(
                Or(o::Or::Bytes),
                mk_0x("f00f"),
                mk_0x("1234"),
                mk_0x("f23f"),
            );
            check(Or(o::Or::Bytes), mk_0x("f0"), mk_0x("1234"), mk_0x("12f4"));
            check(
                Or(o::Or::Bytes),
                mk_0x("f00ff0"),
                mk_0x("1234"),
                mk_0x("f01ff4"),
            );
        }

        #[test]
        fn xor() {
            use overloads as o;

            check(
                Xor(o::Xor::Bool),
                V::Bool(false),
                V::Bool(false),
                V::Bool(false),
            );
            check(
                Xor(o::Xor::Bool),
                V::Bool(false),
                V::Bool(true),
                V::Bool(true),
            );
            check(
                Xor(o::Xor::Bool),
                V::Bool(true),
                V::Bool(false),
                V::Bool(true),
            );
            check(
                Xor(o::Xor::Bool),
                V::Bool(true),
                V::Bool(true),
                V::Bool(false),
            );

            // 101 & 110 = 011
            check(Xor(o::Xor::Nat), V::nat(5), V::nat(6), V::nat(3));
            // large numbers (several "digits" in BigInt)
            check(
                Xor(o::Xor::Nat),
                V::Nat("123456789123456789123456789123456789".parse().unwrap()),
                V::Nat("234567891234567890123456789123456789".parse().unwrap()),
                V::Nat("305938977009223778017053229331513344".parse().unwrap()),
            );

            check(Xor(o::Xor::Bytes), mk_0x("05"), mk_0x("06"), mk_0x("03"));
            check(Xor(o::Xor::Bytes), mk_0x(""), mk_0x("f00f"), mk_0x("f00f"));
            check(
                Xor(o::Xor::Bytes),
                mk_0x("f00f"),
                mk_0x("1234"),
                mk_0x("e23b"),
            );
            check(
                Xor(o::Xor::Bytes),
                mk_0x("f0"),
                mk_0x("1234"),
                mk_0x("12c4"),
            );
            check(
                Xor(o::Xor::Bytes),
                mk_0x("f00ff0"),
                mk_0x("1234"),
                mk_0x("f01dc4"),
            );
        }
    }

    #[test]
    fn test_not() {
        #[track_caller]
        fn check(instr: overloads::Not, inp: TypedValue, expected: TypedValue) {
            let mut stack = stk![inp];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&Not(instr), &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, stk![expected]);
        }

        use overloads::Not as on;

        check(on::Bool, V::Bool(false), V::Bool(true));
        check(on::Bool, V::Bool(true), V::Bool(false));

        check(on::Int, V::int(0), V::int(-1));
        check(on::Int, V::int(5), V::int(-6));
        check(on::Int, V::int(-1), V::int(0));
        check(
            on::Int,
            V::Int(BigInt::parse_bytes(b"123456789123456789123456789123456789", 10).unwrap()),
            V::Int(BigInt::parse_bytes(b"-123456789123456789123456789123456790", 10).unwrap()),
        );

        check(on::Nat, V::nat(0), V::int(-1));
        check(on::Nat, V::nat(5), V::int(-6));
        check(
            on::Nat,
            V::Nat(BigUint::parse_bytes(b"123456789123456789123456789123456789", 10).unwrap()),
            V::Int(BigInt::parse_bytes(b"-123456789123456789123456789123456790", 10).unwrap()),
        );

        check(on::Bytes, mk_0x(""), mk_0x(""));
        check(on::Bytes, mk_0x("1234"), mk_0x("edcb"));
    }

    #[test]
    fn test_dip() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(25), V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Dip(None, vec![Add(overloads::Add::NatNat)])],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dip2() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(5), V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Dip(Some(2), vec![Drop(None)])], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_drop() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(20), V::nat(5)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Drop(None), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_drop2() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Drop(Some(2)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dup() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(20), V::nat(5), V::nat(10), V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dup(None), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dup2() {
        let mut stack = stk![V::nat(20), V::nat(5), V::nat(10)];
        let expected_stack = stk![V::nat(20), V::nat(5), V::nat(10), V::nat(5)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dup(Some(2)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    mod int_comparison {
        use super::*;

        #[track_caller]
        fn test_comp(instr: Instruction, arg: impl Into<BigInt>, result: bool) {
            let mut stack = stk![V::int(20), V::int(arg.into())];
            let expected_stack = stk![V::int(20), V::Bool(result)];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&instr, &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, expected_stack);
            assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap());
        }

        #[test]
        fn gt() {
            test_comp(Gt, 10, true);
            test_comp(Gt, 0, false);
            test_comp(Gt, -10, false);
        }

        #[test]
        fn ge() {
            test_comp(Ge, 10, true);
            test_comp(Ge, 0, true);
            test_comp(Ge, -10, false);
        }

        #[test]
        fn eq() {
            test_comp(Eq, 10, false);
            test_comp(Eq, 0, true);
            test_comp(Eq, -10, false);
        }

        #[test]
        fn neq() {
            test_comp(Neq, 10, true);
            test_comp(Neq, 0, false);
            test_comp(Neq, -10, true);
        }

        #[test]
        fn le() {
            test_comp(Le, 10, false);
            test_comp(Le, 0, true);
            test_comp(Le, -10, true);
        }

        #[test]
        fn lt() {
            test_comp(Lt, 10, false);
            test_comp(Lt, 0, false);
            test_comp(Lt, -10, true);
        }
    }

    #[test]
    fn test_if_t() {
        let mut stack = stk![V::int(20), V::int(5), V::Bool(true)];
        let expected_stack = stk![V::int(20)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)])],
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_if_f() {
        let mut stack = stk![V::int(20), V::int(5), V::Bool(false)];
        let expected_stack = stk![V::int(25)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)])],
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_abs() {
        #[track_caller]
        fn test(arg: impl Into<BigInt>, res: impl Into<BigUint>) {
            let mut stack = stk![V::nat(20), V::Int(arg.into())];
            let expected_stack = stk![V::nat(20), V::Nat(res.into())];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&Abs, &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, expected_stack);
            assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap());
        }
        test(0, 0u32);
        test(10, 10u32);
        test(-10, 10u32);
    }

    #[test]
    fn test_is_nat() {
        #[track_caller]
        fn test(arg: impl Into<BigInt>, res: Option<impl Into<BigUint>>) {
            let mut stack = stk![V::nat(20), V::Int(arg.into())];
            let expected_stack = stk![V::nat(20), V::new_option(res.map(|x| V::Nat(x.into())))];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&IsNat, &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, expected_stack);
            assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap());
        }
        test(0, Some(0u32));
        test(10, Some(10u32));
        test(-10, None::<u32>);
    }

    #[test]
    fn test_int_nat() {
        let mut stack = stk![V::nat(20), V::nat(10)];
        let expected_stack = stk![V::nat(20), V::int(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Int(overloads::Int::Nat), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    #[cfg(feature = "bls")]
    fn test_int_bls12_381_fr() {
        let mut stack = stk![V::Bls12381Fr(bls::Fr::one())];
        let expected_stack = stk![V::int(1)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Int(overloads::Int::Bls12381Fr), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_int_bytes() {
        fn test(input: &str, result: impl Into<BigInt>) {
            let mut stack = stk![V::Bytes(hex::decode(input).unwrap())];
            let expected_stack = stk![V::Int(result.into())];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&Int(overloads::Int::Bytes), &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, expected_stack);
        }
        // checked against octez-client
        test("", 0);
        test("00", 0);
        test("0000", 0);
        test("01", 1);
        test("0001", 1);
        test("000001", 1);
        test("0100", 256);
        test("1000", 4096);
        test("f000", -4096);
        test("00f000", 61440);
        test("ff", -1);
        test("ff00", -256);
        test("00ff00", 65280);
    }

    #[test]
    fn test_nat_bytes() {
        fn test(input: &str, result: impl Into<BigUint>) {
            let mut stack = stk![V::Bytes(hex::decode(input).unwrap())];
            let expected_stack = stk![V::Nat(result.into())];
            let mut ctx = Ctx::default();
            assert!(interpret_one(&Nat, &mut ctx, &mut stack).is_ok());
            assert_eq!(stack, expected_stack);
        }
        // checked against octez-client
        test("", 0u32);
        test("00", 0u32);
        test("0000", 0u32);
        test("01", 1u32);
        test("0001", 1u32);
        test("000001", 1u32);
        test("0100", 256u32);
        test("1000", 4096u32);
        test("f000", 61440u32);
        test("ff", 255u32);
        test("ff00", 65280u32);
        test("00ff00", 65280u32);
    }

    mod bytes {
        use super::*;

        #[test]
        fn nat() {
            #[track_caller]
            fn test(result: &str, input: impl Into<BigUint>) {
                let mut stack = stk![V::Nat(input.into())];
                let expected_stack = stk![V::Bytes(hex::decode(result).unwrap())];
                let mut ctx = Ctx::default();
                assert!(interpret_one(&Bytes(overloads::Bytes::Nat), &mut ctx, &mut stack).is_ok());
                assert_eq!(stack, expected_stack);
            }
            // checked against octez-client
            test("", 0u32);
            test("01", 1u32);
            test("0100", 256u32);
            test("1000", 4096u32);
            test("f000", 61440u32);
            test("ff", 255u32);
            test("ff00", 65280u32);
        }

        #[test]
        fn int() {
            #[track_caller]
            fn test(result: &str, input: impl Into<BigInt>) {
                let mut stack = stk![V::Int(input.into())];
                let expected_stack = stk![V::Bytes(hex::decode(result).unwrap())];
                let mut ctx = Ctx::default();
                assert!(interpret_one(&Bytes(overloads::Bytes::Int), &mut ctx, &mut stack).is_ok());
                assert_eq!(stack, expected_stack);
            }
            // checked against octez-client
            test("", 0);
            test("01", 1);
            test("0100", 256);
            test("1000", 4096);
            test("f000", -4096);
            test("00f000", 61440);
            test("ff", -1);
            test("ff00", -256);
            test("00ff00", 65280);
        }
    }

    #[test]
    fn test_push() {
        let mut stack = stk![V::nat(20), V::nat(10)];
        let expected_stack = stk![V::nat(20), V::nat(10), V::nat(0)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Push(Rc::new(V::nat(0))), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_0() {
        let mut stack = stk![V::nat(20), V::nat(10), V::Bool(false)];
        let expected_stack = stk![V::nat(20), V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Loop(vec![
                Push(Rc::new(V::nat(1))),
                Add(overloads::Add::NatNat),
                Push(Rc::new(V::Bool(false)))
            ])],
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_1() {
        let mut stack = stk![V::nat(20), V::nat(10), V::Bool(true)];
        let expected_stack = stk![V::nat(20), V::nat(11)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Loop(vec![
                Push(Rc::new(V::nat(1))),
                Add(overloads::Add::NatNat),
                Push(Rc::new(V::Bool(false)))
            ])],
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_many() {
        let mut stack = stk![V::nat(20), V::int(10), V::Bool(true)];
        let expected_stack = stk![V::nat(20), V::int(0)];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Loop(vec![
                Push(Rc::new(V::int(-1))),
                Add(overloads::Add::IntInt),
                Dup(None),
                Gt
            ])],
            &mut ctx,
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn loop_left_0() {
        let mut stack = stk![V::new_or(Or::Right(V::nat(0)))];
        let mut ctx = Ctx::default();
        assert_eq!(
            interpret(&[LoopLeft(vec![Failwith(Type::Nat)])], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::nat(0)])
    }

    #[test]
    fn loop_left_1() {
        let mut stack = stk![V::new_or(Or::Left(V::nat(0)))];
        let mut ctx = Ctx::default();
        assert_eq!(
            interpret(
                &[LoopLeft(vec![
                    Drop(None),
                    Push(Rc::new(V::new_or(Or::Right(V::int(1)))))
                ])],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::int(1)])
    }

    // TODO: the current branch doesn't have LEFT and RIGHT instructions, so
    // adding a testcase for LOOP_LEFT with more than one iteration is
    // unnecessarily complicated. Left for future work.

    #[test]
    fn test_iter_list_many() {
        let mut stack = stk![
            V::List(MichelsonList::new()),
            V::List((1..5).map(V::int).collect())
        ];
        assert!(interpret(
            &[Iter(overloads::Iter::List, vec![Cons])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        // NB: walking a list start-to-end and CONSing each element effectively
        // reverses the list.
        assert_eq!(stack, stk![V::List((1..5).rev().map(V::int).collect())]);
    }

    #[test]
    fn test_iter_list_zero() {
        let mut stack = stk![V::Unit, V::List(MichelsonList::new())];
        assert!(interpret(
            &[Iter(overloads::Iter::List, vec![Drop(None)])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::Unit]);
    }

    #[test]
    fn test_iter_set_many() {
        let mut stack = stk![
            V::List(MichelsonList::new()),
            V::Set(rc_set([V::int(1), V::int(2), V::int(3)]))
        ];
        assert!(interpret(
            &[Iter(overloads::Iter::Set, vec![Cons])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(
            stack,
            stk![V::List(
                // NB: traversing the set start-to-end, we're CONSing to a
                // list, thus the first element of the map is the last element
                // of the list.
                vec![V::int(3), V::int(2), V::int(1),].into()
            )]
        );
    }

    #[test]
    fn test_iter_set_zero() {
        let mut stack = stk![V::int(0), V::Set(BTreeSet::new())];
        assert!(interpret(
            &[Iter(overloads::Iter::Set, vec![Add(overloads::Add::IntInt)])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(0)]);
    }

    #[test]
    fn test_iter_map_many() {
        let mut stack = stk![
            V::List(MichelsonList::new()),
            V::Map(rc_map([
                (V::int(1), V::nat(1)),
                (V::int(2), V::nat(2)),
                (V::int(3), V::nat(3)),
            ]))
        ];
        assert!(interpret(
            &[Iter(overloads::Iter::Map, vec![Cons])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(
            stack,
            stk![V::List(
                // NB: traversing the map start-to-end, we're CONSing to a
                // list, thus the first element of the map is the last element
                // of the list.
                vec![
                    V::new_pair(V::int(3), V::nat(3)),
                    V::new_pair(V::int(2), V::nat(2)),
                    V::new_pair(V::int(1), V::nat(1)),
                ]
                .into()
            )]
        );
    }

    #[test]
    fn test_iter_map_zero() {
        let mut stack = stk![V::int(0), V::Map(BTreeMap::new())];
        assert!(interpret(
            &[Iter(overloads::Iter::Map, vec![Car, Add(overloads::Add::IntInt)])],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(0)]);
    }

    #[test]
    fn test_map() {
        fn test(
            overload: overloads::Map,
            collection_length: u32,
            mut input_stack: IStack<'_>,
            expected_stack: IStack<'_>,
            expected_map_gas_cost: u32,
        ) {
            let mut ctx = Ctx::default();

            assert!(
                interpret(&[Map(overload, vec![ISome])], &mut ctx, &mut input_stack,).is_ok()
            );

            assert_eq!(input_stack, expected_stack);

            assert_eq!(
                ctx.gas().milligas().unwrap(),
                Gas::default().milligas().unwrap()
                    - expected_map_gas_cost
                    - interpret_cost::PUSH * collection_length
                    - interpret_cost::SOME * collection_length
                    - interpret_cost::INTERPRET_RET * collection_length
                    // Routing through `interpret` (the iterative driver) instead
                    // of `interpret_one` charges one extra INTERPRET_RET for the
                    // outer top-level block.
                    - interpret_cost::INTERPRET_RET
                    // The driver's `OpenMap*` arms push the accumulator
                    // frame and the body's `NextInstr` (the per-iteration
                    // re-push is done inside `run_interp_driver` and is
                    // not gas-charged — only growth pushes in `handle_step`
                    // are).
                    - interpret_cost::FRAME_PUSH * 2
            );
        }

        test(
            overloads::Map::List,
            3,
            stk![V::List((1..=3).map(V::int).collect())],
            stk![V::List(
                (1..=3).map(|i| V::new_option(Some(V::int(i)))).collect()
            )],
            interpret_cost::MAP_LIST,
        );

        test(
            overloads::Map::Option,
            1,
            stk![V::new_option(Some(V::int(1)))],
            stk![V::new_option(Some(V::new_option(Some(V::int(1)))))],
            interpret_cost::MAP_OPTION,
        );

        test(
            overloads::Map::Map,
            2,
            stk![V::Map(rc_map([
                (V::int(1), V::String("a".into())),
                (V::int(2), V::String("b".into()))
            ]))],
            stk![V::Map(rc_map([
                (
                    V::int(1),
                    V::new_option(Some(V::new_pair(V::int(1), V::String("a".into()))))
                ),
                (
                    V::int(2),
                    V::new_option(Some(V::new_pair(V::int(2), V::String("b".into()))))
                )
            ]))],
            interpret_cost::MAP_MAP,
        );
    }

    #[test]
    fn test_map_empty_collection() {
        fn test(overload: overloads::Map, mut stack: IStack<'_>) {
            let expected_stack = stack.clone();
            assert!(
                interpret(&[Map(overload, vec![ISome])], &mut Ctx::default(), &mut stack,)
                    .is_ok()
            );
            assert_eq!(stack, expected_stack);
        }

        test(overloads::Map::List, stk![V::List(MichelsonList::new())]);
        test(overloads::Map::Option, stk![V::new_option(None)]);
        test(overloads::Map::Map, stk![V::Map(BTreeMap::new())]);
    }

    #[test]
    fn test_map_in_order() {
        fn test(overload: overloads::Map, mut input_stack: IStack<'_>, expected_stack: IStack<'_>) {
            assert!(interpret(
                &[Map(overload, vec![Cons, Unit])],
                &mut Ctx::default(),
                &mut input_stack,
            )
            .is_ok());
            assert_eq!(input_stack, expected_stack);
        }

        // Lists are expected to be iterated from head to tail.
        test(
            overloads::Map::List,
            stk![
                V::List(MichelsonList::new()),
                V::List(vec![V::int(1), V::int(2)].into())
            ],
            stk![
                V::List(vec![V::int(2), V::int(1)].into()),
                V::List(vec![V::Unit, V::Unit].into())
            ],
        );

        // Maps are expected to be iterated in increasing order of keys.
        test(
            overloads::Map::Map,
            stk![
                V::List(MichelsonList::new()),
                V::Map(rc_map([
                    (V::int(1), V::String("a".into())),
                    (V::int(2), V::String("b".into()))
                ]))
            ],
            stk![
                V::List(
                    vec![
                        V::new_pair(V::int(2), V::String("b".into())),
                        V::new_pair(V::int(1), V::String("a".into()))
                    ]
                    .into()
                ),
                V::Map(rc_map([(V::int(1), V::Unit), (V::int(2), V::Unit)]))
            ],
        );
    }

    #[test]
    fn test_map_can_modify_underlying_stack() {
        let mut stack = stk![V::Bool(true), V::List(vec![V::Unit].into())];
        let expected_stack = stk![V::Bool(false), V::List(vec![V::Unit].into())];
        assert!(interpret(
            &[Map(
                overloads::Map::List,
                vec![Dip(None, vec![Not(overloads::Not::Bool)])]
            )],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_map_carry_over() {
        // At the end of each iteration, the current stack is expected to carry over to the next iteration.
        //
        // In other words: each iteration does not run on a clone of the original stack;
        // it runs on the stack from the previous iteration.
        let mut stack = stk![V::int(0), V::List(vec![V::int(1), V::int(2)].into())];
        let expected_stack = stk![V::int(3), V::List(vec![V::Unit, V::Unit].into())];
        assert!(interpret(
            &[Map(
                overloads::Map::List,
                vec![Add(overloads::Add::IntInt), Unit]
            )],
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_swap() {
        let mut stack = stk![V::nat(20), V::int(10)];
        let expected_stack = stk![V::int(10), V::nat(20)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Swap, &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            interpret_one(
                &Failwith(Type::Nat),
                &mut Ctx::default(),
                &mut stk![V::nat(20)]
            ),
            Err(InterpretError::FailedWith(Type::Nat, V::nat(20)))
        );
    }

    #[test]
    fn push_string_value() {
        let mut stack = Stack::new();
        assert_eq!(
            interpret(
                &[Push(Rc::new(V::String("foo".to_owned())))],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::String("foo".to_owned())]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = Stack::new();
        assert_eq!(
            interpret(&[Push(Rc::new(V::Unit))], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(&[Unit], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::Unit]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::UNIT
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_pair() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Push(Rc::new(V::new_pair(
                V::int(-5),
                V::new_pair(V::nat(3), V::Bool(false))
            )))],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(
            stack,
            stk![V::new_pair(
                V::int(-5),
                V::new_pair(V::nat(3), V::Bool(false))
            )]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_option() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Push(Rc::new(V::new_option(Some(V::int(-5)))))],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::new_option(Some(V::int(-5)))]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn car() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[
                Push(Rc::new(V::new_pair(
                    V::int(-5),
                    V::new_pair(V::nat(3), V::Bool(false))
                ))),
                Car
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(-5)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::CAR
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn cdr() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[
                Push(Rc::new(V::new_pair(
                    V::new_pair(V::nat(3), V::Bool(false)),
                    V::int(-5),
                ))),
                Cdr
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(-5)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::CDR
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn pair() {
        let mut stack = stk![V::nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&[Pair], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_pair(V::Bool(false), V::nat(42))]);
    }

    #[test]
    fn pair_n_3() {
        let mut stack = stk![V::String("foo".into()), V::Unit, V::nat(42), V::Bool(false)]; // NB: bool is top
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&PairN(3), ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![
                V::String("foo".into()),
                V::new_pair(V::Bool(false), V::new_pair(V::nat(42), V::Unit))
            ]
        );
        assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap())
    }

    #[test]
    fn pair_n_4() {
        let mut stack = stk![V::String("foo".into()), V::Unit, V::nat(42), V::Bool(false)]; // NB: bool is top
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&PairN(4), ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::new_pair(
                V::Bool(false),
                V::new_pair(V::nat(42), V::new_pair(V::Unit, V::String("foo".into())))
            )]
        );
        assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap())
    }

    #[test]
    fn unpair() {
        let mut stack = stk![V::new_pair(V::Bool(false), V::nat(42))];
        assert!(interpret(&[Unpair], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::nat(42), V::Bool(false)]);
    }

    #[test]
    fn unpair_n_3() {
        let mut stack = stk![V::new_pair(
            V::Bool(false),
            V::new_pair(V::nat(42), V::new_pair(V::Unit, V::String("foo".into())))
        )];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&UnpairN(3), ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![
                V::new_pair(V::Unit, V::String("foo".into())),
                V::nat(42),
                V::Bool(false)
            ],
        );
        assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap())
    }

    #[test]
    fn unpair_n_4() {
        let mut stack = stk![V::new_pair(
            V::Bool(false),
            V::new_pair(V::nat(42), V::new_pair(V::Unit, V::String("foo".into())))
        )];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&UnpairN(4), ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::String("foo".into()), V::Unit, V::nat(42), V::Bool(false)],
        );
        assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap())
    }

    /// Regression for L2-1434 (UNPAIR n): pre-fix `fn fill` recursed one
    /// Rust frame per pair level, so `UNPAIR (N+1)` on a depth-N right-comb
    /// value reliably overflowed the kernel's ~1 MiB Rust stack around
    /// N ≈ 100 (well below `u16::MAX` and within an operation's gas
    /// budget). With the iterative spine walk depth is bounded by gas only.
    /// 60_000 fits in `u16` and dwarfs the pre-fix overflow threshold.
    #[test]
    fn unpair_n_deep_does_not_overflow() {
        use std::thread;
        const DEPTH: u16 = 60_000;
        thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut deep = V::Unit;
                for _ in 0..DEPTH {
                    deep = V::new_pair(V::Unit, deep);
                }
                let mut stack = stk![deep];
                let ctx = &mut Ctx::default();
                ctx.gas = crate::gas::Gas::new(u32::MAX);
                interpret_one(&UnpairN(DEPTH + 1), ctx, &mut stack)
                    .expect("deep UnpairN must walk the spine iteratively");
                assert_eq!(stack.len(), DEPTH as usize + 1);
            })
            .unwrap()
            .join()
            .expect("worker joined");
    }

    #[test]
    fn pair_car() {
        let mut stack = stk![V::nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&[Pair, Car], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::Bool(false)]);
    }

    #[test]
    fn pair_cdr() {
        let mut stack = stk![V::nat(42), V::Bool(false)]; // NB: bool is top
        assert!(interpret(&[Pair, Cdr], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::nat(42)]);
    }

    #[test]
    fn if_none_1() {
        let code = vec![IfNone(vec![Push(Rc::new(V::int(5)))], vec![])];
        // with Some
        let mut stack = stk![V::new_option(Some(V::int(42)))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(42)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_NONE
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_none_2() {
        let code = vec![IfNone(vec![Push(Rc::new(V::int(5)))], vec![])];
        // with None
        let mut stack = stk![V::new_option(None)];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(5)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_NONE
                - interpret_cost::PUSH
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_cons_cons() {
        let code = vec![IfCons(
            vec![Swap, Drop(None)],
            vec![Push(Rc::new(V::int(0)))],
        )];
        let mut stack = stk![V::List(vec![V::int(1), V::int(2)].into())];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(1)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_CONS
                - interpret_cost::SWAP
                - interpret_cost::DROP
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_cons_nil() {
        let code = vec![IfCons(
            vec![Swap, Drop(None)],
            vec![Push(Rc::new(V::int(0)))],
        )];
        let mut stack = stk![V::List(MichelsonList::new())];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(0)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_CONS
                - interpret_cost::PUSH
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_left_left() {
        let code = vec![IfLeft(vec![], vec![Drop(None), Push(Rc::new(V::int(0)))])];
        let mut stack = stk![V::new_or(or::Or::Left(V::int(1)))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(1)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_LEFT
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_left_right() {
        let code = vec![IfLeft(vec![], vec![Drop(None), Push(Rc::new(V::int(0)))])];
        let mut stack = stk![V::new_or(or::Or::Right(V::Unit))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(0)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::IF_LEFT
                - interpret_cost::DROP
                - interpret_cost::PUSH
                - interpret_cost::FRAME_PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn some() {
        let mut stack = stk![V::int(5)];
        let mut ctx = Ctx::default();
        assert!(interpret(&[ISome], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_option(Some(V::int(5)))]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::SOME
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn none() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert!(interpret(&[Instruction::None], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_option(None)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::NONE
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn compare() {
        macro_rules! test {
            ($expr:tt, $res:tt) => {
                let mut stack: IStack<'_> = stk!$expr;
                let expected_cost = interpret_cost::compare(stack.get(0).unwrap().as_ref(), stack.get(1).unwrap().as_ref())
                    .unwrap()
                    + interpret_cost::INTERPRET_RET;
                let mut ctx = Ctx::default();
                assert!(interpret(&[Compare], &mut ctx, &mut stack).is_ok());
                assert_eq!(stack, stk!$res);
                assert_eq!(ctx.gas().milligas().unwrap(), Gas::default().milligas().unwrap() - expected_cost);
            };
        }
        test!([V::int(5), V::int(6)], [V::int(1)]);
        test!([V::int(5), V::int(5)], [V::int(0)]);
        test!([V::int(6), V::int(5)], [V::int(-1)]);
        test!([V::Bool(true), V::Bool(false)], [V::int(-1)]);
        test!([V::Bool(true), V::Bool(true)], [V::int(0)]);
        test!([V::Bool(false), V::Bool(true)], [V::int(1)]);
        test!([V::Bool(false), V::Bool(false)], [V::int(0)]);
        test!(
            [V::String("foo".to_owned()), V::String("bar".to_owned())],
            [V::int(-1)]
        );
        test!([V::Unit, V::Unit], [V::int(0)]);
        test!(
            [V::new_option(Some(V::int(5))), V::Option(None)],
            [V::int(-1)]
        );
    }

    #[test]
    fn amount() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        ctx.amount = 100500;
        assert_eq!(interpret(&[Amount], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Mutez(100500)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::INTERPRET_RET
                - interpret_cost::AMOUNT,
        )
    }

    #[test]
    fn push_int_list() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert_eq!(
            interpret(
                &[Push(Rc::new(V::List(
                    vec![V::int(1), V::int(2), V::int(3),].into()
                )))],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::List(vec![V::int(1), V::int(2), V::int(3),].into())]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn nil() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&[Nil], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::List(MichelsonList::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::NIL
                - interpret_cost::INTERPRET_RET,
        )
    }

    #[test]
    fn cons() {
        let mut stack = stk![V::List(vec![V::int(321)].into()), V::int(123)];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&[Cons], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::List(vec![V::int(123), V::int(321)].into())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::CONS
                - interpret_cost::INTERPRET_RET,
        )
    }

    #[test]
    fn push_map() {
        let mut stack = Stack::new();
        let mut ctx = Ctx::default();
        let map = rc_map([
            (V::int(1), V::String("foo".to_owned())),
            (V::int(2), V::String("bar".to_owned())),
        ]);
        assert_eq!(
            interpret(&[Push(Rc::new(V::Map(map.clone())))], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Map(map)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_map() {
        let mut ctx = Ctx::default();
        let map = rc_map([
            (V::int(1), V::String("foo".to_owned())),
            (V::int(2), V::String("bar".to_owned())),
        ]);
        let mut stack = stk![V::Map(map), V::int(1)];
        assert_eq!(
            interpret(&[Get(overloads::Get::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::new_option(Some(V::String("foo".to_owned())))]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get(&V::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_big_map() {
        // overlay semantics are tested in big_map module, here we only check
        // the interpreter works
        let mut ctx = Ctx::default();
        ctx.big_map_storage = InMemoryLazyStorage::new();
        let big_map_id = ctx
            .big_map_storage
            .big_map_new(&Type::Int, &Type::String, false)
            .unwrap();
        ctx.big_map_storage
            .big_map_update(
                &big_map_id,
                TypedValue::int(1),
                Some(TypedValue::String("foo".to_owned())),
            )
            .unwrap();
        let content = big_map::BigMapContent::FromId(big_map::BigMapFromId {
            id: big_map_id,
            overlay: BTreeMap::from([(
                TypedValue::int(2),
                Some(TypedValue::String("bar".to_owned())),
            )]),
        });
        let big_map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::String,
        };
        let mut stack = stk![TypedValue::BigMap(big_map), TypedValue::int(1)];
        assert_eq!(
            interpret_one(&Get(overloads::Get::BigMap), &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::new_option(Some(TypedValue::String(
                "foo".to_owned()
            )))]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get(&TypedValue::int(1), 1).unwrap()
        );
    }

    #[test]
    fn get_map_none() {
        let mut ctx = Ctx::default();
        let map = rc_map([
            (V::int(1), V::String("foo".to_owned())),
            (V::int(2), V::String("bar".to_owned())),
        ]);
        let mut stack = stk![V::Map(map), V::int(100500)];
        assert_eq!(
            interpret(&[Get(overloads::Get::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Option(None)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get(&V::int(100500), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    mod get_n {
        use super::*;

        #[track_caller]
        fn check(n: u16, val: TypedValue, field_val: TypedValue) {
            let mut stack = stk![val];
            assert_eq!(
                interpret_one(&GetN(n), &mut Ctx::default(), &mut stack),
                Ok(())
            );
            assert_eq!(stack, stk![field_val])
        }

        #[test]
        fn ok_0() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(0, val.clone(), val);
        }

        #[test]
        fn ok_1() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(1, val, V::int(1));
        }

        #[test]
        fn ok_2() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(2, val, V::new_pair(V::int(3), V::int(4)));
        }

        #[test]
        fn ok_3() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(3, val, V::int(3));
        }

        #[test]
        fn ok_4() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(4, val, V::int(4));
        }
    }

    mod update_n {
        use super::*;

        #[track_caller]
        fn check(n: u16, val: TypedValue, new_val: TypedValue) {
            let mut stack = stk![val, TypedValue::nat(100500)];
            assert_eq!(
                interpret_one(&UpdateN(n), &mut Ctx::default(), &mut stack),
                Ok(())
            );
            assert_eq!(stack, stk![new_val])
        }

        #[test]
        fn ok_0() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(0, val, V::nat(100500));
        }

        #[test]
        fn ok_1() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(
                1,
                val,
                V::new_pair(V::nat(100500), V::new_pair(V::int(3), V::int(4))),
            );
        }

        #[test]
        fn ok_2() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(2, val, V::new_pair(V::int(1), V::nat(100500)));
        }

        #[test]
        fn ok_3() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(
                3,
                val,
                V::new_pair(V::int(1), V::new_pair(V::nat(100500), V::int(4))),
            );
        }

        #[test]
        fn ok_4() {
            let val = V::new_pair(V::int(1), V::new_pair(V::int(3), V::int(4)));
            check(
                4,
                val,
                V::new_pair(V::int(1), V::new_pair(V::int(3), V::nat(100500))),
            );
        }
    }

    #[test]
    fn mem_map() {
        let mut ctx = Ctx::default();
        let map = rc_map([
            (TypedValue::int(1), TypedValue::String("foo".to_owned())),
            (TypedValue::int(2), TypedValue::String("bar".to_owned())),
        ]);
        let mut stack = stk![TypedValue::Map(map), TypedValue::int(1)];
        assert_eq!(
            interpret(&[Mem(overloads::Mem::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(true)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_mem(&TypedValue::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn mem_big_map() {
        let mut ctx = Ctx::default();
        let big_map_id = ctx
            .big_map_storage
            .big_map_new(&Type::Int, &Type::String, false)
            .unwrap();
        ctx.big_map_storage
            .big_map_bulk_update(
                &big_map_id,
                [
                    (
                        TypedValue::int(1),
                        Some(TypedValue::String("foo".to_owned())),
                    ),
                    (
                        TypedValue::int(2),
                        Some(TypedValue::String("bar".to_owned())),
                    ),
                ],
            )
            .unwrap();
        let content = big_map::BigMapContent::FromId(big_map::BigMapFromId {
            id: big_map_id,
            overlay: BTreeMap::new(),
        });
        let big_map = BigMap {
            content,
            key_type: Type::Int,
            value_type: Type::String,
        };
        let mut stack = stk![TypedValue::BigMap(big_map), TypedValue::int(1)];
        assert_eq!(
            interpret_one(&Mem(overloads::Mem::BigMap), &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(true)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_mem(&TypedValue::int(1), 0).unwrap()
        );
    }

    #[test]
    fn mem_map_absent() {
        let mut ctx = Ctx::default();
        let map = rc_map([
            (TypedValue::int(1), TypedValue::String("foo".to_owned())),
            (TypedValue::int(2), TypedValue::String("bar".to_owned())),
        ]);
        let mut stack = stk![TypedValue::Map(map), TypedValue::int(100500)];
        assert_eq!(
            interpret(&[Mem(overloads::Mem::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(false)]);
    }

    #[test]
    fn mem_set() {
        let mut ctx = Ctx::default();
        let set = rc_set([TypedValue::int(1), TypedValue::int(2)]);
        let mut stack = stk![TypedValue::Set(set), TypedValue::int(1)];
        assert_eq!(
            interpret(&[Mem(overloads::Mem::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(true)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::set_mem(&TypedValue::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn mem_set_absent() {
        let mut ctx = Ctx::default();
        let set = rc_set([TypedValue::int(1), TypedValue::int(2)]);
        let mut stack = stk![TypedValue::Set(set), TypedValue::int(100500)];
        assert_eq!(
            interpret(&[Mem(overloads::Mem::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(false)]);
    }

    #[test]
    fn empty_set() {
        let mut ctx = Ctx::default();
        let mut stack = Stack::new();
        assert_eq!(interpret(&[EmptySet], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![TypedValue::Set(BTreeSet::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::EMPTY_SET
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn empty_map() {
        let mut ctx = Ctx::default();
        let mut stack = Stack::new();
        assert_eq!(interpret(&[EmptyMap], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![TypedValue::Map(BTreeMap::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::EMPTY_MAP
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn empty_big_map() {
        let mut ctx = Ctx::default();
        let mut stack = Stack::new();
        assert_eq!(
            interpret_one(&EmptyBigMap(Type::Int, Type::Unit), &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::BigMap(BigMap::empty(Type::Int, Type::Unit))]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap() - interpret_cost::EMPTY_BIG_MAP
        );
    }

    #[test]
    fn update_set_insert() {
        let mut ctx = Ctx::default();
        let set = BTreeSet::new();
        let mut stack = stk![
            TypedValue::Set(set),
            TypedValue::Bool(true),
            TypedValue::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Set(rc_set([TypedValue::int(1)])),]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::set_update(&TypedValue::int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_set_remove() {
        let mut ctx = Ctx::default();
        let set = rc_set([TypedValue::int(1)]);
        let mut stack = stk![
            TypedValue::Set(set),
            TypedValue::Bool(false),
            TypedValue::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Set(BTreeSet::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::set_update(&TypedValue::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_set_insert_when_exists() {
        let mut ctx = Ctx::default();
        let set = rc_set([TypedValue::int(1)]);
        let mut stack = stk![
            TypedValue::Set(set.clone()),
            TypedValue::Bool(true),
            TypedValue::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Set(set)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::set_update(&TypedValue::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_set_remove_when_absent() {
        let mut ctx = Ctx::default();
        let mut stack = stk![
            TypedValue::Set(BTreeSet::new()),
            TypedValue::Bool(false),
            TypedValue::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Set(BTreeSet::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::set_update(&TypedValue::int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_insert() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::new();
        let mut stack = stk![
            V::Map(map),
            V::new_option(Some(V::String("foo".to_owned()))),
            V::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::Map(rc_map([(V::int(1), V::String("foo".to_owned()))])),]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_update(&V::int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_update() {
        let mut ctx = Ctx::default();
        let map = rc_map([(V::int(1), V::String("bar".to_owned()))]);
        let mut stack = stk![
            V::Map(map),
            V::new_option(Some(V::String("foo".to_owned()))),
            V::int(1)
        ];
        assert_eq!(
            interpret(&[Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::Map(rc_map([(V::int(1), V::String("foo".to_owned()))])),]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_update(&V::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_remove() {
        let mut ctx = Ctx::default();
        let map = rc_map([(V::int(1), V::String("bar".to_owned()))]);
        let mut stack = stk![V::Map(map), V::new_option(None), V::int(1)];
        assert_eq!(
            interpret(&[Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Map(BTreeMap::new())]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_update(&V::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn size_string() {
        let mut ctx = Ctx::default();
        let mut stack = stk![TypedValue::String("abc".into())];
        assert_eq!(
            interpret(&[Size(overloads::Size::String)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(3)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::SIZE_STRING
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn size_bytes() {
        let mut ctx = Ctx::default();
        let mut stack = stk![TypedValue::Bytes(b"abc".to_vec())];
        assert_eq!(
            interpret(&[Size(overloads::Size::Bytes)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(3)]);
    }

    #[test]
    fn size_list() {
        let mut ctx = Ctx::default();
        let list = (1..=3).map(TypedValue::nat).collect();
        let mut stack = stk![TypedValue::List(list)];
        assert_eq!(
            interpret(&[Size(overloads::Size::List)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(3)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::SIZE_LIST
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn size_set() {
        let mut ctx = Ctx::default();
        let set = rc_set((1..=3).map(TypedValue::nat));
        let mut stack = stk![TypedValue::Set(set)];
        assert_eq!(
            interpret(&[Size(overloads::Size::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(3)]);
    }

    #[test]
    fn size_map() {
        let mut ctx = Ctx::default();
        let map = rc_map([
            (TypedValue::nat(1), TypedValue::nat(1)),
            (TypedValue::nat(2), TypedValue::nat(2)),
        ]);
        let mut stack = stk![TypedValue::Map(map)];
        assert_eq!(
            interpret(&[Size(overloads::Size::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(2)]);
    }

    #[test]
    fn get_and_update_map_insert() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::new();
        let mut stack = stk![
            V::Map(map),
            V::new_option(Some(V::String("foo".to_owned()))),
            V::int(1)
        ];
        assert_eq!(
            interpret_one(
                &GetAndUpdate(overloads::GetAndUpdate::Map),
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![
                V::Map(rc_map([(V::int(1), V::String("foo".to_owned()))])),
                V::new_option(None),
            ]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get_and_update(&V::int(1), 0).unwrap()
        );
    }

    #[test]
    fn get_and_update_map_update() {
        let mut ctx = Ctx::default();
        let map = rc_map([(V::int(1), V::String("bar".to_owned()))]);
        let mut stack = stk![
            V::Map(map),
            V::new_option(Some(V::String("foo".to_owned()))),
            V::int(1)
        ];
        assert_eq!(
            interpret_one(
                &GetAndUpdate(overloads::GetAndUpdate::Map),
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![
                V::Map(rc_map([(V::int(1), V::String("foo".to_owned()))])),
                V::new_option(Some(V::String("bar".into())))
            ]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get_and_update(&V::int(1), 1).unwrap()
        );
    }

    #[test]
    fn get_and_update_map_remove() {
        let mut ctx = Ctx::default();
        let map = rc_map([(V::int(1), V::String("bar".to_owned()))]);
        let mut stack = stk![V::Map(map), V::new_option(None), V::int(1)];
        assert_eq!(
            interpret_one(
                &GetAndUpdate(overloads::GetAndUpdate::Map),
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![
                V::Map(BTreeMap::new()),
                V::new_option(Some(V::String("bar".into()))),
            ]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::map_get_and_update(&V::int(1), 1).unwrap()
        );
    }

    #[test]
    fn update_big_map() {
        fn check<'a>(
            content: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
            overlay: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
            new_value: Option<TypedValue<'a>>,
            result: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
        ) {
            let mut ctx = Ctx::default();
            let id = ctx
                .big_map_storage
                .big_map_new(&Type::Int, &Type::String, false)
                .unwrap();
            ctx.big_map_storage
                .big_map_bulk_update(&id, content)
                .unwrap();
            let content = big_map::BigMapContent::FromId(big_map::BigMapFromId {
                id: id.clone(),
                overlay: overlay.into_iter().collect(),
            });
            let big_map = BigMap {
                content,
                key_type: Type::Int,
                value_type: Type::String,
            };
            let mut stack = stk![
                TypedValue::BigMap(big_map),
                TypedValue::new_option(new_value),
                TypedValue::int(1)
            ];
            assert_eq!(
                interpret_one(&Update(overloads::Update::BigMap), &mut ctx, &mut stack),
                Ok(())
            );
            assert_eq!(
                stack,
                stk![TypedValue::BigMap(BigMap {
                    content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                        id,
                        overlay: result.into_iter().collect()
                    }),
                    key_type: Type::Int,
                    value_type: Type::String,
                })]
            );
            assert!(ctx.gas().milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        // insert
        check(
            [],
            [],
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // update in content
        check(
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            [],
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // update in overlay
        check(
            [],
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // remove in content
        check(
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            [],
            None,
            [(TypedValue::int(1), None)],
        );
        // remove in overlay
        check(
            [],
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            None,
            [(TypedValue::int(1), None)],
        );
    }

    #[test]
    fn get_and_update_big_map() {
        fn check<'a>(
            content: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
            overlay: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
            old_value: Option<TypedValue<'a>>,
            new_value: Option<TypedValue<'a>>,
            result: impl IntoIterator<Item = (TypedValue<'a>, Option<TypedValue<'a>>)>,
        ) {
            let mut ctx = Ctx::default();
            let id = ctx
                .big_map_storage
                .big_map_new(&Type::Int, &Type::String, false)
                .unwrap();
            ctx.big_map_storage
                .big_map_bulk_update(&id, content)
                .unwrap();
            let content = big_map::BigMapContent::FromId(big_map::BigMapFromId {
                id: id.clone(),
                overlay: overlay.into_iter().collect(),
            });
            let big_map = BigMap {
                content,
                key_type: Type::Int,
                value_type: Type::String,
            };
            let mut stack = stk![
                TypedValue::BigMap(big_map),
                TypedValue::new_option(new_value),
                TypedValue::int(1)
            ];
            assert_eq!(
                interpret_one(
                    &GetAndUpdate(overloads::GetAndUpdate::BigMap),
                    &mut ctx,
                    &mut stack
                ),
                Ok(())
            );
            assert_eq!(
                stack,
                stk![
                    TypedValue::BigMap(BigMap {
                        content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                            id,
                            overlay: result.into_iter().collect()
                        }),
                        key_type: Type::Int,
                        value_type: Type::String,
                    }),
                    TypedValue::new_option(old_value),
                ]
            );
            assert!(ctx.gas().milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        // insert
        check(
            [],
            [],
            None,
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // update in content
        check(
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            [],
            Some(TypedValue::String("bar".into())),
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // update in overlay
        check(
            [],
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            Some(TypedValue::String("bar".into())),
            Some(TypedValue::String("foo".into())),
            [(TypedValue::int(1), Some(TypedValue::String("foo".into())))],
        );
        // remove in content
        check(
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            [],
            Some(TypedValue::String("bar".into())),
            None,
            [(TypedValue::int(1), None)],
        );
        // remove in overlay
        check(
            [],
            [(TypedValue::int(1), Some(TypedValue::String("bar".into())))],
            Some(TypedValue::String("bar".into())),
            None,
            [(TypedValue::int(1), None)],
        );
    }

    #[test]
    fn seq() {
        let mut stack = stk![V::int(1), V::nat(2)];
        assert_eq!(
            interpret(
                &[
                    Seq(vec![Pair]),
                    Seq(vec![Seq(vec![Car])]),
                    Seq(vec![]),
                    Seq(vec![Seq(vec![Seq(vec![])])]),
                ],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::nat(2)]);
    }

    #[test]
    fn add_int_nat() {
        let mut stack = stk![V::nat(123), V::int(456)];
        assert_eq!(
            interpret(
                &[Add(overloads::Add::IntNat)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::int(579)]);
    }

    #[test]
    fn add_int_nat_2() {
        let mut stack = stk![V::nat(123), V::int(-456)];
        assert_eq!(
            interpret(
                &[Add(overloads::Add::IntNat)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::int(-333)]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = stk![V::int(789), V::nat(42)];
        assert_eq!(
            interpret(
                &[Add(overloads::Add::NatInt)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::int(831)]);
    }

    #[test]
    fn add_nat_int_2() {
        let mut stack = stk![V::int(-789), V::nat(42)];
        assert_eq!(
            interpret(
                &[Add(overloads::Add::NatInt)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::int(-747)]);
    }

    #[test]
    fn concat_two_string() {
        let mut stack = stk![
            TypedValue::String("def".into()),
            TypedValue::String("abc".into()),
        ];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::TwoStrings)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::String("abcdef".into())]);
    }

    #[test]
    fn concat_two_bytes() {
        let mut stack = stk![
            TypedValue::Bytes(b"def".to_vec()),
            TypedValue::Bytes(b"abc".to_vec()),
        ];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::TwoBytes)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bytes(b"abcdef".to_vec())]);
    }

    #[test]
    fn concat_list_of_3_strings() {
        let mut stack = stk![TypedValue::List(
            vec![
                TypedValue::String("a".into()),
                TypedValue::String("b".into()),
                TypedValue::String("c".into()),
            ]
            .into()
        )];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::ListOfStrings)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::String("abc".into())]);
    }

    #[test]
    fn concat_list_of_1_strings() {
        let mut stack = stk![TypedValue::List(
            vec![TypedValue::String("x".into()),].into()
        ),];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::ListOfStrings)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::String("x".into())]);
    }

    #[test]
    fn concat_list_of_0_strings() {
        let mut stack = stk![TypedValue::List(MichelsonList::new()),];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::ListOfStrings)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::String("".into())]);
    }

    #[test]
    fn concat_list_of_3_bytes() {
        let mut stack = stk![TypedValue::List(
            vec![
                TypedValue::Bytes(b"a".to_vec()),
                TypedValue::Bytes(b"b".to_vec()),
                TypedValue::Bytes(b"c".to_vec()),
            ]
            .into()
        )];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::ListOfBytes)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bytes(b"abc".to_vec())]);
    }

    #[test]
    fn concat_list_of_0_bytes() {
        let mut stack = stk![TypedValue::List(MichelsonList::new())];
        assert_eq!(
            interpret(
                &[Concat(overloads::Concat::ListOfBytes)],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bytes(b"".to_vec())]);
    }

    #[test]
    fn trigger_unreachable_state() {
        let mut stack = Stack::new();
        assert!(matches!(
            interpret(
                &[Add(overloads::Add::NatInt)],
                &mut Ctx::default(),
                &mut stack,
            ),
            Err(InterpretError::InternalError(
                InterpretInvariant::EmptyValueStackPop
            ))
        ));
    }

    #[test]
    fn chain_id_instr() {
        let chain_id = super::ChainId::from_base58_check("NetXynUjJNZm7wi").unwrap();
        let ctx = &mut Ctx::default();
        ctx.chain_id = chain_id.clone();
        let start_milligas = ctx.gas().milligas().unwrap();
        let mut stk = Stack::new();
        assert_eq!(interpret(&[Instruction::ChainId], ctx, &mut stk), Ok(()));
        assert_eq!(stk, stk![V::ChainId(chain_id)]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::CHAIN_ID + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn pack_instr() {
        let stack = &mut stk![TypedValue::new_pair(TypedValue::int(12), TypedValue::Unit)];
        assert_eq!(interpret(&[Pack], &mut Ctx::default(), stack), Ok(()));
        assert_eq!(
            stack,
            &stk![V::Bytes(hex::decode("050707000c030b").unwrap())]
        );
    }

    #[test]
    fn pack_charges_serialization_once() {
        // Regression for the PACK ~2x overcharge: PACK bills only the
        // unparsing pass (~10 mg/byte for a string), not unparsing +
        // serialization (~20 mg/byte). Comparing two payloads isolates the
        // per-byte slope from the fixed node/instruction overhead.
        fn pack_gas(len: usize) -> u32 {
            let mut ctx = Ctx::default();
            let mut stack = stk![V::String("a".repeat(len))];
            interpret_one(&Pack, &mut ctx, &mut stack).unwrap();
            Ctx::default().gas.milligas().unwrap() - ctx.gas().milligas().unwrap()
        }
        let (n1, n2) = (100usize, 1100usize);
        assert_eq!(pack_gas(n2) - pack_gas(n1), 10 * (n2 - n1) as u32);
    }

    #[test]
    fn self_instr() {
        let mut stk = Stack::new();
        let ctx = &mut Ctx::default();
        ctx.self_address = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT".try_into().unwrap();
        assert_eq!(
            interpret(&[ISelf(Entrypoint::default())], ctx, &mut stk),
            Ok(())
        );
        assert_eq!(
            stk,
            stk![V::Contract(
                "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT".try_into().unwrap()
            )]
        );
    }

    #[test]
    fn transfer_tokens() {
        let tt = super::TransferTokens {
            param: TypedValue::nat(42),
            destination_address: addr::Address::try_from("tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw")
                .unwrap(),
            amount: 0,
        };
        let stk = &mut stk![
            V::Contract(tt.destination_address.clone()),
            V::Mutez(tt.amount),
            tt.param.clone()
        ];
        let ctx = &mut Ctx::default();
        ctx.set_operation_counter(100);
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[TransferTokens], ctx, stk), Ok(()));
        assert_eq!(
            stk,
            &stk![V::new_operation(Operation::TransferTokens(tt), 101)]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::TRANSFER_TOKENS + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn set_delegate() {
        use Instruction as I;
        let sd = super::SetDelegate(Some(
            PublicKeyHash::try_from("tz3h4mjmMieZKSaSBWBC7XmeL6JQ3hucFDcP").unwrap(),
        ));
        let stk = &mut stk![V::new_option(Some(V::KeyHash(sd.0.clone().unwrap())))];
        let ctx = &mut Ctx::default();
        ctx.set_operation_counter(100);
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[I::SetDelegate], ctx, stk), Ok(()));
        assert_eq!(
            stk,
            &stk![V::new_operation(Operation::SetDelegate(sd), 101)]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::SET_DELEGATE + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn test_operation_counter() {
        // Here we run two instructions that each generates an operation
        // and check that the counter in the generated operations are incremented
        // as expected.
        use Instruction as I;
        let sd = super::SetDelegate(Some(
            PublicKeyHash::try_from("tz3h4mjmMieZKSaSBWBC7XmeL6JQ3hucFDcP").unwrap(),
        ));
        let stk = &mut stk![
            V::new_option(Some(V::KeyHash(sd.0.clone().unwrap()))),
            V::new_option(Some(V::KeyHash(sd.0.clone().unwrap())))
        ];
        let ctx = &mut Ctx::default();
        ctx.set_operation_counter(100);
        assert_eq!(
            interpret(&[I::SetDelegate, I::Swap, I::SetDelegate], ctx, stk),
            Ok(())
        );
        assert_eq!(
            stk,
            &stk![
                V::new_operation(Operation::SetDelegate(sd.clone()), 101),
                V::new_operation(Operation::SetDelegate(sd), 102)
            ]
        );
    }

    #[test]
    fn self_instr_ep() {
        let mut stk = Stack::new();
        let ctx = &mut Ctx::default();
        ctx.self_address = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT".try_into().unwrap();
        assert_eq!(
            interpret(
                &[ISelf(Entrypoint::try_from("foo").unwrap())],
                ctx,
                &mut stk
            ),
            Ok(())
        );
        assert_eq!(
            stk,
            stk![V::Contract(
                "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT%foo"
                    .try_into()
                    .unwrap()
            )]
        );
    }

    // tuples of (key, message, signature, is_valid).
    // signatures are produced via `octez-client sign bytes <msg> for <key>`
    fn signature_fixtures() -> Vec<(PublicKey, &'static [u8], Signature, bool)> {
        vec!
          [ ( PublicKey::from_b58check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"\0"
            , Signature::from_base58_check("BLsigAmLKnuw12tethjMmotFPaQ6u4XCKrVk6c15dkRXKkjDDjHywbhS3nd4rBT31yrCvvQrS2HntWhDRu7sX8Vvek53zBUwQHqfcHRiVKVj1ehq8CBYs1Z7XW2rkL2XkVNHua4cnvxY7F").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("BLsigBR1jcWq3w6yAFyDn2X6fxBGjB1E7YoywyUhpvHfisPpCgeMQfJHXoj2YW1BZoujsuZRXdU1BTQjWwqT3xAZRGVcsXovVAgEXbMuuaKLSYYbbMQM92gDDT1UCCRZ1RFvutdavYoumy").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("BLpk1wfC8yTMJKYT3Q9YfGtjGiw3qpjbkoPhjoGVys7PjHSochLNxnMW7s4EUs37gvcTPZKDSoWi").unwrap()
            , b"kot"
            , Signature::from_base58_check("BLsigBR1jcWq3w6yAFyDn2X6fxBGjB1E7YoywyUhpvHfisPpCgeMQfJHXoj2YW1BZoujsuZRXdU1BTQjWwqT3xAZRGVcsXovVAgEXbMuuaKLSYYbbMQM92gDDT1UCCRZ1RFvutdavYoumy").unwrap()
            , false
            )
          , ( PublicKey::from_b58check("edpkuwTWKgQNnhR5v17H2DYHbfcxYepARyrPGbf1tbMoGQAj8Ljr3V").unwrap()
            , b"\0"
            , Signature::from_base58_check("edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("edsigtj8LhbJ2B3qhZvqzA49raG65dydFcWZW9b9L7ntF3bb29zxaBFFL8SM1jeBUY66hG122znyVA4wpzLdwxcNZwSK3Szu7iD").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("edpkupH22qrz1sNQt5HSvWfRJFfyJ9dhNbZLptE6GR4JbMoBcACZZH").unwrap()
            , b"kot"
            , Signature::from_base58_check("edsigtrs8bK7vNfiR4Kd9dWasVa1bAWaQSu2ipnmLGZuwQa8ktCEMYVKqbWsbJ7zTS8dgYT9tiSUKorWCPFHosL5zPsiDwBQ6vb").unwrap()
            , false
            )
          , ( PublicKey::from_b58check("sppk7cdA7Afj8MvuBFrP6KsTLfbM5DtH9GwYaRZwCf5tBVCz6UKGQFR").unwrap()
            , b"\0"
            , Signature::from_base58_check("spsig1Ng2bs4PXCbjaFGuojk9K5Pt3CkfbUZyHLLrBxHSmTqrUUxQggi4yJBit3Ljqnqr61UpdTewTLiu4schSCfZvaRwu412oZ").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD").unwrap()
            , b"\0\0"
            , Signature::from_base58_check("spsig1aP7D9oheiraNuM1NgziMPSPKS1F9kSWyFqkE8WigaeU5Uzb3LwY34F7Y7RsF6sY5ZfUda1NWdrC5V4KEfm9jeU1eniHmy").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD").unwrap()
            , b"kot"
            , Signature::from_base58_check("spsig1PJ9LG9ovbpVJ3CucFWL7iBaQZjqEWMvppgLjYiiSwzcxpuUqHr2BUVZDUwkmZKzMNDWJdgtyhYiicz197TbhS4LPpnxDY").unwrap()
            , false
            )
          , ( PublicKey::from_b58check("p2pk66qfVMXhFJWhtFDCT6F3JUM3M1iQpfWe4nPZKWcsqsKQtXXHFkQ").unwrap()
            , b"\0"
            , Signature::from_base58_check("p2sigv6HrN6xB5gQDnmKLC2P3ynwiPn4zfUj7CcZD1cepfFzX7xBDWFQu9uoKWbEzVgxCQxrE1J5X6FGYwF2dpoYcjpdPCBhuD").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("p2pk64bybDUtSjSQnsexpzhedhBo4vkoRX4tWfQQbBxKbA58wJqKkT2").unwrap()
            , b"\x0A"
            , Signature::from_base58_check("p2siggzjojhabur7zZvmNnhkhnU3nYA1ZUR9JSas57RVhNdAmQk6y3hns3F2zPBGsC964PFAE2HC3fbPkcqpVFbjoQQq9dFiZg").unwrap()
            , true
            )
          , ( PublicKey::from_b58check("p2pk64bybDUtSjSQnsexpzhedhBo4vkoRX4tWfQQbBxKbA58wJqKkT2").unwrap()
            , b"kot"
            , Signature::from_base58_check("p2siggzjojhabur7zZvmNnhkhnU3nYA1ZUR9JSas57RVhNdAmQk6y3hns3F2zPBGsC964PFAE2HC3fbPkcqpVFbjoQQq9dFiZg").unwrap()
            , false
            )
            // binary representation
            // edpk
          , ( PublicKey::nom_read_exact(&hex::decode("00aad3f16293766169f7db278c5e0e9db4fb82ffe1cbcc35258059617dc0fec082").unwrap()).unwrap()
            , b"\0"
            , Signature::try_from(hex::decode("91ac1e7fd668854fc7a40feec4034e42c06c068cce10622c607fda232db34c8cf5d8da83098dd891cd4cb4299b3fa0352ae323ad99b24541e54b91888fdc8201").unwrap()).unwrap()
            , true
            )
            // sppk
          , ( PublicKey::nom_read_exact(&hex::decode("0103b524d0184276467c848ac13557fb0ff8bec5907960f72683f22af430503edfc1").unwrap()).unwrap()
            , b"\0"
            , Signature::try_from(hex::decode("80e4e72ffecf72953789625b1125e9f45f432c14e53a01ec68a1e1b77d60cfe96a97443733ba0f7f42db3a56d7a433df2b4fc0035c05ab92d062f33c5bab0244").unwrap()).unwrap()
            , true
            )
            // p2pk
          , ( PublicKey::nom_read_exact(&hex::decode("0202041e5cb7fb3d7bc6fb7b9e94790919a9e76ccc372e6cc9cae925027c08ff95f3").unwrap()).unwrap()
            , b"\x0A"
            , Signature::try_from(hex::decode("12d25210bb02998516bf6a776e1cd55a06c5fbe3c21afbeef29b99d96305e43263c75a4449906e0f2d79ecc973fff9ce7f8c43fee40b04d07c191f00ee176175").unwrap()).unwrap()
            , true
            )
            // BLpk
          , ( PublicKey::nom_read_exact(&hex::decode("03ade3c5ec9e1be3dd08eb355f6e23b8e162b90f563fa5cf0b0299fb9f3aa29218483ead20efa8b350559be88bd99cea6c").unwrap()).unwrap()
            , b"\0"
            , Signature::try_from(hex::decode("a065340a9c902829a4d77312c3327b558d310a37305049fc144021ea837325f994e270537a03acfdf9ef276530366b7c1629cb4d71a2b5967b582bfcfd280becb8c918463eb0e5dd0165702a2494b8856baee31e0e7b9f9e5ae5b4af980e88ee").unwrap()).unwrap()
            , true
            )
          ]
    }

    #[test]
    fn check_signature() {
        for (key, msg, sig, res) in signature_fixtures() {
            let mut stack = stk![V::Bytes(msg.to_vec()), V::Signature(sig), V::Key(key)];
            assert_eq!(
                interpret_one(&CheckSignature, &mut Ctx::default(), &mut stack),
                Ok(())
            );
            assert_eq!(stack, stk![V::Bool(res)])
        }
    }

    #[test]
    fn address_instr() {
        let address: addr::Address = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT%some_entrypoint"
            .try_into()
            .unwrap();
        let contract = V::Contract(address.clone());
        let stk = &mut stk![contract];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret(&[Address], ctx, stk), Ok(()));
        assert_eq!(stk, &stk![V::Address(address)]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::ADDRESS
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn slice_instr_string() {
        fn test(str: &str, offset: u64, length: u64, expected: Option<&str>) {
            let stk = &mut stk![V::String(str.to_string()), V::nat(length), V::nat(offset)];
            let ctx = &mut Ctx::default();
            let expected = expected.map(|str| V::String(str.to_string()));
            assert_eq!(
                interpret(&[Slice(overloads::Slice::String)], ctx, stk),
                Ok(())
            );
            assert_eq!(stk, &stk![V::new_option(expected)]);
        }

        test("foobar", 0, 0, Some(""));
        test("foobar", 0, 3, Some("foo"));
        test("foobar", 3, 3, Some("bar"));
        test("foobar", 3, 4, None);
        test("foobar", 6, 0, None);
        test("foobar", 7, 0, None);
        test("", 0, 0, None);
    }

    #[test]
    fn slice_instr_bytes() {
        fn test(bytes: &[u8], offset: u64, length: u64, expected: Option<&[u8]>) {
            let stk = &mut stk![V::Bytes(bytes.to_vec()), V::nat(length), V::nat(offset)];
            let ctx = &mut Ctx::default();
            let expected = expected.map(|bytes| V::Bytes(bytes.to_vec()));
            assert_eq!(
                interpret(&[Slice(overloads::Slice::Bytes)], ctx, stk),
                Ok(())
            );
            assert_eq!(stk, &stk![V::new_option(expected)]);
        }

        test(b"foobar", 0, 0, Some(b""));
        test(b"foobar", 0, 3, Some(b"foo"));
        test(b"foobar", 3, 3, Some(b"bar"));
        test(b"foobar", 3, 4, None);
        test(b"foobar", 6, 0, None);
        test(b"foobar", 7, 0, None);
        test(b"", 0, 0, None);
    }

    #[test]
    fn left() {
        let mut stack = stk![V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Instruction::Left], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_or(or::Or::Left(V::nat(10)))]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::LEFT
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn right() {
        let mut stack = stk![V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Instruction::Right], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_or(or::Or::Right(V::nat(10)))]);
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Gas::default().milligas().unwrap()
                - interpret_cost::RIGHT
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn ticket() {
        let mut stack = stk![V::nat(100), TypedValue::Unit];
        let mut ctx = Ctx::default();
        let expected_ticket = V::new_ticket(crate::ast::Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content_type: Type::Unit,
            content: V::Unit,
        });

        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(
            interpret(&[Ticket(Type::Unit)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::new_option(Some(expected_ticket))]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::TICKET + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn read_ticket() {
        use crate::ast::Ticket;

        let mut ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content_type: Type::Unit,
            content: V::Unit,
        };
        let ticket_val = V::new_ticket(ticket.clone());
        let mut stack = stk![ticket_val.clone()];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[ReadTicket], &mut ctx, &mut stack), Ok(()));
        let ticketer_address = super::Address {
            hash: ticket.ticketer,
            entrypoint: Entrypoint::default(),
        };
        assert_eq!(
            stack,
            stk![
                ticket_val,
                V::new_pair(
                    V::Address(ticketer_address),
                    V::new_pair(V::Unit, V::nat(100))
                ),
            ]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::READ_TICKET + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn exec() {
        let mut stack = stk![
            TypedValue::Lambda(Closure::Lambda(Lambda::Lambda {
                micheline_code: Micheline::Seq(&[]), // ignored by the interpreter
                code: vec![Unpair, Add(overloads::Add::IntNat)].into(),
            })),
            TypedValue::new_pair(TypedValue::int(1), TypedValue::nat(5))
        ];
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::int(6)]);
    }

    #[test]
    fn exec_rec_1() {
        let lam = Closure::Lambda(Lambda::LambdaRec {
            in_ty: Type::new_pair(Type::Bool, Type::Nat),
            out_ty: Type::Nat,
            micheline_code: Micheline::Seq(&[]),
            code: vec![
                Unpair,
                If(
                    vec![
                        Dup(None),
                        Add(overloads::Add::NatNat),
                        Push(Rc::new(TypedValue::Bool(false))),
                        Pair,
                        Exec,
                    ],
                    vec![Swap, Drop(None)],
                ),
            ]
            .into(),
        });
        let mut stack = stk![
            TypedValue::Lambda(lam.clone()),
            TypedValue::new_pair(TypedValue::Bool(true), TypedValue::nat(5))
        ];
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(10)]);
    }

    #[test]
    fn exec_rec_2() {
        let lam = Closure::Lambda(Lambda::LambdaRec {
            in_ty: Type::new_pair(Type::Bool, Type::Nat),
            out_ty: Type::Nat,
            micheline_code: Micheline::Seq(&[]),
            code: vec![
                Unpair,
                If(
                    vec![
                        Dup(None),
                        Add(overloads::Add::NatNat),
                        Push(Rc::new(TypedValue::Bool(false))),
                        Pair,
                        Exec,
                    ],
                    vec![Swap, Drop(None)],
                ),
            ]
            .into(),
        });
        let mut stack = stk![
            TypedValue::Lambda(lam.clone()),
            TypedValue::new_pair(TypedValue::Bool(false), TypedValue::nat(5))
        ];
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(5)]);
    }

    #[test]
    fn hash_key() {
        // specific key-to-hash correspondence is checked in michelson_key
        // tests, here we only want to check interpreter works, so testing only
        // one particular key
        let mut stack = stk![V::Key(
            "edpktxDQJUF9AqUegbhhD9zJWBCPRJ3PtewuwiuAxrnaQbRmdi2tW1"
                .try_into()
                .unwrap()
        )];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&HashKey, ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::KeyHash(
                "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw".try_into().unwrap()
            )]
        );
        assert_eq!(
            ctx.gas().milligas().unwrap(),
            Ctx::default().gas.milligas().unwrap() - interpret_cost::HASH_KEY
        );
    }

    #[test]
    fn apply_exec() {
        let lam = Closure::Lambda(Lambda::Lambda {
            micheline_code: Micheline::Seq(&[]),
            code: vec![Unpair, Add(overloads::Add::IntNat)].into(),
        });
        let mut stack = stk![TypedValue::Lambda(lam.clone()), TypedValue::int(1)];
        assert_eq!(
            interpret_one(
                &Apply { arg_ty: Type::Int },
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::Lambda(Closure::Apply {
                arg_ty: Type::Int,
                arg_val: Box::new(TypedValue::int(1)),
                closure: Box::new(lam),
            })]
        );
        stack.push(TypedValue::nat(5));
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::int(6)]);
    }

    #[test]
    fn apply_exec_rec_1() {
        let lam = Closure::Lambda(Lambda::LambdaRec {
            in_ty: Type::new_pair(Type::Bool, Type::Nat),
            out_ty: Type::Nat,
            micheline_code: Micheline::Seq(&[]),
            code: vec![
                Unpair,
                If(
                    vec![
                        Dup(None),
                        Add(overloads::Add::NatNat),
                        Push(Rc::new(TypedValue::Bool(false))),
                        Pair,
                        Exec,
                    ],
                    vec![Dip(None, vec![Drop(None)])],
                ),
            ]
            .into(),
        });
        let mut stack = stk![TypedValue::Lambda(lam.clone()), TypedValue::Bool(true)];
        assert_eq!(
            interpret_one(
                &Apply { arg_ty: Type::Bool },
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::Lambda(Closure::Apply {
                arg_ty: Type::Bool,
                arg_val: Box::new(TypedValue::Bool(true)),
                closure: Box::new(lam),
            })]
        );
        stack.push(TypedValue::nat(5));
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(10)]);
    }

    #[test]
    fn apply_exec_rec_2() {
        let lam = Closure::Lambda(Lambda::LambdaRec {
            in_ty: Type::new_pair(Type::Bool, Type::Nat),
            out_ty: Type::Nat,
            micheline_code: Micheline::Seq(&[]),
            code: vec![
                Unpair,
                If(
                    vec![
                        Dup(None),
                        Add(overloads::Add::NatNat),
                        Push(Rc::new(TypedValue::Bool(false))),
                        Pair,
                        Exec,
                    ],
                    vec![Dip(None, vec![Drop(None)])],
                ),
            ]
            .into(),
        });
        let mut stack = stk![TypedValue::Lambda(lam.clone()), TypedValue::Bool(false)];
        assert_eq!(
            interpret_one(
                &Apply { arg_ty: Type::Bool },
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::Lambda(Closure::Apply {
                arg_ty: Type::Bool,
                arg_val: Box::new(TypedValue::Bool(false)),
                closure: Box::new(lam),
            })]
        );
        stack.push(TypedValue::nat(5));
        assert_eq!(
            interpret(&[Exec], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::nat(5)]);
    }

    #[test]
    fn split_ticket() {
        use crate::ast::Ticket;

        // Test successful execution
        let mut ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content_type: Type::Unit,
            content: V::Unit,
        };
        let ticket_val = V::new_ticket(ticket.clone());
        let mut stack = stk![V::new_pair(V::nat(20), V::nat(80)), ticket_val.clone(),];

        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[SplitTicket], &mut ctx, &mut stack), Ok(()));

        let ticket_exp_left = Ticket {
            amount: 20u32.into(),
            ..ticket.clone()
        };

        let ticket_exp_right = Ticket {
            amount: 80u32.into(),
            ..ticket
        };
        assert_eq!(
            stack,
            stk![V::new_option(Some(V::new_pair(
                V::new_ticket(ticket_exp_left.clone()),
                V::new_ticket(ticket_exp_right.clone()),
            ))),]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::split_ticket(&ticket_exp_left.amount, &ticket_exp_right.amount)
                .unwrap()
                + interpret_cost::INTERPRET_RET
        );

        // When one of the amounts is zero
        let mut stack = stk![V::new_pair(V::nat(0), V::nat(100)), ticket_val.clone(),];
        assert_eq!(interpret_one(&SplitTicket, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None),]);

        // When one of the amounts is zero
        let mut stack = stk![V::new_pair(V::nat(100), V::nat(0)), ticket_val.clone(),];
        assert_eq!(interpret_one(&SplitTicket, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None),]);

        // When sum of the amounts is larger than original ticekt
        let mut stack = stk![V::new_pair(V::nat(70), V::nat(80)), ticket_val];
        assert_eq!(interpret_one(&SplitTicket, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None),]);
    }

    #[test]
    fn join_tickets() {
        use crate::ast::Ticket;

        // Test successful execution
        let mut ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content_type: Type::Nat,
            content: V::nat(10),
        };
        let ticket_left = V::new_ticket(ticket.clone());
        let ticket_right_ = Ticket {
            amount: 20u32.into(),
            ..ticket.clone()
        };
        let ticket_right = V::new_ticket(ticket_right_.clone());
        let mut stack = stk![V::new_pair(ticket_left, ticket_right),];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[JoinTickets], &mut ctx, &mut stack), Ok(()));
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::join_tickets(&ticket, &ticket_right_).unwrap()
                + interpret_cost::INTERPRET_RET
        );

        let ticket_result = Ticket {
            amount: 120u32.into(),
            ..ticket
        };

        assert_eq!(
            stack,
            stk![V::new_option(Some(V::new_ticket(ticket_result))),]
        );

        // When content is different
        let mut ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content_type: Type::Nat,
            content: V::nat(10),
        };
        let ticket_left = V::new_ticket(ticket.clone());
        let ticket_right = V::new_ticket(Ticket {
            amount: 20u32.into(),
            content: V::nat(20),
            ..ticket
        });
        let mut stack = stk![V::new_pair(ticket_left, ticket_right),];
        assert_eq!(interpret_one(&JoinTickets, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None),]);

        // When Ticketer is different
        use crate::interpreter::AddressHash;
        let mut ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: AddressHash::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap(),
            amount: 100u32.into(),
            content_type: Type::Nat,
            content: V::nat(10),
        };
        let ticket_left = V::new_ticket(ticket.clone());
        let ticket_right = V::new_ticket(Ticket {
            amount: 20u32.into(),
            ticketer: AddressHash::try_from("KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT").unwrap(),
            ..ticket
        });
        let mut stack = stk![V::new_pair(ticket_left, ticket_right),];
        assert_eq!(interpret_one(&JoinTickets, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None),]);
    }

    mod byte_hashes {
        use super::*;

        #[track_caller]
        fn test(i: Instruction, input: &str, expected_output: &str) {
            let input = hex::decode(input).unwrap();
            let mut stack = stk![V::Bytes(input)];
            assert_eq!(interpret_one(&i, &mut Ctx::default(), &mut stack), Ok(()));
            assert_eq!(stack.len(), 1);
            let V::Bytes(out) = stack.get(0).unwrap().as_ref() else {
                panic!("expected V::Bytes on stack");
            };
            assert_eq!(out, &hex::decode(expected_output).unwrap());
        }
        macro_rules! test {
            ($i:ident; $($input:expr => $output:expr);* $(;)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $i() {
                    $(test(Instruction::$i, $input, $output);)*
                }
            };
        }
        test!(
            Blake2b;
            "00" => "03170a2e7597b7b7e3d84c05391d139a62b157e78786d8c082f29dcf4c111314";
            "deadbeef" => "f3e925002fed7cc0ded46842569eb5c90c910c091d8d04a1bdf96e0db719fd91";
        );
        test!(
            Keccak;
            "00" => "bc36789e7a1e281436464229828f817d6612f7b477d66591ff96a9e064bcc98a";
            "deadbeef" => "d4fd4e189132273036449fc9e11198c739161b4c0116a9a2dccdfa1c492006f1";
        );
        test!(
            Sha256;
            "00" => "6e340b9cffb37a989ca544e6bb780a2c78901d3fb33738768511a30617afa01d";
            "deadbeef" => "5f78c33274e43fa9de5659265c1d917e25c03722dcb0b8d27db8d5feaa813953";
        );
        test!(
            Sha3;
            "00" => "5d53469f20fef4f8eab52b88044ede69c77a6a68a60728609fc4a65ff531e7d0";
            "deadbeef" => "352b82608dad6c7ac3dd665bc2666e5d97803cb13f23a1109e2105e93f42c448";
        );
        test!(
            Sha512;
            "00" => "b8244d028981d693af7b456af8efa4cad63d282e19ff14942c246e50d9351d22704a802a71c3580b6370de4ceb293c324a8423342557d4e5c38438f0e36910ee";
            "deadbeef" => "1284b2d521535196f22175d5f558104220a6ad7680e78b49fa6f20e57ea7b185d71ec1edb137e70eba528dedb141f5d2f8bb53149d262932b27cf41fed96aa7f";
        );
    }

    #[test]
    fn balance() {
        let mut ctx = Ctx::default();
        ctx.balance = 70;
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[Balance], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Mutez(70),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::BALANCE + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn contract() {
        use crate::ast::michelson_address::Address;
        use crate::gas::tc_cost;

        #[track_caller]
        fn run_contract_test<'a>(
            address: Address,
            opt_entrypoints: Option<&[(Entrypoint, Type)]>,
            mut start_stack: IStack<'a>,
            stack_expect: IStack<'a>,
            instr: Instruction<'a>,
            opt_exp_gas: Option<u32>,
        ) {
            let mut ctx = Ctx::default();
            if let Some(e) = opt_entrypoints {
                ctx.set_known_contracts([(address.hash, HashMap::from_iter(Vec::from(e)))]);
            }

            let start_milligas = ctx.gas().milligas().unwrap();
            assert_eq!(interpret(&[instr], &mut ctx, &mut start_stack), Ok(()));
            assert_eq!(start_stack, stack_expect);
            if let Some(exp_gas) = opt_exp_gas {
                assert_eq!(start_milligas - ctx.gas().milligas().unwrap(), exp_gas);
            }
        }

        // Contract calls default entrypoint of type Unit
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::default(), Type::Unit)]),
            stk![V::Address(addr.clone())],
            stk![TypedValue::new_option(Some(V::Contract(addr)))],
            Contract(Type::Unit, Entrypoint::default()),
            Some(
                tc_cost::ty_eq(Type::Int.size_for_gas(), Type::Int.size_for_gas()).unwrap()
                    + interpret_cost::CONTRACT
                    + interpret_cost::INTERPRET_RET,
            ),
        );

        // Contract calls default entrypoint of type other than Unit
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::default(), Type::Int)]),
            stk![V::Address(addr.clone())],
            stk![TypedValue::new_option(Some(V::Contract(addr)))],
            Contract(Type::Int, Entrypoint::default()),
            Some(
                tc_cost::ty_eq(Type::Int.size_for_gas(), Type::Int.size_for_gas()).unwrap()
                    + interpret_cost::CONTRACT
                    + interpret_cost::INTERPRET_RET,
            ),
        );

        // When there is entrypoint embedded in address but not specified
        // in instruction.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::try_from("foo").unwrap(), Type::Int)]),
            stk![V::Address(addr.clone())],
            stk![TypedValue::new_option(Some(V::Contract(addr)))],
            Contract(Type::Int, Entrypoint::default()),
            Some(
                tc_cost::ty_eq(Type::Int.size_for_gas(), Type::Int.size_for_gas()).unwrap()
                    + interpret_cost::CONTRACT
                    + interpret_cost::INTERPRET_RET,
            ),
        );

        // When there is no entrypoint embedded in address but one was specified
        // in instruction.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let expected_address: Address = Address {
            entrypoint: Entrypoint::try_from("foo").unwrap(),
            ..addr.clone()
        };
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::try_from("foo").unwrap(), Type::Int)]),
            stk![V::Address(addr)],
            stk![TypedValue::new_option(Some(V::Contract(expected_address)))],
            Contract(Type::Int, Entrypoint::try_from("foo").unwrap()),
            Some(
                tc_cost::ty_eq(Type::Int.size_for_gas(), Type::Int.size_for_gas()).unwrap()
                    + interpret_cost::CONTRACT
                    + interpret_cost::INTERPRET_RET,
            ),
        );

        // When there is entrypoint embedded in address and also one was specified
        // in instruction and they does not match.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::try_from("foo").unwrap(), Type::Int)]),
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Int, Entrypoint::try_from("foo").unwrap()),
            None,
        );

        // When there is no contract at the address.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar").unwrap();
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Int, Entrypoint::default()),
            None,
        );

        // When there is a contract at the address but the parameter type is different.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[(Entrypoint::default(), Type::String)]),
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Int, Entrypoint::default()),
            None,
        );

        // When there is a contract at the address but the parameter type of the entrypoint is different.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_contract_test(
            addr.clone(),
            Some(&[
                (Entrypoint::try_from("bar").unwrap(), Type::Int),
                (Entrypoint::try_from("foo").unwrap(), Type::String),
            ]),
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Int, Entrypoint::try_from("foo").unwrap()),
            None,
        );

        // When there is a contract at the address and the parameter type of the entrypoint is
        // correct.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let expected_address: Address = Address {
            entrypoint: Entrypoint::try_from("foo").unwrap(),
            ..addr.clone()
        };
        run_contract_test(
            addr.clone(),
            Some(&[
                (Entrypoint::try_from("bar").unwrap(), Type::String),
                (Entrypoint::try_from("foo").unwrap(), Type::Int),
            ]),
            stk![V::Address(addr)],
            stk![TypedValue::new_option(Some(V::Contract(expected_address)))],
            Contract(Type::Int, Entrypoint::try_from("foo").unwrap()),
            None,
        );

        // When the address is implicit.
        let addr: Address = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        let expected_address: Address = Address {
            entrypoint: Entrypoint::default(),
            ..addr.clone()
        };
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(Some(V::Contract(expected_address)))],
            Contract(Type::Unit, Entrypoint::default()),
            None,
        );

        // When the address is implicit.and contract type is Ticket
        let addr: Address = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        let expected_address: Address = Address {
            entrypoint: Entrypoint::default(),
            ..addr.clone()
        };
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(Some(V::Contract(expected_address)))],
            Contract(Type::new_ticket(Type::Unit), Entrypoint::default()),
            None,
        );

        // When the address is implicit and the Contract calls type is something other then unit
        let addr: Address = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Int, Entrypoint::default()),
            None,
        );

        // When the address is implicit and contains an entrypoint
        let addr: Address = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6%foo").unwrap();
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Unit, Entrypoint::default()),
            None,
        );

        // When the address is implicit and contract call contains an entrypoint
        let addr: Address = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6%foo").unwrap();
        run_contract_test(
            addr.clone(),
            None,
            stk![V::Address(addr)],
            stk![TypedValue::new_option(None)],
            Contract(Type::Unit, Entrypoint::try_from("foo").unwrap()),
            None,
        );
    }

    #[test]
    fn level() {
        let mut ctx = Ctx::default();
        ctx.level = 70u32.into();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[Level], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(70),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::LEVEL + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn min_block_time() {
        let mut ctx = Ctx::default();
        ctx.min_block_time = 70u32.into();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[MinBlockTime], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(70),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::MIN_BLOCK_TIME + interpret_cost::INTERPRET_RET
        );
    }

    // Regression for !21984 review note 3376564679: on a view-body error
    // the iterative driver `?`-returns before the `AfterView` frame can
    // restore the saved view context, leaving the view's
    // self_address/sender/amount/balance installed. `interpret` now calls
    // `restore_pending_view_context` on the error path. This exercises that
    // helper directly: it must reinstall the *outermost* pending view's
    // saved context (matching the recursive interpreter's unwind), not the
    // innermost.
    #[test]
    fn view_context_restored_on_error_unwind() {
        let mut ctx = Ctx::default();
        let orig_self = ctx.self_address.clone();
        let orig_sender = ctx.sender.clone();
        let orig_amount = ctx.amount;
        let orig_balance = ctx.balance;

        let inner: AddressHash = "KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye".try_into().unwrap();

        // Frames as they would look mid-unwind from a nested view: the
        // outermost AfterView (lowest index) carries the original context;
        // an inner AfterView carries an intermediate context. The restore
        // must select the outermost.
        let frames = vec![
            super::InterpFrame::AfterView {
                saved_self_address: orig_self.clone(),
                saved_sender: orig_sender.clone(),
                saved_amount: orig_amount,
                saved_balance: orig_balance,
            },
            super::InterpFrame::AfterView {
                saved_self_address: inner.clone(),
                saved_sender: inner.clone(),
                saved_amount: 1,
                saved_balance: 2,
            },
        ];

        // Live context overridden to a view's values (as set_view_context
        // did on view entry).
        ctx.set_view_context(inner.clone(), inner, 999, 12345);
        assert_ne!(ctx.self_address, orig_self, "precondition: context overridden");

        super::restore_pending_view_context(&mut ctx, &frames);

        assert_eq!(ctx.self_address, orig_self, "self_address restored to outermost");
        assert_eq!(ctx.sender, orig_sender);
        assert_eq!(ctx.amount, orig_amount);
        assert_eq!(ctx.balance, orig_balance);
    }

    #[test]
    fn self_address() {
        let addr = super::Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let mut ctx = Ctx::default();
        ctx.self_address = addr.hash.clone();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[SelfAddress], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Address(addr),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::SELF_ADDRESS + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn sender() {
        let addr = super::Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let mut ctx = Ctx::default();
        ctx.sender = addr.hash.clone();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[Sender], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Address(addr),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::SENDER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn source() {
        let addr = super::PublicKeyHash::try_from("tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP").unwrap();
        let mut ctx = Ctx::default();
        ctx.source = addr.clone();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[Source], &mut ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::Address(super::Address {
                hash: addr.into(),
                entrypoint: Entrypoint::default()
            })]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::SOURCE + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn now() {
        let mut ctx = Ctx::default();
        ctx.now = 7000i32.into();
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[Now], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::timestamp(7000),]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::NOW + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn implicit_account() {
        let mut ctx = Ctx::default();
        let key_hash = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let mut stack = stk![V::KeyHash(key_hash)];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[ImplicitAccount], &mut ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::Contract(
                super::Address::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap()
            ),]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::IMPLICIT_ACCOUNT + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn voting_power() {
        let key_hash_1 = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = PublicKeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        let key_hash_3 = PublicKeyHash::try_from("tz3hpojUX9dYL5KLusv42SCBiggB77a2QLGx").unwrap();

        let mut ctx = Ctx::default();
        ctx.set_voting_powers([
            (key_hash_1.clone(), 30u32.into()),
            (key_hash_2.clone(), 50u32.into()),
        ]);
        let mut stack = stk![TypedValue::KeyHash(key_hash_2.clone())];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[VotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(50)]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::VOTING_POWER + interpret_cost::INTERPRET_RET
        );

        // When key_hash is not in context.
        let mut ctx = Ctx::default();
        ctx.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
        let mut stack = stk![TypedValue::KeyHash(key_hash_3)];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[VotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(0)]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::VOTING_POWER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn total_voting_power() {
        let key_hash_1 = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = PublicKeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        let mut ctx = Ctx::default();
        ctx.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
        let mut stack = Stack::new();
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(interpret(&[TotalVotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(80)]);
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::TOTAL_VOTING_POWER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn emit() {
        use crate::ast::annotations::FieldAnnotation;

        let mut ctx = Ctx::default();
        ctx.set_operation_counter(100);

        let mut stack = stk![TypedValue::nat(20)];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(
            interpret(
                &[Instruction::Emit {
                    tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                    arg_ty: Left(Type::Nat)
                }],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::new_operation(
                Operation::Emit(super::Emit {
                    tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                    value: TypedValue::nat(20),
                    arg_ty: Left(Type::Nat)
                }),
                101
            )]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::EMIT + interpret_cost::INTERPRET_RET
        );

        // When type contain annotations
        let mut ctx = Ctx::default();
        ctx.set_operation_counter(100);
        use crate::parser::test_helpers::parse;

        let mut stack = stk![TypedValue::nat(20)];
        let start_milligas = ctx.gas().milligas().unwrap();
        let emit_type_mich = parse("pair (int %f1) (int %f2)").unwrap();
        assert_eq!(
            interpret(
                &[Instruction::Emit {
                    tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                    arg_ty: Or::Right(emit_type_mich.clone())
                }],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![TypedValue::new_operation(
                Operation::Emit(super::Emit {
                    tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                    value: TypedValue::nat(20),
                    arg_ty: Or::Right(emit_type_mich)
                }),
                101
            )]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::EMIT + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    #[cfg(feature = "bls")]
    fn pairing_check() {
        let mut stack = stk![V::List(MichelsonList::from(vec![
            V::new_pair(
                V::new_bls12381_g1(bls::G1::one()),
                V::new_bls12381_g2(bls::G2::one())
            ),
            V::new_pair(
                V::new_bls12381_g1(bls::G1::one()),
                V::new_bls12381_g2(bls::G2::neg_one())
            )
        ]))];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&PairingCheck, ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Bool(true)]);
        assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
    }

    mod mul {
        use super::*;

        #[track_caller]
        fn test_mul(
            overload: overloads::Mul,
            input1: TypedValue,
            input2: TypedValue,
            output: TypedValue,
        ) {
            let mut stack = stk![input2, input1];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Mul(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        #[cfg(feature = "bls")]
        use crate::bls::*;
        #[cfg(feature = "bls")]
        use TypedValue as V;

        #[cfg(feature = "bls")]
        macro_rules! test {
            ($overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test_mul(overloads::Mul::$overload, $i1, $i2, $out);
                }
            };
        }

        // NB: actual bls arithmetic is tested in the bls module, here we only
        // need to check the interpreter works.
        #[cfg(feature = "bls")]
        test!(
            Bls12381G1Bls12381Fr,
            V::new_bls12381_g1(G1::one()),
            V::Bls12381Fr(Fr::one()),
            V::new_bls12381_g1(G1::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381G2Bls12381Fr,
            V::new_bls12381_g2(G2::one()),
            V::Bls12381Fr(Fr::one()),
            V::new_bls12381_g2(G2::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381FrBls12381Fr,
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            NatBls12381Fr,
            V::Nat(1u8.into()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            IntBls12381Fr,
            V::Int(1.into()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381FrNat,
            V::Bls12381Fr(Fr::one()),
            V::Nat(1u8.into()),
            V::Bls12381Fr(Fr::one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381FrInt,
            V::Bls12381Fr(Fr::one()),
            V::Int(1.into()),
            V::Bls12381Fr(Fr::one()),
        );

        macro_rules! test_nats {
            ($overload:ident, $con1:expr, $con2:expr, $con3:expr) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(0),
                        $con2(0),
                        $con3(0u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(1),
                        $con2(0),
                        $con3(0u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(0),
                        $con2(1),
                        $con3(0u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(1),
                        $con2(1),
                        $con3(1u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(100500),
                        $con2(1),
                        $con3(100500u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(1),
                        $con2(100500),
                        $con3(100500u32.into()),
                    );
                    test_mul(
                        overloads::Mul::$overload,
                        $con1(100500),
                        $con2(100500),
                        $con3(10100250000i64.try_into().unwrap()),
                    );
                }
            };
        }

        mod naturals {
            use super::*;
            test_nats!(NatNat, V::nat, V::nat, V::nat);
            test_nats!(NatInt, V::nat, V::int, V::Int);
            test_nats!(IntNat, V::int, V::nat, V::Int);
            test_nats!(IntInt, V::int, V::int, V::Int);
            test_nats!(MutezNat, V::Mutez, V::nat, V::Mutez);
            test_nats!(NatMutez, V::nat, V::Mutez, V::Mutez);
        }
        mod negatives {
            use super::*;

            #[test]
            fn int_int() {
                use overloads::Mul::*;
                test_mul(IntInt, V::int(-1), V::int(0), V::int(0));
                test_mul(IntInt, V::int(0), V::int(-1), V::int(0));
                test_mul(IntInt, V::int(-1), V::int(-1), V::int(1));
                test_mul(IntInt, V::int(-100500), V::int(-1), V::int(100500));
                test_mul(IntInt, V::int(-1), V::int(-100500), V::int(100500));
                test_mul(
                    IntInt,
                    V::int(-100500),
                    V::int(-100500),
                    V::int(10100250000i64),
                );
            }

            #[test]
            fn nat_int() {
                use overloads::Mul::*;
                test_mul(NatInt, V::nat(0), V::int(-1), V::int(0));
                test_mul(NatInt, V::nat(1), V::int(-1), V::int(-1));
                test_mul(NatInt, V::nat(100500), V::int(-1), V::int(-100500));
                test_mul(NatInt, V::nat(1), V::int(-100500), V::int(-100500));
                test_mul(
                    NatInt,
                    V::nat(100500),
                    V::int(-100500),
                    V::int(-10100250000i64),
                );
            }
            #[test]
            fn int_nat() {
                use overloads::Mul::*;
                test_mul(IntNat, V::int(-0), V::nat(1), V::int(0));
                test_mul(IntNat, V::int(-1), V::nat(1), V::int(-1));
                test_mul(IntNat, V::int(-100500), V::nat(1), V::int(-100500));
                test_mul(IntNat, V::int(-1), V::nat(100500), V::int(-100500));
                test_mul(
                    IntNat,
                    V::int(-100500),
                    V::nat(100500),
                    V::int(-10100250000i64),
                );
            }
        }
    }

    mod ediv {
        use super::*;

        #[track_caller]
        fn test_ediv(
            overload: overloads::EDiv,
            input1: TypedValue,
            input2: TypedValue,
            output: TypedValue,
        ) {
            let mut stack = stk![input2, input1];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&EDiv(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        macro_rules! test {
            ($name:ident, $overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    test_ediv(overloads::EDiv::$overload, $i1, $i2, $out);
                }
            };
        }

        test!(
            IntIntPosPosHigherExact,
            IntInt,
            V::int(35),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(5), V::nat(0)))),
        );

        test!(
            IntIntPosPosHigher,
            IntInt,
            V::int(23),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(3), V::nat(2)))),
        );

        test!(
            IntIntPosPosLower,
            IntInt,
            V::int(4),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(4)))),
        );

        test!(
            IntIntNegPosHigherExact,
            IntInt,
            V::int(-35),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(-5), V::nat(0)))),
        );

        test!(
            IntIntNegPosHigher,
            IntInt,
            V::int(-23),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(-4), V::nat(5)))),
        );

        test!(
            IntIntNegPosLower,
            IntInt,
            V::int(-4),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(-1), V::nat(3)))),
        );

        test!(
            IntIntPosNegHigherExact,
            IntInt,
            V::int(35),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(-5), V::nat(0)))),
        );

        test!(
            IntIntPosNegHigher,
            IntInt,
            V::int(23),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(-3), V::nat(2)))),
        );

        test!(
            IntIntPosNegLower,
            IntInt,
            V::int(4),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(4)))),
        );

        test!(
            IntIntNegNegHigherExact,
            IntInt,
            V::int(-35),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(5), V::nat(0)))),
        );

        test!(
            IntIntNegNegHigher,
            IntInt,
            V::int(-23),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(4), V::nat(5)))),
        );

        test!(
            IntIntNegNegLower,
            IntInt,
            V::int(-4),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(1), V::nat(3)))),
        );

        test!(
            IntIntZeroNeg,
            IntInt,
            V::int(0),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(0)))),
        );

        test!(
            IntIntZeroPos,
            IntInt,
            V::int(0),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(0)))),
        );

        test!(
            IntNatPosPosHigherExact,
            IntNat,
            V::int(35),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(5), V::nat(0)))),
        );

        test!(
            IntNatPosPosHigher,
            IntNat,
            V::int(23),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(3), V::nat(2)))),
        );

        test!(
            IntNatPosPosLower,
            IntNat,
            V::int(4),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(4)))),
        );

        test!(
            IntNatNegPosHigherExact,
            IntNat,
            V::int(-35),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(-5), V::nat(0)))),
        );

        test!(
            IntNatNegPosHigher,
            IntNat,
            V::int(-23),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(-4), V::nat(5)))),
        );

        test!(
            IntNatNegPosLower,
            IntNat,
            V::int(-4),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(-1), V::nat(3)))),
        );

        test!(
            IntNatZeroPos,
            IntNat,
            V::int(0),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(0)))),
        );

        test!(
            NatIntPosPosHigherExact,
            NatInt,
            V::nat(35),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(5), V::nat(0)))),
        );

        test!(
            NatIntPosPosHigher,
            NatInt,
            V::nat(23),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(3), V::nat(2)))),
        );

        test!(
            NatIntPosPosLower,
            NatInt,
            V::nat(4),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(4)))),
        );

        test!(
            NatIntPosNegHigherExact,
            NatInt,
            V::nat(35),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(-5), V::nat(0)))),
        );

        test!(
            NatIntPosNegHigher,
            NatInt,
            V::nat(23),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(-3), V::nat(2)))),
        );

        test!(
            NatIntPosNegLower,
            NatInt,
            V::nat(4),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(4)))),
        );

        test!(
            NatIntZeroPos,
            NatInt,
            V::nat(0),
            V::int(7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(0)))),
        );

        test!(
            NatIntZeroNeg,
            NatInt,
            V::nat(0),
            V::int(-7),
            V::new_option(Some(V::new_pair(V::int(0), V::nat(0)))),
        );

        test!(
            NatNatPosPosHigherExact,
            NatNat,
            V::nat(35),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::nat(5), V::nat(0)))),
        );

        test!(
            NatNatPosPosHigher,
            NatNat,
            V::nat(23),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::nat(3), V::nat(2)))),
        );

        test!(
            NatNatPosPosLower,
            NatNat,
            V::nat(4),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::nat(0), V::nat(4)))),
        );

        test!(
            NatNatZeroPos,
            NatNat,
            V::nat(0),
            V::nat(7),
            V::new_option(Some(V::new_pair(V::nat(0), V::nat(0)))),
        );

        test!(
            IntIntNegDivZero,
            IntInt,
            V::int(-4),
            V::int(0),
            V::Option(None),
        );

        test!(
            IntIntPosDivZero,
            IntInt,
            V::int(4),
            V::int(0),
            V::Option(None),
        );

        test!(
            NatIntPosDivZero,
            NatInt,
            V::nat(4),
            V::int(0),
            V::Option(None),
        );

        test!(
            IntNatPosDivZero,
            IntNat,
            V::int(4),
            V::nat(0),
            V::Option(None),
        );

        test!(
            IntNatNegDivZero,
            IntNat,
            V::int(-4),
            V::nat(0),
            V::Option(None),
        );

        test!(
            NatNatPosDivZero,
            NatNat,
            V::nat(4),
            V::nat(0),
            V::Option(None),
        );
    }

    mod neg {
        use super::*;

        #[track_caller]
        fn test_neg(overload: overloads::Neg, input: TypedValue, output: TypedValue) {
            let mut stack = stk![input];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Neg(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        #[cfg(feature = "bls")]
        use crate::bls::*;
        #[cfg(feature = "bls")]
        use TypedValue as V;

        macro_rules! test {
            ($overload:ident, $inp:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test_neg(overloads::Neg::$overload, $inp, $out);
                }
            };
        }

        // NB: actual bls arithmetic is tested in the bls module, here we only
        // need to check the interpreter works.

        #[cfg(feature = "bls")]
        test!(
            Bls12381G1,
            V::new_bls12381_g1(G1::one()),
            V::new_bls12381_g1(G1::neg_one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381G2,
            V::new_bls12381_g2(G2::one()),
            V::new_bls12381_g2(G2::neg_one()),
        );

        #[cfg(feature = "bls")]
        test!(
            Bls12381Fr,
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(-Fr::one()),
        );

        mod positive {
            use super::*;
            test!(Int, V::int(100500), V::int(-100500));
            test!(Nat, V::nat(100500), V::int(-100500));
        }

        mod negative {
            use super::*;
            test!(Int, V::int(-100500), V::int(100500));
        }

        mod zero {
            use super::*;
            test!(Int, V::int(0), V::int(0));
            test!(Nat, V::nat(0), V::int(0));
        }
    }

    mod lsl {
        use super::*;

        #[track_caller]
        fn test_lsl(
            overload: overloads::Lsl,
            input1: TypedValue,
            input2: TypedValue,
            output: TypedValue,
        ) {
            let mut stack = stk![input2, input1];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Lsl(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        macro_rules! test {
            ($name:ident, $overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    test_lsl(overloads::Lsl::$overload, $i1, $i2, $out);
                }
            };
        }

        test!(
            Case1,
            Bytes,
            V::Bytes(hex::decode("06").unwrap()),
            V::nat(0),
            V::Bytes(hex::decode("06").unwrap()),
        );

        test!(
            Case2,
            Bytes,
            V::Bytes(hex::decode("06").unwrap()),
            V::nat(1),
            V::Bytes(hex::decode("000c").unwrap()),
        );

        test!(
            Case3,
            Bytes,
            V::Bytes(hex::decode("06").unwrap()),
            V::nat(8),
            V::Bytes(hex::decode("0600").unwrap()),
        );

        test!(
            Case4,
            Bytes,
            V::Bytes(hex::decode("0006").unwrap()),
            V::nat(1),
            V::Bytes(hex::decode("00000c").unwrap()),
        );
    }

    mod lsr {
        use super::*;

        #[track_caller]
        fn test_lsr(
            overload: overloads::Lsr,
            input1: TypedValue,
            input2: TypedValue,
            output: TypedValue,
        ) {
            let mut stack = stk![input2, input1];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Lsr(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }

        macro_rules! test {
            ($name:ident, $overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $name() {
                    test_lsr(overloads::Lsr::$overload, $i1, $i2, $out);
                }
            };
        }

        test!(
            Case1,
            Bytes,
            V::Bytes(hex::decode("06").unwrap()),
            V::nat(1),
            V::Bytes(hex::decode("03").unwrap()),
        );

        test!(
            Case2,
            Bytes,
            V::Bytes(hex::decode("06").unwrap()),
            V::nat(8),
            V::Bytes(hex::decode("").unwrap()),
        );

        test!(
            Case3,
            Bytes,
            V::Bytes(hex::decode("0006").unwrap()),
            V::nat(1),
            V::Bytes(hex::decode("0003").unwrap()),
        );

        test!(
            Case4,
            Bytes,
            V::Bytes(hex::decode("0006").unwrap()),
            V::nat(8),
            V::Bytes(hex::decode("00").unwrap()),
        );

        test!(
            Case5,
            Bytes,
            V::Bytes(hex::decode("001234").unwrap()),
            V::nat(0),
            V::Bytes(hex::decode("001234").unwrap()),
        );

        test!(
            Case6,
            Bytes,
            V::Bytes(hex::decode("001234").unwrap()),
            V::nat(30),
            V::Bytes(hex::decode("").unwrap()),
        );
    }

    #[test]
    fn test_sub_mutez() {
        fn test(v1: i64, v2: i64, res: Option<i64>) {
            let mut stack = stk![V::Mutez(v2), V::Mutez(v1)];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&SubMutez, ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![V::new_option(res.map(V::Mutez))]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas().unwrap() > ctx.gas().milligas().unwrap());
        }
        test(0, 0, Some(0));
        test(0, 1, None);
        test(1, 0, Some(1));
        test(1, 1, Some(0));
        test(1, 2, None);
        test(100500, 100, Some(100400));
        test(100500, 100500, Some(0));
        test(100500, 100500700, None);
    }

    #[test]
    fn test_dig() {
        let mut stack = stk![V::Unit, V::nat(10), V::int(20), V::Bool(true), V::nat(5)];
        let expected_stack = stk![V::nat(10), V::int(20), V::Bool(true), V::nat(5), V::Unit,];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dig(4), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_dug() {
        let mut stack = stk![V::Unit, V::nat(10), V::int(20), V::Bool(true), V::nat(5)];
        let expected_stack = stk![V::nat(5), V::Unit, V::nat(10), V::int(20), V::Bool(true),];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Dug(4), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn unpack() {
        let mut stack = stk![V::Bytes(hex::decode("0500f1a2f3ad07").unwrap())];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&Unpack(Type::Int), ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(Some(V::int(-987654321)))]);
        assert!(ctx.gas().milligas().unwrap() < Ctx::default().gas.milligas().unwrap());
    }

    /// L2-1377: `UNPACK address` must accept entrypoint bytes outside
    /// the Michelson script-source charset, matching Tezos L1. Each
    /// payload below is the optimized PACK of a `tz1...%<ep>` address:
    /// 0x05 (watermark) || 0x0a (Bytes tag) || u32-BE length || 22-byte
    /// address hash || entrypoint bytes.
    #[test]
    fn unpack_address_l2_1377_relaxed_entrypoint() {
        use crate::ast::michelson_address::Address;

        // hash + "!" (one byte ep), L1 accepts; MIR used to reject.
        let payload =
            hex::decode("050a0000001700007b09f782e0bcd67739510afa819d85976119d5ef21").unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::Address), ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::new_option(Some(V::Address(
                Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%!").unwrap()
            )))]
        );

        // hash + ".foo" (first-char dot, L1 accepts; MIR used to reject).
        let payload =
            hex::decode("050a0000001a00007b09f782e0bcd67739510afa819d85976119d5ef2e666f6f")
                .unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::Address), ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::new_option(Some(V::Address(
                Address::from_base58_check("tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%.foo").unwrap()
            )))]
        );

        // hash + 0xff (non-UTF-8 ep byte, the issue's `ep_nonascii` case).
        // L1 stores the entrypoint name as a raw byte string and accepts
        // this; MIR now matches. The decoded address is built via
        // `from_bytes` because a non-UTF-8 entrypoint has no readable
        // base58 form.
        let payload =
            hex::decode("050a0000001700007b09f782e0bcd67739510afa819d85976119d5efff").unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::Address), ctx, &mut stack),
            Ok(())
        );
        let expected = Address::from_bytes(
            &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5efff").unwrap(),
        )
        .unwrap();
        assert_eq!(stack, stk![V::new_option(Some(V::Address(expected)))]);

        // Control: explicit "default" remains forbidden -> None.
        let payload =
            hex::decode("050a0000001d00007b09f782e0bcd67739510afa819d85976119d5ef64656661756c74")
                .unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::Address), ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::new_option(None)]);

        // Control: > 31 entrypoint bytes is still rejected -> None
        // (shared length bound with L1).
        let mut hex = String::from("050a0000003600007b09f782e0bcd67739510afa819d85976119d5ef");
        for _ in 0..32 {
            hex.push_str("71"); // 'q' x 32
        }
        let payload = hex::decode(&hex).unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::Address), ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::new_option(None)]);
    }

    #[test]
    fn unpack_bad_input() {
        let mut stack = stk![V::Bytes(hex::decode("05ffff").unwrap())];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&Unpack(Type::Int), ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None)]);
    }

    #[test]
    fn unpack_bad_type() {
        let mut stack = stk![V::Bytes(hex::decode("0500f1a2f3ad07").unwrap())];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&Unpack(Type::Unit), ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(None)]);
    }

    // L1's string type only accepts newline `\n` (0x0a) and printable ASCII
    // `0x20..=0x7e`. UNPACK string on bytes carrying any other control char
    // (carriage return `\r`, TAB, NUL, ...) must push `None`, not `Some`.
    // Encoding scheme: PACK tag 05, string tag 01, 4-byte big-endian length,
    // payload.
    #[test]
    fn unpack_string_forbidden_char_returns_none() {
        // "a\rb" — the case from L2-1357.
        let cr = hex::decode("050100000003610d62").unwrap();
        // "\t" alone.
        let tab = hex::decode("05010000000109").unwrap();
        // "\0" alone (already exercised by `validate_str`, kept for symmetry).
        let nul = hex::decode("05010000000100").unwrap();
        for payload in [cr, tab, nul] {
            let mut stack = stk![V::Bytes(payload)];
            let ctx = &mut Ctx::default();
            assert_eq!(
                interpret_one(&Unpack(Type::String), ctx, &mut stack),
                Ok(())
            );
            assert_eq!(stack, stk![V::new_option(None)]);
        }
    }

    // Control: a string whose bytes are all in the L1-permitted set still
    // decodes via UNPACK.
    #[test]
    fn unpack_string_allowed_chars_succeeds() {
        // "ab\n~" — all bytes within `0x20..=0x7e ∪ {0x0a}`.
        let payload = hex::decode("05010000000461620a7e").unwrap();
        let mut stack = stk![V::Bytes(payload)];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(&Unpack(Type::String), ctx, &mut stack),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![V::new_option(Some(V::String("ab\n~".to_owned())))]
        );
    }

    // Non-canonical zarith encodings must be rejected for every zarith-backed
    // type, matching Tezos L1, which returns `None` for these UNPACK inputs.
    // `0500b9c08100` is `int 12345` with a spurious trailing 0x00 group;
    // `05008100` is `nat 1` encoded non-minimally in two bytes.
    #[test]
    fn unpack_noncanonical_zarith_returns_none() {
        let trailing_zero = || hex::decode("0500b9c08100").unwrap();
        let non_minimal = || hex::decode("05008100").unwrap();
        for ty in [Type::Int, Type::Nat, Type::Mutez, Type::Timestamp] {
            let mut stack = stk![V::Bytes(trailing_zero())];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Unpack(ty.clone()), ctx, &mut stack), Ok(()));
            assert_eq!(
                stack,
                stk![V::new_option(None)],
                "UNPACK {ty:?} should reject trailing-zero zarith"
            );

            let mut stack = stk![V::Bytes(non_minimal())];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Unpack(ty.clone()), ctx, &mut stack), Ok(()));
            assert_eq!(
                stack,
                stk![V::new_option(None)],
                "UNPACK {ty:?} should reject non-minimal zarith"
            );
        }
    }

    // Control: the canonical encoding of `int 12345` (`0500b9c001`) still
    // decodes successfully after the trailing-zero rejection above.
    #[test]
    fn unpack_canonical_zarith_succeeds() {
        let mut stack = stk![V::Bytes(hex::decode("0500b9c001").unwrap())];
        let ctx = &mut Ctx::default();
        assert_eq!(interpret_one(&Unpack(Type::Int), ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(Some(V::int(12345)))]);
    }

    #[test]
    fn unpack_oversized_annotation() {
        // Regression for L2-1376: UNPACK of a packed lambda
        // `{ DROP ; PUSH @<ann> int 1 }`. L1 caps annotation tokens (sigil
        // included) at 255 bytes, so the 256-byte case must decode to None
        // (matching L1), while 255 still yields Some. The PACK bytes are built
        // directly, since the text parser now rejects oversized annotations.
        fn packed_lambda(ann_len: usize) -> Vec<u8> {
            use crate::parser::test_helpers::parse;
            let mut gas = Gas::default();
            let mut lambda = parse("Lambda_rec { DROP ; DROP ; PUSH int 1 }").unwrap();
            lambda
                .annotate(
                    Annotation::Variable("a".repeat(ann_len - 1).into()),
                    &mut gas,
                )
                .unwrap();
            lambda.encode_for_pack().unwrap().unwrap()
        }

        // 255-byte annotation token: decodes and typechecks -> Some.
        let mut stack = stk![V::Bytes(packed_lambda(255))];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(
                &Unpack(Type::new_lambda(Type::Unit, Type::Int)),
                ctx,
                &mut stack
            ),
            Ok(())
        );
        let top = stack
            .pop()
            .expect("UNPACK must leave a result on the stack");
        assert!(
            matches!(&*top, V::Option(Some(_))),
            "255-byte annotation should unpack to Some, got {top:?}"
        );

        // 256-byte annotation token: decode rejects the annotation -> None.
        let mut stack = stk![V::Bytes(packed_lambda(256))];
        let ctx = &mut Ctx::default();
        assert_eq!(
            interpret_one(
                &Unpack(Type::new_lambda(Type::Unit, Type::Int)),
                ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::new_option(None)]);
    }

    #[test]
    fn create_contract() {
        use crate::parser::test_helpers::parse;

        let cs_mich =
            parse("{ parameter unit; storage unit; code { DROP; UNIT; NIL operation; PAIR; }}")
                .unwrap();
        let mut ctx = Ctx::default();
        ctx.set_operation_counter(100);
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(ctx.gas(), true, true)
            .unwrap();
        let expected_addr = "KT1D5WSrhAnvHDrcNg8AtDoQCFaeikYjim6K";
        let expected_op = TypedValue::new_operation(
            Operation::CreateContract(super::CreateContract {
                delegate: None,
                amount: 100,
                storage: TypedValue::Unit,
                code: Rc::new(cs.clone()),
                micheline_code: &cs_mich,
                address: ContractKt1Hash::try_from(expected_addr).unwrap(),
            }),
            101,
        );
        let mut stack = stk![
            TypedValue::Unit,
            TypedValue::Mutez(100),
            TypedValue::new_option(None)
        ];
        let start_milligas = ctx.gas().milligas().unwrap();
        assert_eq!(
            interpret(
                &[CreateContract(Rc::new(cs), &cs_mich)],
                &mut ctx,
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(
            stack,
            stk![
                TypedValue::Address(addr::Address::try_from(expected_addr).unwrap()),
                expected_op
            ]
        );
        assert_eq!(
            start_milligas - ctx.gas().milligas().unwrap(),
            interpret_cost::CREATE_CONTRACT + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn three_layered_balanced_entrypoints() {
        use crate::parser::test_helpers::parse;

        let code = r#"{parameter (or (or (or (address %A) (bool %B)) (or (string %C) (key %D))) (or (or (nat %E) (signature %F)) (or (timestamp %G) (unit %H))));
        storage unit ;
        code {
            CDR ;
            NIL operation ;
            PAIR }}"#;

        let cs_mich = parse(code).unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap();

        let expected_entrypoints = HashMap::from([
            (
                FieldAnnotation::from_str_unchecked("A"),
                (
                    vec![Direction::Left, Direction::Left, Direction::Left],
                    Type::Address,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("B"),
                (
                    vec![Direction::Left, Direction::Left, Direction::Right],
                    Type::Bool,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("C"),
                (
                    vec![Direction::Left, Direction::Right, Direction::Left],
                    Type::String,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("D"),
                (
                    vec![Direction::Left, Direction::Right, Direction::Right],
                    Type::Key,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("E"),
                (
                    vec![Direction::Right, Direction::Left, Direction::Left],
                    Type::Nat,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("F"),
                (
                    vec![Direction::Right, Direction::Left, Direction::Right],
                    Type::Signature,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("G"),
                (
                    vec![Direction::Right, Direction::Right, Direction::Left],
                    Type::Timestamp,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("H"),
                (
                    vec![Direction::Right, Direction::Right, Direction::Right],
                    Type::Unit,
                ),
            ),
            (
                FieldAnnotation::default(),
                (
                    Vec::new(),
                    Type::new_or(
                        Type::new_or(
                            Type::new_or(Type::Address, Type::Bool),
                            Type::new_or(Type::String, Type::Key),
                        ),
                        Type::new_or(
                            Type::new_or(Type::Nat, Type::Signature),
                            Type::new_or(Type::Timestamp, Type::Unit),
                        ),
                    ),
                ),
            ),
        ]);

        // Check that each entrypoint is parsed correctly
        for (entrypoint, (path, ty)) in expected_entrypoints {
            match cs.annotations.get(&entrypoint) {
                Some((parsed_path, parsed_ty)) => {
                    assert_eq!(
                        parsed_path, &path,
                        "Incorrect path for entrypoint: {entrypoint:?}"
                    );
                    assert_eq!(
                        parsed_ty, &ty,
                        "Incorrect type for entrypoint: {entrypoint:?}"
                    );
                }
                _ => panic!("Entrypoint not parsed: {entrypoint:?}"),
            }
        }
    }

    #[test]
    fn seven_layered_left_pivoted_entrypoint() {
        use crate::parser::test_helpers::parse;

        let code = r#"{parameter (or (address %A) (or (bool %B) (or (string %C) (or (key %D) (or (nat %E) (or (signature %F) (or (timestamp %G) (unit %H))))))));
        storage unit ;
        code {
            CDR ;
            NIL operation ;
            PAIR }}"#;

        let cs_mich = parse(code).unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap();

        let parsed_entrypoints = cs.annotations;
        let expected_entrypoints = HashMap::from([
            (
                FieldAnnotation::from_str_unchecked("A"),
                (vec![Direction::Left], Type::Address),
            ),
            (
                FieldAnnotation::from_str_unchecked("B"),
                (vec![Direction::Right, Direction::Left], Type::Bool),
            ),
            (
                FieldAnnotation::from_str_unchecked("C"),
                (
                    vec![Direction::Right, Direction::Right, Direction::Left],
                    Type::String,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("D"),
                (
                    vec![
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Left,
                    ],
                    Type::Key,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("E"),
                (
                    vec![
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Left,
                    ],
                    Type::Nat,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("F"),
                (
                    vec![
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Left,
                    ],
                    Type::Signature,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("G"),
                (
                    vec![
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Left,
                    ],
                    Type::Timestamp,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("H"),
                (
                    vec![
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                        Direction::Right,
                    ],
                    Type::Unit,
                ),
            ),
            (
                FieldAnnotation::default(),
                (
                    Vec::new(),
                    Type::new_or(
                        Type::Address,
                        Type::new_or(
                            Type::Bool,
                            Type::new_or(
                                Type::String,
                                Type::new_or(
                                    Type::Key,
                                    Type::new_or(
                                        Type::Nat,
                                        Type::new_or(
                                            Type::Signature,
                                            Type::new_or(Type::Timestamp, Type::Unit),
                                        ),
                                    ),
                                ),
                            ),
                        ),
                    ),
                ),
            ),
        ]);

        // Check that each entrypoint is parsed correctly
        for (entrypoint, (path, ty)) in expected_entrypoints {
            match parsed_entrypoints.get(&entrypoint) {
                Some((parsed_path, parsed_ty)) => {
                    assert_eq!(
                        parsed_path, &path,
                        "Incorrect path for entrypoint: {entrypoint:?}"
                    );
                    assert_eq!(
                        parsed_ty, &ty,
                        "Incorrect type for entrypoint: {entrypoint:?}"
                    );
                }
                _ => panic!("Entrypoint not parsed: {entrypoint:?}"),
            }
        }
    }

    #[test]
    fn seven_layered_right_pivoted_entrypoint() {
        use crate::parser::test_helpers::parse;

        let code = r#"{parameter (or (or (or (or (or (or (or (timestamp %G) (unit %H)) (signature %F)) (nat %E)) (key %D)) (string %C)) (bool %B)) (address %A));
        storage unit ;
        code {
            CDR ;
            NIL operation ;
            PAIR }}"#;

        let cs_mich = parse(code).unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap();

        let parsed_entrypoints = cs.annotations;
        let expected_entrypoints = HashMap::from([
            (
                FieldAnnotation::from_str_unchecked("A"),
                (vec![Direction::Right], Type::Address),
            ),
            (
                FieldAnnotation::from_str_unchecked("B"),
                (vec![Direction::Left, Direction::Right], Type::Bool),
            ),
            (
                FieldAnnotation::from_str_unchecked("C"),
                (
                    vec![Direction::Left, Direction::Left, Direction::Right],
                    Type::String,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("D"),
                (
                    vec![
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Right,
                    ],
                    Type::Key,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("E"),
                (
                    vec![
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Right,
                    ],
                    Type::Nat,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("F"),
                (
                    vec![
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Right,
                    ],
                    Type::Signature,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("G"),
                (
                    vec![
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                    ],
                    Type::Timestamp,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("H"),
                (
                    vec![
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Left,
                        Direction::Right,
                    ],
                    Type::Unit,
                ),
            ),
            (
                FieldAnnotation::default(),
                (
                    Vec::new(),
                    Type::new_or(
                        Type::new_or(
                            Type::new_or(
                                Type::new_or(
                                    Type::new_or(
                                        Type::new_or(
                                            Type::new_or(Type::Timestamp, Type::Unit),
                                            Type::Signature,
                                        ),
                                        Type::Nat,
                                    ),
                                    Type::Key,
                                ),
                                Type::String,
                            ),
                            Type::Bool,
                        ),
                        Type::Address,
                    ),
                ),
            ),
        ]);

        // Check that each entrypoint is parsed correctly
        for (entrypoint, (path, ty)) in expected_entrypoints {
            match parsed_entrypoints.get(&entrypoint) {
                Some((parsed_path, parsed_ty)) => {
                    assert_eq!(
                        parsed_path, &path,
                        "Incorrect path for entrypoint: {entrypoint:?}"
                    );
                    assert_eq!(
                        parsed_ty, &ty,
                        "Incorrect type for entrypoint: {entrypoint:?}"
                    );
                }
                _ => panic!("Entrypoint not parsed: {entrypoint:?}"),
            }
        }
    }

    #[test]
    fn explicit_default_entrypoint() {
        use crate::parser::test_helpers::parse;

        let code = r#"{parameter (or (or (or (address %A) (bool %B)) (or (string %C) (key %D))) (or (or (nat %default) (signature %F)) (or (timestamp %G) (unit %H))));
        storage unit ;
        code {
            CDR ;
            NIL operation ;
            PAIR }}"#;

        let cs_mich = parse(code).unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap();

        let parsed_entrypoints = cs.annotations;
        let expected_entrypoints = HashMap::from([
            (
                FieldAnnotation::from_str_unchecked("A"),
                (
                    vec![Direction::Left, Direction::Left, Direction::Left],
                    Type::Address,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("B"),
                (
                    vec![Direction::Left, Direction::Left, Direction::Right],
                    Type::Bool,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("C"),
                (
                    vec![Direction::Left, Direction::Right, Direction::Left],
                    Type::String,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("D"),
                (
                    vec![Direction::Left, Direction::Right, Direction::Right],
                    Type::Key,
                ),
            ),
            (
                FieldAnnotation::default(),
                (
                    vec![Direction::Right, Direction::Left, Direction::Left],
                    Type::Nat,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("F"),
                (
                    vec![Direction::Right, Direction::Left, Direction::Right],
                    Type::Signature,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("G"),
                (
                    vec![Direction::Right, Direction::Right, Direction::Left],
                    Type::Timestamp,
                ),
            ),
            (
                FieldAnnotation::from_str_unchecked("H"),
                (
                    vec![Direction::Right, Direction::Right, Direction::Right],
                    Type::Unit,
                ),
            ),
        ]);

        // Check that each entrypoint is parsed correctly
        for (entrypoint, (path, ty)) in expected_entrypoints {
            match parsed_entrypoints.get(&entrypoint) {
                Some((parsed_path, parsed_ty)) => {
                    assert_eq!(
                        parsed_path, &path,
                        "Incorrect path for entrypoint: {entrypoint:?}"
                    );
                    assert_eq!(
                        parsed_ty, &ty,
                        "Incorrect type for entrypoint: {entrypoint:?}"
                    );
                }
                _ => panic!("Entrypoint not parsed: {entrypoint:?}"),
            }
        }
    }

    #[test]
    fn contract_address_computation() {
        use tezos_crypto_rs::hash::OperationHash;

        assert_eq!(
            compute_contract_address(
                &OperationHash::from_base58_check(
                    "onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D"
                )
                .unwrap(),
                0
            ),
            ContractKt1Hash::try_from("KT1UvfyLytrt71jh63YV4Yex5SmbNXpWHxtg").unwrap(),
        );
    }

    // -- Conversion impls for the formerly-panicking error paths --

    #[test]
    fn stack_oob_converts_to_stack_oob_variant() {
        // Stack errors that spawn in the interpreter propagate directly
        // via `From<StackOob> for InterpretError` (no detour through
        // `TcError`); the value stays structured.
        let err: InterpretError = StackOob.into();
        assert_eq!(err, InterpretError::StackOob(StackOob));
    }

    #[test]
    fn compare_error_incomparable_converts_to_compare_error_variant() {
        let err: InterpretError = CompareError::Incomparable.into();
        assert_eq!(
            err,
            InterpretError::CompareError(CompareError::Incomparable)
        );
    }
}
