// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Michelson typechecker definitions. Most functions defined as associated
//! functions on [Micheline], see there for more.

use chrono::prelude::DateTime;
use num_bigint::{BigInt, BigUint, TryFromBigIntError};
use num_traits::{Signed, Zero};
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;
use tezos_crypto_rs::{base58::FromBase58CheckError, hash::FromBytesError, public_key::PublicKey};
use tezos_data_encoding::nom::{error::convert_error, NomReader};
use tezos_protocol::entrypoint;

pub mod type_props;

use type_props::TypeProperty;

use crate::ast::annotations::AnnotationError;
use crate::ast::big_map::{self, BigMap, BigMapId};
use crate::ast::micheline::{
    micheline_fields, micheline_instructions, micheline_literals, micheline_types,
    micheline_unsupported_instructions, micheline_unsupported_types, micheline_values,
};
use crate::ast::michelson_address::AddressHash;
use crate::ast::*;
#[cfg(feature = "bls")]
use crate::bls;
use crate::context::TypecheckingCtx;
use crate::gas::{self, tc_cost, CompareError, CostOverflow, Gas, OutOfGas};
use crate::lexer::Prim;
use crate::stack::*;

/// Typechecker error type.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TcError {
    /// Two stacks didn't compare equal when they should have.
    #[error("type stacks not equal: {0:?} != {1:?}")]
    StacksNotEqual(TypeStack, TypeStack, StacksNotEqualReason),
    /// Ran out of gas during typechecking (direct `Gas::consume`).
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
    /// A cost-helper overflowed while computing a gas cost (the helpers do
    /// pure cost arithmetic; gas is charged separately at the call site).
    /// Kept structure-preserving rather than flattened into
    /// `OutOfGas`/`InternalError`.
    #[error(transparent)]
    CostOverflow(#[from] CostOverflow),
    /// A comparison-cost helper failed (cost computation or an attempt to
    /// compare incomparable values — the latter is an internal invariant
    /// violation, unreachable for typechecked input).
    #[error(transparent)]
    CompareError(#[from] CompareError),
    /// The type didn't satisfy a given [TypeProperty].
    #[error("type is not {0}: {1:?}")]
    InvalidTypeProperty(TypeProperty, Type),
    /// Encountered FAIL instruction not in tail position.
    #[error("FAIL instruction is not in tail position")]
    FailNotInTail,
    /// Typechecking would recurse beyond the protocol-defined depth limit
    /// (10000). Mirrors L1's `Typechecking_too_many_recursive_calls` in
    /// `src/proto_alpha/lib_protocol/script_ir_translator.ml:571`. Currently
    /// guards the value-level Lambda recursion (`step_typecheck_value` →
    /// `typecheck_lambda` → `typecheck`); the instruction-level LAMBDA path
    /// is iterative and unbounded by gas.
    #[error("too many recursive calls during typechecking")]
    TypecheckingTooManyRecursiveCalls,
    /// Failed to interpret a number as a value of some type due to a numeric
    /// conversion error.
    #[error("numeric conversion failed: {0}")]
    NumericConversion(TryFromBigIntError<()>),
    /// Types are not equal when they should be.
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    /// Encountered the forbidden `DUP 0` instruction.
    #[error("DUP 0 is forbidden")]
    Dup0,
    /// Encountered a forbidden `PAIR 0`, `PAIR 1`, `UNPAIR 0` or `UNPAIR 1`
    /// instruction.
    #[error("{0} {1} is forbidden")]
    PairN01(Prim, u16),
    /// Failed typechecking the value as the given type.
    #[error("value {0} is invalid for type {1:?}")]
    InvalidValueForType(String, Type),
    /// When typechecking a `map` or `big_map`, encountered a non-`Elt` element
    /// in a sequence.
    #[error("value {0:?} is invalid element for container type {1:?}")]
    InvalidEltForMap(String, Type),
    /// Elements of a `map`, `set` or a `big_map` were not sorted in the
    /// ascending order.
    #[error("sequence elements must be in strictly ascending order for type {0:?}")]
    ElementsNotSorted(Type),
    /// Duplicate keys/elements when typechecking a `map`, `set` or a `big_map`.
    #[error("sequence elements must contain no duplicate keys for type {0:?}")]
    DuplicateElements(Type),
    /// The given instruction can not be used with its input stack.
    #[error("no matching overload for {instr} on stack {stack:?}{}", .reason.as_ref().map_or("".to_owned(), |x| format!(", reason: {x}")))]
    NoMatchingOverload {
        /// The instruction being typechecked.
        instr: Prim,
        /// The offending input stack.
        stack: TypeStack,
        /// Optional details.
        reason: Option<NoMatchingOverloadReason>,
    },
    /// Encountered an error when typechecking a value represented as raw bytes
    /// or a base58-check string.
    #[error("invalid value for type {0:?}: {1}")]
    ByteReprError(Type, ByteReprError),
    /// Failed to typecheck an annotation as an entrypoint.
    #[error("invalid entrypoint: {0}")]
    EntrypointError(ByteReprError),
    /// Failed to typecheck value of type `chain_id`.
    #[error("invalid value for chain_id: {0}")]
    ChainIdError(#[from] ChainIdError),
    /// Encountered a SELF instruction in a forbidden context.
    #[error("SELF instruction is forbidden in this context")]
    SelfForbidden,
    /// Entrypoint not found.
    #[error("no such entrypoint: {0}")]
    NoSuchEntrypoint(Entrypoint),
    /// Contract with the given address not found.
    #[error("no such contract")]
    NoSuchContract,
    /// Implicit account typechecked as a `contract 'ty` where `'ty` is neither
    /// `unit` nor `ticket 'a`
    #[error("unexpected implicit account parameter type: {0:?}")]
    UnexpectedImplicitAccountType(Type),
    /// In `CONTRACT` instruction, entrypoint was specified both in the address
    /// and as an annotation to the instruction.
    #[error("entrypoint specified from two different sources")]
    EntrypointAmbiguity,
    /// Encountered unexpected Micheline syntax.
    #[error("unexpected syntax: {0}")]
    UnexpectedMicheline(String),
    /// When typechecking a complete script, encountered duplicate top-level
    /// field, viz. `code`, `parameter`, or `storage`.
    #[error("duplicate top-level element: {0}")]
    DuplicateTopLevelElt(Prim),
    /// When typechecking a complete script, didn't find a required top-level
    /// field, viz. `code`, `parameter`, or `storage`.
    #[error("missing top-level element: {0}")]
    MissingTopLevelElt(Prim),
    /// Instructions like `DUP n` and `PAIR n` accept an argument that must be a
    /// natural between 0 and 1023 inclusive. Found an integer outside this
    /// bounds instead.
    #[error("expected a natural between 0 and 1023, but got {0}")]
    ExpectedU10(BigInt),
    /// Encountered an error when working with annotations.
    #[error(transparent)]
    AnnotationError(#[from] AnnotationError),
    /// Found a duplicate entrypoint when parsing a type.
    #[error("duplicate entrypoint: {0}")]
    DuplicateEntrypoint(Entrypoint),
    /// Encountered an explicit default entrypoint annotation where it is
    /// forbidden, e.g. with `CONTRACT` instruction.
    #[error("explicit default entrypoint is forbidden in: {0}")]
    ExplicitDefaultEntrypointError(Prim),
    /// Instruction is not yet implemented.
    #[error("Unhandled instruction: {0}")]
    TodoInstr(Prim),
    /// Type is not yet implemented.
    #[error("Unhandled type: {0}")]
    TodoType(Prim),
    /// `big_map` with the supplied identifier not found in the storage.
    #[error("big map with ID {0} not found in the lazy storage")]
    BigMapNotFound(BigInt),
    /// An error occurred when working with `big_map` storage.
    #[error("lazy storage error: {0:?}")]
    LazyStorageError(String),
    /// Output stack after `MAP` instruction's code block is empty.
    #[error("MAP block returned an empty stack")]
    MapBlockEmptyStack,
    /// All branches of a `MAP` instruction's code block are failing.
    #[error("all branches of a MAP block use FAILWITH, its type cannot be inferred")]
    MapBlockFail,
    /// Two views with the same name where declared in a script
    #[error("two views were declared with the same name {0}")]
    DuplicatedView(String),
    /// View names must be at most 31 characters and match the expression [a-zA-Z0-9_.%@]*
    #[error("Invalid name for view {0}")]
    InvalidViewName(String),
    /// View instructions must be a sequence
    #[error("{0} instructions are not a sequence")]
    NonSeqViewInstrs(String),
    /// Side-effectful instruction used directly in a view body.
    /// Allowed in lambdas within views (they can be returned and executed elsewhere).
    // TODO: !21472
    // Update link once the "Forbidden instructions" section is added to the views doc.
    /// See https://octez.tezos.com/docs/active/views.html
    #[error("{0} is forbidden in view context")]
    ForbiddenInView(Prim),
    /// A stack access went out of bounds. Unreachable for valid Micheline
    /// (the typechecker fixes the stack shape before access); kept as a
    /// structured variant so the error is resolved at the right depth
    /// rather than flattened into a stringly error.
    #[error(transparent)]
    StackOob(#[from] StackOob),
    /// Internal invariant violation surfaced by the `pop!` macro and the
    /// `stack_get` / `stack_top_mut` helpers (empty-stack pop, out-of-range
    /// access, or a popped value of an unexpected type). Unreachable for
    /// valid Micheline — the typechecker establishes the stack shape and
    /// element types before these run. The variant tag identifies which
    /// path fired; payloads carry forensic context where useful.
    #[error("internal typechecker error: {0}")]
    InternalError(TcInvariant),
}

/// Categorises invariant violations reachable only on malformed input or a
/// typechecker bug. Variants either have no payload (single-site invariants
/// where the tag alone identifies the broken precondition) or carry typed,
/// equality-stable fields (`usize`, `u16`, `&'static str`) that preserve
/// forensic context without re-introducing the brittleness of formatted
/// strings: `PartialEq` stays well-defined for tests, and the `Display` impl
/// surfaces the runtime values when a path does fire.
#[allow(missing_docs)]
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TcInvariant {
    #[error("stack index out of bounds")]
    StackIndexOob,
    #[error("attempted to pop from an empty type stack")]
    EmptyTypeStackPop,
    #[error("type mismatch when popping the type stack: expected {expected}")]
    TypeMismatchOnPop { expected: &'static str },
    #[error("type stack too short for indexed access: index {index}, len {len}")]
    TypeStackTooShort { index: usize, len: usize },
    #[error("type stack unexpectedly empty")]
    EmptyTypeStack,
    #[error("comparison of incomparable values")]
    ComparisonOfIncomparable,
    #[error("expected Micheline::App")]
    ExpectedMichelineApp,
    #[error("PAIR {n} produced no elements during typechecking")]
    EmptyPairN { n: u16 },
    #[error("unexpected prim in Left/Right typed value")]
    UnexpectedLeftRightPrim,
    #[error("typecheck_value did not return the expected variant: expected {expected}")]
    TypecheckValueWrongVariant { expected: &'static str },
    #[error("expected ticket type on the left of JOIN_TICKETS pair")]
    ExpectedTicketLeftJoin,
    #[error("expected ticket type on the right of JOIN_TICKETS pair")]
    ExpectedTicketRightJoin,
    #[error("expected nested Pair(content, amount) in ticket value")]
    ExpectedTicketNestedPair,
    #[error("expected Nat for ticket amount")]
    ExpectedTicketNatAmount,
    #[error("worklist result stack unexpectedly empty: expected {expected}")]
    EmptyResultStack { expected: &'static str },
    /// The outer `FailingTypeStack` became `Failed` at a worklist site
    /// that the driver invariant expected to be `Ok`. Distinct from
    /// `EmptyResultStack` (about result-stack emptiness) — this one is
    /// about the *type* stack having lost its `Ok` shape.
    #[error("outer type stack unexpectedly Failed: {where_}")]
    OuterStackUnexpectedlyFailed { where_: &'static str },
    /// Worklist result-stack accumulator left with a non-singleton
    /// cardinality where the driver invariant expects exactly one root
    /// entry. Distinct from `EmptyResultStack` (about emptiness) — this
    /// one is about the wrong *count*.
    #[error("worklist accumulator wrong cardinality at {where_}: expected {expected}, got {got}")]
    ResultStackLen {
        expected: usize,
        got: usize,
        where_: &'static str,
    },
}

/// L1 parity: mirrors `Typechecking_too_many_recursive_calls` (returned
/// when `stack_depth > 10000` in `script_ir_translator.ml:571`).
///
/// Implementation rationale (vs. plumbing through `TypecheckingCtx`):
/// the kernel runs in a single thread, so a thread-local + RAII guard
/// keeps the counter balanced across every `?`-propagated `Err` without
/// every recursive callsite needing manual save/restore. Under
/// `panic = "abort"` (the kernel's profile via `sdk/rust/Cargo.toml`)
/// `Drop` does not fire on panic, but the abort terminates the runtime
/// before the next operation runs — the next operation re-initialises
/// the thread-local, so counter pollution across operations is moot.
/// Under `panic = "unwind"` (native embedders, debug tooling) `Drop`
/// fires on the unwind path and the counter balances normally.
const MAX_LAMBDA_TYPECHECK_DEPTH: u16 = 10_000;

std::thread_local! {
    static LAMBDA_TYPECHECK_DEPTH: std::cell::Cell<u16> = const { std::cell::Cell::new(0) };
}

struct LambdaTypecheckDepthGuard;

impl LambdaTypecheckDepthGuard {
    fn enter() -> Result<Self, TcError> {
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            let cur = d.get();
            // L1 admits the call when `stack_depth <= 10000` (rejects on
            // `stack_depth > 10000`); the depth counter is incremented
            // *before* the recursive call, so the first 10001 entries
            // succeed (`stack_depth` 0..=10000). MIR's counter is also
            // pre-increment here — match the admit threshold exactly.
            if cur > MAX_LAMBDA_TYPECHECK_DEPTH {
                return Err(TcError::TypecheckingTooManyRecursiveCalls);
            }
            d.set(cur + 1);
            Ok(LambdaTypecheckDepthGuard)
        })
    }
}

impl Drop for LambdaTypecheckDepthGuard {
    fn drop(&mut self) {
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            let cur = d.get();
            // Defense-in-depth: paired enter/Drop should never put the
            // counter at 0 here. A debug-build trip surfaces an
            // invariant violation that `saturating_sub` would silently
            // mask in release.
            debug_assert!(
                cur > 0,
                "LambdaTypecheckDepthGuard::Drop fired with counter at 0",
            );
            d.set(cur.saturating_sub(1));
        });
    }
}

impl From<TryFromBigIntError<()>> for TcError {
    fn from(error: TryFromBigIntError<()>) -> Self {
        TcError::NumericConversion(error)
    }
}

impl From<ByteReprError> for TcError {
    fn from(value: ByteReprError) -> Self {
        Self::ByteReprError(Type::Bytes, value)
    }
}

/// Errors happening when typechecking a value of type `chain_id`.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ChainIdError {
    /// Error happened when typechecking a (supposedly) base58-check encoded
    /// string as `chain_id`.
    #[error("{0}")]
    FromBase58CheckError(String),
    /// Error happened when typechecking raw bytes as `chain_id`.
    #[error("{0}")]
    FromBytesError(String),
}

impl From<FromBase58CheckError> for ChainIdError {
    fn from(value: FromBase58CheckError) -> Self {
        Self::FromBase58CheckError(value.to_string())
    }
}

impl From<FromBytesError> for ChainIdError {
    fn from(value: FromBytesError) -> Self {
        Self::FromBytesError(value.to_string())
    }
}

/// More detailed, optional explanation for [TcError::NoMatchingOverload].
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum NoMatchingOverloadReason {
    /// Input stack is too short.
    #[error("stack too short, expected at least {expected}")]
    StackTooShort {
        /// Expected minimal stack size
        expected: usize,
    },
    /// Types don't match.
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    /// Expected a type `pair 'a 'b` in the input stack, but did not find it.
    #[error("expected pair 'a 'b, but got {0:?}")]
    ExpectedPair(Type),
    /// Expected a type `option 'a` in the input stack, but did not find it.
    #[error("expected option 'a, but got {0:?}")]
    ExpectedOption(Type),
    /// Expected a type `list 'a` in the input stack, but did not find it.
    #[error("expected list 'a, but got {0:?}")]
    ExpectedList(Type),
    /// Expected a type `or 'a 'b` in the input stack, but did not find it.
    #[error("expected or 'a 'b, but got {0:?}")]
    ExpectedOr(Type),
    /// Expected a comparable type in the input stack, but it was not
    /// comparable.
    #[error("type not comparable: {0:?}")]
    TypeNotComparable(Type),
}

/// More detailed explanation for [TcError::StacksNotEqual]
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum StacksNotEqualReason {
    /// The given types in the stacks do not match.
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    /// Stack lengths differ.
    #[error("lengths are different: {0} != {1}")]
    LengthsDiffer(usize, usize),
}

/// Generic type mismatch error.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("types not equal: {0:?} != {1:?}")]
pub struct TypesNotEqual(Type, Type);

/// Check that name is a valid view name
pub fn check_view_name(name: &str) -> Result<(), TcError> {
    let is_valid = name.bytes().all(|b| {
        matches!(
            b,
            b'a'..=b'z'
                | b'A'..=b'Z'
                | b'0'..=b'9'
                | b'_'
                | b'.'
                | b'%'
                | b'@'
        )
    });
    if name.len() > 31 || !is_valid {
        return Err(TcError::InvalidViewName(name.to_owned()));
    };
    Ok(())
}

/// View version where fields are not typechecked
#[derive(Clone, PartialEq, Eq, Debug)]
#[allow(missing_docs)]
pub struct MichelineView<A> {
    pub input_type: A,
    pub output_type: A,
    pub code: A,
}

/// ContractScript version where fields are not typechecked
#[allow(missing_docs)]
#[derive(PartialEq, Debug)]
pub struct MichelineContractScript<A> {
    pub parameter_ty: A,
    pub storage_ty: A,
    pub code: A,
    pub views: HashMap<String, MichelineView<A>>,
}

impl<'arena> MichelineContractScript<&'_ Micheline<'arena>> {
    /// Typecheck the contract script. Validates the script's types, then
    /// typechecks the code and checks the result stack is as expected. Returns
    /// typechecked script.
    pub fn typecheck_script(
        self,
        gas: &mut Gas,
        allow_lazy_storage_in_storage: bool,
        typecheck_views: bool,
    ) -> Result<ContractScript<'arena>, TcError> {
        let MichelineContractScript {
            parameter_ty,
            storage_ty,
            code,
            views: mich_views,
        } = self;
        let (entrypoints, anns, parameter) =
            parse_parameter_ty_with_entrypoints(gas, parameter_ty)?;
        let storage = storage_ty.parse_ty(gas)?;
        let mut views = HashMap::new();
        // `typecheck_views` stays a bool here: it is a pre-existing public
        // parameter shared with the frozen kernels. Convert once at this
        // boundary to the swap-safe internal flag.
        let tc_views = if typecheck_views {
            TypecheckViews::Enabled
        } else {
            TypecheckViews::Disabled
        };
        for (name, view) in mich_views {
            let input_type = view.input_type.parse_ty(gas)?;
            let output_type = view.output_type.parse_ty(gas)?;
            if typecheck_views {
                input_type.ensure_prop(gas, TypeProperty::ViewInput)?;
                output_type.ensure_prop(gas, TypeProperty::ViewOutput)?;
                match view.code {
                    Micheline::Seq(instrs) => {
                        typecheck_view_with_views(
                            instrs,
                            gas,
                            Type::Pair(PairBox::new(input_type.clone(), storage.clone())),
                            output_type.clone(),
                            tc_views,
                        )?;
                    }
                    _ => return Err(TcError::NonSeqViewInstrs(name)),
                }
            }
            views.insert(
                name,
                View {
                    input_type,
                    output_type,
                    code: view.code.clone(),
                },
            );
        }
        parameter.ensure_prop(gas, TypeProperty::Passable)?;
        storage.ensure_prop(
            gas,
            if allow_lazy_storage_in_storage {
                TypeProperty::Storable
            } else {
                // Note: the only difference between BigMapValue and
                // Storable is that the former always forbids lazy
                // storage type (big maps and sapling states)
                TypeProperty::BigMapValue
            },
        )?;
        let mut stack = tc_stk![Type::new_pair(parameter.clone(), storage.clone())];
        let code = typecheck_instruction_with_views(
            code,
            gas,
            Some(&entrypoints),
            &mut stack,
            false,
            tc_views,
        )?;
        unify_stacks(
            gas,
            &mut tc_stk![Type::new_pair(
                Type::new_list(Type::Operation),
                storage.clone()
            )],
            stack,
        )?;
        Ok(ContractScript {
            code,
            parameter,
            storage,
            annotations: anns,
            views,
        })
    }
}

impl<'a> Micheline<'a> {
    /// Typechecks `Micheline` as a value, given its type (also as `Micheline`).
    /// Validates the type.
    pub fn typecheck_value(
        &self,
        ctx: &mut impl TypecheckingCtx<'a>,
        value_type: &Micheline<'a>,
    ) -> Result<TypedValue<'a>, TcError> {
        let ty = parse_ty(ctx.gas(), value_type)?;
        typecheck_value(self, ctx, &ty)
    }

    /// Typechecks `Micheline` as an instruction (or a sequence of instruction),
    /// given its input stack type as a slice of `Micheline`. Last element of
    /// the slice is on the top of the stack.
    /// Validates the type.
    ///
    /// When `self_type` is `None`, `SELF` instruction is forbidden (e.g. like
    /// in lambdas).
    pub fn typecheck_instruction(
        &self,
        gas: &mut Gas,
        self_type: Option<&Micheline<'a>>,
        stack: &[Micheline<'a>],
    ) -> Result<Instruction<'a>, TcError> {
        let entrypoints = self_type
            .map(|ty| {
                let (entrypoints, _, ty) = parse_parameter_ty_with_entrypoints(gas, ty)?;
                ty.ensure_prop(gas, TypeProperty::Passable)?;
                Ok::<_, TcError>(entrypoints)
            })
            .transpose()?;

        let TopIsLast(checked_stack) = stack
            .iter()
            .map(|ty| parse_ty(gas, ty))
            .collect::<Result<_, TcError>>()?;
        let mut opt_stack = FailingTypeStack::Ok(checked_stack);
        typecheck_instruction(self, gas, entrypoints.as_ref(), &mut opt_stack, false)
    }

    /// Parse `Micheline` as a type. Validates the type.
    pub fn parse_ty(&self, gas: &mut Gas) -> Result<Type, TcError> {
        parse_ty(gas, self)
    }

    /// Interpreting `Micheline` as a contract parameter type, collect its
    /// entrypoints into [Entrypoints].
    pub fn get_entrypoints(&self, gas: &mut Gas) -> Result<Entrypoints, TcError> {
        let (entrypoints, _, _) = parse_parameter_ty_with_entrypoints(gas, self)?;
        Ok(entrypoints)
    }

    /// Separate script into its components
    pub fn split_script(&self) -> Result<MichelineContractScript<&'a Micheline<'a>>, TcError> {
        let seq = match self {
            // top-level allows one level of nesting
            Micheline::Seq([Micheline::Seq(seq)]) => seq,
            Micheline::Seq(seq) => seq,
            x => return Err(TcError::UnexpectedMicheline(format!("{x:?}"))),
        };
        let mut parameter_ty = None;
        let mut storage_ty = None;
        let mut code = None;
        let mut views = HashMap::new();
        fn set_if_none<T>(elt: Prim, var: &mut Option<T>, value: T) -> Result<(), TcError> {
            if var.is_none() {
                *var = Some(value);
                Ok(())
            } else {
                Err(TcError::DuplicateTopLevelElt(elt))
            }
        }
        for elt in seq.iter() {
            match elt {
                Micheline::App(Prim::code, [content], anns) if anns.is_empty() => {
                    set_if_none(Prim::code, &mut code, content)?
                }
                Micheline::App(Prim::parameter, [content], anns) if anns.is_empty() => {
                    set_if_none(Prim::parameter, &mut parameter_ty, content)?
                }
                Micheline::App(Prim::storage, [content], anns) if anns.is_empty() => {
                    set_if_none(Prim::storage, &mut storage_ty, content)?
                }
                Micheline::App(
                    Prim::view,
                    [Micheline::String(name), input_type, output_type, code],
                    anns,
                ) if anns.is_empty() => {
                    check_view_name(name)?;
                    let previous_view = views.insert(
                        name.into(),
                        MichelineView {
                            input_type,
                            output_type,
                            code,
                        },
                    );
                    if previous_view.is_some() {
                        return Err(TcError::DuplicatedView(name.clone()));
                    }
                }
                Micheline::Seq(..)
                | micheline_instructions!()
                | micheline_literals!()
                | micheline_types!()
                | micheline_fields!()
                | micheline_values!() => {
                    return Err(TcError::UnexpectedMicheline(format!("{elt:?}")));
                }
            }
        }

        Ok(MichelineContractScript {
            parameter_ty: parameter_ty.ok_or(TcError::MissingTopLevelElt(Prim::parameter))?,
            storage_ty: storage_ty.ok_or(TcError::MissingTopLevelElt(Prim::storage))?,
            code: code.ok_or(TcError::MissingTopLevelElt(Prim::code))?,
            views,
        })
    }
}

pub(crate) fn parse_ty<'a>(gas: &mut Gas, ty: &Micheline<'a>) -> Result<Type, TcError> {
    parse_ty_with_entrypoints(gas, ty, None, &mut HashMap::new(), Vec::new())
}

/// Worklist frame for the iterative type parser. Visit consumes the
/// PARSE_TYPE_STEP cost, dispatches leaves inline and pushes Build plus
/// child Visits for compound types so LIFO popping yields the recursive
/// order.
enum PtyFrame<'a, 'b> {
    Visit {
        ty: &'b Micheline<'a>,
        with_entrypoints: bool,
        path: Vec<Direction>,
    },
    Build {
        ty: &'b Micheline<'a>,
        with_entrypoints: bool,
        path: Vec<Direction>,
        kind: BuildKind,
    },
    /// Run `ensure_prop` on the result currently on top of `results`,
    /// without consuming it. Used to enforce property checks at the same
    /// point the recursive code did — in particular, the Map/BigMap key
    /// `Comparable` check fires immediately after the key is parsed,
    /// *before* the value subtree is parsed. Keeps the failure point
    /// (and gas-exhaustion point) bit-identical with the recursive code.
    EnsureProp { prop: TypeProperty },
}

enum BuildKind {
    Pair,
    Or,
    Lambda,
    Map,
    BigMap,
    Option,
    List,
    Set,
    Ticket,
    Contract,
}

/// Push the frames that drive a parse_ty visit of one nested type
/// (Option, List, Set, Contract, Ticket): the outer `Build` frame for
/// the parent followed by a `Visit` for the inner child. Children
/// never carry an entrypoint path; entrypoint propagation lives on the
/// outer Build via `with_entrypoints` / `path`.
fn push_single_child_visit<'a, 'b>(
    frames: &mut Vec<PtyFrame<'a, 'b>>,
    ty: &'b Micheline<'a>,
    with_entrypoints: bool,
    path: Vec<Direction>,
    kind: BuildKind,
    child: &'b Micheline<'a>,
) {
    frames.push(PtyFrame::Build {
        ty,
        with_entrypoints,
        path,
        kind,
    });
    frames.push(PtyFrame::Visit {
        ty: child,
        with_entrypoints: false,
        path: Vec::new(),
    });
}

/// Push the frames that drive a parse_ty visit of two nested types
/// (Lambda, Map, BigMap). Order: outer Build, then right child Visit,
/// then left child Visit. LIFO popping yields left-then-right.
fn push_pair_children_visit<'a, 'b>(
    frames: &mut Vec<PtyFrame<'a, 'b>>,
    ty: &'b Micheline<'a>,
    with_entrypoints: bool,
    path: Vec<Direction>,
    kind: BuildKind,
    left: &'b Micheline<'a>,
    right: &'b Micheline<'a>,
) {
    frames.push(PtyFrame::Build {
        ty,
        with_entrypoints,
        path,
        kind,
    });
    frames.push(PtyFrame::Visit {
        ty: right,
        with_entrypoints: false,
        path: Vec::new(),
    });
    frames.push(PtyFrame::Visit {
        ty: left,
        with_entrypoints: false,
        path: Vec::new(),
    });
}

/// Register the field annotation of this `ty` as an entrypoint if the
/// node is being parsed as part of an entrypoint-bearing path, then push
/// the parsed Type onto the result stack.
fn finalize_node<'a, 'b>(
    ty: &'b Micheline<'a>,
    parsed_ty: Type,
    with_entrypoints: bool,
    path: Vec<Direction>,
    entrypoints: Option<&mut Entrypoints>,
    routed_annotations: &mut HashMap<FieldAnnotation<'a>, (Vec<Direction>, Type)>,
    results: &mut Vec<Type>,
) -> Result<(), TcError> {
    if with_entrypoints {
        if let Option::Some(eps) = entrypoints {
            // we just ensured it's an application of some type primitive
            let Micheline::App(_prim, _args, anns) = ty else {
                return Err(TcError::InternalError(
                    TcInvariant::ExpectedMichelineApp,
                ));
            };
            if let Option::Some(field_ann) = anns.get_single_field_ann()? {
                routed_annotations.insert(field_ann.clone(), (path, parsed_ty.clone()));
                if let Ok(entrypoint) = Entrypoint::try_from(field_ann) {
                    let entry = eps.entry(entrypoint);
                    match entry {
                        Entry::Occupied(e) => {
                            return Err(TcError::DuplicateEntrypoint(e.key().clone()));
                        }
                        Entry::Vacant(e) => {
                            e.insert(parsed_ty.clone());
                        }
                    };
                }
            }
        }
    }
    results.push(parsed_ty);
    Ok(())
}

fn parse_ty_with_entrypoints<'a, 'b>(
    gas: &mut Gas,
    ty: &'b Micheline<'a>,
    mut entrypoints: Option<&mut Entrypoints>,
    routed_annotations: &mut HashMap<FieldAnnotation<'a>, (Vec<Direction>, Type)>,
    path: Vec<Direction>,
) -> Result<Type, TcError> {
    use Micheline::*;
    let with_entrypoints_root = entrypoints.is_some();
    let mut frames: Vec<PtyFrame<'a, 'b>> = vec![PtyFrame::Visit {
        ty,
        with_entrypoints: with_entrypoints_root,
        path,
    }];
    let mut results: Vec<Type> = Vec::new();

    while let Some(frame) = frames.pop() {
        match frame {
            PtyFrame::Visit {
                ty,
                with_entrypoints,
                path,
            } => {
                use Prim::*;
                gas.consume(gas::tc_cost::PARSE_TYPE_STEP)?;
                let unexpected = || Err(TcError::UnexpectedMicheline(format!("{ty:?}")));
                // Small helper that captures the current Visit's metadata so
                // leaf arms can finalize in one short line.
                macro_rules! emit_leaf {
                    ($t:expr) => {{
                        finalize_node(
                            ty,
                            $t,
                            with_entrypoints,
                            path,
                            entrypoints.as_deref_mut(),
                            routed_annotations,
                            &mut results,
                        )?;
                    }};
                }
                match ty {
                    App(int, [], _) => emit_leaf!(Type::Int),
                    App(int, ..) => return unexpected(),

                    App(nat, [], _) => emit_leaf!(Type::Nat),
                    App(nat, ..) => return unexpected(),

                    App(bool, [], _) => emit_leaf!(Type::Bool),
                    App(bool, ..) => return unexpected(),

                    App(mutez, [], _) => emit_leaf!(Type::Mutez),
                    App(mutez, ..) => return unexpected(),

                    App(string, [], _) => emit_leaf!(Type::String),
                    App(string, ..) => return unexpected(),

                    App(operation, [], _) => emit_leaf!(Type::Operation),
                    App(operation, ..) => return unexpected(),

                    App(never, [], _) => emit_leaf!(Type::Never),
                    App(never, ..) => return unexpected(),

                    App(unit, [], _) => emit_leaf!(Type::Unit),
                    App(unit, ..) => return unexpected(),

                    App(address, [], _) => emit_leaf!(Type::Address),
                    App(address, ..) => return unexpected(),

                    App(chain_id, [], _) => emit_leaf!(Type::ChainId),
                    App(chain_id, ..) => return unexpected(),

                    App(timestamp, [], _) => emit_leaf!(Type::Timestamp),
                    App(timestamp, ..) => return unexpected(),

                    App(bytes, [], _) => emit_leaf!(Type::Bytes),
                    App(bytes, ..) => return unexpected(),

                    App(key, [], _) => emit_leaf!(Type::Key),
                    App(key, ..) => return unexpected(),

                    App(key_hash, [], _) => emit_leaf!(Type::KeyHash),
                    App(key_hash, ..) => return unexpected(),

                    App(signature, [], _) => emit_leaf!(Type::Signature),
                    App(signature, ..) => return unexpected(),

                    #[cfg(feature = "bls")]
                    App(bls12_381_fr, [], _) => emit_leaf!(Type::Bls12381Fr),
                    #[cfg(feature = "bls")]
                    App(bls12_381_fr, ..) => return unexpected(),

                    #[cfg(feature = "bls")]
                    App(bls12_381_g1, [], _) => emit_leaf!(Type::Bls12381G1),
                    #[cfg(feature = "bls")]
                    App(bls12_381_g1, ..) => return unexpected(),

                    #[cfg(feature = "bls")]
                    App(bls12_381_g2, [], _) => emit_leaf!(Type::Bls12381G2),
                    #[cfg(feature = "bls")]
                    App(bls12_381_g2, ..) => return unexpected(),

                    App(ticket, [t], _) => push_single_child_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::Ticket,
                        t,
                    ),
                    App(ticket, ..) => return unexpected(),

                    App(pair, [ty1, ty2, rest @ ..], _) => {
                        // The recursive shape is right leaning:
                        // new_pair(ty1, new_pair(ty2, new_pair(ty3, ...))).
                        // We push N - 1 Build::Pair frames then the N children
                        // in reverse so the first child pops first and the
                        // outermost Build::Pair pops last.
                        let total_children = 2 + rest.len();
                        for _ in 0..(total_children - 1) {
                            // All but the outermost Build::Pair are anonymous:
                            // they do not carry the entrypoint path of the
                            // original pair node; only the outermost one does.
                            frames.push(PtyFrame::Build {
                                ty,
                                with_entrypoints: false,
                                path: Vec::new(),
                                kind: BuildKind::Pair,
                            });
                        }
                        // Override the outermost Build::Pair (the first pushed)
                        // to carry the real entrypoint metadata.
                        let outermost_idx = frames.len() - (total_children - 1);
                        frames[outermost_idx] = PtyFrame::Build {
                            ty,
                            with_entrypoints,
                            path,
                            kind: BuildKind::Pair,
                        };
                        // Push children in reverse so the first child pops first.
                        for child in rest.iter().rev() {
                            frames.push(PtyFrame::Visit {
                                ty: child,
                                with_entrypoints: false,
                                path: Vec::new(),
                            });
                        }
                        frames.push(PtyFrame::Visit {
                            ty: ty2,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                        frames.push(PtyFrame::Visit {
                            ty: ty1,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                    }
                    App(pair, ..) => return unexpected(),

                    App(or, [l, r], _) => {
                        let mut left_path = path.clone();
                        left_path.push(Direction::Left);
                        let mut right_path = path.clone();
                        right_path.push(Direction::Right);
                        frames.push(PtyFrame::Build {
                            ty,
                            with_entrypoints,
                            path,
                            kind: BuildKind::Or,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: r,
                            with_entrypoints,
                            path: right_path,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: l,
                            with_entrypoints,
                            path: left_path,
                        });
                    }
                    App(or, ..) => return unexpected(),

                    App(option, [t], _) => push_single_child_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::Option,
                        t,
                    ),
                    App(option, ..) => return unexpected(),

                    App(list, [t], _) => push_single_child_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::List,
                        t,
                    ),
                    App(list, ..) => return unexpected(),

                    App(lambda, [ty1, ty2], _) => push_pair_children_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::Lambda,
                        ty1,
                        ty2,
                    ),
                    App(lambda, ..) => return unexpected(),

                    App(contract, [t], _) => push_single_child_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::Contract,
                        t,
                    ),
                    App(contract, ..) => return unexpected(),

                    App(set, [k], _) => push_single_child_visit(
                        &mut frames,
                        ty,
                        with_entrypoints,
                        path,
                        BuildKind::Set,
                        k,
                    ),
                    App(set, ..) => return unexpected(),

                    App(map, [k, v], _) => {
                        // Push order matches the recursive code's evaluation
                        // order: parse k → ensure k is Comparable → parse v →
                        // build new_map. LIFO pop yields that sequence.
                        frames.push(PtyFrame::Build {
                            ty,
                            with_entrypoints,
                            path,
                            kind: BuildKind::Map,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: v,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                        frames.push(PtyFrame::EnsureProp {
                            prop: TypeProperty::Comparable,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: k,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                    }
                    App(map, ..) => return unexpected(),

                    App(big_map, [k, v], _) => {
                        // Push order matches the recursive code: parse k →
                        // ensure k Comparable → parse v → ensure v BigMapValue
                        // → build new_big_map.
                        frames.push(PtyFrame::Build {
                            ty,
                            with_entrypoints,
                            path,
                            kind: BuildKind::BigMap,
                        });
                        frames.push(PtyFrame::EnsureProp {
                            prop: TypeProperty::BigMapValue,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: v,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                        frames.push(PtyFrame::EnsureProp {
                            prop: TypeProperty::Comparable,
                        });
                        frames.push(PtyFrame::Visit {
                            ty: k,
                            with_entrypoints: false,
                            path: Vec::new(),
                        });
                    }
                    App(big_map, ..) => return unexpected(),

                    Seq(..)
                    | micheline_fields!()
                    | micheline_instructions!()
                    | micheline_literals!()
                    | micheline_values!() => return unexpected(),

                    App(prim @ micheline_unsupported_types!(), ..) => {
                        return Err(TcError::TodoType(*prim));
                    }
                }
            }
            PtyFrame::Build {
                ty,
                with_entrypoints,
                path,
                kind,
            } => {
                let parsed = match kind {
                    BuildKind::Pair => {
                        let r = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "pair r" }))?;
                        let l = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "pair l" }))?;
                        Type::new_pair(l, r)
                    }
                    BuildKind::Or => {
                        let r = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "or r" }))?;
                        let l = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "or l" }))?;
                        Type::new_or(l, r)
                    }
                    BuildKind::Lambda => {
                        let ret = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "lambda ret" }))?;
                        let arg = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "lambda arg" }))?;
                        Type::new_lambda(arg, ret)
                    }
                    BuildKind::Map => {
                        // ensure_prop(k, Comparable) already ran via the
                        // EnsureProp frame between Visit(k) and Visit(v),
                        // preserving recursive-order failure semantics.
                        let v = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "map v" }))?;
                        let k = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "map k" }))?;
                        Type::new_map(k, v)
                    }
                    BuildKind::BigMap => {
                        // ensure_prop(k, Comparable) and ensure_prop(v,
                        // BigMapValue) ran via EnsureProp frames between
                        // the Visits.
                        let v = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "big_map v" }))?;
                        let k = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "big_map k" }))?;
                        Type::new_big_map(k, v)
                    }
                    BuildKind::Option => {
                        let t = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "option t" }))?;
                        Type::new_option(t)
                    }
                    BuildKind::List => {
                        let t = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "list t" }))?;
                        Type::new_list(t)
                    }
                    BuildKind::Set => {
                        let t = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "set t" }))?;
                        t.ensure_prop(gas, TypeProperty::Comparable)?;
                        Type::new_set(t)
                    }
                    BuildKind::Ticket => {
                        let t = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ticket t" }))?;
                        // The inner type of ticket only needs to be comparable.
                        t.ensure_prop(gas, TypeProperty::Comparable)?;
                        Type::new_ticket(t)
                    }
                    BuildKind::Contract => {
                        let t = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "contract t" }))?;
                        // The argument of contract only needs to be passable;
                        // contract itself is duplicable and packable.
                        t.ensure_prop(gas, TypeProperty::Passable)?;
                        Type::new_contract(t)
                    }
                };
                finalize_node(
                    ty,
                    parsed,
                    with_entrypoints,
                    path,
                    entrypoints.as_deref_mut(),
                    routed_annotations,
                    &mut results,
                )?;
            }
            PtyFrame::EnsureProp { prop } => {
                // Run ensure_prop on the result currently on top of `results`
                // without consuming it; the result remains available for the
                // surrounding Build frame to pop. Borrowing avoids a clone
                // of the (possibly deep) Type.
                let t = results.last().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ensure_prop target" }))?;
                t.ensure_prop(gas, prop)?;
            }
        }
    }
    Ok(results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "root type" }))?)
}

#[allow(clippy::type_complexity)]
fn parse_parameter_ty_with_entrypoints<'a>(
    gas: &mut Gas,
    parameter_ty: &Micheline<'a>,
) -> Result<
    (
        Entrypoints,
        HashMap<FieldAnnotation<'a>, (Vec<Direction>, Type)>,
        Type,
    ),
    TcError,
> {
    let mut entrypoints = Entrypoints::new();
    let mut routed_annotations = HashMap::new();
    let parameter = parse_ty_with_entrypoints(
        gas,
        parameter_ty,
        Some(&mut entrypoints),
        &mut routed_annotations,
        Vec::new(),
    )?;
    entrypoints
        .entry(Entrypoint::default())
        .or_insert_with(|| parameter.clone());
    routed_annotations
        .entry(FieldAnnotation::default())
        .or_insert_with(|| (vec![], parameter.clone()));
    Ok((entrypoints, routed_annotations, parameter))
}

/// Typecheck a sequence of instructions. Assumes the passed stack is valid, i.e.
/// doesn't contain illegal types like `set operation` or `contract operation`.
///
/// When `self_entrypoints` is `None`, `SELF` instruction is forbidden (e.g.
/// like in lambdas).
///
/// Entrypoint map is carried as an argument, not as part of context, because it
/// has to be locally overridden during typechecking.
/// What a per instruction step yields. Non recursive arms produce a
/// finished instruction; control flow arms with unbounded depth (IF
/// family, LOOP family, ITER, DIP, SEQ, LAMBDA / LAMBDA_REC, MAP)
/// produce an Open variant that the driver expands into nested block
/// frames. The LAMBDA / MAP arms switch the driver into a fresh
/// (LAMBDA) or cloned-and-extended (MAP) type-stack context for the
/// body, then restore the outer context on the matching After frame.
enum StepResult<'a, 'b> {
    Done(Instruction<'a>),
    OpenIf {
        t_block: &'b [Micheline<'a>],
        f_block: &'b [Micheline<'a>],
        saved_f_stack: FailingTypeStack,
    },
    OpenIfNone {
        none_block: &'b [Micheline<'a>],
        some_block: &'b [Micheline<'a>],
        some_top_ty: Type,
    },
    OpenIfCons {
        cons_block: &'b [Micheline<'a>],
        nil_block: &'b [Micheline<'a>],
        list_ty: Type,
        cons_top_ty: Type,
    },
    OpenIfLeft {
        left_block: &'b [Micheline<'a>],
        right_block: &'b [Micheline<'a>],
        right_top_ty: Type,
    },
    OpenLoop {
        body: &'b [Micheline<'a>],
        initial_copy: FailingTypeStack,
    },
    OpenLoopLeft {
        body: &'b [Micheline<'a>],
        initial_copy: FailingTypeStack,
        r_ty: Type,
    },
    OpenIter {
        body: &'b [Micheline<'a>],
        variant: overloads::Iter,
        outer_opt_stack: FailingTypeStack,
    },
    OpenDip {
        body: &'b [Micheline<'a>],
        opt_height: Option<u16>,
        protected: TypeStack,
    },
    OpenSeq {
        body: &'b [Micheline<'a>],
    },
    /// LAMBDA / LAMBDA_REC instruction, or a `PUSH (lambda T1 T2) <body>`
    /// value: route the body through the outer worklist driver so nested
    /// LAMBDAs do not grow the Rust call stack one frame per nesting level.
    /// The body runs on a fresh `[in_ty]` stack (or `[self_ty, in_ty]` for
    /// the recursive form) with `self_entrypoints = None` and `in_view =
    /// false`. When `for_push == true`, the AfterLambda finalizer wraps the
    /// resulting lambda in `Instruction::Push(TypedValue::Lambda(...))` and
    /// the outer stack receives the lambda type at finalize time; otherwise
    /// it emits `Instruction::Lambda(...)` and the outer stack must already
    /// carry the lambda type (set at instruction-step time).
    ///
    /// `depth_guard` carries the `LambdaTypecheckDepthGuard` for the PUSH
    /// path (Some) and is None for the instruction-level path. Threading it
    /// through the worklist preserves the RAII invariant: the guard's Drop
    /// fires whether the AfterLambda frame is consumed normally or the
    /// `frames` Vec is torn down on the Err path.
    OpenLambda {
        body: &'b [Micheline<'a>],
        in_ty: Type,
        out_ty: Type,
        recursive: bool,
        micheline_code: Micheline<'a>,
        for_push: bool,
        depth_guard: Option<LambdaTypecheckDepthGuard>,
    },
    /// MAP block on a List / Option / Map. The body runs on a clone of the
    /// outer stack plus `inner_ty` pushed on top (the element / pair type).
    /// `kind` records which container to rewrap with after the body completes.
    OpenMapBlock {
        body: &'b [Micheline<'a>],
        inner_ty: Type,
        kind: MapKind,
    },
}

/// Which container wraps the result of a `MAP` block — selected from the
/// outer stack's source type before the body runs.
enum MapKind {
    /// MAP over `list e` → result is `list e'`.
    List,
    /// MAP over `option e` → result is `option e'`.
    Option,
    /// MAP over `map k e` → result is `map k e'`. Carries the unchanged
    /// key type so the After frame can rewrap.
    Map(Type),
}

/// Worklist frame for the iterative instruction typechecker driver.
enum TcIFrame<'a, 'b> {
    /// Process the next instruction in this block. When `idx == block.len()`,
    /// the matching After frame finalizes.
    NextInstr {
        block: &'b [Micheline<'a>],
        idx: usize,
    },
    AfterIfThen {
        f_block: &'b [Micheline<'a>],
        saved_f_stack: FailingTypeStack,
    },
    AfterIfFalse {
        saved_t_stack: FailingTypeStack,
    },
    AfterIfNoneNone {
        some_block: &'b [Micheline<'a>],
        saved_some_stack: FailingTypeStack,
    },
    AfterIfNoneSome {
        saved_none_stack: FailingTypeStack,
    },
    AfterIfConsCons {
        nil_block: &'b [Micheline<'a>],
        saved_nil_stack: FailingTypeStack,
    },
    AfterIfConsNil {
        saved_cons_stack: FailingTypeStack,
    },
    AfterIfLeftLeft {
        right_block: &'b [Micheline<'a>],
        saved_right_stack: FailingTypeStack,
    },
    AfterIfLeftRight {
        saved_left_stack: FailingTypeStack,
    },
    AfterLoop {
        initial_copy: FailingTypeStack,
    },
    AfterLoopLeft {
        initial_copy: FailingTypeStack,
        r_ty: Type,
    },
    AfterIter {
        variant: overloads::Iter,
        outer_opt_stack: FailingTypeStack,
    },
    AfterDip {
        opt_height: Option<u16>,
        protected: TypeStack,
    },
    AfterSeq,
    /// Finalize a `LAMBDA` / `LAMBDA_REC` instruction whose body was just
    /// typechecked on a fresh stack. Pops the body's instructions from the
    /// outer `block_results`, unifies the body's resulting stack with
    /// `[out_ty]`, builds the Lambda struct, restores the outer state, and
    /// pushes `I::Lambda(...)` onto the (now-restored) outer
    /// `block_results`.
    AfterLambda {
        in_ty: Type,
        out_ty: Type,
        recursive: bool,
        micheline_code: Micheline<'a>,
        saved_outer_stack: FailingTypeStack,
        saved_self_entrypoints: Option<&'b Entrypoints>,
        saved_in_view: bool,
        /// `true` when the lambda came from a `PUSH (lambda …) <body>` value:
        /// finalize as `Instruction::Push(Rc::new(TypedValue::Lambda(...)))`
        /// and push the lambda type onto the (restored) outer stack. `false`
        /// for the bare `LAMBDA`/`LAMBDA_REC` instruction, where the type was
        /// already pushed onto the outer stack at step time.
        for_push: bool,
        /// `LambdaTypecheckDepthGuard` for the PUSH path, taken at the PUSH
        /// step and dropped here -- either on the finalize Ok/Err path of
        /// this match arm (the local binding falls out of scope) or on the
        /// driver Err exit, when `frames: Vec<TcIFrame>` is torn down. Either
        /// way the counter decrement runs exactly once per matching
        /// increment, preserving L1 parity across `?`-propagated Errs. None
        /// for the instruction-level LAMBDA path (it does not touch the
        /// counter, matching pre-fix behavior).
        depth_guard: Option<LambdaTypecheckDepthGuard>,
    },
    /// Finalize a `MAP` block. Pops the body's instructions, pops the
    /// inner-result type off the body's stack, restores the outer stack,
    /// pushes the rewrapped container type, and pushes `I::Map(...)` onto
    /// the outer `block_results`.
    AfterMapBlock {
        kind: MapKind,
        saved_outer_stack: FailingTypeStack,
    },
}

/// Whether an embedded `CREATE_CONTRACT`'s child-script views are validated.
/// `Enabled` only at origination (matching L1); re-typecheck and runtime paths
/// use `Disabled`. A distinct type (not a bare `bool`) so it cannot be
/// transposed with an adjacent flag such as `in_view` or `recursive`.
#[derive(Clone, Copy)]
pub enum TypecheckViews {
    /// Validate embedded child-script views (origination).
    Enabled,
    /// Skip child-view validation (re-typecheck, runtime).
    Disabled,
}

impl TypecheckViews {
    fn enabled(self) -> bool {
        matches!(self, TypecheckViews::Enabled)
    }
}

/// Iterative driver: typechecks every nested block from one explicit
/// worklist instead of recursing through `typecheck` / `typecheck_instruction`.
fn typecheck<'a, 'b>(
    ast: &'b [Micheline<'a>],
    gas: &mut Gas,
    self_entrypoints: Option<&'b Entrypoints>,
    opt_stack: &mut FailingTypeStack,
    in_view: bool,
    typecheck_views: TypecheckViews,
) -> Result<Vec<Instruction<'a>>, TcError> {
    let mut frames: Vec<TcIFrame<'a, 'b>> = vec![TcIFrame::NextInstr { block: ast, idx: 0 }];
    // One accumulator per active block. The root accumulator is at index 0;
    // nested blocks push and pop their own.
    let mut block_results: Vec<Vec<Instruction<'a>>> = vec![Vec::new()];
    // Mutable state across the driver: switched by Open/After Lambda frames
    // when a LAMBDA / LAMBDA_REC body is entered (fresh scope with
    // self_entrypoints=None, in_view=false). MAP_BLOCK does NOT switch
    // these — it inherits the outer context.
    // `typecheck_views` is intentionally NOT in this set (unlike in_view): it
    // is an origination-wide constant, so a CREATE_CONTRACT in a lambda body
    // stays origination-validated. Resetting it here would reopen L2-1635.
    let mut cur_self_entrypoints = self_entrypoints;
    let mut cur_in_view = in_view;
    while let Some(frame) = frames.pop() {
        match frame {
            TcIFrame::NextInstr { block, idx } => {
                if idx >= block.len() {
                    // Block finished; the next frame in line will consume
                    // the top of `block_results`.
                    continue;
                }
                let instr = &block[idx];
                // Re-push NextInstr advanced so subsequent finalizations
                // know where to resume; this re-push lands *below* any
                // nested frames we are about to add for the current instr.
                let parent = TcIFrame::NextInstr {
                    block,
                    idx: idx + 1,
                };
                let step = typecheck_instruction_step(
                    instr,
                    gas,
                    cur_self_entrypoints,
                    opt_stack,
                    cur_in_view,
                    typecheck_views,
                )?;
                match step {
                    StepResult::Done(typed) => {
                        block_results.last_mut().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "block result" }))?.push(typed);
                        frames.push(parent);
                    }
                    StepResult::OpenIf {
                        t_block,
                        f_block,
                        saved_f_stack,
                    } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterIfThen {
                            f_block,
                            saved_f_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: t_block,
                            idx: 0,
                        });
                    }
                    StepResult::OpenIfNone {
                        none_block,
                        some_block,
                        some_top_ty,
                    } => {
                        // The current opt_stack (post pop of the Option's
                        // inner type) is what the None arm runs against;
                        // the Some arm needs its own clone with `some_top_ty`
                        // pushed on top.
                        let mut some_stack: TypeStack =
                            opt_stack.access_mut(TcError::FailNotInTail)?.clone();
                        some_stack.push(some_top_ty);
                        let saved_some_stack = FailingTypeStack::Ok(some_stack);
                        frames.push(parent);
                        frames.push(TcIFrame::AfterIfNoneNone {
                            some_block,
                            saved_some_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: none_block,
                            idx: 0,
                        });
                    }
                    StepResult::OpenIfCons {
                        cons_block,
                        nil_block,
                        list_ty,
                        cons_top_ty,
                    } => {
                        // current opt_stack is the post pop nil stack. Build
                        // the cons stack: post pop + list_ty (the popped
                        // list type, pushed back) + element type.
                        let live = opt_stack.access_mut(TcError::FailNotInTail)?;
                        let mut cons_stack: TypeStack = live.clone();
                        cons_stack.push(list_ty);
                        cons_stack.push(cons_top_ty);
                        let cons_opt_stack = FailingTypeStack::Ok(cons_stack);
                        // Save the nil stack (live), install the cons stack
                        // as live so the cons arm runs against it.
                        let saved_nil_stack = std::mem::replace(opt_stack, cons_opt_stack);
                        frames.push(parent);
                        frames.push(TcIFrame::AfterIfConsCons {
                            nil_block,
                            saved_nil_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: cons_block,
                            idx: 0,
                        });
                    }
                    StepResult::OpenIfLeft {
                        left_block,
                        right_block,
                        right_top_ty,
                    } => {
                        // current opt_stack already has the left type pushed
                        // (set up by the step before yielding). Build the
                        // right stack from the clone.
                        let mut right_stack: TypeStack =
                            opt_stack.access_mut(TcError::FailNotInTail)?.clone();
                        // Replace the top of right_stack (currently the
                        // left type) with the right type.
                        *right_stack.get_mut(0)? = right_top_ty;
                        let saved_right_stack = FailingTypeStack::Ok(right_stack);
                        frames.push(parent);
                        frames.push(TcIFrame::AfterIfLeftLeft {
                            right_block,
                            saved_right_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: left_block,
                            idx: 0,
                        });
                    }
                    StepResult::OpenLoop { body, initial_copy } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterLoop { initial_copy });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    StepResult::OpenLoopLeft {
                        body,
                        initial_copy,
                        r_ty,
                    } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterLoopLeft { initial_copy, r_ty });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    StepResult::OpenIter {
                        body,
                        variant,
                        outer_opt_stack,
                    } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterIter {
                            variant,
                            outer_opt_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    StepResult::OpenDip {
                        body,
                        opt_height,
                        protected,
                    } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterDip {
                            opt_height,
                            protected,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    StepResult::OpenSeq { body } => {
                        frames.push(parent);
                        frames.push(TcIFrame::AfterSeq);
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr {
                            block: body,
                            idx: 0,
                        });
                    }
                    StepResult::OpenLambda {
                        body,
                        in_ty,
                        out_ty,
                        recursive,
                        micheline_code,
                        for_push,
                        depth_guard,
                    } => {
                        // Save outer state.
                        let saved_self_entrypoints = cur_self_entrypoints;
                        let saved_in_view = cur_in_view;
                        // Build the body's initial stack: [in_ty] for plain
                        // LAMBDA, [self_ty, in_ty] for LAMBDA_REC (self_ty on
                        // bottom so the body can DUP / EXEC self).
                        let body_initial = if recursive {
                            let self_ty = Type::new_lambda(in_ty.clone(), out_ty.clone());
                            tc_stk![self_ty, in_ty.clone()]
                        } else {
                            tc_stk![in_ty.clone()]
                        };
                        let saved_outer_stack = std::mem::replace(opt_stack, body_initial);
                        cur_self_entrypoints = None;
                        cur_in_view = false;
                        // Continue the outer block after the lambda
                        // resolves.
                        frames.push(parent);
                        frames.push(TcIFrame::AfterLambda {
                            in_ty,
                            out_ty,
                            recursive,
                            micheline_code,
                            saved_outer_stack,
                            saved_self_entrypoints,
                            saved_in_view,
                            for_push,
                            depth_guard,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr { block: body, idx: 0 });
                    }
                    StepResult::OpenMapBlock {
                        body,
                        inner_ty,
                        kind,
                    } => {
                        // The body runs on a CLONE of the outer post-pop
                        // stack with `inner_ty` pushed (matches the
                        // recursive `typecheck_map_block` which clones
                        // `stack` before pushing). AfterMapBlock restores
                        // the outer stack and rewraps with the container
                        // type. self_entrypoints / in_view stay inherited.
                        let body_initial = match opt_stack {
                            FailingTypeStack::Ok(s) => {
                                let mut cloned = s.clone();
                                cloned.push(inner_ty);
                                FailingTypeStack::Ok(cloned)
                            }
                            // If the outer stack is already failing, the
                            // body inherits the failure (matches the
                            // recursive behavior — typecheck_map_block on a
                            // failing nested stack is propagated by typecheck).
                            FailingTypeStack::Failed => FailingTypeStack::Failed,
                        };
                        let saved_outer_stack = std::mem::replace(opt_stack, body_initial);
                        frames.push(parent);
                        frames.push(TcIFrame::AfterMapBlock {
                            kind,
                            saved_outer_stack,
                        });
                        block_results.push(Vec::new());
                        frames.push(TcIFrame::NextInstr { block: body, idx: 0 });
                    }
                }
            }
            TcIFrame::AfterIfThen {
                f_block,
                saved_f_stack,
            } => {
                // Post-true opt_stack becomes the saved_t_stack; swap
                // saved_f_stack into the live opt_stack so the false arm
                // runs against it.
                let saved_t_stack = std::mem::replace(opt_stack, saved_f_stack);
                frames.push(TcIFrame::AfterIfFalse { saved_t_stack });
                block_results.push(Vec::new());
                frames.push(TcIFrame::NextInstr {
                    block: f_block,
                    idx: 0,
                });
            }
            TcIFrame::AfterIfFalse { saved_t_stack } => {
                let f_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if false block" }))?;
                let t_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if true block" }))?;
                // Match the recursive code's unify_stacks(post_true, post_false)
                // so error messages name the same stacks in the same order.
                let post_false = std::mem::replace(opt_stack, saved_t_stack);
                unify_stacks(gas, opt_stack, post_false)?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if parent" }))?
                    .push(Instruction::If(t_instructions, f_instructions));
            }
            TcIFrame::AfterIfNoneNone {
                some_block,
                saved_some_stack,
            } => {
                let saved_none_stack = std::mem::replace(opt_stack, saved_some_stack);
                frames.push(TcIFrame::AfterIfNoneSome { saved_none_stack });
                block_results.push(Vec::new());
                frames.push(TcIFrame::NextInstr {
                    block: some_block,
                    idx: 0,
                });
            }
            TcIFrame::AfterIfNoneSome { saved_none_stack } => {
                let some_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_none some" }))?;
                let none_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_none none" }))?;
                let post_some = std::mem::replace(opt_stack, saved_none_stack);
                unify_stacks(gas, opt_stack, post_some)?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_none parent" }))?
                    .push(Instruction::IfNone(none_instructions, some_instructions));
            }
            TcIFrame::AfterIfConsCons {
                nil_block,
                saved_nil_stack,
            } => {
                // post-cons live stack saved; install nil stack to run nil.
                let saved_cons_stack = std::mem::replace(opt_stack, saved_nil_stack);
                frames.push(TcIFrame::AfterIfConsNil { saved_cons_stack });
                block_results.push(Vec::new());
                frames.push(TcIFrame::NextInstr {
                    block: nil_block,
                    idx: 0,
                });
            }
            TcIFrame::AfterIfConsNil { saved_cons_stack } => {
                let nil_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_cons nil" }))?;
                let cons_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_cons cons" }))?;
                // Recursive code orders unify(opt_stack=post_nil, cons=post_cons)
                // since the nil arm runs second; mirror that here.
                unify_stacks(gas, opt_stack, saved_cons_stack)?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_cons parent" }))?
                    .push(Instruction::IfCons(cons_instructions, nil_instructions));
            }
            TcIFrame::AfterIfLeftLeft {
                right_block,
                saved_right_stack,
            } => {
                let saved_left_stack = std::mem::replace(opt_stack, saved_right_stack);
                frames.push(TcIFrame::AfterIfLeftRight { saved_left_stack });
                block_results.push(Vec::new());
                frames.push(TcIFrame::NextInstr {
                    block: right_block,
                    idx: 0,
                });
            }
            TcIFrame::AfterIfLeftRight { saved_left_stack } => {
                let right_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_left right" }))?;
                let left_instructions = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_left left" }))?;
                let post_right = std::mem::replace(opt_stack, saved_left_stack);
                unify_stacks(gas, opt_stack, post_right)?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "if_left parent" }))?
                    .push(Instruction::IfLeft(left_instructions, right_instructions));
            }
            TcIFrame::AfterLoop { initial_copy } => {
                let body = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "loop body" }))?;
                unify_stacks(gas, opt_stack, initial_copy)?;
                // Pop the remaining bool (if not failed) and emit I::Loop.
                opt_stack.access_mut(()).ok().map(Stack::pop);
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "loop parent" }))?
                    .push(Instruction::Loop(body));
            }
            TcIFrame::AfterLoopLeft { initial_copy, r_ty } => {
                let body = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "loop_left body" }))?;
                unify_stacks(gas, opt_stack, initial_copy)?;
                *opt_stack.access_mut(TcError::FailNotInTail)?.get_mut(0)? = r_ty;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "loop_left parent" }))?
                    .push(Instruction::LoopLeft(body));
            }
            TcIFrame::AfterIter {
                variant,
                outer_opt_stack,
            } => {
                let body = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "iter body" }))?;
                // The live opt_stack is post-body inner stack; outer was
                // saved separately. Unify them, then restore outer.
                let post_body = std::mem::replace(opt_stack, outer_opt_stack);
                unify_stacks(gas, opt_stack, post_body)?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "iter parent" }))?
                    .push(Instruction::Iter(variant, body));
            }
            TcIFrame::AfterDip {
                opt_height,
                protected,
            } => {
                let body = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "dip body" }))?;
                opt_stack
                    .access_mut(TcError::FailNotInTail)?
                    .append(&mut { protected });
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "dip parent" }))?
                    .push(Instruction::Dip(opt_height, body));
            }
            TcIFrame::AfterSeq => {
                let body = block_results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "seq body" }))?;
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "seq parent" }))?
                    .push(Instruction::Seq(body));
            }
            TcIFrame::AfterLambda {
                in_ty,
                out_ty,
                recursive,
                micheline_code,
                saved_outer_stack,
                saved_self_entrypoints,
                saved_in_view,
                for_push,
                depth_guard,
            } => {
                // Bind the guard locally so it drops on every match-arm exit
                // (Ok finalize OR `?`-propagated Err below). Decrement is
                // implicit via `Drop for LambdaTypecheckDepthGuard`. None for
                // the instruction-level path is a no-op drop.
                let _depth_guard = depth_guard;
                let body_instrs = block_results.pop().ok_or(TcError::InternalError(
                    TcInvariant::EmptyResultStack { expected: "lambda body" },
                ))?;
                // Unify body's resulting stack with [out_ty], matching the
                // recursive `typecheck_lambda` which calls
                // `unify_stacks(gas, stk, tc_stk![out_ty.clone()])`.
                unify_stacks(gas, opt_stack, tc_stk![out_ty.clone()])?;
                // Restore outer context.
                *opt_stack = saved_outer_stack;
                cur_self_entrypoints = saved_self_entrypoints;
                cur_in_view = saved_in_view;
                let code = Rc::from(body_instrs);
                let parent_results = block_results.last_mut().ok_or(TcError::InternalError(
                    TcInvariant::EmptyResultStack {
                        expected: "lambda parent",
                    },
                ))?;
                if for_push {
                    // PUSH-lambda value path: `LambdaRec` needs owned
                    // `in_ty`/`out_ty`, and `Type::new_lambda` below needs
                    // them too -- so the recursive case still clones once.
                    // `Lambda::Lambda` ignores them, leaving the originals
                    // free to move into the stack-type push.
                    let lam = if recursive {
                        Lambda::LambdaRec {
                            micheline_code,
                            code,
                            in_ty: in_ty.clone(),
                            out_ty: out_ty.clone(),
                        }
                    } else {
                        Lambda::Lambda {
                            micheline_code,
                            code,
                        }
                    };
                    // Push the lambda type onto the (restored) outer stack —
                    // the regular PUSH step path does this immediately, but
                    // here it has to wait until the body has been typechecked
                    // because the outer stack was swapped out for the body's
                    // fresh `[in_ty]`. The restored stack was Ok at PUSH step
                    // time (the step handler acquires it via
                    // `access_mut(FailNotInTail)?` before returning), so this
                    // `access_mut` cannot fail in practice; the error code
                    // matches the step-time choice.
                    opt_stack
                        .access_mut(TcError::FailNotInTail)?
                        .push(Type::new_lambda(in_ty, out_ty));
                    parent_results.push(Instruction::Push(Rc::new(TypedValue::Lambda(
                        Closure::Lambda(lam),
                    ))));
                } else {
                    // Instruction-level LAMBDA: the outer stack already
                    // carries the lambda type (pushed at step time), so
                    // `in_ty`/`out_ty` can move straight into `LambdaRec`
                    // instead of being cloned. `Type`'s composite variants
                    // are all `Rc<...>`-backed (ast.rs), so the saving is one
                    // `Rc::clone` (refcount bump) per nested
                    // instruction-level `LAMBDA_REC` rather than a deep walk
                    // — small per call, but cumulative on deep nesting.
                    let lam = if recursive {
                        Lambda::LambdaRec {
                            micheline_code,
                            code,
                            in_ty,
                            out_ty,
                        }
                    } else {
                        Lambda::Lambda {
                            micheline_code,
                            code,
                        }
                    };
                    parent_results.push(Instruction::Lambda(lam));
                }
            }
            TcIFrame::AfterMapBlock {
                kind,
                saved_outer_stack,
            } => {
                let body_instrs = block_results.pop().ok_or(TcError::InternalError(
                    TcInvariant::EmptyResultStack { expected: "map body" },
                ))?;
                // Mirror the recursive `typecheck_map_block` error path:
                //   - body's stack must be Ok (else MapBlockFail);
                //   - top must be `ty2` (else MapBlockEmptyStack);
                //   - rest must equal the outer post-pop stack (else the
                //     stack-shape error from ensure_stacks_eq).
                let body_stack = std::mem::replace(opt_stack, saved_outer_stack);
                let mut body_stack = match body_stack {
                    FailingTypeStack::Ok(s) => s,
                    FailingTypeStack::Failed => return Err(TcError::MapBlockFail),
                };
                let ty2 = body_stack.pop().ok_or(TcError::MapBlockEmptyStack)?;
                // The outer stack cannot be `Failed` here: the MAP arm
                // only fires after a successful `pop!` from an `Ok`
                // outer matching `[.., T::List(..) | T::Option(..) |
                // T::Map(..)]`. Surface a structured `TcInvariant`
                // rather than a panic if the invariant ever breaks.
                let outer = match opt_stack {
                    FailingTypeStack::Ok(s) => s,
                    FailingTypeStack::Failed => {
                        return Err(TcError::InternalError(
                            TcInvariant::OuterStackUnexpectedlyFailed {
                                where_: "AfterMapBlock: outer stack after MAP body",
                            },
                        ))
                    }
                };
                ensure_stacks_eq(gas, outer, &body_stack)?;
                let (instr, wrapped) = match kind {
                    MapKind::List => (
                        Instruction::Map(overloads::Map::List, body_instrs),
                        Type::new_list(ty2),
                    ),
                    MapKind::Option => (
                        Instruction::Map(overloads::Map::Option, body_instrs),
                        Type::new_option(ty2),
                    ),
                    MapKind::Map(kty) => (
                        Instruction::Map(overloads::Map::Map, body_instrs),
                        Type::new_map(kty, ty2),
                    ),
                };
                // `outer` is reborrowed from `opt_stack` for the rewrap;
                // its `Ok` shape was already proven above.
                outer.push(wrapped);
                block_results
                    .last_mut()
                    .ok_or(TcError::InternalError(TcInvariant::EmptyResultStack {
                        expected: "map parent",
                    }))?
                    .push(instr);
            }
        }
    }

    // Driver invariant: when the worklist is exhausted, exactly one
    // (root) accumulator remains. A mismatch is a programmer-invariant
    // violation, surfaced as a structured `TcInvariant` rather than a
    // raw panic — same contract as the per-frame pops cleaned up by the
    // earlier panic-removal pass.
    // Defense-in-depth: keep a debug_assert! so a misimplementation that
    // leaves a non-singleton accumulator trips a clear stack trace in
    // debug builds, on top of the structured Err return for release.
    debug_assert_eq!(
        block_results.len(),
        1,
        "block_results stack not drained: {} entries left",
        block_results.len(),
    );
    if block_results.len() != 1 {
        return Err(TcError::InternalError(TcInvariant::ResultStackLen {
            expected: 1,
            got: block_results.len(),
            where_: "typecheck driver tail",
        }));
    }
    block_results
        .pop()
        .ok_or(TcError::InternalError(TcInvariant::ResultStackLen {
            expected: 1,
            got: 0,
            where_: "typecheck driver tail (after len check)",
        }))
}

macro_rules! nothing_to_none {
    () => {
        Option::None
    };
    ($e:expr) => {
        Option::Some($e)
    };
}

/// Typecheck a single instruction. Assumes passed stack is valid, i.e. doesn't
/// contain illegal types like `set operation` or `contract operation`.
///
/// When `self_entrypoints` is `None`, `SELF` instruction is forbidden (e.g.
/// like in lambdas).
///
/// Entrypoint map is carried as an argument, not as part of context, because it
/// has to be locally overridden during typechecking.
/// Element at `idx` from the top of the type stack. Maps an out-of-range
/// access to a descriptive [`TcError::InternalError`] — unreachable for valid
/// Micheline, since the typechecker establishes the stack shape before access.
fn stack_get(stack: &TypeStack, idx: usize) -> Result<&Type, TcError> {
    stack.get(idx).map_err(|_| {
        TcError::InternalError(TcInvariant::TypeStackTooShort {
            index: idx,
            len: stack.len(),
        })
    })
}

/// Mutable reference to the top of the type stack. Maps an empty stack to
/// [`TcError::InternalError`] — unreachable for valid Micheline.
fn stack_top_mut(stack: &mut TypeStack) -> Result<&mut Type, TcError> {
    stack
        .get_mut(0)
        .map_err(|_| TcError::InternalError(TcInvariant::EmptyTypeStack))
}

pub(crate) fn typecheck_instruction<'a, 'b>(
    i: &'b Micheline<'a>,
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
    in_view: bool,
) -> Result<Instruction<'a>, TcError> {
    typecheck_instruction_with_views(
        i,
        gas,
        self_entrypoints,
        opt_stack,
        in_view,
        TypecheckViews::Disabled,
    )
}

/// Like [typecheck_instruction] but threads `typecheck_views`: `true` at
/// origination validates an embedded `CREATE_CONTRACT`'s child-script views
/// like L1; re-typecheck and runtime callers pass `false`.
fn typecheck_instruction_with_views<'a, 'b>(
    i: &'b Micheline<'a>,
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
    in_view: bool,
    typecheck_views: TypecheckViews,
) -> Result<Instruction<'a>, TcError> {
    let mut results = typecheck(
        std::slice::from_ref(i),
        gas,
        self_entrypoints,
        opt_stack,
        in_view,
        typecheck_views,
    )?;
    Ok(results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "typecheck_instruction produced one" }))?)
}

/// Per instruction step used by the iterative driver. For non recursive
/// arms, returns `StepResult::Done(...)`; for unbounded depth control
/// flow arms (IF family, LOOP family, ITER, DIP, SEQ), returns an Open
/// variant that the driver expands into nested block frames.
fn typecheck_instruction_step<'a, 'b>(
    i: &'b Micheline<'a>,
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
    in_view: bool,
    typecheck_views: TypecheckViews,
) -> Result<StepResult<'a, 'b>, TcError> {
    use Instruction as I;
    use NoMatchingOverloadReason as NMOR;
    use Type as T;

    let stack = opt_stack.access_mut(TcError::FailNotInTail)?;

    // helper to reduce boilerplate. Usage:
    // `pop!()` pops the top of the stack and returns it; if the stack is
    // empty, returns TcError::InternalError via `?`.
    // `pop!(T::Foo)` pops the stack, expects a `T::Foo(x)`, and returns `x`;
    // a non-matching value returns TcError::InternalError via `?`.
    //
    // The variants use `ref` binding + `Clone::clone` rather than
    // destructuring by move because `Type` has a manual `Drop` impl (for
    // iterative destruction of deep types) and Rust forbids moving out
    // of a type with `Drop`. Cloning the field is cheap for the variants
    // we destructure: they all hold an `Rc<...>` whose clone just bumps
    // the refcount.
    macro_rules! pop {
        () => {{
            stack
                .pop()
                .ok_or(TcError::InternalError(TcInvariant::EmptyTypeStackPop))?
        }};
        ($p:path) => {{
            let v = pop!();
            match v {
                #[allow(unused_parens)]
                $p(ref i) => Clone::clone(i),
                _ => {
                    return Err(TcError::InternalError(
                        TcInvariant::TypeMismatchOnPop {
                            expected: stringify!($p),
                        },
                    ))
                }
            }
        }};
    }

    // Error handling when stack isn't matched for an instruction. Two forms:
    // no_overload!(instr, len <n>) -- when stack is too short, where <n> is expected length
    // no_overload!(instr, <expr>) -- otherwise, where <expr> is Into<NoMatchingOverloadReason>,
    // and is optional.
    macro_rules! no_overload {
        ($instr:expr, len $expected_len:expr) => {
            {
                return Err(TcError::NoMatchingOverload {
                    instr: $instr,
                    stack: stack.clone(),
                    reason: Option::Some(NoMatchingOverloadReason::StackTooShort {
                        expected: $expected_len
                    }),
                })
            }
        };
        ($instr:expr$(, $reason:expr)?) => {
            {
                return Err(TcError::NoMatchingOverload {
                    instr: $instr,
                    stack: stack.clone(),
                    reason: nothing_to_none!($($reason.into())?)
                })
            }
        };
    }

    /// Pattern synonym matching any `Micheline` except `Seq`
    macro_rules! micheline_non_seq {
        () => {
            micheline_fields!()
                | micheline_instructions!()
                | micheline_literals!()
                | micheline_types!()
                | micheline_values!()
        };
    }

    /// Pattern synonym matching any number of slice elements _except_ the
    /// number passed as the argument. If the number is suffixed with `seq`, the
    /// pattern will also match the number of elements equal to the argument iff
    /// either of those isn't `Micheline::Seq`. If the number is suffixed with
    /// `last_seq`, only the last argument will get the special treatment for
    /// `Seq`.
    ///
    /// This is useful for the last "catchall" pattern for invalid Micheline to
    /// recover some measure of totality.
    macro_rules! expect_args {
        (0) => {
            [_, ..]
        };
        (1) => {
            [] | [_, _, ..]
        };
        (2) => {
            [] | [_] | [_, _, _, ..]
        };
        (3) => {
            [] | [_] | [_, _] | [_, _, _, _, ..]
        };
        (1 seq) => {
            expect_args!(1) | [micheline_non_seq!()]
        };
        (2 seq) => {
            expect_args!(2)
                | [micheline_non_seq!(), micheline_non_seq!()]
                | [Micheline::Seq(..), micheline_non_seq!()]
                | [micheline_non_seq!(), Micheline::Seq(..)]
        };
        (3 last_seq) => {
            expect_args!(3) | [_, _, micheline_non_seq!()]
        };
    }

    gas.consume(gas::tc_cost::INSTR_STEP)?;

    use Micheline::*;
    use Prim::*;

    macro_rules! unexpected_micheline {
        () => {
            return Err(TcError::UnexpectedMicheline(format!("{i:?}")))
        };
    }

    let stack_slice = stack.as_slice();
    Ok(StepResult::Done(match (i, stack_slice.as_slice()) {
        (
            micheline_types!() | micheline_literals!() | micheline_fields!() | micheline_values!(),
            _,
        ) => unexpected_micheline!(),

        (App(ADD, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Add(overloads::Add::NatNat)
        }
        (App(ADD, [], _), [.., T::Int, T::Int]) => {
            pop!();
            I::Add(overloads::Add::IntInt)
        }
        (App(ADD, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            *stack_top_mut(stack)? = T::Int;
            I::Add(overloads::Add::IntNat)
        }
        (App(ADD, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            I::Add(overloads::Add::NatInt)
        }
        (App(ADD, [], _), [.., T::Mutez, T::Mutez]) => {
            pop!();
            I::Add(overloads::Add::MutezMutez)
        }
        #[cfg(feature = "bls")]
        (App(ADD, [], _), [.., T::Bls12381Fr, T::Bls12381Fr]) => {
            pop!();
            I::Add(overloads::Add::Bls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(ADD, [], _), [.., T::Bls12381G1, T::Bls12381G1]) => {
            pop!();
            I::Add(overloads::Add::Bls12381G1)
        }
        #[cfg(feature = "bls")]
        (App(ADD, [], _), [.., T::Bls12381G2, T::Bls12381G2]) => {
            pop!();
            I::Add(overloads::Add::Bls12381G2)
        }
        (App(ADD, [], _), [.., T::Timestamp, T::Int]) => {
            pop!();
            I::Add(overloads::Add::IntTimestamp)
        }
        (App(ADD, [], _), [.., T::Int, T::Timestamp]) => {
            pop!();
            *stack_top_mut(stack)? = T::Timestamp;
            I::Add(overloads::Add::TimestampInt)
        }
        (App(ADD, [], _), [.., _, _]) => no_overload!(ADD),
        (App(ADD, [], _), [_] | []) => no_overload!(ADD, len 2),
        (App(ADD, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(MUL, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Mul(overloads::Mul::NatNat)
        }
        (App(MUL, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            I::Mul(overloads::Mul::NatInt)
        }
        (App(MUL, [], _), [.., T::Int, T::Int]) => {
            pop!();
            I::Mul(overloads::Mul::IntInt)
        }
        (App(MUL, [], _), [.., T::Mutez, T::Nat]) => {
            pop!();
            I::Mul(overloads::Mul::NatMutez)
        }
        (App(MUL, [], _), [.., T::Nat, T::Int]) => {
            stack.drop_top(2)?;
            stack.push(T::Int);
            I::Mul(overloads::Mul::IntNat)
        }
        (App(MUL, [], _), [.., T::Nat, T::Mutez]) => {
            stack.drop_top(2)?;
            stack.push(T::Mutez);
            I::Mul(overloads::Mul::MutezNat)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Bls12381G1]) => {
            stack.drop_top(2)?;
            stack.push(T::Bls12381G1);
            I::Mul(overloads::Mul::Bls12381G1Bls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Bls12381G2]) => {
            stack.drop_top(2)?;
            stack.push(T::Bls12381G2);
            I::Mul(overloads::Mul::Bls12381G2Bls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Bls12381Fr]) => {
            pop!();
            I::Mul(overloads::Mul::Bls12381FrBls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Nat]) => {
            pop!();
            I::Mul(overloads::Mul::NatBls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Int]) => {
            pop!();
            I::Mul(overloads::Mul::IntBls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Nat, T::Bls12381Fr]) => {
            stack.drop_top(2)?;
            stack.push(T::Bls12381Fr);
            I::Mul(overloads::Mul::Bls12381FrNat)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Int, T::Bls12381Fr]) => {
            stack.drop_top(2)?;
            stack.push(T::Bls12381Fr);
            I::Mul(overloads::Mul::Bls12381FrInt)
        }
        (App(MUL, [], _), [.., _, _]) => no_overload!(MUL),
        (App(MUL, [], _), [_] | []) => no_overload!(MUL, len 2),
        (App(MUL, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EDIV, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Nat, T::Nat));
            I::EDiv(overloads::EDiv::NatNat)
        }
        (App(EDIV, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::NatInt)
        }
        (App(EDIV, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::IntNat)
        }
        (App(EDIV, [], _), [.., T::Int, T::Int]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::IntInt)
        }
        (App(EDIV, [], _), [.., T::Nat, T::Mutez]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Mutez, T::Mutez));
            I::EDiv(overloads::EDiv::MutezNat)
        }
        (App(EDIV, [], _), [.., T::Mutez, T::Mutez]) => {
            pop!();
            *stack_top_mut(stack)? = T::new_option(T::new_pair(T::Nat, T::Mutez));
            I::EDiv(overloads::EDiv::MutezMutez)
        }
        (App(EDIV, [], _), [.., _, _]) => no_overload!(EDIV),
        (App(EDIV, [], _), [_] | []) => no_overload!(EDIV, len 2),
        (App(EDIV, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NEG, [], _), [.., T::Nat]) => {
            *stack_top_mut(stack)? = T::Int;
            I::Neg(overloads::Neg::Nat)
        }
        // NB: stack type doesn't change in these NEG overloads
        (App(NEG, [], _), [.., T::Int]) => I::Neg(overloads::Neg::Int),
        #[cfg(feature = "bls")]
        (App(NEG, [], _), [.., T::Bls12381G1]) => I::Neg(overloads::Neg::Bls12381G1),
        #[cfg(feature = "bls")]
        (App(NEG, [], _), [.., T::Bls12381G2]) => I::Neg(overloads::Neg::Bls12381G2),
        #[cfg(feature = "bls")]
        (App(NEG, [], _), [.., T::Bls12381Fr]) => I::Neg(overloads::Neg::Bls12381Fr),
        (App(NEG, [], _), [.., _]) => no_overload!(NEG),
        (App(NEG, [], _), []) => no_overload!(NEG, len 1),
        (App(NEG, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SUB, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            *stack_top_mut(stack)? = T::Int;
            I::Sub(overloads::Sub::NatNat)
        }
        (App(SUB, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            I::Sub(overloads::Sub::NatInt)
        }
        (App(SUB, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            *stack_top_mut(stack)? = T::Int;
            I::Sub(overloads::Sub::IntNat)
        }
        (App(SUB, [], _), [.., T::Int, T::Int]) => {
            pop!();
            I::Sub(overloads::Sub::IntInt)
        }
        (App(SUB, [], _), [.., T::Int, T::Timestamp]) => {
            pop!();
            *stack_top_mut(stack)? = T::Timestamp;
            I::Sub(overloads::Sub::TimestampInt)
        }
        (App(SUB, [], _), [.., T::Timestamp, T::Timestamp]) => {
            pop!();
            *stack_top_mut(stack)? = T::Int;
            I::Sub(overloads::Sub::TimestampTimestamp)
        }
        (App(SUB, [], _), [.., _, _]) => no_overload!(SUB),
        (App(SUB, [], _), [] | [_]) => no_overload!(SUB, len 2),
        (App(SUB, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SUB_MUTEZ, [], _), [.., T::Mutez, T::Mutez]) => {
            pop!();
            *stack_top_mut(stack)? = Type::new_option(T::Mutez);
            I::SubMutez
        }
        (App(SUB_MUTEZ, [], _), [.., _, _]) => no_overload!(SUB_MUTEZ),
        (App(SUB_MUTEZ, [], _), [] | [_]) => no_overload!(SUB_MUTEZ, len 2),
        (App(SUB_MUTEZ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(AND, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::And(overloads::And::NatNat)
        }
        (App(AND, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            I::And(overloads::And::IntNat)
        }
        (App(AND, [], _), [.., T::Bool, T::Bool]) => {
            pop!();
            I::And(overloads::And::Bool)
        }
        (App(AND, [], _), [.., T::Bytes, T::Bytes]) => {
            pop!();
            I::And(overloads::And::Bytes)
        }
        (App(OR, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Or(overloads::Or::Nat)
        }
        (App(OR, [], _), [.., T::Bool, T::Bool]) => {
            pop!();
            I::Or(overloads::Or::Bool)
        }
        (App(OR, [], _), [.., T::Bytes, T::Bytes]) => {
            pop!();
            I::Or(overloads::Or::Bytes)
        }
        (App(XOR, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Xor(overloads::Xor::Nat)
        }
        (App(XOR, [], _), [.., T::Bool, T::Bool]) => {
            pop!();
            I::Xor(overloads::Xor::Bool)
        }
        (App(XOR, [], _), [.., T::Bytes, T::Bytes]) => {
            pop!();
            I::Xor(overloads::Xor::Bytes)
        }
        (App(prim @ (AND | OR | XOR), [], _), [.., _, _]) => no_overload!(*prim),
        (App(prim @ (AND | OR | XOR), [], _), [_] | []) => no_overload!(*prim, len 2),
        (App(AND | OR | XOR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LSL, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Lsl(overloads::Lsl::Nat)
        }
        (App(LSL, [], _), [.., T::Nat, T::Bytes]) => {
            pop!();
            *stack_top_mut(stack)? = T::Bytes;
            I::Lsl(overloads::Lsl::Bytes)
        }
        (App(LSL, [], _), [.., _, _]) => no_overload!(LSL),
        (App(LSL, [], _), [] | [_]) => no_overload!(LSL, len 2),
        (App(LSL, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LSR, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Lsr(overloads::Lsr::Nat)
        }
        (App(LSR, [], _), [.., T::Nat, T::Bytes]) => {
            pop!();
            *stack_top_mut(stack)? = T::Bytes;
            I::Lsr(overloads::Lsr::Bytes)
        }
        (App(LSR, [], _), [.., _, _]) => no_overload!(LSR),
        (App(LSR, [], _), [] | [_]) => no_overload!(LSR, len 2),
        (App(LSR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NOT, [], _), [.., T::Bool]) => I::Not(overloads::Not::Bool),
        (App(NOT, [], _), [.., T::Int]) => I::Not(overloads::Not::Int),
        (App(NOT, [], _), [.., T::Nat]) => {
            *stack_top_mut(stack)? = T::Int;
            I::Not(overloads::Not::Nat)
        }
        (App(NOT, [], _), [.., T::Bytes]) => I::Not(overloads::Not::Bytes),
        (App(NOT, [], _), [.., _]) => no_overload!(NOT),
        (App(NOT, [], _), []) => no_overload!(NOT, len 1),
        (App(NOT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(DIP, args, _), ..) => {
            let (opt_height, nested) = match args {
                [Int(height), Seq(nested)] => (Option::Some(validate_u10(height)?), nested),
                [Seq(nested)] => (Option::None, nested),
                _ => unexpected_micheline!(),
            };
            let protected_height = opt_height.unwrap_or(1) as usize;
            gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;
            ensure_stack_len(Prim::DIP, stack, protected_height)?;
            let protected = stack.split_off(protected_height);
            return Ok(StepResult::OpenDip {
                body: nested,
                opt_height,
                protected,
            });
        }

        (App(DROP, args, _), ..) => {
            let opt_height = match args {
                [Int(height)] => Option::Some(validate_u10(height)?),
                [] => Option::None,
                _ => unexpected_micheline!(),
            };
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            gas.consume(gas::tc_cost::drop_n(&opt_height)?)?;
            ensure_stack_len(Prim::DROP, stack, drop_height)?;
            stack.drop_top(drop_height)?;
            I::Drop(opt_height)
        }

        // DUP instruction requires an argument that is > 0.
        (App(DUP, [Int(n)], _), ..) if n.is_zero() => return Err(TcError::Dup0),
        (App(DUP, args, _), ..) => {
            let opt_height = match args {
                [Int(height)] => Option::Some(validate_u10(height)?),
                [] => Option::None,
                _ => unexpected_micheline!(),
            };
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            ensure_stack_len(Prim::DUP, stack, dup_height)?;
            let ty = stack_get(stack, dup_height - 1)?;
            ty.ensure_prop(gas, TypeProperty::Duplicable)?;
            stack.push(ty.clone());
            I::Dup(opt_height)
        }

        (App(DIG, [Int(height)], _), ..) => {
            let dig_height = validate_u10(height)?;
            ensure_stack_len(Prim::DIG, stack, dig_height as usize)?;
            gas.consume(gas::tc_cost::dig_n(dig_height as usize)?)?;
            if dig_height > 0 {
                let e = stack.remove(dig_height as usize)?;
                stack.push(e);
            }
            I::Dig(dig_height)
        }
        (App(DIG, [_], _), ..) => unexpected_micheline!(),
        (App(DIG, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(DUG, [Int(height)], _), ..) => {
            let dug_height = validate_u10(height)?;
            gas.consume(gas::tc_cost::dug_n(dug_height as usize)?)?;
            if dug_height > 0 {
                ensure_stack_len(Prim::DUG, stack, dug_height as usize)?;
                let e = pop!();
                stack.insert(dug_height as usize, e)?;
            }
            I::Dug(dug_height)
        }
        (App(DUG, [_], _), ..) => unexpected_micheline!(),
        (App(DUG, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(GT, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Gt
        }
        (App(GT, [], _), [.., t]) => no_overload!(GT, TypesNotEqual(T::Int, (*t).clone())),
        (App(GT, [], _), []) => no_overload!(GT, len 1),
        (App(GT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(GE, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Ge
        }
        (App(GE, [], _), [.., t]) => no_overload!(GE, TypesNotEqual(T::Int, (*t).clone())),
        (App(GE, [], _), []) => no_overload!(GE, len 1),
        (App(GE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EQ, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Eq
        }
        (App(EQ, [], _), [.., t]) => no_overload!(EQ, TypesNotEqual(T::Int, (*t).clone())),
        (App(EQ, [], _), []) => no_overload!(EQ, len 1),
        (App(EQ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NEQ, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Neq
        }
        (App(NEQ, [], _), [.., t]) => no_overload!(NEQ, TypesNotEqual(T::Int, (*t).clone())),
        (App(NEQ, [], _), []) => no_overload!(NEQ, len 1),
        (App(NEQ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LE, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Le
        }
        (App(LE, [], _), [.., t]) => no_overload!(LE, TypesNotEqual(T::Int, (*t).clone())),
        (App(LE, [], _), []) => no_overload!(LE, len 1),
        (App(LE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LT, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = T::Bool;
            I::Lt
        }
        (App(LT, [], _), [.., t]) => no_overload!(LT, TypesNotEqual(T::Int, (*t).clone())),
        (App(LT, [], _), []) => no_overload!(LT, len 1),
        (App(LT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IF, [Seq(nested_t), Seq(nested_f)], _), [.., T::Bool]) => {
            pop!();
            return Ok(StepResult::OpenIf {
                t_block: nested_t,
                f_block: nested_f,
                saved_f_stack: FailingTypeStack::Ok(stack.clone()),
            });
        }
        (App(IF, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF, TypesNotEqual(T::Bool, (*t).clone()))
        }
        (App(IF, [Seq(_), Seq(_)], _), []) => no_overload!(IF, len 1),
        (App(IF, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_NONE, [Seq(when_none), Seq(when_some)], _), [.., T::Option(..)]) => {
            let ty = pop!(T::Option);
            return Ok(StepResult::OpenIfNone {
                none_block: when_none,
                some_block: when_some,
                some_top_ty: ty.as_ref().clone(),
            });
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_NONE, NMOR::ExpectedOption((*t).clone()))
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), []) => no_overload!(IF_NONE, len 1),
        (App(IF_NONE, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_CONS, [Seq(when_cons), Seq(when_nil)], _), [.., T::List(..)]) => {
            let ty = pop!(T::List);
            let elem_ty = ty.as_ref().clone();
            return Ok(StepResult::OpenIfCons {
                cons_block: when_cons,
                nil_block: when_nil,
                list_ty: Type::List(ty),
                cons_top_ty: elem_ty,
            });
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_CONS, NMOR::ExpectedList((*t).clone()))
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), []) => no_overload!(IF_CONS, len 1),
        (App(IF_CONS, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_LEFT, [Seq(when_left), Seq(when_right)], _), [.., T::Or(..)]) => {
            let (tl, tr) = pop!(T::Or).as_ref().clone();
            // The live stack becomes the left arm's stack; the driver
            // builds the right arm's stack by cloning and swapping the top.
            stack.push(tl);
            return Ok(StepResult::OpenIfLeft {
                left_block: when_left,
                right_block: when_right,
                right_top_ty: tr,
            });
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_LEFT, NMOR::ExpectedOr((*t).clone()))
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), []) => no_overload!(IF_LEFT, len 1),
        (App(IF_LEFT, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(INT, [], _), [.., T::Nat]) => {
            *stack_top_mut(stack)? = Type::Int;
            I::Int(overloads::Int::Nat)
        }
        #[cfg(feature = "bls")]
        (App(INT, [], _), [.., T::Bls12381Fr]) => {
            *stack_top_mut(stack)? = Type::Int;
            I::Int(overloads::Int::Bls12381Fr)
        }
        (App(INT, [], _), [.., T::Bytes]) => {
            *stack_top_mut(stack)? = Type::Int;
            I::Int(overloads::Int::Bytes)
        }
        (App(INT, [], _), [.., _]) => no_overload!(INT),
        (App(INT, [], _), []) => no_overload!(INT, len 1),
        (App(INT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NAT, [], _), [.., T::Bytes]) => {
            *stack_top_mut(stack)? = Type::Nat;
            I::Nat
        }
        (App(NAT, [], _), [.., _]) => no_overload!(NAT),
        (App(NAT, [], _), []) => no_overload!(NAT, len 1),
        (App(NAT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(BYTES, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = Type::Bytes;
            I::Bytes(overloads::Bytes::Int)
        }
        (App(BYTES, [], _), [.., T::Nat]) => {
            *stack_top_mut(stack)? = Type::Bytes;
            I::Bytes(overloads::Bytes::Nat)
        }
        (App(BYTES, [], _), [.., _]) => no_overload!(BYTES),
        (App(BYTES, [], _), []) => no_overload!(BYTES, len 1),
        (App(BYTES, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(ABS, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = Type::Nat;
            I::Abs
        }
        (App(ABS, [], _), [.., t]) => no_overload!(ABS, TypesNotEqual(T::Int, (*t).clone())),
        (App(ABS, [], _), []) => no_overload!(ABS, len 1),
        (App(ABS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(ISNAT, [], _), [.., T::Int]) => {
            *stack_top_mut(stack)? = Type::new_option(Type::Nat);
            I::IsNat
        }
        (App(ISNAT, [], _), [.., t]) => no_overload!(ISNAT, TypesNotEqual(T::Int, (*t).clone())),
        (App(ISNAT, [], _), []) => no_overload!(ISNAT, len 1),
        (App(ISNAT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LOOP, [Seq(nested)], _), [.., T::Bool]) => {
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            pop!();
            return Ok(StepResult::OpenLoop {
                body: nested,
                initial_copy: opt_copy,
            });
        }
        (App(LOOP, [Seq(_)], _), [.., ty]) => {
            no_overload!(LOOP, TypesNotEqual(T::Bool, (*ty).clone()))
        }
        (App(LOOP, [Seq(_)], _), []) => no_overload!(LOOP, len 1),
        (App(LOOP, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(LOOP_LEFT, [Seq(nested)], _), [.., T::Or(_)]) => {
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            let (l_ty, r_ty) = pop!(T::Or).as_ref().clone();
            stack.push(l_ty);
            return Ok(StepResult::OpenLoopLeft {
                body: nested,
                initial_copy: opt_copy,
                r_ty,
            });
        }
        (App(LOOP_LEFT, [Seq(_)], _), [.., ty]) => {
            no_overload!(LOOP_LEFT, NMOR::ExpectedOr((*ty).clone()))
        }
        (App(LOOP_LEFT, [Seq(_)], _), []) => no_overload!(LOOP_LEFT, len 1),
        (App(LOOP_LEFT, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(ITER, [Seq(nested)], ..), [.., T::List(..)]) => {
            let ty = pop!(T::List);
            // Save the post pop outer stack, then push the element type so
            // the live stack becomes the inner (body) stack.
            let outer_opt_stack = FailingTypeStack::Ok(stack.clone());
            stack.push(ty.as_ref().clone());
            return Ok(StepResult::OpenIter {
                body: nested,
                variant: overloads::Iter::List,
                outer_opt_stack,
            });
        }
        (App(ITER, [Seq(nested)], _), [.., T::Set(..)]) => {
            let ty = pop!(T::Set);
            let outer_opt_stack = FailingTypeStack::Ok(stack.clone());
            stack.push(ty.as_ref().clone());
            return Ok(StepResult::OpenIter {
                body: nested,
                variant: overloads::Iter::Set,
                outer_opt_stack,
            });
        }
        (App(ITER, [Seq(nested)], _), [.., T::Map(..)]) => {
            let kty_vty_box = pop!(T::Map);
            let outer_opt_stack = FailingTypeStack::Ok(stack.clone());
            stack.push(T::Pair(kty_vty_box));
            return Ok(StepResult::OpenIter {
                body: nested,
                variant: overloads::Iter::Map,
                outer_opt_stack,
            });
        }
        (App(ITER, [Seq(_)], _), [.., _]) => no_overload!(ITER),
        (App(ITER, [Seq(_)], _), []) => no_overload!(ITER, len 1),
        (App(ITER, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(MAP, [Seq(nested_instrs)], ..), [.., T::List(..)]) => {
            // Pop the source container; the body runs on a clone of the
            // post-pop stack with the inner type pushed. AfterMapBlock
            // restores the outer stack and rewraps with new_list(ty2).
            let ty1 = pop!(T::List).as_ref().clone();
            return Ok(StepResult::OpenMapBlock {
                body: nested_instrs,
                inner_ty: ty1,
                kind: MapKind::List,
            });
        }
        (App(MAP, [Seq(nested_instrs)], ..), [.., T::Option(..)]) => {
            let ty1 = pop!(T::Option).as_ref().clone();
            return Ok(StepResult::OpenMapBlock {
                body: nested_instrs,
                inner_ty: ty1,
                kind: MapKind::Option,
            });
        }
        (App(MAP, [Seq(nested_instrs)], ..), [.., T::Map(..)]) => {
            let (kty, ty1) = pop!(T::Map).as_ref().clone();
            return Ok(StepResult::OpenMapBlock {
                body: nested_instrs,
                inner_ty: T::new_pair(kty.clone(), ty1),
                kind: MapKind::Map(kty),
            });
        }
        (App(MAP, [Seq(_)], _), [.., _]) => no_overload!(MAP),
        (App(MAP, [Seq(_)], _), []) => no_overload!(MAP, len 1),
        (App(MAP, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(PUSH, [t, v], _), ..) => {
            let t = parse_ty(gas, t)?;
            t.ensure_prop(gas, TypeProperty::Pushable)?;
            // PUSH-of-lambda is routed through the OpenLambda/AfterLambda
            // worklist so a chain of nested `PUSH (lambda T1 T2) { PUSH
            // (lambda …) { … } ; DROP }` does not grow the Rust call stack
            // one frame per nesting level (L2-1431 revalidation,
            // francois.thire 2026-05-29 — the L1-parity depth guard caps at
            // 10 000 but the kernel's ~1 MiB Rust stack overflows around
            // depth 100). Other pushable types fall through to the existing
            // synchronous `typecheck_value`, which is bounded by its own
            // TvFrame worklist — no Rust-stack growth on deep Pair/Or/etc.
            match (&t, v) {
                (T::Lambda(tys), raw @ (Seq(instrs) | App(Prim::Lambda_rec, [Seq(instrs)], _))) => {
                    let (in_ty, out_ty) = tys.as_ref();
                    // Mirror `typecheck_value`'s value-level Lambda arm: take
                    // the RAII depth guard so the PUSH path stays within the
                    // same 10 000-deep envelope as the non-PUSH value paths.
                    // The guard is moved into `OpenLambda` and threaded into
                    // `AfterLambda`; its Drop runs whether AfterLambda
                    // finalizes normally, propagates `?` Err mid-finalize, or
                    // is torn down with `frames: Vec<TcIFrame>` on the
                    // driver's Err exit path. Without this RAII threading, a
                    // failure inside the body (unify mismatch, ill-typed
                    // sub-instruction, OOG) would leak the increment to
                    // subsequent operations on the same kernel thread, mis-
                    // tripping `TypecheckingTooManyRecursiveCalls`.
                    let depth_guard = LambdaTypecheckDepthGuard::enter()?;
                    let recursive = matches!(raw, App(Prim::Lambda_rec, ..));
                    let micheline_code = Micheline::Seq(instrs);
                    let in_ty = in_ty.clone();
                    let out_ty = out_ty.clone();
                    return Ok(StepResult::OpenLambda {
                        body: instrs,
                        in_ty,
                        out_ty,
                        recursive,
                        micheline_code,
                        for_push: true,
                        depth_guard: Option::Some(depth_guard),
                    });
                }
                _ => {
                    // contracts and big maps are not pushable so it's OK to typecheck values using default
                    let mut ctx = crate::context::PushableTypecheckingContext { gas };
                    let v = typecheck_value_with_views(v, &mut ctx, &t, typecheck_views)?;
                    stack.push(t);
                    I::Push(Rc::new(v))
                }
            }
        }
        (App(PUSH, expect_args!(2), _), _) => unexpected_micheline!(),

        (App(SWAP, [], _), [.., _, _]) => {
            stack.swap(0, 1)?;
            I::Swap
        }
        (App(SWAP, [], _), [] | [_]) => no_overload!(SWAP, len 2),
        (App(SWAP, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(FAILWITH, [], _), [.., _]) => {
            let ty = pop!();
            // NB: the docs for the FAILWITH instruction
            // https://tezos.gitlab.io/michelson-reference/#instr-FAILWITH claim
            // the type needs to be packable, but that's not quite correct, as
            // `contract _` is forbidden. The correct constraint is seemingly
            // "pushable", as "pushable" is just "packable" without `contract _`
            ty.ensure_prop(gas, TypeProperty::Pushable)?;
            // mark stack as failed
            *opt_stack = FailingTypeStack::Failed;
            I::Failwith(ty)
        }
        (App(FAILWITH, [], _), []) => no_overload!(FAILWITH, len 1),
        (App(FAILWITH, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NEVER, [], _), [.., T::Never]) => {
            *opt_stack = FailingTypeStack::Failed;
            I::Never
        }
        (App(NEVER, [], _), [.., t]) => {
            no_overload!(NEVER, TypesNotEqual(T::Never, (*t).clone()))
        }
        (App(NEVER, [], _), []) => no_overload!(NEVER, len 1),
        (App(NEVER, ..), _) => unexpected_micheline!(),

        (App(UNIT, [], _), ..) => {
            stack.push(T::Unit);
            I::Unit
        }
        (App(UNIT, ..), _) => unexpected_micheline!(),

        (App(CAR, [], _), [.., T::Pair(..)]) => {
            let l = pop!(T::Pair).0.clone();
            stack.push(l);
            I::Car
        }
        (App(CAR, [], _), [.., ty]) => no_overload!(CAR, NMOR::ExpectedPair((*ty).clone())),
        (App(CAR, [], _), []) => no_overload!(CAR, len 1),
        (App(CAR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CDR, [], _), [.., T::Pair(..)]) => {
            let r = pop!(T::Pair).1.clone();
            stack.push(r);
            I::Cdr
        }
        (App(CDR, [], _), [.., ty]) => no_overload!(CDR, NMOR::ExpectedPair((*ty).clone())),
        (App(CDR, [], _), []) => no_overload!(CDR, len 1),
        (App(CDR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(PAIR, [], _), [.., _, _]) => {
            let (l, r) = (pop!(), pop!());
            stack.push(Type::new_pair(l, r));
            I::Pair
        }
        (App(PAIR, [], _), [] | [_]) => no_overload!(PAIR, len 2),
        (App(PAIR, [Micheline::Int(n)], _), _) => {
            let n = validate_u10(n)?;
            if n < 2 {
                return Err(TcError::PairN01(PAIR, n));
            }
            if stack.len() < n as usize {
                no_overload!(PAIR, len n as usize);
            }
            gas.consume(tc_cost::pair_n(n as usize)?)?;
            let res = stack
                .drain_top(n as usize)?
                .rev()
                .reduce(|acc, e| Type::new_pair(e, acc))
                .ok_or(TcError::InternalError(TcInvariant::EmptyPairN { n }))?;
            stack.push(res);
            I::PairN(n)
        }
        (App(PAIR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UNPAIR, [], _), [.., T::Pair(..)]) => {
            let (l, r) = pop!(T::Pair).as_ref().clone();
            stack.push(r);
            stack.push(l);
            I::Unpair
        }
        (App(UNPAIR, [], _), [.., ty]) => no_overload!(UNPAIR, NMOR::ExpectedPair((*ty).clone())),
        (App(UNPAIR, [], _), []) => no_overload!(UNPAIR, len 1),
        (App(UNPAIR, [Micheline::Int(n)], _), [.., _]) => {
            let n = validate_u10(n)?;
            if n < 2 {
                return Err(TcError::PairN01(UNPAIR, n));
            }
            gas.consume(tc_cost::unpair_n(n as usize)?)?;
            stack.reserve(n as usize);
            let p = pop!();
            // Walk n-1 levels down the right spine of p, collecting left
            // children. After the loop, current points at the deepest
            // right element. Iterative form of the previous recursive fn;
            // bounded by n <= 1023 but flattened so the recursion is not
            // a latent WASM stack consumer.
            let mut current: &Type = &p;
            let mut lefts: Vec<Type> = Vec::with_capacity((n - 1) as usize);
            for _ in 0..(n - 1) {
                match current {
                    Type::Pair(p) => {
                        lefts.push(p.0.clone());
                        current = &p.1;
                    }
                    other => {
                        return Err(TcError::NoMatchingOverload {
                            instr: UNPAIR,
                            stack: stack.clone(),
                            reason: Option::Some(NMOR::ExpectedPair(other.clone())),
                        });
                    }
                }
            }
            // Original recursion pushed the deepest right first, then each
            // left as it unwound. Mirror that order.
            stack.push(current.clone());
            for ty in lefts.into_iter().rev() {
                stack.push(ty);
            }
            I::UnpairN(n)
        }
        (App(UNPAIR, [Micheline::Int(_)], _), []) => no_overload!(UNPAIR, len 1),
        (App(UNPAIR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SOME, [], _), [.., _]) => {
            let ty = pop!();
            stack.push(T::new_option(ty));
            I::ISome
        }
        (App(SOME, [], _), []) => no_overload!(SOME, len 1),
        (App(SOME, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NONE, [ty], _), _) => {
            let ty = parse_ty(gas, ty)?;
            stack.push(T::new_option(ty));
            I::None
        }
        (App(NONE, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(COMPARE, [], _), [.., u, t]) => {
            ensure_ty_eq(gas, t, u).map_err(|e| match e {
                TcError::TypesNotEqual(e) => TcError::NoMatchingOverload {
                    instr: Prim::COMPARE,
                    stack: stack.clone(),
                    reason: Option::Some(e.into()),
                },
                e => e,
            })?;
            t.ensure_prop(gas, TypeProperty::Comparable)?;
            pop!();
            *stack_top_mut(stack)? = T::Int;
            I::Compare
        }
        (App(COMPARE, [], _), [] | [_]) => no_overload!(COMPARE, len 2),
        (App(COMPARE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(AMOUNT, [], _), ..) => {
            stack.push(T::Mutez);
            I::Amount
        }
        (App(AMOUNT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NIL, [ty], _), ..) => {
            let ty = parse_ty(gas, ty)?;
            stack.push(T::new_list(ty));
            I::Nil
        }
        (App(NIL, ..), _) => unexpected_micheline!(),

        (App(CONS, [], _), [.., T::List(ty1), ty2]) => {
            ensure_ty_eq(gas, ty1, ty2)?;
            pop!();
            I::Cons
        }
        (App(CONS, [], _), [.., ty, _]) => no_overload!(CONS, NMOR::ExpectedList((*ty).clone())),
        (App(CONS, [], _), [] | [_]) => no_overload!(CONS, len 2),
        (App(CONS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CONCAT, [], _), [.., T::String, T::String]) => {
            pop!();
            I::Concat(overloads::Concat::TwoStrings)
        }
        (App(CONCAT, [], _), [.., T::Bytes, T::Bytes]) => {
            pop!();
            I::Concat(overloads::Concat::TwoBytes)
        }
        (App(CONCAT, [], _), [.., T::List(ty)]) => {
            let ty = ty.as_ref();
            let overload = match ty {
                T::String => overloads::Concat::ListOfStrings,
                T::Bytes => overloads::Concat::ListOfBytes,
                _ => no_overload!(CONCAT),
            };
            *stack_top_mut(stack)? = ty.clone(); // cheap clone, `ty` is either `String` or `Bytes`
            I::Concat(overload)
        }
        (App(CONCAT, [], _), [.., _]) => no_overload!(CONCAT),
        (App(CONCAT, [], _), []) => no_overload!(CONCAT, len 1),
        (App(CONCAT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EMPTY_SET, [ty], _), _) => {
            let ty = parse_ty(gas, ty)?;
            ty.ensure_prop(gas, TypeProperty::Comparable)?;
            stack.push(T::new_set(ty));
            I::EmptySet
        }
        (App(EMPTY_SET, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(EMPTY_MAP, [kty, vty], _), _) => {
            let kty = parse_ty(gas, kty)?;
            kty.ensure_prop(gas, TypeProperty::Comparable)?;
            let vty = parse_ty(gas, vty)?;
            stack.push(T::new_map(kty, vty));
            I::EmptyMap
        }
        (App(EMPTY_MAP, expect_args!(2), _), _) => unexpected_micheline!(),

        (App(EMPTY_BIG_MAP, [kty, vty], _), _) => {
            let kty = parse_ty(gas, kty)?;
            kty.ensure_prop(gas, TypeProperty::Comparable)?;
            let vty = parse_ty(gas, vty)?;
            vty.ensure_prop(gas, TypeProperty::BigMapValue)?;
            stack.push(T::new_big_map(kty.clone(), vty.clone()));
            I::EmptyBigMap(kty, vty)
        }
        (App(EMPTY_BIG_MAP, expect_args!(2), _), _) => unexpected_micheline!(),

        (App(MEM, [], _), [.., T::Set(..), _]) => {
            let ty_ = pop!();
            let ty = pop!(T::Set);
            ensure_ty_eq(gas, &ty, &ty_)?;
            stack.push(T::Bool);
            I::Mem(overloads::Mem::Set)
        }
        (App(MEM, [], _), [.., T::Map(..), _]) => {
            let kty_ = pop!();
            let map_tys = pop!(T::Map);
            ensure_ty_eq(gas, &map_tys.0, &kty_)?;
            stack.push(T::Bool);
            I::Mem(overloads::Mem::Map)
        }
        (App(MEM, [], _), [.., T::BigMap(..), _]) => {
            let kty_ = pop!();
            let map_tys = pop!(T::BigMap);
            ensure_ty_eq(gas, &map_tys.0, &kty_)?;
            stack.push(T::Bool);
            I::Mem(overloads::Mem::BigMap)
        }
        (App(MEM, [], _), [.., _, _]) => no_overload!(MEM),
        (App(MEM, [], _), [] | [_]) => no_overload!(MEM, len 2),
        (App(MEM, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(GET, [], _), [.., T::Map(..), _]) => {
            let kty_ = pop!();
            let map_tys = pop!(T::Map);
            ensure_ty_eq(gas, &map_tys.0, &kty_)?;
            stack.push(T::new_option(map_tys.1.clone()));
            I::Get(overloads::Get::Map)
        }
        (App(GET, [], _), [.., T::BigMap(..), _]) => {
            let kty_ = pop!();
            let map_tys = pop!(T::BigMap);
            ensure_ty_eq(gas, &map_tys.0, &kty_)?;
            stack.push(T::new_option(map_tys.1.clone()));
            I::Get(overloads::Get::BigMap)
        }
        (App(GET, [], _), [.., _, _]) => no_overload!(GET),
        (App(GET, [], _), [] | [_]) => no_overload!(GET, len 2),

        (App(GET, [Micheline::Int(n)], _), [.., _]) => {
            // NB: it's important to NOT pop from the stack here, otherwise
            // no_overload! below won't report the type on the top of the stack.
            let ty = stack_top_mut(stack)?;
            let n = validate_u10(n)?;
            gas.consume(tc_cost::get_n(n as usize)?)?;
            let res = match get_nth_field_ref(n, ty) {
                Ok(res) => res,
                Err(ty) => no_overload!(GET, NMOR::ExpectedPair(ty)),
            };
            // this is a bit hacky, but borrow rules leave few other options
            let extracted = std::mem::replace(res, T::Unit);
            *ty = extracted;
            I::GetN(n)
        }
        (App(GET, [Micheline::Int(_)], _), []) => no_overload!(GET, len 1),

        (App(GET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UPDATE, [], _), [.., T::Set(ty), T::Bool, ty_]) => {
            ensure_ty_eq(gas, ty, ty_)?;
            stack.drop_top(2)?;
            I::Update(overloads::Update::Set)
        }
        (App(UPDATE, [], _), [.., T::Map(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            stack.drop_top(2)?;
            I::Update(overloads::Update::Map)
        }
        (App(UPDATE, [], _), [.., T::BigMap(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            stack.drop_top(2)?;
            I::Update(overloads::Update::BigMap)
        }
        (App(UPDATE, [], _), [.., _, _, _]) => no_overload!(UPDATE),
        (App(UPDATE, [], _), [] | [_] | [_, _]) => no_overload!(UPDATE, len 3),

        (App(UPDATE, [Micheline::Int(n)], _), [.., _, _]) => {
            let n = validate_u10(n)?;
            let new_val = pop!();
            let old_val = match get_nth_field_ref(n, stack_top_mut(stack)?) {
                Ok(res) => res,
                Err(ty) => {
                    // restores the initial stack
                    stack.push(new_val);
                    no_overload!(UPDATE, NMOR::ExpectedPair(ty));
                }
            };
            *old_val = new_val;
            I::UpdateN(n)
        }
        (App(UPDATE, [Micheline::Int(_)], _), [] | [_]) => no_overload!(UPDATE, len 2),

        (App(UPDATE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(GET_AND_UPDATE, [], _), [.., T::Map(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            pop!();
            I::GetAndUpdate(overloads::GetAndUpdate::Map)
        }
        (App(GET_AND_UPDATE, [], _), [.., T::BigMap(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            pop!();
            I::GetAndUpdate(overloads::GetAndUpdate::BigMap)
        }
        (App(GET_AND_UPDATE, [], _), [.., _, _, _]) => no_overload!(GET_AND_UPDATE),
        (App(GET_AND_UPDATE, [], _), [] | [_] | [_, _]) => no_overload!(GET_AND_UPDATE, len 3),
        (App(GET_AND_UPDATE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SIZE, [], _), [.., T::String]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::Size(overloads::Size::String)
        }
        (App(SIZE, [], _), [.., T::Bytes]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::Size(overloads::Size::Bytes)
        }
        (App(SIZE, [], _), [.., T::List(_)]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::Size(overloads::Size::List)
        }
        (App(SIZE, [], _), [.., T::Set(_)]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::Size(overloads::Size::Set)
        }
        (App(SIZE, [], _), [.., T::Map(..)]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::Size(overloads::Size::Map)
        }
        (App(SIZE, [], _), [.., _]) => no_overload!(SIZE),
        (App(SIZE, [], _), []) => no_overload!(SIZE, len 1),
        (App(SIZE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CHAIN_ID, [], _), ..) => {
            stack.push(T::ChainId);
            I::ChainId
        }
        (App(CHAIN_ID, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SELF, [], anns), ..) => {
            let entrypoint = anns
                .get_single_field_ann()?
                .map(Entrypoint::try_from)
                .transpose()
                .map_err(TcError::EntrypointError)?
                .unwrap_or_default();
            stack.push(T::new_contract(
                self_entrypoints
                    .ok_or(TcError::SelfForbidden)?
                    .get(&entrypoint)
                    .ok_or_else(|| TcError::NoSuchEntrypoint(entrypoint.clone()))?
                    .clone(),
            ));
            I::ISelf(entrypoint)
        }
        (App(SELF, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(ADDRESS, [], _), [.., T::Contract(..)]) => {
            let _ = *pop!(T::Contract);
            stack.push(T::Address);
            I::Address
        }
        (App(ADDRESS, [], _), [.., _]) => no_overload!(ADDRESS),
        (App(ADDRESS, [], _), []) => no_overload!(ADDRESS, len 1),
        (App(ADDRESS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(PACK, [], _), [.., _]) => {
            let t = pop!();
            t.ensure_prop(gas, TypeProperty::Packable)?;
            stack.push(T::Bytes);
            I::Pack
        }
        (App(PACK, [], _), []) => no_overload!(PACK, len 1),
        (App(PACK, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UNPACK, [ty], _), [.., T::Bytes]) => {
            let ty = parse_ty(gas, ty)?;
            // NB: one would suppose the type needs to be packable, but that's
            // not quite correct, as `contract _` is forbidden. The correct
            // constraint is seemingly "pushable", as "pushable" is just
            // "packable" without `contract _`
            ty.ensure_prop(gas, TypeProperty::Pushable)?;
            *stack_top_mut(stack)? = T::new_option(ty.clone());
            I::Unpack(ty)
        }
        (App(UNPACK, [_], _), [.., ty]) => {
            no_overload!(UNPACK, TypesNotEqual(T::Bytes, (*ty).clone()))
        }
        (App(UNPACK, [_], _), []) => no_overload!(UNPACK, len 1),
        (App(UNPACK, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(TRANSFER_TOKENS, [], _), _) if in_view => {
            Err(TcError::ForbiddenInView(Prim::TRANSFER_TOKENS))?
        }
        (App(TRANSFER_TOKENS, [], _), [.., T::Contract(ct), T::Mutez, arg_t]) => {
            ensure_ty_eq(gas, ct, arg_t)?;
            stack.drop_top(3)?;
            stack.push(T::Operation);
            I::TransferTokens
        }
        (App(TRANSFER_TOKENS, [], _), [.., _, _, _]) => no_overload!(TRANSFER_TOKENS),
        (App(TRANSFER_TOKENS, [], _), [] | [_] | [_, _]) => no_overload!(TRANSFER_TOKENS, len 3),
        (App(TRANSFER_TOKENS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SET_DELEGATE, [], _), _) if in_view => {
            Err(TcError::ForbiddenInView(Prim::SET_DELEGATE))?
        }
        (App(SET_DELEGATE, [], _), [.., T::Option(ot)]) if matches!(ot.as_ref(), T::KeyHash) => {
            pop!();
            stack.push(T::Operation);
            I::SetDelegate
        }
        (App(SET_DELEGATE, [], _), [.., _]) => no_overload!(SET_DELEGATE),
        (App(SET_DELEGATE, [], _), []) => no_overload!(SET_DELEGATE, len 1),
        (App(SET_DELEGATE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CHECK_SIGNATURE, [], _), [.., T::Bytes, T::Signature, T::Key]) => {
            stack.drop_top(2)?;
            *stack_top_mut(stack)? = T::Bool;
            I::CheckSignature
        }
        (App(CHECK_SIGNATURE, [], _), [.., _, _, _]) => no_overload!(CHECK_SIGNATURE),
        (App(CHECK_SIGNATURE, [], _), [] | [_] | [_, _]) => no_overload!(CHECK_SIGNATURE, len 3),
        (App(CHECK_SIGNATURE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SLICE, [], _), [.., T::String, T::Nat, T::Nat]) => {
            stack.drop_top(2)?;
            *stack_top_mut(stack)? = T::new_option(T::String);
            I::Slice(overloads::Slice::String)
        }
        (App(SLICE, [], _), [.., T::Bytes, T::Nat, T::Nat]) => {
            stack.drop_top(2)?;
            *stack_top_mut(stack)? = T::new_option(T::Bytes);
            I::Slice(overloads::Slice::Bytes)
        }
        (App(SLICE, [], _), [.., _, _, _]) => no_overload!(SLICE),
        (App(SLICE, [], _), _) => no_overload!(SLICE, len 3),
        (App(SLICE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LEFT, [ty_right], _), [.., _]) => {
            let ty_left = pop!();
            let ty_right = parse_ty(gas, ty_right)?;
            stack.push(T::new_or(ty_left, ty_right));
            I::Left
        }
        (App(LEFT, [_ty_right], _), []) => no_overload!(LEFT, len 1),
        (App(LEFT, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(RIGHT, [ty_left], _), [.., _]) => {
            let ty_right = pop!();
            let ty_left = parse_ty(gas, ty_left)?;
            stack.push(T::new_or(ty_left, ty_right));
            I::Right
        }
        (App(RIGHT, [_ty_left], _), []) => no_overload!(RIGHT, len 1),
        (App(RIGHT, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(prim @ (LAMBDA | LAMBDA_REC), [ty1, ty2, Seq(instrs)], _), ..) => {
            let in_ty = parse_ty(gas, ty1)?;
            let out_ty = parse_ty(gas, ty2)?;
            // Push the lambda's resulting type onto the outer stack now;
            // the AfterLambda frame will keep this when it restores the
            // outer context after the body has been typechecked through
            // the worklist driver. Avoids recursing into typecheck_lambda
            // (and from there back into typecheck) on every nested LAMBDA.
            stack.push(Type::new_lambda(in_ty.clone(), out_ty.clone()));
            let micheline_code = Micheline::Seq(instrs);
            return Ok(StepResult::OpenLambda {
                body: instrs,
                in_ty,
                out_ty,
                recursive: matches!(prim, LAMBDA_REC),
                micheline_code,
                for_push: false,
                depth_guard: Option::None,
            });
        }
        (App(LAMBDA | LAMBDA_REC, expect_args!(3 last_seq), _), _) => unexpected_micheline!(),

        (App(EXEC, [], _), [.., T::Lambda(_), _]) => {
            let ty = pop!();
            let lam_tys = pop!(T::Lambda);
            ensure_ty_eq(gas, &lam_tys.0, &ty)?;
            stack.push(lam_tys.1.clone());
            I::Exec
        }
        (App(EXEC, [], _), [.., _, _]) => no_overload!(EXEC),
        (App(EXEC, [], _), [] | [_]) => no_overload!(EXEC, len 2),
        (App(EXEC, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(HASH_KEY, [], _), [.., T::Key]) => {
            *stack_top_mut(stack)? = T::KeyHash;
            I::HashKey
        }
        (App(HASH_KEY, [], _), [.., t]) => {
            no_overload!(HASH_KEY, TypesNotEqual(T::Key, (*t).clone()))
        }
        (App(HASH_KEY, [], _), []) => no_overload!(HASH_KEY, len 1),
        (App(HASH_KEY, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(APPLY, [], _), [.., T::Lambda(_), _]) => {
            let ty = pop!();
            let lam_ty = pop!(T::Lambda);
            let pair_ty = match &lam_ty.0 {
                T::Pair(p) => p,
                t => {
                    return Err(TcError::NoMatchingOverload {
                        instr: APPLY,
                        stack: stack.clone(),
                        reason: Option::Some(NMOR::ExpectedPair((*t).clone())),
                    });
                }
            };
            ensure_ty_eq(gas, &pair_ty.0, &ty)?;
            // NB: L1's APPLY checks the captured type is packable, but with
            // `contract _` forbidden (`check_packable ~allow_contract:false`).
            // The correct constraint is seemingly "pushable", as "pushable" is
            // just "packable" without `contract _`
            ty.ensure_prop(gas, TypeProperty::Pushable)?;
            stack.push(T::new_lambda(pair_ty.1.clone(), lam_ty.1.clone()));
            I::Apply { arg_ty: ty }
        }
        (App(APPLY, [], _), [.., _, _]) => no_overload!(APPLY),
        (App(APPLY, [], _), [] | [_]) => no_overload!(APPLY, len 2),
        (App(APPLY, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(TICKET, [], _), [.., T::Nat, _]) => {
            let content_ty = pop!();
            // The content of a ticket must be comparable; this mirrors the
            // written `ticket t` parser (BuildKind::Ticket), which also
            // enforces Comparable. Without this check, MIR would accept e.g.
            // `ticket (ticket string)`, which L1 rejects ("comparable type
            // expected"). Such nested tickets also enable ticket duplication
            // via READ_TICKET.
            content_ty.ensure_prop(gas, TypeProperty::Comparable)?;
            *stack_top_mut(stack)? = T::new_option(T::new_ticket(content_ty.clone()));
            I::Ticket(content_ty)
        }
        (App(TICKET, [], _), [.., _, _]) => no_overload!(TICKET),
        (App(TICKET, [], _), [] | [_]) => no_overload!(TICKET, len 2),
        (App(TICKET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(READ_TICKET, [], _), [.., T::Ticket(t)]) => {
            stack.push(T::new_pair(
                T::Address,
                T::new_pair(t.as_ref().clone(), T::Nat),
            ));
            I::ReadTicket
        }
        (App(READ_TICKET, [], _), [.., _]) => no_overload!(READ_TICKET),
        (App(READ_TICKET, [], _), []) => no_overload!(READ_TICKET, len 1),
        (App(READ_TICKET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SPLIT_TICKET, [], _), [.., T::Pair(n), T::Ticket(_)])
            if matches!(n.as_ref(), (T::Nat, T::Nat)) =>
        {
            let typ = pop!();
            *stack_top_mut(stack)? = Type::new_option(Type::new_pair(typ.clone(), typ));
            I::SplitTicket
        }
        (App(SPLIT_TICKET, [], _), [.., _, _]) => no_overload!(SPLIT_TICKET),
        (App(SPLIT_TICKET, [], _), [] | [_]) => no_overload!(SPLIT_TICKET, len 2),
        (App(SPLIT_TICKET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(JOIN_TICKETS, [], _), [.., T::Pair(tickets)])
            if matches!(tickets.as_ref(), (Type::Ticket(_), Type::Ticket(_))) =>
        {
            let Type::Ticket(lt) = &tickets.0 else {
                return Err(TcError::InternalError(
                    TcInvariant::ExpectedTicketLeftJoin,
                ));
            };
            let Type::Ticket(rt) = &tickets.1 else {
                return Err(TcError::InternalError(
                    TcInvariant::ExpectedTicketRightJoin,
                ));
            };

            ensure_ty_eq(gas, lt, rt)?;
            *stack_top_mut(stack)? = Type::new_option(tickets.0.clone());
            I::JoinTickets
        }
        (App(JOIN_TICKETS, [], _), [.., _]) => no_overload!(JOIN_TICKETS),
        (App(JOIN_TICKETS, [], _), []) => no_overload!(JOIN_TICKETS, len 1),
        (App(JOIN_TICKETS, expect_args!(0), _), _) => unexpected_micheline!(),

        // stack type doesn't change in these instructions, so we don't touch it
        (App(BLAKE2B, [], _), [.., T::Bytes]) => I::Blake2b,
        (App(KECCAK, [], _), [.., T::Bytes]) => I::Keccak,
        (App(SHA256, [], _), [.., T::Bytes]) => I::Sha256,
        (App(SHA3, [], _), [.., T::Bytes]) => I::Sha3,
        (App(SHA512, [], _), [.., T::Bytes]) => I::Sha512,
        (App(prim @ (BLAKE2B | KECCAK | SHA256 | SHA3 | SHA512), [], _), [.., t]) => {
            no_overload!(*prim, TypesNotEqual(T::Bytes, (*t).clone()))
        }
        (App(prim @ (BLAKE2B | KECCAK | SHA256 | SHA3 | SHA512), [], _), []) => {
            no_overload!(*prim, len 1)
        }
        (App(BLAKE2B | KECCAK | SHA256 | SHA3 | SHA512, expect_args!(0), _), _) => {
            unexpected_micheline!()
        }

        (App(BALANCE, [], _), ..) => {
            stack.push(T::Mutez);
            I::Balance
        }
        (App(BALANCE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CONTRACT, [t], anns), [.., T::Address]) => {
            pop!();
            let entrypoint = match anns.get_single_field_ann()? {
                Option::None => Option::None,
                Option::Some(field_annot) => match Entrypoint::try_from(field_annot) {
                    Ok(ep) => Option::Some(ep),
                    Err(err) => return Err(TcError::EntrypointError(err)),
                },
            };
            // explicit default entrypoint is forbidden
            if let Option::Some(ep) = &entrypoint {
                if ep.is_default() {
                    return Err(TcError::ExplicitDefaultEntrypointError(CONTRACT));
                }
            }
            // if no entrypoint is specified, default is assumed
            let entrypoint = entrypoint.unwrap_or_default();
            let t = parse_ty(gas, t)?;
            // The argument of contract only needs to be passable; this mirrors
            // the written `contract t` parser, which also enforces Passable.
            // Without this check, MIR would accept e.g. `CONTRACT operation`,
            // which L1 rejects ("operation type forbidden in parameter, ...").
            t.ensure_prop(gas, TypeProperty::Passable)?;
            stack.push(T::new_option(T::new_contract(t.clone())));
            I::Contract(t, entrypoint)
        }
        (App(CONTRACT, [_], _), [.., _]) => no_overload!(CONTRACT),
        (App(CONTRACT, [_], _), []) => no_overload!(CONTRACT, len 1),
        (App(CONTRACT, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(LEVEL, [], _), ..) => {
            stack.push(T::Nat);
            I::Level
        }
        (App(LEVEL, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(MIN_BLOCK_TIME, [], _), ..) => {
            stack.push(T::Nat);
            I::MinBlockTime
        }
        (App(MIN_BLOCK_TIME, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SELF_ADDRESS, [], _), ..) => {
            stack.push(T::Address);
            I::SelfAddress
        }
        (App(SELF_ADDRESS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SENDER, [], _), ..) => {
            stack.push(T::Address);
            I::Sender
        }
        (App(SENDER, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SOURCE, [], _), ..) => {
            stack.push(T::Address);
            I::Source
        }
        (App(SOURCE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NOW, [], _), ..) => {
            stack.push(T::Timestamp);
            I::Now
        }
        (App(NOW, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IMPLICIT_ACCOUNT, [], _), [.., T::KeyHash]) => {
            *stack_top_mut(stack)? = T::new_contract(T::Unit);
            I::ImplicitAccount
        }
        (App(IMPLICIT_ACCOUNT, [], _), [.., _]) => no_overload!(IMPLICIT_ACCOUNT),
        (App(IMPLICIT_ACCOUNT, [], _), []) => no_overload!(IMPLICIT_ACCOUNT, len 1),
        (App(IMPLICIT_ACCOUNT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IS_IMPLICIT_ACCOUNT, [], _), [.., T::Address]) => {
            *stack_top_mut(stack)? = T::new_option(T::KeyHash);
            I::IsImplicitAccount
        }
        (App(IS_IMPLICIT_ACCOUNT, [], _), [.., _]) => no_overload!(IS_IMPLICIT_ACCOUNT),
        (App(IS_IMPLICIT_ACCOUNT, [], _), []) => no_overload!(IS_IMPLICIT_ACCOUNT, len 1),
        (App(IS_IMPLICIT_ACCOUNT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(TOTAL_VOTING_POWER, [], _), ..) => {
            stack.push(T::Nat);
            I::TotalVotingPower
        }
        (App(TOTAL_VOTING_POWER, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(VOTING_POWER, [], _), [.., T::KeyHash]) => {
            *stack_top_mut(stack)? = T::Nat;
            I::VotingPower
        }
        (App(VOTING_POWER, [], _), [.., _]) => no_overload!(VOTING_POWER),
        (App(VOTING_POWER, [], _), []) => no_overload!(VOTING_POWER, len 1),
        (App(VOTING_POWER, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EMIT, [t], anns), [.., _]) => {
            let emit_val_type = pop!();
            let emit_type_arg = parse_ty(gas, t)?;
            ensure_ty_eq(gas, &emit_type_arg, &emit_val_type)?;
            // NB: the docs for the EMIT instruction
            // https://tezos.gitlab.io/michelson-reference/#instr-EMIT claim
            // the type needs to be packable, but that's not quite correct, as
            // `contract _` is forbidden. The correct constraint is seemingly
            // "pushable", as "pushable" is just "packable" without `contract _`
            emit_val_type.ensure_prop(gas, TypeProperty::Pushable)?;
            let opt_tag = anns.get_single_field_ann()?;
            if let Option::Some(t) = &opt_tag {
                entrypoint::check_ep_name_len(t.as_str().as_bytes())
                    .map_err(|e| TcError::EntrypointError(e.into()))?;
            }
            stack.push(Type::Operation);
            I::Emit {
                tag: opt_tag,
                arg_ty: Or::Right(t.clone()),
            }
        }
        (App(EMIT, [], anns), [.., _]) => {
            let emit_val_type = pop!();
            emit_val_type.ensure_prop(gas, TypeProperty::Pushable)?;
            let opt_tag = anns.get_single_field_ann()?;
            if let Option::Some(t) = &opt_tag {
                entrypoint::check_ep_name_len(t.as_str().as_bytes())
                    .map_err(|e| TcError::EntrypointError(e.into()))?;
            }
            stack.push(Type::Operation);
            I::Emit {
                tag: opt_tag,
                arg_ty: Or::Left(emit_val_type),
            }
        }
        (App(EMIT, [], _), []) => no_overload!(EMIT, len 1),
        (App(EMIT, [_], _), []) => no_overload!(EMIT, len 1),
        (App(EMIT, [_, _, ..], _), _) => unexpected_micheline!(),

        #[cfg(feature = "bls")]
        (App(PAIRING_CHECK, [], _), [.., T::List(ty)])
            if match ty.as_ref() {
                T::Pair(p) => matches!(p.as_ref(), (T::Bls12381G1, T::Bls12381G2)),
                _ => false,
            } =>
        {
            *stack_top_mut(stack)? = T::Bool;
            I::PairingCheck
        }
        #[cfg(feature = "bls")]
        (App(PAIRING_CHECK, [], _), [.., t]) => no_overload!(
            PAIRING_CHECK,
            TypesNotEqual(
                T::new_list(T::new_pair(T::Bls12381G1, T::Bls12381G2)),
                (*t).clone()
            )
        ),
        #[cfg(feature = "bls")]
        (App(PAIRING_CHECK, [], _), []) => no_overload!(PAIRING_CHECK, len 1),
        #[cfg(feature = "bls")]
        (App(PAIRING_CHECK, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CREATE_CONTRACT, ..), _) if in_view => {
            Err(TcError::ForbiddenInView(Prim::CREATE_CONTRACT))?
        }
        (App(CREATE_CONTRACT, [cs], _), [.., new_storage, T::Mutez, T::Option(opt_keyhash)])
            if matches!(opt_keyhash.as_ref(), Type::KeyHash) =>
        {
            #[cfg(feature = "allow_lazy_storage_transfer")]
            let allow_lazy_storage_in_storage = true;
            #[cfg(not(feature = "allow_lazy_storage_transfer"))]
            let allow_lazy_storage_in_storage = false;
            // Validate the child script's views like L1 (parse_views), but
            // only at origination (re-typecheck/runtime pass typecheck_views=false).
            let contract_script = cs.split_script()?.typecheck_script(
                gas,
                allow_lazy_storage_in_storage,
                typecheck_views.enabled(),
            )?;
            ensure_ty_eq(gas, &contract_script.storage, new_storage)?;
            stack.drop_top(3)?;
            stack.push(Type::Address);
            stack.push(Type::Operation);
            I::CreateContract(Rc::new(contract_script), cs)
        }
        (App(CREATE_CONTRACT, [_], _), [.., _, _, _]) => {
            no_overload!(CREATE_CONTRACT)
        }
        (App(CREATE_CONTRACT, [_], _), [] | [_] | [_, _]) => {
            no_overload!(CREATE_CONTRACT, len 3)
        }
        (App(CREATE_CONTRACT, expect_args!(1), _), _) => unexpected_micheline!(),
        (App(VIEW, [name, return_ty], _), [.., T::Address, _arg_type]) => {
            let name = match name {
                Micheline::String(s) => {
                    check_view_name(s)?;
                    s.clone()
                }
                _ => return Err(TcError::UnexpectedMicheline(format!("{name:?}"))),
            };
            let arg_type = pop!();
            pop!();
            let return_type = parse_ty(gas, return_ty)?;
            return_type.ensure_prop(gas, TypeProperty::ViewOutput)?;
            stack.push(Type::new_option(return_type.clone()));
            I::IView {
                name,
                arg_type,
                return_type,
            }
        }
        (App(VIEW, [_, _], _), [.., _, _]) => no_overload!(VIEW),
        (App(VIEW, [_, _], _), [] | [_]) => no_overload!(VIEW, len 2),
        (App(VIEW, expect_args!(2), _), _) => unexpected_micheline!(),

        (App(CAST, [cast_ty], _), [.., value]) => {
            let cast_ty = parse_ty(gas, cast_ty)?;
            ensure_ty_eq(gas, &cast_ty, value)?;
            // Stack unchanged
            // Here `I::Seq(Vec::new())` means that nothing needs to be interpreted.
            I::Seq(Vec::new())
        }
        (App(CAST, [_], _), []) => no_overload!(CAST, len 1),
        (App(CAST, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(RENAME, [], _), [..]) => {
            // Stack unchanged
            // Here `I::Seq(Vec::new())` means that nothing needs to be interpreted.
            I::Seq(Vec::new())
        }
        (App(RENAME, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(prim @ micheline_unsupported_instructions!(), ..), _) => {
            Err(TcError::TodoInstr(*prim))?
        }

        (Seq(nested), _) => return Ok(StepResult::OpenSeq { body: nested }),
    }))
}

pub(crate) fn typecheck_contract_address<'a>(
    _ctx: &mut impl TypecheckingCtx<'a>,
    address: Address,
    ep: Entrypoint,
    typ: &Type,
) -> Result<Address, TcError> {
    // Among the entrypoints from the address and from the instruction, the defaults
    // ones are overridable by the non-default entrypoints. If both contain explicitly
    // specified non-default ones, then the result is None.
    let entrypoint = match (address.entrypoint.is_default(), ep.is_default()) {
        (true, true) => Entrypoint::default(),
        (false, true) => address.entrypoint,
        (true, false) => ep,
        (false, false) => return Err(TcError::EntrypointAmbiguity),
    };
    match address.hash {
        // If the address is implicit, we handle it separately.
        AddressHash::Implicit(_) => {
            // For implicit addresses, the entrypoint of the call has to be the default one
            // and the type of the contract should be Unit or Ticket.
            if !entrypoint.is_default() {
                return Err(TcError::NoSuchEntrypoint(entrypoint));
            }
            if matches!(typ, Type::Unit | Type::Ticket(_)) {
                Ok(Address {
                    entrypoint,
                    hash: address.hash,
                })
            } else {
                Err(TcError::UnexpectedImplicitAccountType(typ.clone()))
            }
        }
        AddressHash::Kt1(_) | AddressHash::Sr1(_) => {
            // Check if we have a contract at the address_hash of the given address.
            // and we have found a valid entrypoint to use.
            let contract_entrypoints = _ctx
                .lookup_entrypoints(&address.hash)
                .ok_or(TcError::NoSuchContract)?;

            // Do we have the entrypoint for the call in the entrypoints parsed
            // from the destination contract parameter?
            let contract_entrypoint_type = contract_entrypoints
                .get(&entrypoint)
                .ok_or(TcError::NoSuchEntrypoint(entrypoint.clone()))?;

            // if the entrypoint is present, check that it is of the required
            // type.
            ensure_ty_eq(_ctx.gas(), typ, contract_entrypoint_type)?;

            Ok(Address {
                entrypoint,
                hash: address.hash,
            })
        }
    }
}

fn get_nth_field_ref(mut m: u16, mut ty: &mut Type) -> Result<&mut Type, Type> {
    Ok(loop {
        match (m, ty) {
            (0, ty_) => break ty_,
            (1, Type::Pair(p)) => break &mut p.make_mut().0,
            (_, Type::Pair(p)) => {
                ty = &mut p.make_mut().1;
                m -= 2;
            }
            (_, ty) => {
                let ty = ty.clone();
                return Err(ty);
            }
        }
    })
}

/// Worklist frame for the iterative value typechecker. `'a` is the arena
/// lifetime that owns `Micheline` content; `'b` is the borrow lifetime of
/// the input passed to `typecheck_value` and may be shorter than `'a`.
enum TvFrame<'a, 'b> {
    /// Typecheck a Micheline value at the expected Type.
    Visit { v: &'b Micheline<'a>, t: Type },
    /// Behave as `Visit { v: App(Pair, vs, NO_ANNS), t }` without
    /// constructing the synthetic Micheline (no arena is available).
    VisitPairTail { vs: &'b [Micheline<'a>], t: Type },
    /// Pop two `TypedValue`s, push `TV::new_pair`.
    BuildPair,
    /// Pop one `TypedValue`, wrap as `Or::Left`.
    BuildLeft,
    /// Pop one `TypedValue`, wrap as `Or::Right`.
    BuildRight,
    /// Pop one `TypedValue`, wrap as `Option::Some`.
    BuildSome,
    /// Pop a `TV::Address`, resolve a `Contract` at this type.
    BuildContract { contract_ty: Rc<Type> },
    /// List element iteration: one `Rc<TypedValue>` per remaining child.
    ListAccum {
        remaining: &'b [Micheline<'a>],
        elem_t: Type,
        acc: Vec<Rc<TypedValue<'a>>>,
    },
    /// Set element iteration with ordering check.
    SetAccum {
        remaining: &'b [Micheline<'a>],
        elem_t: Type,
        set_ty: Type,
        acc: BTreeSet<TypedValue<'a>>,
        prev: Option<TypedValue<'a>>,
    },
    /// Map or in-memory big map iteration.
    MapAccum {
        remaining: &'b [Micheline<'a>],
        key_t: Type,
        val_t: Type,
        map_ty: Type,
        finalize: MapFinalize,
        acc: BTreeMap<TypedValue<'a>, TypedValue<'a>>,
        prev_key: Option<TypedValue<'a>>,
        stage: MapStage<'a, 'b>,
    },
    /// Big map from id with a diff overlay.
    BigMapDiffAccum {
        remaining: &'b [Micheline<'a>],
        key_t: Type,
        val_t: Type,
        map_ty: Type,
        big_map_id: BigMapId,
        tk_final: Type,
        tv_final: Type,
        acc: BTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>,
        prev_key: Option<TypedValue<'a>>,
        stage: BigMapDiffStage<'a, 'b>,
    },
    /// Ticket from the explicit four argument form: pop ticketer, content,
    /// and amount results (in that LIFO order they were pushed) and build.
    BuildTicketExplicit { content_type: Rc<Type> },
    /// Ticket from the legacy pair form: pop one `TV::Pair`, destructure.
    BuildTicketLegacy { content_type: Type },
}

/// How a map style iteration finishes once all elements are typed.
enum MapFinalize {
    /// Produces `TV::Map`.
    Map,
    /// Produces `TV::BigMap(BigMap::new(tk, tv, ...))`.
    BigMapInMem { tk: Type, tv: Type },
}

enum MapStage<'a, 'b> {
    AwaitingKey { v_expr: &'b Micheline<'a> },
    AwaitingValue { key: TypedValue<'a> },
}

enum BigMapDiffStage<'a, 'b> {
    AwaitingKey { v_expr: &'b Micheline<'a> },
    AwaitingValue { key: TypedValue<'a> },
}

/// Check `set` ordering between `prev` and `next`. Returns `Err` on a
/// duplicate or out of order element, consumes the comparison gas.
fn set_ordering_check<'a>(
    ctx: &mut impl TypecheckingCtx<'a>,
    set_ty: &Type,
    prev: &Option<TypedValue<'a>>,
    next: &TypedValue<'a>,
) -> Result<(), TcError> {
    if let Some(prev) = prev {
        ctx.gas()
            .consume(gas::interpret_cost::compare(prev, next)?)?;
        match prev.cmp(next) {
            std::cmp::Ordering::Less => Ok(()),
            std::cmp::Ordering::Equal => Err(TcError::DuplicateElements(set_ty.clone())),
            std::cmp::Ordering::Greater => Err(TcError::ElementsNotSorted(set_ty.clone())),
        }
    } else {
        Ok(())
    }
}

/// Check map key ordering between `prev_key` and `next_key`.
fn map_key_ordering_check<'a>(
    ctx: &mut impl TypecheckingCtx<'a>,
    map_ty: &Type,
    prev_key: &Option<TypedValue<'a>>,
    next_key: &TypedValue<'a>,
) -> Result<(), TcError> {
    if let Some(prev) = prev_key {
        ctx.gas()
            .consume(gas::interpret_cost::compare(prev, next_key)?)?;
        match prev.cmp(next_key) {
            std::cmp::Ordering::Less => Ok(()),
            std::cmp::Ordering::Equal => Err(TcError::DuplicateElements(map_ty.clone())),
            std::cmp::Ordering::Greater => Err(TcError::ElementsNotSorted(map_ty.clone())),
        }
    } else {
        Ok(())
    }
}

/// Typecheck a value. Assumes passed the type is valid, i.e. doesn't contain
/// illegal types like `set operation` or `contract operation`.
/// Destructure a Micheline as `Elt(key, val)` (the element shape inside
/// a typed `map`, `big_map`, or `set`-encoded sequence). Returns the
/// inner key and value Micheline, or a `TcError::InvalidEltForMap`
/// carrying a cloned copy of `map_ty` for context. The clone runs only
/// on the error path.
fn expect_elt<'a, 'b>(
    elt: &'b Micheline<'a>,
    map_ty: &Type,
) -> Result<(&'b Micheline<'a>, &'b Micheline<'a>), TcError> {
    match elt {
        Micheline::App(Prim::Elt, [k_expr, v_expr], _) => Ok((k_expr, v_expr)),
        _ => Err(TcError::InvalidEltForMap(
            format!("{elt:?}"),
            map_ty.clone(),
        )),
    }
}

/// Typecheck a value. Assumes the passed type is valid, i.e. does not contain
/// illegal types like `set operation` or `contract operation`.
pub fn typecheck_value<'a, 'b>(
    v: &'b Micheline<'a>,
    ctx: &mut impl TypecheckingCtx<'a>,
    t: &Type,
) -> Result<TypedValue<'a>, TcError> {
    typecheck_value_with_views(v, ctx, t, TypecheckViews::Disabled)
}

/// [typecheck_value] threading `typecheck_views`: validates a `CREATE_CONTRACT`
/// embedded in a value-level lambda (a pushed value, or an initial storage
/// value) at origination. See [typecheck_instruction_with_views].
pub(crate) fn typecheck_value_with_views<'a, 'b>(
    v: &'b Micheline<'a>,
    ctx: &mut impl TypecheckingCtx<'a>,
    t: &Type,
    typecheck_views: TypecheckViews,
) -> Result<TypedValue<'a>, TcError> {
    let mut frames: Vec<TvFrame<'a, 'b>> = vec![TvFrame::Visit { v, t: t.clone() }];
    let mut results: Vec<TypedValue<'a>> = Vec::new();

    while let Some(frame) = frames.pop() {
        step_typecheck_value(frame, ctx, &mut frames, &mut results, typecheck_views)?;
    }
    Ok(results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "typecheck_value root" }))?)
}

fn step_typecheck_value<'a, 'b>(
    frame: TvFrame<'a, 'b>,
    ctx: &mut impl TypecheckingCtx<'a>,
    frames: &mut Vec<TvFrame<'a, 'b>>,
    results: &mut Vec<TypedValue<'a>>,
    typecheck_views: TypecheckViews,
) -> Result<(), TcError> {
    use Micheline as V;
    use Type as T;
    use TypedValue as TV;

    match frame {
        TvFrame::Visit { v, t } => visit_value(v, t, ctx, frames, results, typecheck_views),
        TvFrame::VisitPairTail { vs, t } => {
            // Implements the recursive code's
            // `typecheck_value(&V::App(Pair, vrs, NO_ANNS), ctx, tr)` step
            // without constructing the synthetic App: dispatch directly
            // on (t, vs) as if vs were the args of an App(Pair).
            ctx.gas().consume(gas::tc_cost::VALUE_STEP)?;
            match (&t, vs) {
                (T::Pair(pt), [vl, rest @ ..]) if !rest.is_empty() => {
                    let (tl, tr) = pt.as_ref();
                    frames.push(TvFrame::BuildPair);
                    if rest.len() == 1 {
                        frames.push(TvFrame::Visit {
                            v: &rest[0],
                            t: tr.clone(),
                        });
                    } else {
                        frames.push(TvFrame::VisitPairTail {
                            vs: rest,
                            t: tr.clone(),
                        });
                    }
                    frames.push(TvFrame::Visit {
                        v: vl,
                        t: tl.clone(),
                    });
                    Ok(())
                }
                _ => Err(TcError::InvalidValueForType(
                    // Format via the same synthetic App the recursive
                    // typecheck_value built. `Micheline`'s manual,
                    // iterative `Debug` (L2-1436) is the contract that
                    // keeps this rendering stable as `Annotations`' Debug
                    // evolves — and that does not overflow the kernel
                    // stack on adversarially deep payloads.
                    format!("{:?}", Micheline::App(Prim::Pair, vs, NO_ANNS)),
                    t,
                )),
            }
        }
        TvFrame::BuildPair => {
            let r = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "pair r" }))?;
            let l = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "pair l" }))?;
            results.push(TV::new_pair(l, r));
            Ok(())
        }
        TvFrame::BuildLeft => {
            let v = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "left" }))?;
            results.push(TV::new_or(crate::ast::Or::Left(v)));
            Ok(())
        }
        TvFrame::BuildRight => {
            let v = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "right" }))?;
            results.push(TV::new_or(crate::ast::Or::Right(v)));
            Ok(())
        }
        TvFrame::BuildSome => {
            let v = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "some" }))?;
            results.push(TV::new_option(Some(v)));
            Ok(())
        }
        TvFrame::BuildContract { contract_ty } => {
            let addr = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "contract addr" }))?;
            let t_addr = match addr {
                TV::Address(a) => a,
                _ => {
                    return Err(TcError::InternalError(
                        TcInvariant::TypecheckValueWrongVariant {
                            expected: "TV::Address",
                        },
                    ))
                }
            };
            results.push(
                typecheck_contract_address(ctx, t_addr, Entrypoint::default(), &contract_ty)
                    .map(TypedValue::Contract)?,
            );
            Ok(())
        }
        TvFrame::ListAccum {
            remaining,
            elem_t,
            mut acc,
        } => {
            let elem = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "list elem" }))?;
            acc.push(Rc::new(elem));
            if let Some((next, rest)) = remaining.split_first() {
                frames.push(TvFrame::ListAccum {
                    remaining: rest,
                    elem_t: elem_t.clone(),
                    acc,
                });
                frames.push(TvFrame::Visit { v: next, t: elem_t });
            } else {
                results.push(TV::List(MichelsonList::from(acc)));
            }
            Ok(())
        }
        TvFrame::SetAccum {
            remaining,
            elem_t,
            set_ty,
            mut acc,
            prev,
        } => {
            let elem = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "set elem" }))?;
            set_ordering_check(ctx, &set_ty, &prev, &elem)?;
            let next_prev = Some(elem.clone());
            acc.insert(elem);
            if let Some((next, rest)) = remaining.split_first() {
                frames.push(TvFrame::SetAccum {
                    remaining: rest,
                    elem_t: elem_t.clone(),
                    set_ty,
                    acc,
                    prev: next_prev,
                });
                frames.push(TvFrame::Visit { v: next, t: elem_t });
            } else {
                results.push(TV::Set(acc.into_iter().map(Rc::new).collect()));
            }
            Ok(())
        }
        TvFrame::MapAccum {
            remaining,
            key_t,
            val_t,
            map_ty,
            finalize,
            acc,
            prev_key,
            stage,
        } => match stage {
            MapStage::AwaitingKey { v_expr } => {
                let key = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "map key" }))?;
                frames.push(TvFrame::MapAccum {
                    remaining,
                    key_t,
                    val_t: val_t.clone(),
                    map_ty,
                    finalize,
                    acc,
                    prev_key,
                    stage: MapStage::AwaitingValue { key },
                });
                frames.push(TvFrame::Visit {
                    v: v_expr,
                    t: val_t,
                });
                Ok(())
            }
            MapStage::AwaitingValue { key } => {
                let value = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "map value" }))?;
                map_key_ordering_check(ctx, &map_ty, &prev_key, &key)?;
                let next_prev = Some(key.clone());
                let mut acc = acc;
                acc.insert(key, value);
                if let Some((next_elt, rest)) = remaining.split_first() {
                    let (next_k_expr, next_v_expr) = expect_elt(next_elt, &map_ty)?;
                    frames.push(TvFrame::MapAccum {
                        remaining: rest,
                        key_t: key_t.clone(),
                        val_t,
                        map_ty,
                        finalize,
                        acc,
                        prev_key: next_prev,
                        stage: MapStage::AwaitingKey {
                            v_expr: next_v_expr,
                        },
                    });
                    frames.push(TvFrame::Visit {
                        v: next_k_expr,
                        t: key_t,
                    });
                } else {
                    match finalize {
                        MapFinalize::Map => {
                            results.push(TV::Map(
                                acc.into_iter()
                                    .map(|(k, v)| (Rc::new(k), Rc::new(v)))
                                    .collect(),
                            ));
                        }
                        MapFinalize::BigMapInMem { tk, tv } => {
                            results.push(TV::BigMap(BigMap::new(tk, tv, acc)));
                        }
                    }
                }
                Ok(())
            }
        },
        TvFrame::BigMapDiffAccum {
            remaining,
            key_t,
            val_t,
            map_ty,
            big_map_id,
            tk_final,
            tv_final,
            acc,
            prev_key,
            stage,
        } => match stage {
            BigMapDiffStage::AwaitingKey { v_expr } => {
                let key = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "big_map key" }))?;
                // Decide what to do with v_expr.
                match v_expr {
                    V::App(Prim::Some, [inner], _) => {
                        frames.push(TvFrame::BigMapDiffAccum {
                            remaining,
                            key_t,
                            val_t: val_t.clone(),
                            map_ty,
                            big_map_id,
                            tk_final,
                            tv_final,
                            acc,
                            prev_key,
                            stage: BigMapDiffStage::AwaitingValue { key },
                        });
                        frames.push(TvFrame::Visit { v: inner, t: val_t });
                    }
                    V::App(Prim::None, [], _) => {
                        // None entry: no value typecheck; commit (key, None)
                        // right away and continue with the next Elt.
                        map_key_ordering_check(ctx, &map_ty, &prev_key, &key)?;
                        let next_prev = Some(key.clone());
                        let mut acc = acc;
                        acc.insert(key, None);
                        if let Some((next_elt, rest)) = remaining.split_first() {
                            let (next_k_expr, next_v_expr) = expect_elt(next_elt, &map_ty)?;
                            frames.push(TvFrame::BigMapDiffAccum {
                                remaining: rest,
                                key_t: key_t.clone(),
                                val_t,
                                map_ty,
                                big_map_id,
                                tk_final,
                                tv_final,
                                acc,
                                prev_key: next_prev,
                                stage: BigMapDiffStage::AwaitingKey {
                                    v_expr: next_v_expr,
                                },
                            });
                            frames.push(TvFrame::Visit {
                                v: next_k_expr,
                                t: key_t,
                            });
                        } else {
                            results.push(TV::BigMap(BigMap {
                                content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                                    id: big_map_id,
                                    overlay: acc,
                                }),
                                key_type: tk_final,
                                value_type: tv_final,
                            }));
                        }
                    }
                    other => {
                        return Err(TcError::InvalidEltForMap(format!("{other:?}"), map_ty));
                    }
                }
                Ok(())
            }
            BigMapDiffStage::AwaitingValue { key } => {
                let value = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "big_map value" }))?;
                map_key_ordering_check(ctx, &map_ty, &prev_key, &key)?;
                let next_prev = Some(key.clone());
                let mut acc = acc;
                acc.insert(key, Some(value));
                if let Some((next_elt, rest)) = remaining.split_first() {
                    let (next_k_expr, next_v_expr) = expect_elt(next_elt, &map_ty)?;
                    frames.push(TvFrame::BigMapDiffAccum {
                        remaining: rest,
                        key_t: key_t.clone(),
                        val_t,
                        map_ty,
                        big_map_id,
                        tk_final,
                        tv_final,
                        acc,
                        prev_key: next_prev,
                        stage: BigMapDiffStage::AwaitingKey {
                            v_expr: next_v_expr,
                        },
                    });
                    frames.push(TvFrame::Visit {
                        v: next_k_expr,
                        t: key_t,
                    });
                } else {
                    results.push(TV::BigMap(BigMap {
                        content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                            id: big_map_id,
                            overlay: acc,
                        }),
                        key_type: tk_final,
                        value_type: tv_final,
                    }));
                }
                Ok(())
            }
        },
        TvFrame::BuildTicketExplicit { content_type } => {
            let amount = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ticket amount" }))?;
            let content = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ticket content" }))?;
            let ticketer = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ticket ticketer" }))?;
            let ticketer = match ticketer {
                TV::Address(a) => a,
                _ => {
                    return Err(TcError::InternalError(
                        TcInvariant::TypecheckValueWrongVariant {
                            expected: "TV::Address",
                        },
                    ))
                }
            };
            let amount = match amount {
                TV::Nat(n) => n,
                _ => {
                    return Err(TcError::InternalError(
                        TcInvariant::TypecheckValueWrongVariant {
                            expected: "TV::Nat",
                        },
                    ))
                }
            };
            results.push(TV::new_ticket(Ticket {
                ticketer: ticketer.hash,
                content_type: content_type.as_ref().clone(),
                content,
                amount,
            }));
            Ok(())
        }
        TvFrame::BuildTicketLegacy { content_type } => {
            let pair = results.pop().ok_or(TcError::InternalError(TcInvariant::EmptyResultStack { expected: "ticket legacy pair" }))?;
            match pair {
                TV::Pair(left, right) => {
                    let address = match TypedValue::unwrap_rc(left) {
                        TV::Address(a) => a,
                        _ => {
                            return Err(TcError::InternalError(
                                TcInvariant::TypecheckValueWrongVariant {
                                    expected: "TV::Address",
                                },
                            ))
                        }
                    };
                    let (content, amount) = match TypedValue::unwrap_rc(right) {
                        TV::Pair(c, a) => (c, a),
                        _ => {
                            return Err(TcError::InternalError(
                                TcInvariant::ExpectedTicketNestedPair,
                            ))
                        }
                    };
                    let amount = match TypedValue::unwrap_rc(amount) {
                        TV::Nat(n) => n,
                        _ => {
                            return Err(TcError::InternalError(
                                TcInvariant::ExpectedTicketNatAmount,
                            ))
                        }
                    };
                    results.push(TV::new_ticket(Ticket {
                        ticketer: address.hash,
                        content_type,
                        content: TypedValue::unwrap_rc(content),
                        amount,
                    }));
                    Ok(())
                }
                other => {
                    // The synthetic pair value did not typecheck as a Pair,
                    // which means the legacy ticket form was rejected.
                    Err(TcError::InvalidValueForType(
                        format!("{other:?}"),
                        Type::new_ticket(content_type),
                    ))
                }
            }
        }
    }
}

fn visit_value<'a, 'b>(
    v: &'b Micheline<'a>,
    t: Type,
    ctx: &mut impl TypecheckingCtx<'a>,
    frames: &mut Vec<TvFrame<'a, 'b>>,
    results: &mut Vec<TypedValue<'a>>,
    typecheck_views: TypecheckViews,
) -> Result<(), TcError> {
    use Micheline as V;
    use Type as T;
    use TypedValue as TV;

    ctx.gas().consume(gas::tc_cost::VALUE_STEP)?;
    let invalid = || TcError::InvalidValueForType(format!("{v:?}"), t.clone());

    match (&t, v) {
        (T::Nat, V::Int(n)) => results.push(TV::Nat(BigUint::try_from(n)?)),
        (T::Int, V::Int(n)) => results.push(TV::Int(n.clone())),
        (T::Bool, V::App(Prim::True, [], _)) => results.push(TV::Bool(true)),
        (T::Bool, V::App(Prim::False, [], _)) => results.push(TV::Bool(false)),
        (T::Mutez, V::Int(n)) if !n.is_negative() => results.push(TV::Mutez(i64::try_from(n)?)),
        (T::String, V::String(s)) => results.push(TV::String(s.clone())),
        (T::Unit, V::App(Prim::Unit, [], _)) => results.push(TV::Unit),
        (T::Pair(pt), V::App(Prim::Pair, [vl, rest @ ..], _) | V::Seq([vl, rest @ ..]))
            if !rest.is_empty() =>
        {
            let (tl, tr) = pt.as_ref();
            frames.push(TvFrame::BuildPair);
            if rest.len() == 1 {
                frames.push(TvFrame::Visit {
                    v: &rest[0],
                    t: tr.clone(),
                });
            } else {
                frames.push(TvFrame::VisitPairTail {
                    vs: rest,
                    t: tr.clone(),
                });
            }
            frames.push(TvFrame::Visit {
                v: vl,
                t: tl.clone(),
            });
        }
        (T::Or(ot), V::App(prim @ (Prim::Left | Prim::Right), [val], _)) => {
            let (tl, tr) = ot.as_ref();
            match prim {
                Prim::Left => {
                    frames.push(TvFrame::BuildLeft);
                    frames.push(TvFrame::Visit {
                        v: val,
                        t: tl.clone(),
                    });
                }
                Prim::Right => {
                    frames.push(TvFrame::BuildRight);
                    frames.push(TvFrame::Visit {
                        v: val,
                        t: tr.clone(),
                    });
                }
                _ => {
                    return Err(TcError::InternalError(
                        TcInvariant::UnexpectedLeftRightPrim,
                    ))
                }
            }
        }
        (T::Option(ty), V::App(Prim::Some, [inner], _)) => {
            frames.push(TvFrame::BuildSome);
            frames.push(TvFrame::Visit {
                v: inner,
                t: ty.as_ref().clone(),
            });
        }
        (T::Option(_), V::App(Prim::None, [], _)) => results.push(TV::new_option(None)),
        (T::List(ty), V::Seq(vs)) => {
            if let Some((first, rest)) = vs.split_first() {
                let elem_t = ty.as_ref().clone();
                frames.push(TvFrame::ListAccum {
                    remaining: rest,
                    elem_t: elem_t.clone(),
                    acc: Vec::with_capacity(vs.len()),
                });
                frames.push(TvFrame::Visit {
                    v: first,
                    t: elem_t,
                });
            } else {
                results.push(TV::List(MichelsonList::default()));
            }
        }
        (T::Set(ty), V::Seq(vs)) => {
            let elem_t = ty.as_ref().clone();
            ctx.gas().consume(gas::tc_cost::construct_set(
                elem_t.size_for_gas(),
                vs.len(),
            )?)?;
            if let Some((first, rest)) = vs.split_first() {
                frames.push(TvFrame::SetAccum {
                    remaining: rest,
                    elem_t: elem_t.clone(),
                    set_ty: t.clone(),
                    acc: BTreeSet::new(),
                    prev: None,
                });
                frames.push(TvFrame::Visit {
                    v: first,
                    t: elem_t,
                });
            } else {
                results.push(TV::Set(BTreeSet::new()));
            }
        }
        (T::Map(m), V::Seq(vs)) => {
            let (tk, tv) = m.as_ref();
            let key_t = tk.clone();
            ctx.gas()
                .consume(gas::tc_cost::construct_map(key_t.size_for_gas(), vs.len())?)?;
            visit_map_or_bigmap_inmem(
                t.clone(),
                vs,
                key_t,
                tv.clone(),
                MapFinalize::Map,
                frames,
                results,
            )?;
        }
        (T::BigMap(m), V::Seq(vs)) => {
            // In-memory big map: same syntax as a regular map.
            let (tk, tv) = m.as_ref();
            let key_t = tk.clone();
            ctx.gas()
                .consume(gas::tc_cost::construct_map(key_t.size_for_gas(), vs.len())?)?;
            visit_map_or_bigmap_inmem(
                t.clone(),
                vs,
                key_t,
                tv.clone(),
                MapFinalize::BigMapInMem {
                    tk: tk.clone(),
                    tv: tv.clone(),
                },
                frames,
                results,
            )?;
        }
        (T::BigMap(m), v) => {
            let (id, vs_opt, diff) = match v {
                V::Int(i) => (i, None, false),
                V::App(Prim::Pair, [V::Int(i), V::Seq(vs)], _) => (i, Some(vs), true),
                _ => return Err(invalid()),
            };
            let (tk, tv) = m.as_ref();
            let big_map_id: BigMapId = id.clone().into();
            let (key_type, value_type) = ctx
                .big_map_get_type(&big_map_id)
                .map_err(|e| TcError::LazyStorageError(e.to_string()))?
                .ok_or(TcError::BigMapNotFound(id.clone()))?;
            ensure_ty_eq(ctx.gas(), &key_type, tk)?;
            ensure_ty_eq(ctx.gas(), &value_type, tv)?;

            let map_ty = t.clone();
            let tk_final = tk.clone();
            let tv_final = tv.clone();
            let key_t = tk.clone();
            let val_t = tv.clone();

            match vs_opt {
                None => {
                    results.push(TV::BigMap(BigMap {
                        content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                            id: big_map_id,
                            overlay: BTreeMap::default(),
                        }),
                        key_type: tk_final,
                        value_type: tv_final,
                    }));
                }
                Some(vs) => {
                    ctx.gas()
                        .consume(gas::tc_cost::construct_map(key_t.size_for_gas(), vs.len())?)?;
                    if let Some((first_elt, rest)) = vs.split_first() {
                        let (k_expr, v_expr) = expect_elt(first_elt, &map_ty)?;
                        if !diff {
                            // Non-diff form happens only when vs_opt is None;
                            // reaching here means we matched the diff shape.
                            // Keep this branch for safety even though `diff`
                            // is always true once vs_opt is Some.
                        }
                        frames.push(TvFrame::BigMapDiffAccum {
                            remaining: rest,
                            key_t: key_t.clone(),
                            val_t,
                            map_ty,
                            big_map_id,
                            tk_final,
                            tv_final,
                            acc: BTreeMap::new(),
                            prev_key: None,
                            stage: BigMapDiffStage::AwaitingKey { v_expr },
                        });
                        frames.push(TvFrame::Visit {
                            v: k_expr,
                            t: key_t,
                        });
                    } else {
                        // Empty diff: build with empty overlay.
                        results.push(TV::BigMap(BigMap {
                            content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                                id: big_map_id,
                                overlay: BTreeMap::default(),
                            }),
                            key_type: tk_final,
                            value_type: tv_final,
                        }));
                    }
                }
            }
        }
        (T::Address, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_READABLE)?;
            results.push(TV::Address(
                Address::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::Address, e))?,
            ));
        }
        (T::Address, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            results.push(TV::Address(
                Address::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::Address, e))?,
            ));
        }
        (T::Contract(ty), addr) => {
            frames.push(TvFrame::BuildContract {
                contract_ty: ty.clone_rc(),
            });
            frames.push(TvFrame::Visit {
                v: addr,
                t: T::Address,
            });
        }
        (T::ChainId, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::CHAIN_ID_READABLE)?;
            results.push(TV::ChainId(
                ChainId::from_base58_check(str).map_err(|x| TcError::ChainIdError(x.into()))?,
            ));
        }
        (T::ChainId, V::Bytes(bs)) => {
            use tezos_crypto_rs::hash::HashTrait;
            ctx.gas().consume(gas::tc_cost::CHAIN_ID_OPTIMIZED)?;
            results.push(TV::ChainId(
                ChainId::try_from_bytes(bs).map_err(|x| TcError::ChainIdError(x.into()))?,
            ));
        }
        (T::Bytes, V::Bytes(bs)) => results.push(TV::Bytes(bs.clone())),
        (T::Key, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_READABLE)?;
            results.push(TV::Key(
                PublicKey::from_b58check(str)
                    .map_err(|e| TcError::ByteReprError(T::Key, e.into()))?,
            ));
        }
        (T::Key, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_OPTIMIZED)?;
            results.push(TV::Key(
                PublicKey::nom_read_exact(bs)
                    .map_err(|e| TcError::ByteReprError(T::Key, e.into()))?,
            ));
        }
        (T::Signature, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_READABLE)?;
            results
                .push(TV::Signature(Signature::from_base58_check(str).map_err(
                    |e| TcError::ByteReprError(T::Signature, e.into()),
                )?));
        }
        (T::Signature, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_OPTIMIZED)?;
            results
                .push(TV::Signature(Signature::try_from(bs.clone()).map_err(
                    |e| TcError::ByteReprError(T::Signature, e.into()),
                )?));
        }
        (T::KeyHash, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_READABLE)?;
            results.push(TV::KeyHash(
                PublicKeyHash::from_b58check(str)
                    .map_err(|e| TcError::ByteReprError(T::KeyHash, e.into()))?,
            ));
        }
        (T::KeyHash, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            results.push(TV::KeyHash(PublicKeyHash::nom_read_exact(bs).map_err(
                |err| {
                    TcError::ByteReprError(
                        T::KeyHash,
                        ByteReprError::WrongFormat(format!(
                            "public key hash, optimized {}",
                            convert_error(bs, err)
                        )),
                    )
                },
            )?));
        }
        (T::Timestamp, V::Int(n)) => results.push(TV::Timestamp(n.clone())),
        (T::Timestamp, V::String(n)) => {
            ctx.gas()
                .consume(gas::tc_cost::timestamp_decoding(n.len())?)?;
            // Match L1's `Script_timestamp.of_string`
            // (src/proto_alpha/lib_protocol/script_timestamp.ml): try RFC3339
            // first, then fall back to arbitrary-precision integer parsing via
            // Zarith's `Z.of_string` grammar (`tezos_data_encoding`'s `Zarith`
            // `FromStr`, which mirrors `Z.of_string` byte-for-byte).
            if let Ok(dt) = DateTime::parse_from_rfc3339(n) {
                // Chrono represents a leap second (e.g. `:60`) by keeping the
                // POSIX timestamp at `:59` and storing nanoseconds in the
                // `[1_000_000_000, 2_000_000_000)` range. L1's Ptime, on the
                // other hand, folds a leap second into the following POSIX
                // second. Realign with Ptime so the same RFC3339 string
                // decodes to the same epoch second on both sides.
                let leap_offset = i64::from(dt.timestamp_subsec_nanos() >= 1_000_000_000);
                results.push(TV::Timestamp(BigInt::from(dt.timestamp() + leap_offset)));
            } else if let Ok(z) = n.parse::<tezos_data_encoding::types::Zarith>() {
                results.push(TV::Timestamp(z.0));
            } else {
                return Err(invalid());
            }
        }
        (
            T::Lambda(tys),
            raw @ (V::Seq(instrs) | V::App(Prim::Lambda_rec, [V::Seq(instrs)], _)),
        ) => {
            let (in_ty, out_ty) = tys.as_ref();
            // The value-level Lambda path recurses through `typecheck_lambda`
            // → `typecheck` → `step_typecheck_instruction` (PUSH) →
            // `typecheck_value` → this arm. Each level adds Rust frames; an
            // adversarial nested `PUSH (lambda …) {PUSH (lambda …) {…}}` can
            // exhaust the kernel's 1 MiB WASM stack within a single
            // operation's gas budget. Mirror L1's defense — see
            // `script_ir_translator.ml:571` for the `stack_depth > 10000`
            // check that returns `Typechecking_too_many_recursive_calls`.
            // RAII guard ensures depth balance even on Err propagation.
            let _depth_guard = LambdaTypecheckDepthGuard::enter()?;
            results.push(TV::Lambda(Closure::Lambda(typecheck_lambda(
                instrs,
                ctx.gas(),
                in_ty.clone(),
                out_ty.clone(),
                matches!(raw, V::App(Prim::Lambda_rec, ..)),
                typecheck_views,
            )?)));
        }
        (
            T::Ticket(content_type),
            V::App(Prim::Ticket, [ticketer, content_type_bis, content, amount], _),
        ) => {
            let parsed_bis = parse_ty(ctx.gas(), content_type_bis)?;
            ensure_ty_eq(ctx.gas(), content_type, &parsed_bis)?;
            // BuildTicketExplicit pops 3 in LIFO order: amount, content, ticketer.
            frames.push(TvFrame::BuildTicketExplicit {
                content_type: content_type.clone_rc(),
            });
            frames.push(TvFrame::Visit {
                v: amount,
                t: T::Nat,
            });
            frames.push(TvFrame::Visit {
                v: content,
                t: content_type.as_ref().clone(),
            });
            frames.push(TvFrame::Visit {
                v: ticketer,
                t: T::Address,
            });
        }
        (T::Ticket(content_type), m) => {
            let content_type = content_type.as_ref().clone();
            let pair_t = Type::new_pair(
                Type::Address,
                Type::new_pair(content_type.clone(), Type::Nat),
            );
            // Recursive call into the iterative driver. Any Err inside the
            // synthetic pair typecheck is remapped to InvalidValueForType
            // against the ticket type, matching the recursive form's
            // outer `_ => Err(invalid())` arm. Pushing BuildTicketLegacy
            // afterwards keeps the deconstruction in the worklist.
            let pair = typecheck_value_with_views(m, ctx, &pair_t, typecheck_views)
                .map_err(|_| invalid())?;
            results.push(pair);
            frames.push(TvFrame::BuildTicketLegacy { content_type });
        }
        #[cfg(feature = "bls")]
        (T::Bls12381Fr, V::Int(i)) => {
            ctx.gas().consume(gas::tc_cost::BLS_FR)?;
            results.push(TV::Bls12381Fr(bls::Fr::from_big_int(i)));
        }
        #[cfg(feature = "bls")]
        (T::Bls12381Fr, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_FR)?;
            results.push(TV::Bls12381Fr(bls::Fr::from_bytes(bs).ok_or_else(invalid)?));
        }
        #[cfg(feature = "bls")]
        (T::Bls12381G1, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_G1)?;
            results.push(TV::new_bls12381_g1(
                bls::G1::from_bytes(bs).ok_or_else(invalid)?,
            ));
        }
        #[cfg(feature = "bls")]
        (T::Bls12381G2, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_G2)?;
            results.push(TV::new_bls12381_g2(
                bls::G2::from_bytes(bs).ok_or_else(invalid)?,
            ));
        }
        (_, _) => return Err(invalid()),
    }
    Ok(())
}

/// Shared setup for the (T::Map, Seq) and (T::BigMap, Seq) arms.
fn visit_map_or_bigmap_inmem<'a, 'b>(
    map_ty: Type,
    vs: &'b [Micheline<'a>],
    key_t: Type,
    val_t: Type,
    finalize: MapFinalize,
    frames: &mut Vec<TvFrame<'a, 'b>>,
    results: &mut Vec<TypedValue<'a>>,
) -> Result<(), TcError> {
    use TypedValue as TV;

    if let Some((first_elt, rest)) = vs.split_first() {
        let (k_expr, v_expr) = expect_elt(first_elt, &map_ty)?;
        frames.push(TvFrame::MapAccum {
            remaining: rest,
            key_t: key_t.clone(),
            val_t,
            map_ty,
            finalize,
            acc: BTreeMap::new(),
            prev_key: None,
            stage: MapStage::AwaitingKey { v_expr },
        });
        frames.push(TvFrame::Visit {
            v: k_expr,
            t: key_t,
        });
    } else {
        match finalize {
            MapFinalize::Map => results.push(TV::Map(BTreeMap::new())),
            MapFinalize::BigMapInMem { tk, tv } => {
                results.push(TV::BigMap(BigMap::new(tk, tv, BTreeMap::new())))
            }
        }
    }
    Ok(())
}

#[allow(missing_docs)]
pub fn typecheck_lambda<'a>(
    instrs: &'a [Micheline<'a>],
    gas: &mut Gas,
    in_ty: Type,
    out_ty: Type,
    recursive: bool,
    typecheck_views: TypecheckViews,
) -> Result<Lambda<'a>, TcError> {
    let stk = &mut if recursive {
        let self_ty = Type::new_lambda(in_ty.clone(), out_ty.clone());
        tc_stk![self_ty, in_ty.clone()]
    } else {
        tc_stk![in_ty.clone()]
    };
    // in_view=false: lambdas create a new scope, view restrictions don't propagate.
    let code = Rc::from(typecheck(instrs, gas, None, stk, false, typecheck_views)?);
    unify_stacks(gas, stk, tc_stk![out_ty.clone()])?;
    let micheline_code = Micheline::Seq(instrs);
    Ok(if recursive {
        Lambda::LambdaRec {
            micheline_code,
            code,
            in_ty,
            out_ty,
        }
    } else {
        Lambda::Lambda {
            micheline_code,
            code,
        }
    })
}

/// Typecheck a view body. Similar to [typecheck_lambda] but specialized for
/// views: sets the `in_view` flag to forbid side-effectful
/// instructions.
pub fn typecheck_view<'a>(
    instrs: &'a [Micheline<'a>],
    gas: &mut Gas,
    in_ty: Type,
    out_ty: Type,
) -> Result<Rc<[Instruction<'a>]>, TcError> {
    typecheck_view_with_views(instrs, gas, in_ty, out_ty, TypecheckViews::Disabled)
}

/// [typecheck_view] threading `typecheck_views`. `CREATE_CONTRACT` is forbidden
/// directly in a view but allowed inside a lambda-in-view, so its child-script
/// views must still be validated at origination.
pub(crate) fn typecheck_view_with_views<'a>(
    instrs: &'a [Micheline<'a>],
    gas: &mut Gas,
    in_ty: Type,
    out_ty: Type,
    typecheck_views: TypecheckViews,
) -> Result<Rc<[Instruction<'a>]>, TcError> {
    let stk = &mut tc_stk![in_ty];
    let code = Rc::from(typecheck(instrs, gas, None, stk, true, typecheck_views)?);
    unify_stacks(gas, stk, tc_stk![out_ty])?;
    Ok(code)
}

fn validate_u10(n: &BigInt) -> Result<u16, TcError> {
    let res = u16::try_from(n).map_err(|_| TcError::ExpectedU10(n.clone()))?;
    if res >= 1024 {
        return Err(TcError::ExpectedU10(n.clone()));
    }
    Ok(res)
}

/// Ensures type stack is at least of the required length, otherwise returns
/// `Err(StackTooShort)`.
fn ensure_stack_len(instr: Prim, stack: &TypeStack, l: usize) -> Result<(), TcError> {
    if stack.len() >= l {
        Ok(())
    } else {
        Err(TcError::NoMatchingOverload {
            instr,
            stack: stack.clone(),
            reason: Some(NoMatchingOverloadReason::StackTooShort { expected: l }),
        })
    }
}

/// Tries to unify two stacks, putting the result in `dest`. If stacks can't be
/// unified, i.e. neither stack is failed and stacks are not equal, returns
/// `Err(StacksNotEqual)`. If it runs out of gas, returns `Err(OutOfGas)`
/// instead.
///
/// Failed stacks unify with anything.
fn unify_stacks(
    gas: &mut Gas,
    dest: &mut FailingTypeStack,
    aux: FailingTypeStack,
) -> Result<(), TcError> {
    match &dest {
        FailingTypeStack::Ok(stack1) => {
            if let FailingTypeStack::Ok(stack2) = aux {
                ensure_stacks_eq(gas, stack1, &stack2)?;
            }
        }
        FailingTypeStack::Failed => {
            // if main stack is failing, assign aux to main, as aux may be OK
            *dest = aux;
        }
    }
    Ok(())
}

fn ensure_stacks_eq(gas: &mut Gas, stack1: &TypeStack, stack2: &TypeStack) -> Result<(), TcError> {
    if stack1.len() != stack2.len() {
        return Err(TcError::StacksNotEqual(
            stack1.clone(),
            stack2.clone(),
            StacksNotEqualReason::LengthsDiffer(stack1.len(), stack2.len()),
        ));
    }
    for (ty1, ty2) in stack1.iter().zip(stack2.iter()) {
        ensure_ty_eq(gas, ty1, ty2).map_err(|e| match e {
            TcError::TypesNotEqual(e) => {
                TcError::StacksNotEqual(stack1.clone(), stack2.clone(), e.into())
            }
            err => err,
        })?;
    }
    Ok(())
}

pub(crate) fn ensure_ty_eq(gas: &mut Gas, ty1: &Type, ty2: &Type) -> Result<(), TcError> {
    gas.consume(gas::tc_cost::ty_eq(ty1.size_for_gas(), ty2.size_for_gas())?)?;
    if ty1 != ty2 {
        Err(TypesNotEqual(ty1.clone(), ty2.clone()).into())
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod typecheck_tests {
    use super::{Lambda, Or};
    use crate::ast::big_map::LazyStorage;
    use crate::ast::michelson_address as addr;
    use crate::ast::or::Or::{Left, Right};
    use crate::context::Ctx;
    use crate::gas::{CostOverflow, Gas};
    use crate::parser::test_helpers::*;
    use crate::typechecker::*;
    use std::collections::HashMap;
    use std::rc::Rc;
    use Instruction::*;
    use Option::None;

    /// Parses a 100k deep right leaning `pair` type without overflowing the
    /// Rust call stack, then drops it at end of scope -- exercising the
    /// iterative `Drop for Type` this MR adds. Both the parse and the drop
    /// would overflow at this depth with a recursive implementation.
    #[test]
    fn deeply_nested_pair_parses() {
        const DEPTH: usize = 100_000;
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
        let int_app: Micheline<'_> = Micheline::App(Prim::int, &[], NO_ANNS);
        let mut current: Micheline<'_> = int_app.clone();
        for _ in 0..DEPTH {
            let pair_args = arena.alloc_extend([int_app.clone(), current]);
            current = Micheline::App(Prim::pair, pair_args, NO_ANNS);
        }
        let mut gas = Gas::new(u32::MAX);
        let parsed = parse_ty(&mut gas, &current).expect("deep pair parses");
        assert!(matches!(parsed, Type::Pair(_)), "expected outer Type::Pair");
        // `parsed` (a 100k-deep Type) drops here through the iterative
        // `Drop for Type`; a recursive destructor would overflow.
    }

    /// Typechecks a 100k deep nested IF, demonstrating that both the
    /// iterative instruction typechecker driver and the iterative `Drop`
    /// on `Instruction` allow the resulting tree to be built and freed
    /// without overflowing the Rust call stack.
    #[test]
    fn deeply_nested_if_typechecks() {
        const DEPTH: usize = 100_000;
        // Build: IF { PUSH bool True ; IF { PUSH bool True ; IF { ... } { } } { } } { }
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
        let mut current_then: &[Micheline<'_>] = &[];
        for _ in 0..DEPTH {
            // each layer: PUSH bool True ; IF { current_then } { }
            let push_true = Micheline::App(
                Prim::PUSH,
                arena.alloc_extend([
                    Micheline::App(Prim::bool, &[], NO_ANNS),
                    Micheline::App(Prim::True, &[], NO_ANNS),
                ]),
                NO_ANNS,
            );
            let if_instr = Micheline::App(
                Prim::IF,
                arena.alloc_extend([Micheline::Seq(current_then), Micheline::Seq(&[])]),
                NO_ANNS,
            );
            current_then = arena.alloc_extend([push_true, if_instr]);
        }
        // Top level: push a True, then run the deeply nested IF.
        let push_true = Micheline::App(
            Prim::PUSH,
            arena.alloc_extend([
                Micheline::App(Prim::bool, &[], NO_ANNS),
                Micheline::App(Prim::True, &[], NO_ANNS),
            ]),
            NO_ANNS,
        );
        let root_if = Micheline::App(
            Prim::IF,
            arena.alloc_extend([Micheline::Seq(current_then), Micheline::Seq(&[])]),
            NO_ANNS,
        );
        let program = arena.alloc_extend([push_true, root_if]);
        let mut gas = Gas::new(u32::MAX);
        let mut stack = tc_stk![];
        let _result =
            typecheck(program, &mut gas, None, &mut stack, false, TypecheckViews::Disabled)
                .expect("deep IF typechecks");
    }

    /// Typechecks a 100k deep right leaning pair value at the matching
    /// pair type. `TypedValue` does not implement `Drop` itself (that would
    /// forbid by move destructuring of its variants); instead the test
    /// flattens the resulting value with `drain_deep_typed_value` before it
    /// is dropped, which is the same draining routine deep value producers
    /// would call in production code paths.
    #[test]
    fn deeply_nested_pair_value_typechecks() {
        const DEPTH: usize = 100_000;
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();

        // Build the type: pair(int, pair(int, pair(int, ..., int))).
        let int_app: Micheline<'_> = Micheline::App(Prim::int, &[], NO_ANNS);
        let mut ty_node: Micheline<'_> = int_app.clone();
        for _ in 0..DEPTH {
            let pair_args = arena.alloc_extend([int_app.clone(), ty_node]);
            ty_node = Micheline::App(Prim::pair, pair_args, NO_ANNS);
        }
        let mut gas = Gas::new(u32::MAX);
        let parsed_ty = parse_ty(&mut gas, &ty_node).expect("deep pair type parses");

        // Build the value: pair(0, pair(0, pair(0, ..., 0))).
        let zero: Micheline<'_> = Micheline::Int(0.into());
        let mut v_node: Micheline<'_> = zero.clone();
        for _ in 0..DEPTH {
            let pair_args = arena.alloc_extend([zero.clone(), v_node]);
            v_node = Micheline::App(Prim::Pair, pair_args, NO_ANNS);
        }
        let mut ctx = Ctx::default();
        ctx.gas = Gas::new(u32::MAX);
        let mut tv =
            typecheck_value(&v_node, &mut ctx, &parsed_ty).expect("deep pair value typechecks");
        crate::ast::drain_deep_typed_value(&mut tv);
    }

    /// Typechecks `IF {} {}` on a stack carrying a 100k deep `pair` type,
    /// exercising the iterative `size_for_gas` and iterative `PartialEq`
    /// on `Type` through unify_stacks / ensure_ty_eq. The previous
    /// derive-based equality would have recursed to depth 100k and blown
    /// the WASM stack.
    #[test]
    fn deeply_nested_if_unifies_deep_types() {
        const DEPTH: usize = 100_000;
        let mut deep = Type::Int;
        for _ in 0..DEPTH {
            deep = Type::new_pair(Type::Int, deep);
        }
        let mut stack = tc_stk![deep, Type::Bool];
        let if_instr = parse("IF {} {}").unwrap();
        let mut gas = Gas::new(u32::MAX);
        typecheck_instruction(&if_instr, &mut gas, &mut stack).expect("IF unifies deep types");
    }

    /// Packs a 100k deep `TypedValue::Pair`, exercising the iterative
    /// `TypedValue::into_micheline_optimized_legacy` plus iterative
    /// `gas::collect_micheline_size`. Mirrors the PACK scenario from the
    /// Linear "Prevent stack overflows in MIR" project (dynamically
    /// building a deep value with `UNIT ; PAIR`, then packing it).
    #[test]
    fn deeply_nested_pair_packs() {
        use crate::ast::IntoMicheline;
        use std::rc::Rc;
        const DEPTH: usize = 100_000;
        let mut deep: TypedValue<'_> = TypedValue::Unit;
        for _ in 0..DEPTH {
            deep = TypedValue::Pair(Rc::new(TypedValue::Unit), Rc::new(deep));
        }
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
        let mich = deep
            .into_micheline_optimized_legacy(&arena, &mut Gas::new(u32::MAX))
            .expect("into_micheline succeeds");
        let _bytes = mich
            .encode_for_pack()
            .expect("gas suffices")
            .expect("pack succeeds");
    }

    /// hack to simplify syntax in tests
    fn typecheck_instruction<'a>(
        i: &Micheline<'a>,
        gas: &mut Gas,
        opt_stack: &mut FailingTypeStack,
    ) -> Result<Instruction<'a>, TcError> {
        super::typecheck_instruction(i, gas, None, opt_stack, false)
    }

    #[test]
    fn test_dup() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DUP 1").unwrap(), &mut gas, &mut stack),
            Ok(Dup(Some(1)))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_dup_n() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DUP 2").unwrap(), &mut gas, &mut stack),
            Ok(Dup(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_swap() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        let expected_stack = tc_stk![Type::Int, Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("SWAP").unwrap(), &mut gas, &mut stack),
            Ok(Swap)
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_abs() {
        let mut stack = tc_stk![Type::Int];
        let expected_stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ABS").unwrap(), &mut gas, &mut stack),
            Ok(Abs)
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_abs_mismatch() {
        let mut stack = tc_stk![Type::Unit];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ABS").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::ABS,
                stack: stk![Type::Unit],
                reason: Some(TypesNotEqual(Type::Int, Type::Unit).into())
            })
        );
    }

    #[test]
    fn test_abs_too_short() {
        too_short_test(&parse("ABS").unwrap(), Prim::ABS, 1);
    }

    #[test]
    fn test_is_nat() {
        let mut stack = tc_stk![Type::Int];
        let expected_stack = tc_stk![Type::new_option(Type::Nat)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ISNAT").unwrap(), &mut gas, &mut stack),
            Ok(IsNat)
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_is_nat_mismatch() {
        let mut stack = tc_stk![Type::Unit];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ISNAT").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::ISNAT,
                stack: stk![Type::Unit],
                reason: Some(TypesNotEqual(Type::Int, Type::Unit).into())
            })
        );
    }

    #[test]
    fn test_is_nat_too_short() {
        too_short_test(&parse("ISNAT").unwrap(), Prim::ISNAT, 1);
    }

    mod int {
        use super::*;

        fn test(input: Type, overload: overloads::Int) {
            let mut stack = tc_stk![input];
            let expected_stack = tc_stk![Type::Int];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("INT").unwrap(), &mut gas, &mut stack),
                Ok(Instruction::Int(overload)),
            );
            assert_eq!(stack, expected_stack);
            assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        macro_rules! test {
            ($overload:ident) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test(Type::$overload, overloads::Int::$overload);
                }
            };
        }

        test!(Nat);
        test!(Bls12381Fr);
        test!(Bytes);

        #[test]
        fn test_int_short() {
            too_short_test(&parse("INT").unwrap(), Prim::INT, 1);
        }

        #[test]
        fn test_int_mismatch() {
            let mut stack = tc_stk![Type::String];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("INT").unwrap(), &mut gas, &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::INT,
                    stack: stk![Type::String],
                    reason: None,
                })
            );
        }
    }

    mod nat {
        use super::*;

        #[test]
        fn ok() {
            let mut stack = tc_stk![Type::Bytes];
            let expected_stack = tc_stk![Type::Nat];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("NAT").unwrap(), &mut gas, &mut stack),
                Ok(Instruction::Nat),
            );
            assert_eq!(stack, expected_stack);
            assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        #[test]
        fn test_int_short() {
            too_short_test(&parse("NAT").unwrap(), Prim::NAT, 1);
        }

        #[test]
        fn test_int_mismatch() {
            let mut stack = tc_stk![Type::String];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("NAT").unwrap(), &mut gas, &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::NAT,
                    stack: stk![Type::String],
                    reason: None,
                })
            );
        }
    }

    mod bytes {
        use super::*;

        fn test(input: Type, overload: overloads::Bytes) {
            let mut stack = tc_stk![input];
            let expected_stack = tc_stk![Type::Bytes];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("BYTES").unwrap(), &mut gas, &mut stack),
                Ok(Instruction::Bytes(overload)),
            );
            assert_eq!(stack, expected_stack);
            assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        macro_rules! test {
            ($overload:ident) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test(Type::$overload, overloads::Bytes::$overload);
                }
            };
        }

        test!(Nat);
        test!(Int);

        #[test]
        fn test_int_short() {
            too_short_test(&parse("BYTES").unwrap(), Prim::BYTES, 1);
        }

        #[test]
        fn test_int_mismatch() {
            let mut stack = tc_stk![Type::String];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("BYTES").unwrap(), &mut gas, &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::BYTES,
                    stack: stk![Type::String],
                    reason: None,
                })
            );
        }
    }

    #[test]
    fn test_drop() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DROP").unwrap(), &mut gas, &mut stack),
            Ok(Drop(None))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_drop_n() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        let expected_stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DROP 2").unwrap(), &mut gas, &mut stack),
            Ok(Drop(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440 - 2 * 50
        );
    }

    #[test]
    fn test_push() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("PUSH int 1").unwrap(), &mut gas, &mut stack),
            Ok(Push(Rc::new(TypedValue::int(1))))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_dip() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Bool];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DIP 1 {PUSH nat 6}").unwrap(), &mut gas, &mut stack),
            Ok(Dip(Some(1), vec![Push(Rc::new(TypedValue::nat(6)))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    mod sub_mutez {
        use super::*;

        #[test]
        fn ok() {
            let mut stack = tc_stk![Type::Mutez, Type::Mutez];
            let expected_stack = tc_stk![Type::new_option(Type::Mutez)];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("SUB_MUTEZ").unwrap(), &mut gas, &mut stack),
                Ok(SubMutez)
            );
            assert_eq!(stack, expected_stack);
            assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
        }

        #[test]
        fn mismatch() {
            let mut stack = tc_stk![Type::Unit, Type::Mutez];
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("SUB_MUTEZ").unwrap(), &mut gas, &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::SUB_MUTEZ,
                    stack: stk![Type::Unit, Type::Mutez],
                    reason: None,
                })
            );
        }

        #[test]
        fn too_short() {
            too_short_test(&parse("SUB_MUTEZ").unwrap(), Prim::SUB_MUTEZ, 2);
        }
    }

    #[test]
    fn test_add_int_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        let expected_stack = tc_stk![Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::IntInt))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_add_nat_nat() {
        let mut stack = tc_stk![Type::Nat, Type::Nat];
        let expected_stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::NatNat))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_add_mutez_mutez() {
        let mut stack = tc_stk![Type::Mutez, Type::Mutez];
        let expected_stack = tc_stk![Type::Mutez];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::MutezMutez))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_binary_bitwise_operators() {
        for ty in &[Type::Bool, Type::Nat, Type::Bytes] {
            for prim in &[Prim::AND, Prim::OR, Prim::XOR] {
                let mut stack = tc_stk![ty.clone(), ty.clone()];
                let expected_stack = tc_stk![ty.clone()];
                let expected_instr = match (prim, ty) {
                    (Prim::AND, Type::Bool) => And(overloads::And::Bool),
                    (Prim::AND, Type::Nat) => And(overloads::And::NatNat),
                    (Prim::AND, Type::Bytes) => And(overloads::And::Bytes),
                    (Prim::OR, Type::Bool) => Or(overloads::Or::Bool),
                    (Prim::OR, Type::Nat) => Or(overloads::Or::Nat),
                    (Prim::OR, Type::Bytes) => Or(overloads::Or::Bytes),
                    (Prim::XOR, Type::Bool) => Xor(overloads::Xor::Bool),
                    (Prim::XOR, Type::Nat) => Xor(overloads::Xor::Nat),
                    (Prim::XOR, Type::Bytes) => Xor(overloads::Xor::Bytes),
                    _ => panic!("Bad test parameters"),
                };
                let mut gas = Gas::default();
                assert_eq!(
                    typecheck_instruction(
                        &Micheline::App(*prim, &[], NO_ANNS),
                        &mut gas,
                        &mut stack
                    ),
                    Ok(expected_instr)
                );
                assert_eq!(stack, expected_stack);
            }
        }
    }

    #[test]
    fn test_and_int_nat() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        let expected_stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("AND").unwrap(), &mut gas, &mut stack),
            Ok(And(overloads::And::IntNat))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_and_short() {
        too_short_test(&parse("AND").unwrap(), Prim::AND, 2);
    }

    #[test]
    fn test_or_short() {
        too_short_test(&parse("OR").unwrap(), Prim::OR, 2);
    }

    #[test]
    fn test_xor_short() {
        too_short_test(&parse("XOR").unwrap(), Prim::XOR, 2);
    }

    #[test]
    fn test_and_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("AND").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::AND,
                stack: stk![Type::String, Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_or_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("OR").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::OR,
                stack: stk![Type::String, Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_xor_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("XOR").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::XOR,
                stack: stk![Type::String, Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_not() {
        for ty in &[Type::Bool, Type::Nat, Type::Int, Type::Bytes] {
            let mut stack = tc_stk![ty.clone()];
            let mut res_ty = ty.clone();
            if matches!(ty, Type::Nat) {
                res_ty = Type::Int
            };
            let expected_stack = tc_stk![res_ty];

            let expected_overload = match ty {
                Type::Bool => overloads::Not::Bool,
                Type::Int => overloads::Not::Int,
                Type::Nat => overloads::Not::Nat,
                Type::Bytes => overloads::Not::Bytes,
                _ => panic!("Bad test parameters"),
            };
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(&parse("NOT").unwrap(), &mut gas, &mut stack),
                Ok(Not(expected_overload))
            );
            assert_eq!(stack, expected_stack);
        }
    }

    #[test]
    fn test_not_short() {
        too_short_test(&parse("NOT").unwrap(), Prim::NOT, 1);
    }

    #[test]
    fn test_not_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("NOT").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::NOT,
                stack: stk![Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_add_bls12_381_fr() {
        let mut stack = tc_stk![Type::Bls12381Fr, Type::Bls12381Fr];
        let expected_stack = tc_stk![Type::Bls12381Fr];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::Bls12381Fr))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_add_bls12_381_g1() {
        let mut stack = tc_stk![Type::Bls12381G1, Type::Bls12381G1];
        let expected_stack = tc_stk![Type::Bls12381G1];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::Bls12381G1))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_add_bls12_381_g2() {
        let mut stack = tc_stk![Type::Bls12381G2, Type::Bls12381G2];
        let expected_stack = tc_stk![Type::Bls12381G2];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Ok(Add(overloads::Add::Bls12381G2))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            gas.milligas().unwrap(),
            Gas::default().milligas().unwrap() - 440
        );
    }

    #[test]
    fn test_loop() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {PUSH bool True}").unwrap(),
                &mut gas,
                &mut stack
            ),
            Ok(Loop(vec![Push(Rc::new(TypedValue::Bool(true)))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_loop_stacks_not_equal_length() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {PUSH int 1; PUSH bool True}").unwrap(),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual(
                stk![Type::Int, Type::Int, Type::Bool],
                stk![Type::Int, Type::Bool],
                StacksNotEqualReason::LengthsDiffer(3, 2)
            )
        );
    }

    #[test]
    fn test_loop_stacks_not_equal_types() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {DROP; PUSH bool False; PUSH bool True}").unwrap(),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual(
                stk![Type::Bool, Type::Bool],
                stk![Type::Int, Type::Bool],
                TypesNotEqual(Type::Bool, Type::Int).into()
            )
        );
    }

    #[test]
    fn test_loop_left() {
        let mut stack = tc_stk![Type::Int, Type::new_or(Type::Unit, Type::Nat)];
        let expected_stack = tc_stk![Type::Int, Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP_LEFT {DROP; PUSH (or unit nat) (Right 123)}").unwrap(),
                &mut gas,
                &mut stack
            ),
            Ok(LoopLeft(vec![
                Drop(None),
                Push(Rc::new(TypedValue::new_or(Or::Right(TypedValue::nat(123)))))
            ]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
    }

    #[test]
    fn test_loop_left_stacks_not_equal_length() {
        let mut stack = tc_stk![Type::Int, Type::new_or(Type::Unit, Type::Nat)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP_LEFT {PUSH (or unit nat) (Right 123)}").unwrap(),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual(
                stk![Type::Int, Type::Unit, Type::new_or(Type::Unit, Type::Nat)],
                stk![Type::Int, Type::new_or(Type::Unit, Type::Nat)],
                StacksNotEqualReason::LengthsDiffer(3, 2)
            )
        );
    }

    #[test]
    fn test_loop_left_stacks_not_equal_types() {
        let mut stack = tc_stk![Type::Int, Type::new_or(Type::Unit, Type::Nat)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP_LEFT {DROP; PUSH bool True}").unwrap(),
                &mut gas,
                &mut stack
            )
            .unwrap_err(),
            TcError::StacksNotEqual(
                stk![Type::Int, Type::Bool],
                stk![Type::Int, Type::new_or(Type::Unit, Type::Nat)],
                TypesNotEqual(Type::Bool, Type::new_or(Type::Unit, Type::Nat)).into()
            )
        );
    }

    #[test]
    fn test_loop_left_too_short() {
        too_short_test(
            &parse("LOOP_LEFT { FAILWITH }").unwrap(),
            Prim::LOOP_LEFT,
            1,
        );
    }

    #[test]
    fn test_iter_list() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut gas, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_too_short() {
        too_short_test(&parse("ITER {}").unwrap(), Prim::ITER, 1)
    }

    #[test]
    fn test_iter_list_inner_mismatch() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut gas, &mut stack),
            Err(TcError::StacksNotEqual(
                stk![],
                stk![Type::Int],
                StacksNotEqualReason::LengthsDiffer(0, 1)
            ))
        );
    }

    #[test]
    fn test_iter_set() {
        let mut stack = tc_stk![Type::new_set(Type::Int)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut gas, &mut stack),
            Ok(Iter(overloads::Iter::Set, vec![Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_set_inner_mismatch() {
        let mut stack = tc_stk![Type::new_set(Type::Int)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut gas, &mut stack),
            Err(TcError::StacksNotEqual(
                stk![],
                stk![Type::Int],
                StacksNotEqualReason::LengthsDiffer(0, 1)
            ))
        );
    }

    #[test]
    fn test_iter_map() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::Nat)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { CAR; DROP }").unwrap(), &mut gas, &mut stack),
            Ok(Iter(overloads::Iter::Map, vec![Car, Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_map_inner_mismatch() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::Nat)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut gas, &mut stack),
            Err(TcError::StacksNotEqual(
                stk![],
                stk![Type::new_pair(Type::Int, Type::Nat)],
                StacksNotEqualReason::LengthsDiffer(0, 1)
            ))
        );
    }

    #[test]
    fn test_iter_arg_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::ITER,
                stack: stk![Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_iter_fail() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { FAILWITH }").unwrap(), &mut gas, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Failwith(Type::Int)]))
        );
        assert_eq!(stack, tc_stk![])
    }

    #[test]
    fn test_map() {
        fn test(input: Type, expected_output: Type, expected_overload: overloads::Map) {
            let mut stack = tc_stk![input];
            assert_eq!(
                typecheck_instruction(
                    &parse("MAP { SOME }").unwrap(),
                    &mut Gas::default(),
                    &mut stack
                ),
                Ok(Map(expected_overload, vec![ISome]))
            );
            assert_eq!(stack, tc_stk![expected_output]);
        }
        test(
            Type::new_list(Type::Int),
            Type::new_list(Type::new_option(Type::Int)),
            overloads::Map::List,
        );
        test(
            Type::new_option(Type::Int),
            Type::new_option(Type::new_option(Type::Int)),
            overloads::Map::Option,
        );
        test(
            Type::new_map(Type::String, Type::Int),
            Type::new_map(
                Type::String,
                Type::new_option(Type::new_pair(Type::String, Type::Int)),
            ),
            overloads::Map::Map,
        );
    }

    #[test]
    fn test_map_must_return_new_element() {
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { DROP; }").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::new_list(Type::Int)]
            ),
            Err(TcError::MapBlockEmptyStack)
        );
    }

    #[test]
    fn test_map_can_access_underlying_stack() {
        // The MAP block should be able to access elements deeper in the stack.
        let mut stack = tc_stk![Type::String, Type::new_list(Type::Nat)];
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { DROP; DUP; }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Map(overloads::Map::List, vec![Drop(None), Dup(None)]))
        );
        assert_eq!(stack, tc_stk![Type::String, Type::new_list(Type::String)]);
    }

    #[test]
    fn test_map_cannot_modify_type_of_underlying_stack() {
        // Given a stack of type `x : A`, the MAP block cannot modify the type
        // of the underlying stack, `A`.
        //
        // In this test, the MAP block attempts to change the stack
        // from `string : int` to `string : bool`
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { DIP { EQ }; }").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::Int, Type::new_list(Type::String)]
            ),
            Err(TcError::StacksNotEqual(
                stk![Type::Int],
                stk![Type::Bool],
                StacksNotEqualReason::TypesNotEqual(TypesNotEqual(Type::Int, Type::Bool))
            ))
        );
    }

    #[test]
    fn test_map_block_fail() {
        // Typechecking fails if all branches of a MAP block fail.
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { FAILWITH; }").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::new_list(Type::Int)]
            ),
            Err(TcError::MapBlockFail)
        );

        assert_eq!(
            typecheck_instruction(
                &parse("MAP { PUSH bool True; IF {FAILWITH} {FAILWITH}; }").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::new_list(Type::Int)]
            ),
            Err(TcError::MapBlockFail)
        );

        // Typechecking succeeds if at least one branch of a MAP block does NOT fail.
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { PUSH bool True; IF {SOME} {FAILWITH}; }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Map(
                overloads::Map::List,
                vec![
                    Push(Rc::new(TypedValue::Bool(true))),
                    If(vec![ISome], vec![Failwith(Type::Int)])
                ]
            ))
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::new_option(Type::Int))]);

        let mut stack = tc_stk![Type::new_list(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("MAP { PUSH bool True; IF {FAILWITH} {SOME}; }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Map(
                overloads::Map::List,
                vec![
                    Push(Rc::new(TypedValue::Bool(true))),
                    If(vec![Failwith(Type::Int)], vec![ISome])
                ]
            ))
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::new_option(Type::Int))]);
    }

    #[test]
    fn test_map_too_short() {
        too_short_test(&parse("MAP {}").unwrap(), Prim::MAP, 1)
    }

    #[test]
    fn test_map_arg_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("MAP { }").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::MAP,
                stack: stk![Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            typecheck_instruction(
                &parse("FAILWITH").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::Int]
            ),
            Ok(Failwith(Type::Int))
        );
    }

    #[test]
    fn test_never() {
        assert_eq!(
            typecheck_instruction(
                &parse("NEVER").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::Never]
            ),
            Ok(Never)
        );
    }

    #[test]
    fn test_failed_stacks() {
        macro_rules! test_fail {
            ($code:expr) => {
                assert_eq!(
                    typecheck_instruction(
                        &parse($code).unwrap(),
                        &mut Gas::default(),
                        &mut tc_stk![]
                    ),
                    Err(TcError::FailNotInTail)
                );
            };
        }
        test_fail!("{ PUSH int 1; FAILWITH; PUSH int 1 }");
        test_fail!("{ PUSH int 1; DIP { PUSH int 1; FAILWITH } }");
        test_fail!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1; FAILWITH }; GT }");
        test_fail!("{ PUSH (or never unit) (Right Unit); IF_LEFT { NEVER; PUSH int 1 } {} }");
        macro_rules! test_ok {
            ($code:expr) => {
                assert!(typecheck_instruction(
                    &parse($code).unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![]
                )
                .is_ok());
            };
        }
        test_ok!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1 }; GT }");
        test_ok!("{ PUSH bool True; IF { PUSH int 1 } { PUSH int 1; FAILWITH }; GT }");
        test_ok!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1; FAILWITH } }");
        test_ok!("{ PUSH bool True; LOOP { PUSH int 1; FAILWITH }; PUSH int 1 }");
        test_ok!("{ PUSH (or never unit) (Right Unit); IF_LEFT { NEVER } {}; DROP }");
    }

    #[test]
    fn string_values() {
        assert_eq!(
            typecheck_value(&"foo".into(), &mut Ctx::default(), &Type::String),
            Ok(TypedValue::String("foo".to_owned()))
        )
    }

    #[test]
    fn push_string_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH string "foo""#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::String("foo".to_owned()))))
        );
        assert_eq!(stack, tc_stk![Type::String]);
    }

    #[test]
    fn push_bls_fr_int() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH bls12_381_fr 100500"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::Bls12381Fr(
                bls::Fr::from_big_int(&100500.into())
            ))))
        );
        assert_eq!(stack, tc_stk![Type::Bls12381Fr]);
    }

    #[test]
    fn push_bls_fr_hex() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH bls12_381_fr 0x01"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::Bls12381Fr(
                bls::Fr::from_bytes(&[1]).unwrap()
            ))))
        );
        assert_eq!(stack, tc_stk![Type::Bls12381Fr]);
    }

    #[test]
    fn push_bls_fr_hex_too_long() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH bls12_381_fr 0x000000000000000000000000000000000000000000000000000000000000000000"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType("Bytes([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])".into(), Type::Bls12381Fr))
        );
    }

    #[test]
    fn push_bls_g1() {
        let mut stack = tc_stk![];
        let hex_val = "400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g1 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_bls12381_g1(
                bls::G1::from_bytes(&hex::decode(hex_val).unwrap()).unwrap()
            ))))
        );
    }

    #[test]
    fn push_bls_g1_short() {
        let mut stack = tc_stk![];
        let hex_val = "40000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g1 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "Bytes([64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])".into(),
                Type::Bls12381G1,
            ))
        );
    }

    #[test]
    fn push_bls_g1_long() {
        let mut stack = tc_stk![];
        let hex_val = "40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g1 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "Bytes([64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])".into(),
                Type::Bls12381G1,
            ))
        );
    }

    #[test]
    fn push_bls_g2() {
        let mut stack = tc_stk![];
        let hex_val = "400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g2 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_bls12381_g2(
                bls::G2::from_bytes(&hex::decode(hex_val).unwrap()).unwrap()
            ))))
        );
    }

    #[test]
    fn push_bls_g2_short() {
        let mut stack = tc_stk![];
        let hex_val = "40000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g2 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "Bytes([64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])".into(),
                Type::Bls12381G2,
            ))
        );
    }

    #[test]
    fn push_bls_g2_long() {
        let mut stack = tc_stk![];
        let hex_val = "40000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH bls12_381_g2 0x{hex_val}")).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "Bytes([64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0])".into(),
                Type::Bls12381G2,
            ))
        );
    }

    #[test]
    fn push_unit_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH unit Unit").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::Unit)))
        );
        assert_eq!(stack, tc_stk![Type::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("UNIT").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Unit)
        );
        assert_eq!(stack, tc_stk![Type::Unit]);
    }

    #[test]
    fn push_pair_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (pair int nat bool) (Pair -5 3 False)").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_pair(
                TypedValue::int(-5),
                TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
            ))))
        );
        assert_eq!(
            stack,
            tc_stk![Type::new_pair(
                Type::Int,
                Type::new_pair(Type::Nat, Type::Bool)
            )]
        );
    }

    #[test]
    fn push_or_value_left() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (or int bool) (Left 1)").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_or(or::Or::Left(
                TypedValue::int(1)
            )))))
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Bool)]);
    }

    #[test]
    fn push_or_value_right() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (or int bool) (Right False)").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_or(or::Or::Right(
                TypedValue::Bool(false)
            )))))
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Bool)]);
    }

    #[test]
    fn push_option_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (option nat) (Some 3)").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_option(Some(
                TypedValue::nat(3)
            )))))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Nat)]);
    }

    #[test]
    fn push_option_none_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (option nat) None").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::new_option(None))))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Nat)]);
    }

    #[test]
    fn car() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CAR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Seq(vec![
                Push(Rc::new(TypedValue::new_pair(
                    TypedValue::int(-5),
                    TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
                ))),
                Car
            ]))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn cdr() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CDR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Seq(vec![
                Push(Rc::new(TypedValue::new_pair(
                    TypedValue::int(-5),
                    TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
                ))),
                Cdr
            ]))
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Bool)]);
    }

    #[test]
    fn car_fail() {
        let mut stack = tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("CAR").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CAR,
                stack: stk![Type::Unit],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Unit)),
            })
        );
    }

    #[test]
    fn cdr_fail() {
        let mut stack = tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("CDR").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CDR,
                stack: stk![Type::Unit],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Unit)),
            })
        );
    }

    #[test]
    fn pair() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Pair)
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Int)]);
    }

    #[test]
    fn pair_n_3() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR 3").unwrap(), &mut Gas::default(), &mut stack),
            Ok(PairN(3))
        );
        assert_eq!(
            stack,
            tc_stk![
                Type::String,
                Type::new_pair(Type::Nat, Type::new_pair(Type::Int, Type::Unit))
            ]
        );
    }

    #[test]
    fn pair_n_4() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR 4").unwrap(), &mut Gas::default(), &mut stack),
            Ok(PairN(4))
        );
        assert_eq!(
            stack,
            tc_stk![Type::new_pair(
                Type::Nat,
                Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
            )]
        );
    }

    #[test]
    fn pair_n_neg() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR -1").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::ExpectedU10((-1).into()))
        );
    }

    #[test]
    fn pair_n_0() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR 0").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::PairN01(Prim::PAIR, 0))
        );
    }

    #[test]
    fn pair_n_1() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR 1").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::PairN01(Prim::PAIR, 1))
        );
    }

    #[test]
    fn pair_n_too_large() {
        let mut stack = tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(
                &parse("PAIR 1024").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::ExpectedU10(1024.into()))
        );
    }

    #[test]
    fn pair_n_too_short() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("PAIR 10").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::PAIR,
                stack: stk![Type::Int, Type::Nat],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 10 })
            })
        );
    }

    #[test]
    fn unpair() {
        let mut stack = tc_stk![Type::new_pair(Type::Nat, Type::Int)];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Unpair)
        );
        assert_eq!(stack, tc_stk![Type::Int, Type::Nat]);
    }

    #[test]
    fn unpair_n_3() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR 3").unwrap(), &mut Gas::default(), &mut stack),
            Ok(UnpairN(3))
        );
        assert_eq!(
            stack,
            tc_stk![
                Type::new_pair(Type::Unit, Type::String),
                Type::Int,
                Type::Nat
            ]
        )
    }

    #[test]
    fn unpair_n_4() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR 4").unwrap(), &mut Gas::default(), &mut stack),
            Ok(UnpairN(4))
        );
        assert_eq!(
            stack,
            tc_stk![Type::String, Type::Unit, Type::Int, Type::Nat],
        );
    }

    #[test]
    fn unpair_n_neg() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(
                &parse("UNPAIR -1").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::ExpectedU10((-1).into()))
        );
    }

    #[test]
    fn unpair_n_0() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR 0").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::PairN01(Prim::UNPAIR, 0))
        );
    }

    #[test]
    fn unpair_n_1() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR 1").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::PairN01(Prim::UNPAIR, 1))
        );
    }

    #[test]
    fn unpair_n_too_large() {
        let mut stack = tc_stk![Type::new_pair(
            Type::Nat,
            Type::new_pair(Type::Int, Type::new_pair(Type::Unit, Type::String))
        )];
        assert_eq!(
            typecheck_instruction(
                &parse("UNPAIR 1024").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::ExpectedU10(1024.into()))
        );
    }

    #[test]
    fn unpair_n_too_short() {
        let mut stack = tc_stk![]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR 2").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UNPAIR,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );
    }

    #[test]
    fn pair_car() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(
                &parse("{ PAIR; CAR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Seq(vec![Pair, Car]))
        );
        assert_eq!(stack, tc_stk![Type::Nat]);
    }

    #[test]
    fn pair_cdr() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(
                &parse("{ PAIR; CDR }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Seq(vec![Pair, Cdr]))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_none() {
        let mut stack = tc_stk![Type::new_option(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_NONE { PUSH int 5; } {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(IfNone(vec![Push(Rc::new(TypedValue::int(5)))], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_cons() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_CONS { DROP 2 } {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(IfCons(vec![Drop(Some(2))], vec![]))
        );
        assert_eq!(stack, tc_stk![]);
        too_short_test(&parse("IF_CONS {} {}").unwrap(), Prim::IF_CONS, 1)
    }

    #[test]
    fn if_cons_mismatch() {
        let mut stack = tc_stk![Type::String];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_CONS { DROP 2 } {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF_CONS,
                stack: stk![Type::String],
                reason: Some(NoMatchingOverloadReason::ExpectedList(Type::String))
            })
        );
    }

    #[test]
    fn if_cons_branch_mismatch() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_CONS {} {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::StacksNotEqual(
                stk![],
                stk![Type::new_list(Type::Int), Type::Int],
                StacksNotEqualReason::LengthsDiffer(0, 2)
            ))
        );
    }

    #[test]
    fn if_left() {
        let mut stack = tc_stk![Type::new_or(Type::Int, Type::Bool)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_LEFT { GT } {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(IfLeft(vec![Gt], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn if_left_too_short() {
        too_short_test(&parse("IF_LEFT {} {}").unwrap(), Prim::IF_LEFT, 1)
    }

    #[test]
    fn if_left_same_branch_ty() {
        let mut stack = tc_stk![Type::new_or(Type::Int, Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_LEFT {} {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(IfLeft(vec![], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_left_mismatch() {
        let mut stack = tc_stk![Type::String];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_LEFT {} {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF_LEFT,
                stack: stk![Type::String],
                reason: Some(NoMatchingOverloadReason::ExpectedOr(Type::String))
            })
        );
    }

    #[test]
    fn if_left_branch_mismatch() {
        let mut stack = tc_stk![Type::new_or(Type::Int, Type::Nat)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_LEFT {} {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::StacksNotEqual(
                stk![Type::Int],
                stk![Type::Nat],
                TypesNotEqual(Type::Int, Type::Nat).into(),
            ))
        );
    }

    #[test]
    fn if_none_fail() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_NONE { PUSH int 5; } {}").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF_NONE,
                stack: stk![Type::Int],
                reason: Some(NoMatchingOverloadReason::ExpectedOption(Type::Int)),
            })
        );
    }

    #[test]
    fn some() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("SOME").unwrap(), &mut Gas::default(), &mut stack),
            Ok(ISome)
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn none() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("NONE int").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Instruction::None)
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn compare_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("COMPARE").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Compare)
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn compare_int_fail() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("COMPARE").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::COMPARE,
                stack: stk![Type::Int, Type::Nat],
                reason: Some(TypesNotEqual(Type::Nat, Type::Int).into())
            })
        );
    }

    #[test]
    fn amount() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("AMOUNT").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Amount)
        );
        assert_eq!(stack, tc_stk![Type::Mutez]);
    }

    #[test]
    fn push_int_list() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (list int) { 1; 2; 3 }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::List(
                vec![TypedValue::int(1), TypedValue::int(2), TypedValue::int(3),].into()
            ))))
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn push_int_list_fail() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (list int) { 1; Unit; 3 }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "App(Unit, [], [])".into(),
                Type::Int
            ))
        );
    }

    #[test]
    fn nil() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("NIL int").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Nil)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Cons)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons_too_short() {
        too_short_test(&parse("CONS").unwrap(), Prim::CONS, 2);
    }

    #[test]
    fn cons_mismatch_elt() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into())
        );
    }

    #[test]
    fn cons_mismatch_list() {
        let mut stack = tc_stk![Type::String, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CONS,
                stack: stk![Type::String, Type::Nat],
                reason: Some(NoMatchingOverloadReason::ExpectedList(Type::String))
            })
        );
    }

    #[test]
    fn nil_operation() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("NIL operation").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Nil)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Operation)]);
    }

    #[test]
    fn failwith_operation() {
        let mut stack = tc_stk![Type::new_list(Type::Operation)];
        assert_eq!(
            typecheck_instruction(&parse("FAILWITH").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn apply_rejects_big_map_capture() {
        // L1 calls `check_packable ~allow_contract:false` on the captured
        // type; in MIR the equivalent property is `Pushable` (the only
        // "packable, contracts disallowed" property). A `big_map` capture is
        // not pushable, so APPLY must reject it, matching L1.
        let mut stack = tc_stk![
            Type::new_lambda(
                Type::new_pair(Type::new_big_map(Type::Nat, Type::String), Type::Unit),
                Type::Unit,
            ),
            Type::new_big_map(Type::Nat, Type::String),
        ];
        assert_eq!(
            typecheck_instruction(&parse("APPLY").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_big_map(Type::Nat, Type::String),
            ))
        );
    }

    #[test]
    fn apply_rejects_ticket_capture() {
        let mut stack = tc_stk![
            Type::new_lambda(
                Type::new_pair(Type::new_ticket(Type::String), Type::Unit),
                Type::Unit,
            ),
            Type::new_ticket(Type::String),
        ];
        assert_eq!(
            typecheck_instruction(&parse("APPLY").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_ticket(Type::String),
            ))
        );
    }

    #[test]
    fn apply_rejects_contract_capture() {
        // `contract` is the type that separates pushable from packable: it is
        // packable but not pushable. APPLY rejects it via the Pushable check,
        // matching L1's `check_packable ~allow_contract:false`. Had we used
        // Packable instead, this capture would have been wrongly accepted.
        let mut stack = tc_stk![
            Type::new_lambda(
                Type::new_pair(Type::new_contract(Type::Unit), Type::Unit),
                Type::Unit,
            ),
            Type::new_contract(Type::Unit),
        ];
        assert_eq!(
            typecheck_instruction(&parse("APPLY").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_contract(Type::Unit),
            ))
        );
    }

    #[test]
    fn apply_accepts_packable_capture() {
        // Control: a pushable capture (`nat`) is still accepted, and APPLY
        // partially applies the lambda down to `lambda unit unit`.
        let mut stack = tc_stk![
            Type::new_lambda(Type::new_pair(Type::Nat, Type::Unit), Type::Unit),
            Type::Nat,
        ];
        assert!(
            typecheck_instruction(&parse("APPLY").unwrap(), &mut Gas::default(), &mut stack).is_ok()
        );
        assert_eq!(stack, tc_stk![Type::new_lambda(Type::Unit, Type::Unit)]);
    }

    #[test]
    fn never_wrong_type() {
        let mut stack = tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("NEVER").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::NEVER,
                stack: stk![Type::Unit],
                reason: Some(NoMatchingOverloadReason::TypesNotEqual(TypesNotEqual(
                    Type::Never,
                    Type::Unit
                )))
            })
        );
    }

    #[test]
    fn concat_two_strings() {
        let mut stack = tc_stk![Type::String, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("CONCAT").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Concat(overloads::Concat::TwoStrings))
        );
        assert_eq!(stack, tc_stk![Type::String]);
    }

    #[test]
    fn concat_list_of_strings() {
        let mut stack = tc_stk![Type::new_list(Type::String)];
        assert_eq!(
            typecheck_instruction(&parse("CONCAT").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Concat(overloads::Concat::ListOfStrings))
        );
        assert_eq!(stack, tc_stk![Type::String]);
    }

    #[test]
    fn push_set() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { 1; 2 }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::Set(
                [TypedValue::int(1), TypedValue::int(2)]
                    .into_iter()
                    .map(Rc::new)
                    .collect()
            ))))
        );
        assert_eq!(stack, tc_stk![Type::new_set(Type::Int)]);
    }

    #[test]
    fn push_set_unsorted() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { 2; 1 }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::ElementsNotSorted(Type::new_set(Type::Int)))
        );
    }

    #[test]
    fn push_set_incomparable() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set (list int)) { }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn push_set_wrong_key_type() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { "1"; 2 }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "String(\"1\")".into(),
                Type::Int
            ))
        );
    }

    #[test]
    fn push_set_duplicate_key() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { 1; 1 }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::DuplicateElements(Type::new_set(Type::Int)))
        );
    }

    #[test]
    fn push_map() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt 1 "foo"; Elt 2 "bar" }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Push(Rc::new(TypedValue::Map(
                [
                    (TypedValue::int(1), TypedValue::String("foo".to_owned())),
                    (TypedValue::int(2), TypedValue::String("bar".to_owned()))
                ]
                .into_iter()
                .map(|(key, value)| (Rc::new(key), Rc::new(value)))
                .collect()
            ))))
        );
        assert_eq!(stack, tc_stk![Type::new_map(Type::Int, Type::String)]);
    }

    #[test]
    fn push_map_unsorted() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt 2 "foo"; Elt 1 "bar" }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::ElementsNotSorted(Type::new_map(
                Type::Int,
                Type::String
            )))
        );
    }

    #[test]
    fn push_map_incomparable() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map (list int) string) { Elt { 2 } "foo"; Elt { 1 } "bar" }"#)
                    .unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn push_map_incomparable_empty() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map (list int) string) { }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn push_map_wrong_key_type() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt "1" "foo"; Elt 2 "bar" }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                "String(\"1\")".into(),
                Type::Int
            ))
        );
    }

    #[test]
    fn push_map_wrong_elt() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt 1 "foo"; "bar" }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidEltForMap(
                "String(\"bar\")".to_owned(),
                Type::new_map(Type::Int, Type::String)
            ))
        );
    }

    #[test]
    fn push_map_duplicate_key() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt 1 "foo"; Elt 1 "bar" }"#).unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::DuplicateElements(Type::new_map(
                Type::Int,
                Type::String
            )))
        );
    }

    #[test]
    fn empty_set() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("EMPTY_SET int").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(EmptySet)
        );
        assert_eq!(stack, tc_stk![Type::new_set(Type::Int)]);
    }

    #[test]
    fn empty_big_map() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("EMPTY_BIG_MAP int unit").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(EmptyBigMap(Type::Int, Type::Unit))
        );
        assert_eq!(stack, tc_stk![Type::new_big_map(Type::Int, Type::Unit)]);
    }

    #[test]
    fn empty_set_incomparable() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("EMPTY_SET operation").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn get_map() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("GET").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Get(overloads::Get::Map))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn get_big_map() {
        let mut stack = tc_stk![Type::new_big_map(Type::Int, Type::String), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("GET").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Get(overloads::Get::BigMap))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn get_map_incomparable() {
        assert_eq!(
            parse("GET").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("map (list int) string").unwrap(),
                    parse("list int").unwrap(),
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn get_map_wrong_type() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("GET").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into()),
        );
    }

    mod get_n {
        use super::*;

        #[track_caller]
        fn check(n: u16, ty: Type, field_ty: Type) {
            let mut stack = tc_stk![ty];
            assert_eq!(
                typecheck_instruction(
                    &parse(&format!("GET {n}")).unwrap(),
                    &mut Gas::default(),
                    &mut stack
                ),
                Ok(GetN(n))
            );
            assert_eq!(stack, tc_stk![field_ty])
        }

        #[test]
        fn ok_0() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(0, ty.clone(), ty);
        }

        #[test]
        fn ok_1() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(1, ty, Type::Unit);
        }

        #[test]
        fn ok_2() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(
                2,
                ty,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
        }

        #[test]
        fn ok_3() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(3, ty, Type::Nat);
        }

        #[test]
        fn ok_4() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(4, ty, Type::new_pair(Type::String, Type::Int));
        }

        #[test]
        fn ok_5() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(5, ty, Type::String);
        }

        #[test]
        fn ok_6() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            check(6, ty, Type::Int);
        }

        #[test]
        fn fail_7() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            let mut stack = tc_stk![ty.clone()];
            assert_eq!(
                typecheck_instruction(&parse("GET 7").unwrap(), &mut Gas::default(), &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::GET,
                    stack: stk![ty],
                    reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Int))
                })
            );
        }

        #[test]
        fn too_short() {
            too_short_test(&parse("GET 0").unwrap(), Prim::GET, 1);
        }

        #[test]
        fn too_large() {
            let ty = Type::new_pair(
                Type::Unit,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int)),
            );
            let mut stack = tc_stk![ty];
            assert_eq!(
                typecheck_instruction(&parse("GET 1024").unwrap(), &mut Gas::default(), &mut stack),
                Err(TcError::ExpectedU10(1024.into()))
            );
        }
    }

    mod update_n {
        use super::*;

        #[track_caller]
        fn check(n: u16, ty: Type, new_ty: Type) {
            let mut stack = tc_stk![ty, Type::Unit];
            assert_eq!(
                typecheck_instruction(
                    &parse(&format!("UPDATE {n}")).unwrap(),
                    &mut Gas::default(),
                    &mut stack
                ),
                Ok(UpdateN(n))
            );
            assert_eq!(stack, tc_stk![new_ty])
        }

        #[test]
        fn ok_0() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));
            check(0, ty, Type::Unit);
        }

        #[test]
        fn ok_1() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));
            check(
                1,
                ty,
                Type::new_pair(Type::Unit, Type::new_pair(Type::String, Type::Int)),
            );
        }

        #[test]
        fn ok_2() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));
            check(2, ty, Type::new_pair(Type::Nat, Type::Unit));
        }

        #[test]
        fn ok_3() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));
            check(
                3,
                ty,
                Type::new_pair(Type::Nat, Type::new_pair(Type::Unit, Type::Int)),
            );
        }

        #[test]
        fn ok_4() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));
            check(
                4,
                ty,
                Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Unit)),
            );
        }

        #[test]
        fn fail_5() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));

            let mut stack = tc_stk![ty.clone(), Type::Unit];
            assert_eq!(
                typecheck_instruction(&parse("UPDATE 5").unwrap(), &mut Gas::default(), &mut stack),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::UPDATE,
                    stack: stk![ty, Type::Unit],
                    reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Int))
                })
            );
        }

        #[test]
        fn too_short() {
            too_short_test(&parse("UPDATE 0").unwrap(), Prim::UPDATE, 2);
        }

        #[test]
        fn too_large() {
            let ty = Type::new_pair(Type::Nat, Type::new_pair(Type::String, Type::Int));

            let mut stack = tc_stk![ty, Type::Unit];
            assert_eq!(
                typecheck_instruction(
                    &parse("UPDATE 1024").unwrap(),
                    &mut Gas::default(),
                    &mut stack
                ),
                Err(TcError::ExpectedU10(1024.into()))
            );
        }
    }

    #[test]
    fn mem_map() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("MEM").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Mem(overloads::Mem::Map))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn mem_big_map() {
        let mut stack = tc_stk![Type::new_big_map(Type::Int, Type::String), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("MEM").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Mem(overloads::Mem::BigMap))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn mem_map_incomparable() {
        assert_eq!(
            parse("MEM").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("map (list int) string").unwrap(),
                    parse("list int").unwrap(),
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn mem_map_wrong_type() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("MEM").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into()),
        );
    }

    #[test]
    fn mem_set() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("MEM").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Mem(overloads::Mem::Set))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn mem_set_wrong_type() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("MEM").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into()),
        );
    }

    #[test]
    fn update_set() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Bool, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Update(overloads::Update::Set))
        );
        assert_eq!(stack, tc_stk![Type::new_set(Type::Int)]);
    }

    #[test]
    fn update_set_wrong_ty() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Bool, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into())
        );
    }

    #[test]
    fn update_set_incomparable() {
        assert_eq!(
            parse("UPDATE").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("set (list int)").unwrap(),
                    parse("bool").unwrap(),
                    parse("list int").unwrap()
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn update_map() {
        let mut stack = tc_stk![
            Type::new_map(Type::Int, Type::String),
            Type::new_option(Type::String),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Update(overloads::Update::Map))
        );
        assert_eq!(stack, tc_stk![Type::new_map(Type::Int, Type::String)]);
    }

    #[test]
    fn update_big_map() {
        let mut stack = tc_stk![
            Type::new_big_map(Type::Int, Type::String),
            Type::new_option(Type::String),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Update(overloads::Update::BigMap))
        );
        assert_eq!(stack, tc_stk![Type::new_big_map(Type::Int, Type::String)]);
    }

    #[test]
    fn update_map_wrong_ty() {
        let mut stack = tc_stk![
            Type::new_map(Type::Int, Type::String),
            Type::new_option(Type::Nat),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Gas::default(), &mut stack),
            Err(TypesNotEqual(Type::String, Type::Nat).into())
        );
    }

    #[test]
    fn update_map_incomparable() {
        assert_eq!(
            parse("UPDATE").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("map (list int) string").unwrap(),
                    parse("option string").unwrap(),
                    parse("list int").unwrap(),
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn get_and_update_map() {
        let mut stack = tc_stk![
            Type::new_map(Type::Int, Type::String),
            Type::new_option(Type::String),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(
                &parse("GET_AND_UPDATE").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(GetAndUpdate(overloads::GetAndUpdate::Map))
        );
        assert_eq!(
            stack,
            tc_stk![
                Type::new_map(Type::Int, Type::String),
                Type::new_option(Type::String)
            ]
        );
    }

    #[test]
    fn get_and_update_big_map() {
        let mut stack = tc_stk![
            Type::new_big_map(Type::Int, Type::String),
            Type::new_option(Type::String),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(
                &parse("GET_AND_UPDATE").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(GetAndUpdate(overloads::GetAndUpdate::BigMap))
        );
        assert_eq!(
            stack,
            tc_stk![
                Type::new_big_map(Type::Int, Type::String),
                Type::new_option(Type::String)
            ]
        );
    }

    #[test]
    fn get_and_update_map_wrong_ty() {
        let mut stack = tc_stk![
            Type::new_map(Type::Int, Type::String),
            Type::new_option(Type::Nat),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(
                &parse("GET_AND_UPDATE").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Err(TypesNotEqual(Type::String, Type::Nat).into())
        );
    }

    #[test]
    fn get_and_update_map_incomparable() {
        assert_eq!(
            parse("GET_AND_UPDATE").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("map (list int) string").unwrap(),
                    parse("option string").unwrap(),
                    parse("list int").unwrap(),
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn get_and_update_map_too_short() {
        too_short_test(&parse("GET_AND_UPDATE").unwrap(), Prim::GET_AND_UPDATE, 3)
    }

    #[test]
    fn size() {
        fn check(inp_ty: Type, expected_overload: overloads::Size) {
            let mut stack = tc_stk![inp_ty];
            assert_eq!(
                typecheck_instruction(&parse("SIZE").unwrap(), &mut Gas::default(), &mut stack),
                Ok(Size(expected_overload))
            );
            assert_eq!(stack, tc_stk![Type::Nat]);
        }
        check(Type::String, overloads::Size::String);
        check(Type::Bytes, overloads::Size::Bytes);
        check(Type::new_list(Type::Int), overloads::Size::List);
        check(Type::new_set(Type::Int), overloads::Size::Set);
        check(Type::new_map(Type::Int, Type::Nat), overloads::Size::Map);
    }

    #[test]
    fn size_wrong_ty() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("SIZE").unwrap(), &mut Gas::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SIZE,
                stack: stk![Type::Int],
                reason: None
            })
        );
    }

    #[test]
    fn test_size_short() {
        too_short_test(&parse("SIZE").unwrap(), Prim::SIZE, 1);
    }

    #[test]
    fn seq() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(
                &parse("{ { PAIR }; {{ CAR; }}; {}; {{{}}}; {{{{{DROP}}}}} }").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Seq(vec![
                Seq(vec![Pair]),
                Seq(vec![Seq(vec![Car])]),
                Seq(vec![]),
                Seq(vec![Seq(vec![Seq(vec![])])]),
                Seq(vec![Seq(vec![Seq(vec![Seq(vec![Seq(vec![Drop(None)])])])])])
            ]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn add_int_nat() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Add(overloads::Add::IntNat))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Add(overloads::Add::NatInt))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[track_caller]
    fn too_short_test(instr: &Micheline, prim: Prim, len: usize) {
        for n in 0..len {
            let mut gas = Gas::default();
            assert_eq!(
                typecheck_instruction(instr, &mut gas, &mut tc_stk![Type::Unit; n]),
                Err(TcError::NoMatchingOverload {
                    instr: prim,
                    stack: stk![Type::Unit; n],
                    reason: Some(NoMatchingOverloadReason::StackTooShort { expected: len })
                })
            );
        }
    }

    #[test]
    fn test_add_short() {
        too_short_test(&parse("ADD").unwrap(), Prim::ADD, 2);
    }

    #[test]
    fn test_add_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::ADD,
                stack: stk![Type::String, Type::String],
                reason: None
            })
        );
    }

    #[test]
    fn test_dup0() {
        let mut stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DUP 0").unwrap(), &mut gas, &mut stack),
            Err(TcError::Dup0)
        );
    }

    mod int_comparison {
        use super::*;

        macro_rules! test {
            ($op:ident, $instr:expr) => {
                #[allow(non_snake_case)]
                mod $op {
                    use super::*;

                    #[test]
                    fn ok() {
                        let mut stack = tc_stk![Type::Int];
                        let expected_stack = tc_stk![Type::Bool];
                        let mut gas = Gas::default();
                        assert_eq!(
                            typecheck_instruction(
                                &parse(stringify!($op)).unwrap(),
                                &mut gas,
                                &mut stack
                            ),
                            Ok($instr)
                        );
                        assert_eq!(stack, expected_stack);
                        assert!(gas.milligas().unwrap() < Gas::default().milligas().unwrap());
                    }

                    #[test]
                    fn mismatch() {
                        let mut stack = tc_stk![Type::String];
                        let mut gas = Gas::default();
                        assert_eq!(
                            typecheck_instruction(
                                &parse(stringify!($op)).unwrap(),
                                &mut gas,
                                &mut stack
                            ),
                            Err(TcError::NoMatchingOverload {
                                instr: Prim::$op,
                                stack: stk![Type::String],
                                reason: Some(TypesNotEqual(Type::Int, Type::String).into())
                            })
                        );
                    }

                    #[test]
                    fn short() {
                        too_short_test(&parse(stringify!($op)).unwrap(), Prim::$op, 1);
                    }
                }
            };
        }

        test!(EQ, Eq);
        test!(NEQ, Neq);
        test!(GT, Gt);
        test!(LT, Lt);
        test!(GE, Ge);
        test!(LE, Le);
    }

    #[test]
    fn test_if_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("IF {} {}").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Bool, Type::String).into())
            })
        );
    }

    #[test]
    fn test_if_short() {
        too_short_test(&parse("IF {} {}").unwrap(), Prim::IF, 1);
    }

    #[test]
    fn test_if_none_short() {
        too_short_test(&parse("IF_NONE {} {}").unwrap(), Prim::IF_NONE, 1);
    }

    #[test]
    fn test_loop_short() {
        too_short_test(&parse("LOOP {}").unwrap(), Prim::LOOP, 1);
    }

    #[test]
    fn test_loop_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("LOOP {}").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::LOOP,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Bool, Type::String).into()),
            })
        );
    }

    #[test]
    fn test_unpair_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UNPAIR,
                stack: stk![Type::String],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::String)),
            })
        );
    }

    #[test]
    fn test_swap_short() {
        too_short_test(&parse("SWAP").unwrap(), Prim::SWAP, 2);
    }

    #[test]
    fn test_pair_short() {
        too_short_test(&parse("PAIR").unwrap(), Prim::PAIR, 2);
    }

    #[test]
    fn test_concat_short() {
        too_short_test(&parse("CONCAT").unwrap(), Prim::CONCAT, 1);
    }

    #[test]
    fn test_mem_short() {
        too_short_test(&parse("MEM").unwrap(), Prim::MEM, 2);
    }

    #[test]
    fn test_get_short() {
        too_short_test(&parse("GET").unwrap(), Prim::GET, 2);
    }

    #[test]
    fn test_update_short() {
        too_short_test(&parse("UPDATE").unwrap(), Prim::UPDATE, 3);
    }

    #[test]
    fn test_failwith_short() {
        too_short_test(&parse("FAILWITH").unwrap(), Prim::FAILWITH, 1);
    }

    #[test]
    fn test_never_short() {
        too_short_test(&parse("NEVER").unwrap(), Prim::NEVER, 1);
    }

    #[test]
    fn test_car_short() {
        too_short_test(&parse("CAR").unwrap(), Prim::CAR, 1);
    }

    #[test]
    fn test_cdr_short() {
        too_short_test(&parse("CDR").unwrap(), Prim::CDR, 1);
    }

    #[test]
    fn test_some_short() {
        too_short_test(&parse("SOME").unwrap(), Prim::SOME, 1);
    }

    #[test]
    fn test_compare_short() {
        too_short_test(&parse("COMPARE").unwrap(), Prim::COMPARE, 2);
    }

    #[test]
    fn test_unpair_short() {
        too_short_test(&parse("UNPAIR").unwrap(), Prim::UNPAIR, 1);
    }

    #[test]
    fn test_compare_gas_exhaustion() {
        let gas = &mut Gas::new(gas::tc_cost::INSTR_STEP);
        assert_eq!(
            typecheck_instruction(
                &parse("COMPARE").unwrap(),
                gas,
                &mut tc_stk![Type::Unit, Type::Unit]
            ),
            Err(TcError::OutOfGas(OutOfGas))
        );
    }

    #[test]
    fn test_compare_incomparable() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("COMPARE").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Operation, Type::Operation]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_get_mismatch() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("GET").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Unit, Type::Unit]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::GET,
                stack: stk![Type::Unit, Type::Unit],
                reason: None,
            })
        );
    }

    #[test]
    fn test_mem_mismatch() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("MEM").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Unit, Type::Unit]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::MEM,
                stack: stk![Type::Unit, Type::Unit],
                reason: None,
            })
        );
    }

    #[test]
    fn test_update_mismatch() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("UPDATE").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Unit, Type::Unit, Type::Unit]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UPDATE,
                stack: stk![Type::Unit, Type::Unit, Type::Unit],
                reason: None,
            })
        );
    }

    #[test]
    fn test_push_operation_fail() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH operation Unit").unwrap(),
                &mut gas,
                &mut tc_stk![]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_non_passable_parameter() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter operation;",
                "storage nat;",
                "code FAILWITH"
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Passable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_non_storable_storage() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter nat;",
                "storage operation;",
                "code FAILWITH"
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Storable,
                Type::Operation
            ))
        );
    }

    /// Regression for L2-1635: a parent whose CREATE_CONTRACT embeds a child
    /// script with an L1-invalid view must be rejected at origination
    /// (typecheck_views=true), matching L1's parse_views on the embedded
    /// script. Re-typecheck paths (typecheck_views=false, used for entrypoint
    /// extraction and stored-contract execution) must NOT re-validate child
    /// views, so contracts originated before this check keep executing and no
    /// extra gas is charged.
    #[test]
    fn create_contract_validates_child_views_at_origination() {
        fn origination(src: &str) -> Result<(), TcError> {
            parse_contract_script(src)
                .unwrap()
                .split_script()
                .unwrap()
                .typecheck_script(&mut Gas::default(), true, true)
                .map(|_| ())
        }
        fn re_typecheck(src: &str) -> Result<(), TcError> {
            parse_contract_script(src)
                .unwrap()
                .split_script()
                .unwrap()
                .typecheck_script(&mut Gas::default(), true, false)
                .map(|_| ())
        }

        // Child view output is a `big_map` (forbidden ViewOutput).
        let bad_output = concat!(
            "parameter unit; storage unit;",
            "code { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "  CREATE_CONTRACT",
            "    { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "      view \"bad\" unit (big_map string nat) { DROP; EMPTY_BIG_MAP string nat } };",
            "  DROP; DROP; UNIT; NIL operation; PAIR }"
        );
        assert_eq!(
            origination(bad_output),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewOutput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
        // Re-typecheck must not re-validate the child view (no brick, no extra gas).
        assert!(re_typecheck(bad_output).is_ok());

        // Child view body returns the wrong type.
        let bad_return = concat!(
            "parameter unit; storage unit;",
            "code { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "  CREATE_CONTRACT",
            "    { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "      view \"bad\" unit unit { DROP; PUSH nat 0 } };",
            "  DROP; DROP; UNIT; NIL operation; PAIR }"
        );
        assert!(matches!(
            origination(bad_return),
            Err(TcError::StacksNotEqual(..))
        ));

        // Child view body uses a forbidden-in-view instruction.
        let effectful = concat!(
            "parameter unit; storage unit;",
            "code { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "  CREATE_CONTRACT",
            "    { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "      view \"bad\" unit unit { CDR; DROP; NONE key_hash; SET_DELEGATE; DROP; UNIT } };",
            "  DROP; DROP; UNIT; NIL operation; PAIR }"
        );
        assert_eq!(
            origination(effectful),
            Err(TcError::ForbiddenInView(Prim::SET_DELEGATE))
        );

        // Positive control: a valid child view still typechecks.
        let good = concat!(
            "parameter unit; storage unit;",
            "code { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "  CREATE_CONTRACT",
            "    { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "      view \"good\" unit nat { DROP; PUSH nat 0 } };",
            "  DROP; DROP; UNIT; NIL operation; PAIR }"
        );
        assert!(origination(good).is_ok());

        // Bad child view inside a value-level lambda nested in a PUSHed value
        // (not a direct PUSH-of-lambda): must also be rejected at origination,
        // matching L1 (parse_data typechecks lambda values eagerly). The lambda
        // is wrapped in a pair so it flows through the value typechecker rather
        // than the instruction-level OpenLambda path.
        let nested_lambda = concat!(
            "parameter unit; storage unit;",
            "code { DROP;",
            "  PUSH (pair nat (lambda unit unit))",
            "    (Pair 0 { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "      CREATE_CONTRACT",
            "        { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "          view \"bad\" unit (big_map string nat) { DROP; EMPTY_BIG_MAP string nat } };",
            "      DROP; DROP; UNIT });",
            "  DROP; UNIT; NIL operation; PAIR }"
        );
        assert_eq!(
            origination(nested_lambda),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewOutput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
        assert!(re_typecheck(nested_lambda).is_ok());

        // Bad GRANDCHILD view inside a CREATE_CONTRACT nested in a LAMBDA in the
        // parent's own VIEW body. CREATE_CONTRACT is forbidden directly in a
        // view, but allowed inside a lambda-in-view, and L1 validates the
        // embedded child's views there too. Must be rejected at origination.
        let lambda_in_view = concat!(
            "parameter unit; storage unit;",
            "code { CAR; NIL operation; PAIR };",
            "view \"v\" unit unit",
            "  { DROP;",
            "    LAMBDA unit unit",
            "      { DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "        CREATE_CONTRACT",
            "          { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "            view \"bad\" unit (big_map string nat) { DROP; EMPTY_BIG_MAP string nat } };",
            "        DROP; DROP; UNIT };",
            "    DROP; UNIT }"
        );
        assert_eq!(
            origination(lambda_in_view),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewOutput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
        assert!(re_typecheck(lambda_in_view).is_ok());

        // Storage-VALUE path: at origination the kernel typechecks the initial
        // storage value via `typecheck_storage_with_views` (Enabled), a path
        // `typecheck_script` never reaches. A storage lambda embedding a bad
        // child view is rejected at origination, yet still accepted on the
        // runtime re-typecheck (`typecheck_storage`, Disabled).
        let parent = parse_contract_script(
            "parameter unit; storage (lambda unit unit); code { CDR; NIL operation; PAIR }",
        )
        .unwrap()
        .split_script()
        .unwrap()
        .typecheck_script(&mut Gas::default(), true, false)
        .unwrap();
        let storage_lambda = parse(concat!(
            "{ DROP; UNIT; PUSH mutez 0; NONE key_hash;",
            "  CREATE_CONTRACT",
            "    { parameter unit; storage unit; code { CDR; NIL operation; PAIR };",
            "      view \"bad\" unit (big_map string nat) { DROP; EMPTY_BIG_MAP string nat } };",
            "  DROP; DROP; UNIT }"
        ))
        .unwrap();
        assert_eq!(
            parent
                .typecheck_storage_with_views(
                    &mut Ctx::default(),
                    &storage_lambda,
                    TypecheckViews::Enabled,
                )
                .map(|_| ()),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewOutput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
        assert!(parent
            .typecheck_storage(&mut Ctx::default(), &storage_lambda)
            .is_ok());
    }

    #[test]
    fn test_invalid_map() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (map (list unit) unit);",
                "storage nat;",
                "code FAILWITH;",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    // Test that the empty sequence cannot be given the type `map (list unit) unit`
    // because `list unit` is not comparable and therefore `map (list unit) unit` is
    // not well formed.
    fn test_invalid_map_value() {
        let mut ctx = Ctx::default();
        assert_eq!(
            Micheline::Seq(&[]).typecheck_value(&mut ctx, &parse("map (list unit) unit").unwrap()),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    // Test that the comparability of map keys is also checked when the
    // map type appears nested inside the parameter type
    fn test_nested_invalid_map() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (pair unit (option (map (list unit) unit)));",
                "storage nat;",
                "code FAILWITH",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_invalid_big_map_key_type() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (big_map (list unit) unit);",
                "storage nat;",
                "code FAILWITH;",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_invalid_big_map_value_type() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (big_map int (contract unit));",
                "storage nat;",
                "code { UNIT; FAILWITH };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::BigMapValue,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    // Test that the empty sequence cannot be given the type `big_map (list unit) unit`
    // because `list unit` is not comparable and therefore `big_map (list unit) unit` is
    // not well formed.
    fn test_invalid_big_map_value() {
        let mut ctx = Ctx::default();
        assert_eq!(
            Micheline::Seq(&[])
                .typecheck_value(&mut ctx, &parse("big_map (list unit) unit").unwrap()),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_parsing_big_map_value() {
        let mut ctx = Ctx::default();
        let storage = &mut ctx.big_map_storage;
        let id0 = storage.big_map_new(&Type::Int, &Type::Int, false).unwrap();
        storage
            .big_map_update(&id0, TypedValue::int(5), Some(TypedValue::int(5)))
            .unwrap();

        // Only ID - ok case
        assert_eq!(
            typecheck_value(
                &Micheline::Int(0.into()),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Ok(TypedValue::BigMap(BigMap {
                content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                    id: id0.clone(),
                    overlay: BTreeMap::new()
                }),
                key_type: Type::Int,
                value_type: Type::Int
            }))
        );

        // Only ID - non-existing big map
        assert_eq!(
            typecheck_value(
                &Micheline::Int(5.into()),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Err(TcError::BigMapNotFound(5.into()))
        );

        // Only ID - key mismatch
        // NB: Octez implementation just says that big map does not exists,
        // but this difference seems fine.
        assert_eq!(
            typecheck_value(
                &Micheline::Int(0.into()),
                &mut ctx,
                &Type::new_big_map(Type::Nat, Type::Int)
            ),
            Err(TcError::TypesNotEqual(TypesNotEqual(Type::Int, Type::Nat)))
        );

        // Only ID - value mismatch
        assert_eq!(
            typecheck_value(
                &Micheline::Int(0.into()),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Nat)
            ),
            Err(TcError::TypesNotEqual(TypesNotEqual(Type::Int, Type::Nat)))
        );

        // Only overlay - ok case
        assert_eq!(
            typecheck_value(
                &parse("{Elt 7 8}").unwrap(),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Ok(TypedValue::BigMap(BigMap {
                content: big_map::BigMapContent::InMemory(BTreeMap::from([(
                    TypedValue::int(7),
                    TypedValue::int(8)
                )])),
                key_type: Type::Int,
                value_type: Type::Int
            }))
        );

        // Only overlay - key type mismatch case
        assert_eq!(
            typecheck_value(
                &parse("{Elt \"a\" 8}").unwrap(),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Err(TcError::InvalidValueForType(
                "String(\"a\")".into(),
                Type::Int
            ))
        );

        // ID and overlay - forget some
        assert_eq!(
            typecheck_value(
                &parse("Pair 0 {Elt 7 8}").unwrap(),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Err(TcError::InvalidEltForMap(
                "Int(8)".into(),
                Type::BigMap(PairBox::new(Type::Int, Type::Int))
            ))
        );

        // ID and overlay - Some case
        assert_eq!(
            typecheck_value(
                &parse("Pair 0 {Elt 7 (Some 8)}").unwrap(),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Ok(TypedValue::BigMap(BigMap {
                content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                    id: id0.clone(),
                    overlay: BTreeMap::from([(TypedValue::int(7), Some(TypedValue::int(8)))])
                }),
                key_type: Type::Int,
                value_type: Type::Int
            }))
        );

        // ID and overlay - None case
        assert_eq!(
            typecheck_value(
                &parse("Pair 0 {Elt 7 None}").unwrap(),
                &mut ctx,
                &Type::new_big_map(Type::Int, Type::Int)
            ),
            Ok(TypedValue::BigMap(BigMap {
                content: big_map::BigMapContent::FromId(big_map::BigMapFromId {
                    id: id0,
                    overlay: BTreeMap::from([(TypedValue::int(7), None)])
                }),
                key_type: Type::Int,
                value_type: Type::Int
            }))
        );
    }

    #[test]
    fn test_contract_not_storable() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage (contract unit);",
                "code FAILWITH;",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Storable,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_contract_not_pushable() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (contract unit) Unit").unwrap(),
                &mut gas,
                &mut tc_stk![]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_contract_with_unpassable_arg() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract operation);",
                "storage unit;",
                "code FAILWITH;"
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Passable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_script_typechecking() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::Unit,
                storage: Type::Unit,
                code: Seq(vec![Car, Nil, Pair]),
                annotations: HashMap::from([(
                    FieldAnnotation::default(),
                    (Vec::new(), Type::Unit)
                )]),
                views: HashMap::new(),
            })
        );
    }

    #[test]
    fn test_script_typechecking_with_views() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "this_view_name_has_31_char_okay" unit unit { CAR };"#,
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::Unit,
                storage: Type::Unit,
                code: Seq(vec![Car, Nil, Pair]),
                annotations: HashMap::from([(
                    FieldAnnotation::default(),
                    (Vec::new(), Type::Unit)
                )]),
                views: HashMap::from_iter([(
                    "this_view_name_has_31_char_okay".into(),
                    View {
                        input_type: Type::Unit,
                        output_type: Type::Unit,
                        code: parse("{ CAR }").unwrap()
                    }
                )]),
            })
        );
    }

    #[test]
    fn test_script_typechecking_with_two_views() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "a" unit unit { CAR };"#,
                r#"view "b" unit unit { CDR };"#,
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::Unit,
                storage: Type::Unit,
                code: Seq(vec![Car, Nil, Pair]),
                annotations: HashMap::from([(
                    FieldAnnotation::default(),
                    (Vec::new(), Type::Unit)
                )]),
                views: HashMap::from_iter([
                    (
                        "a".into(),
                        View {
                            input_type: Type::Unit,
                            output_type: Type::Unit,
                            code: parse("{ CAR }").unwrap()
                        }
                    ),
                    (
                        "b".into(),
                        View {
                            input_type: Type::Unit,
                            output_type: Type::Unit,
                            code: parse("{ CDR }").unwrap()
                        }
                    )
                ]),
            })
        );
    }

    #[test]
    fn test_script_typechecking_with_duplicated_views() {
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "a" unit unit { CAR };"#,
                r#"view "a" unit unit { CDR };"#,
            ))
            .unwrap()
            .split_script(),
            Err(TcError::DuplicatedView("a".into()))
        );
    }

    #[test]
    fn test_script_typechecking_with_long_view_name() {
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "this_is_a_very_long_view_name_32" unit unit { CAR };"#,
            ))
            .unwrap()
            .split_script(),
            Err(TcError::InvalidViewName(
                "this_is_a_very_long_view_name_32".into()
            ))
        );
    }

    #[test]
    fn test_script_typechecking_with_special_character_view_name() {
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "?_cannot_be_view_name" unit unit { CAR };"#,
            ))
            .unwrap()
            .split_script(),
            Err(TcError::InvalidViewName("?_cannot_be_view_name".into()))
        );
    }

    #[test]
    fn test_script_typechecking_with_big_map_input_view() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage (big_map string nat);",
                "code { CDR; NIL operation; PAIR };",
                r#"view "big_map_input_view" (big_map string nat) unit { CDR };"#,
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewInput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
    }

    #[test]
    fn test_script_typechecking_with_big_map_output_view() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "big_map_output_view" unit (big_map string nat) { DROP ; EMPTY_BIG_MAP string nat; };"#,
            ))
            .unwrap()
            .split_script().unwrap().typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::ViewOutput,
                Type::BigMap(PairBox::new(Type::String, Type::Nat))
            ))
        );
    }

    #[test]
    fn test_script_typechecking_with_different_storage_type_view() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage string;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "multiply_view" nat nat { UNPAIR ; MUL; };"#,
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::NoMatchingOverload {
                instr: Prim::MUL,
                stack: stk![Type::String, Type::Nat],
                reason: None,
            })
        );
    }

    #[test]
    fn test_script_typechecking_with_non_seq_view() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage string;",
                "code { CAR; NIL operation; PAIR };",
                r#"view "non_seq_view" nat nat UNPAIR;"#,
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::NonSeqViewInstrs("non_seq_view".into()))
        );
    }

    #[test]
    fn test_script_typchecking_view_instruction_ok() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage unit;",
                r#"code { CAR ; NIL operation ; PAIR ; PUSH address "KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq" ; UNIT ; VIEW "hello_view" string ; DROP ; };"#
            ))
            .unwrap()
            .split_script().unwrap().typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::Unit,
                storage: Type::Unit,
                code: Seq(vec![
                    Car,
                    Nil,
                    Pair,
                    Push(Rc::new(TypedValue::Address(addr::Address
                        { hash: AddressHash::try_from("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq").unwrap(),
                          entrypoint: Entrypoint::default() }
                        ))),
                    Unit,
                    IView { name: "hello_view".into(), arg_type: Type::Unit, return_type: Type::String }, Drop(None)]),
                annotations: HashMap::from([(
                    FieldAnnotation::default(),
                    (Vec::new(), Type::Unit)
                )]),
                views: HashMap::new(),
            })
        );
    }

    #[test]
    fn test_typchecking_view_too_short() {
        too_short_test(&parse("VIEW \"hello_view\" string").unwrap(), Prim::VIEW, 2);
    }

    #[test]
    fn test_contract_is_passable() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract unit);",
                "storage unit;",
                "code { DROP; UNIT; FAILWITH };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::new_contract(Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![Drop(None), Unit, Failwith(Type::Unit)]),
                annotations: HashMap::from([(
                    FieldAnnotation::default(),
                    (Vec::new(), Type::new_contract(Type::Unit))
                )]),
                views: HashMap::new(),
            })
        );
    }

    #[test]
    fn test_fail_with_contract_should_fail() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract unit);",
                "storage unit;",
                "code FAILWITH;",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_push_address() {
        #[track_caller]
        fn test_ok(lit: &str, bytes: &str, exp: addr::Address) {
            let exp = Ok(Push(Rc::new(TypedValue::Address(exp))));
            assert_eq!(
                &typecheck_instruction(
                    &parse(&format!("PUSH address {lit}")).unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
            assert_eq!(
                &typecheck_instruction(
                    &parse(&format!("PUSH address {bytes}")).unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
        }
        fn hex<T: Into<AddressHash>>(
            con: fn(Vec<u8>) -> Result<T, FromBytesError>,
            hex: &str,
            ep: &str,
        ) -> addr::Address {
            addr::Address {
                hash: con(hex::decode(hex).unwrap()).unwrap().into(),
                entrypoint: Entrypoint::try_from(ep).unwrap(),
            }
        }
        use tezos_crypto_rs::hash::*;
        // hex representations are obtained via `octez-client hash data`
        test_ok(
            r#""tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j""#,
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef",
            hex(
                ContractTz1Hash::try_from,
                "7b09f782e0bcd67739510afa819d85976119d5ef",
                "default",
            ),
        );
        test_ok(
            r#""tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH""#,
            "0x00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
            hex(
                ContractTz2Hash::try_from,
                "0a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
                "default",
            ),
        );
        test_ok(
            r#""tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r""#,
            "0x00025cfa532f50de3e12befc0ad21603835dd7698d35",
            hex(
                ContractTz3Hash::try_from,
                "5cfa532f50de3e12befc0ad21603835dd7698d35",
                "default",
            ),
        );
        test_ok(
            r#""tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN""#,
            "0x00036342f30484dd46b6074373aa6ddca9dfb70083d6",
            hex(
                ContractTz4Hash::try_from,
                "6342f30484dd46b6074373aa6ddca9dfb70083d6",
                "default",
            ),
        );
        test_ok(
            r#""KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye""#,
            "0x011f2d825fdd9da219235510335e558520235f4f5400",
            hex(
                ContractKt1Hash::try_from,
                "1f2d825fdd9da219235510335e558520235f4f54",
                "default",
            ),
        );
        test_ok(
            r#""sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf""#,
            "0x03d601f22256d2ad1faec0c64374e527c6e62f2e5a00",
            hex(
                SmartRollupHash::try_from,
                "d601f22256d2ad1faec0c64374e527c6e62f2e5a",
                "default",
            ),
        );
        // with entrypoints
        test_ok(
            r#""tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo""#,
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef666f6f",
            hex(
                ContractTz1Hash::try_from,
                "7b09f782e0bcd67739510afa819d85976119d5ef",
                "foo",
            ),
        );
        test_ok(
            r#""tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%foo""#,
            "0x00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe666f6f",
            hex(
                ContractTz2Hash::try_from,
                "0a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
                "foo",
            ),
        );
        test_ok(
            r#""tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%foo""#,
            "0x00025cfa532f50de3e12befc0ad21603835dd7698d35666f6f",
            hex(
                ContractTz3Hash::try_from,
                "5cfa532f50de3e12befc0ad21603835dd7698d35",
                "foo",
            ),
        );
        test_ok(
            r#""tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%foo""#,
            "0x00036342f30484dd46b6074373aa6ddca9dfb70083d6666f6f",
            hex(
                ContractTz4Hash::try_from,
                "6342f30484dd46b6074373aa6ddca9dfb70083d6",
                "foo",
            ),
        );
        test_ok(
            r#""KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo""#,
            "0x011f2d825fdd9da219235510335e558520235f4f5400666f6f",
            hex(
                ContractKt1Hash::try_from,
                "1f2d825fdd9da219235510335e558520235f4f54",
                "foo",
            ),
        );
        test_ok(
            r#""sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo""#,
            "0x03d601f22256d2ad1faec0c64374e527c6e62f2e5a00666f6f",
            hex(
                SmartRollupHash::try_from,
                "d601f22256d2ad1faec0c64374e527c6e62f2e5a",
                "foo",
            ),
        );

        macro_rules! assert_matches {
            ($e:expr, $p:pat $(if $cond:expr)?) => {
                match $e {
                    $p $(if $cond)? => (),
                    _ => panic!("expected {:?} to match {}", $e, stringify!($p)),
                }
            };
        }

        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address \"tz1foobarfoobarfoobarfoobarfoobarfoo\"").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address \"tz9foobarfoobarfoobarfoobarfoobarfoo\"").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address \"tz\"").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x0001fffe").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
        typecheck_instruction(
            &parse("PUSH address 0xff00fe0000000000000000000000000000000000000000").unwrap(),
            &mut Gas::default(),
            &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(Type::Address, ByteReprError::UnknownPrefix(p))) if p == "0xff"
            );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x00fffe0000000000000000000000000000000000000000").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x00").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x011f2d825fdd9da219235510335e558520235f4f5401666f6f")
                    .unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x03d601f22256d2ad1faec0c64374e527c6e62f2e5a666f6f").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
    }

    /// L2-1377: `PUSH address "...%<ep>"` and `PUSH address 0x...<ep>` must
    /// typecheck with entrypoint bytes outside the Michelson script-source
    /// charset, matching Tezos L1's `parse_address`.
    #[test]
    fn test_push_address_l2_1377_relaxed_entrypoint() {
        #[track_caller]
        fn assert_ok(lit: &str, bytes: &str, ep: &str) {
            let exp_addr = addr::Address::from_base58_check(lit).unwrap();
            assert_eq!(exp_addr.entrypoint.as_str(), Some(ep));
            let exp = Ok(Push(Rc::new(TypedValue::Address(exp_addr))));
            assert_eq!(
                &typecheck_instruction(
                    &parse(&format!("PUSH address \"{lit}\"")).unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
            assert_eq!(
                &typecheck_instruction(
                    &parse(&format!("PUSH address {bytes}")).unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
        }
        // Single non-charset ASCII byte (issue's `ep_bang` case).
        assert_ok(
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%!",
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef21",
            "!",
        );
        // Dot as first character (issue's `ep_dotfirst` case).
        assert_ok(
            "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%.foo",
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef2e666f6f",
            ".foo",
        );

        // Non-UTF-8 entrypoint byte (issue's `ep_nonascii` case): only
        // expressible via the optimized bytes form, since a readable string
        // literal is always valid UTF-8. L1 accepts it; MIR must too.
        {
            let exp_addr = addr::Address::from_bytes(
                &hex::decode("00007b09f782e0bcd67739510afa819d85976119d5efff").unwrap(),
            )
            .unwrap();
            assert_eq!(exp_addr.entrypoint.as_bytes(), &[0xff_u8]);
            let exp = Ok(Push(Rc::new(TypedValue::Address(exp_addr))));
            assert_eq!(
                &typecheck_instruction(
                    &parse("PUSH address 0x00007b09f782e0bcd67739510afa819d85976119d5efff")
                        .unwrap(),
                    &mut Gas::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
        }

        macro_rules! assert_matches {
            ($e:expr, $p:pat $(if $cond:expr)?) => {
                match $e {
                    $p $(if $cond)? => (),
                    _ => panic!("expected {:?} to match {}", $e, stringify!($p)),
                }
            };
        }

        // Sanity: too-long entrypoint is still rejected (shared bound).
        assert_matches!(
            typecheck_instruction(
                &parse(
                    "PUSH address \"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq\""
                )
                .unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
        // Sanity: explicit "default" in readable form still rejected.
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address \"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%default\"").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(
                Type::Address,
                ByteReprError::WrongFormat(_)
            ))
        );
    }

    #[test]
    fn test_push_chain_id() {
        let bytes = "f3d48554";
        let exp = hex::decode(bytes).unwrap();
        let exp = Ok(Push(Rc::new(TypedValue::ChainId(
            super::ChainId::try_from(exp).unwrap(),
        ))));
        let lit = "NetXynUjJNZm7wi";
        assert_eq!(
            &typecheck_instruction(
                &parse(&format!("PUSH chain_id \"{lit}\"")).unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            &exp
        );
        assert_eq!(
            &typecheck_instruction(
                &parse(&format!("PUSH chain_id 0x{bytes}")).unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            &exp
        );
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH chain_id \"foobar\"").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ChainIdError(
                tezos_crypto_rs::base58::FromBase58CheckError::InvalidChecksum.into()
            ))
        );
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH chain_id 0xbeef").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ChainIdError(
                tezos_crypto_rs::hash::FromBytesError::InvalidSize.into()
            ))
        );
    }

    #[test]
    fn chain_id_instr() {
        assert_eq!(
            typecheck_instruction(
                &parse("CHAIN_ID").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![]
            ),
            Ok(Instruction::ChainId)
        );
    }

    #[test]
    fn pack_instr() {
        let stk = &mut tc_stk![Type::new_pair(Type::Int, Type::Unit)];
        assert_eq!(
            super::typecheck_instruction(
                &parse("PACK").unwrap(),
                &mut Gas::default(),
                None,
                stk,
                false
            ),
            Ok(Instruction::Pack)
        );
        assert_eq!(stk, &tc_stk![Type::Bytes]);
    }

    #[test]
    fn pack_instr_non_packable() {
        assert_eq!(
            typecheck_instruction(
                &parse("PACK").unwrap(),
                &mut Gas::default(),
                &mut tc_stk![Type::Operation]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Packable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn self_instr() {
        let stk = &mut tc_stk![];
        assert_eq!(
            super::typecheck_instruction(
                &parse("SELF").unwrap(),
                &mut Gas::default(),
                Some(&[(Entrypoint::default(), Type::Nat)].into()),
                stk,
                false,
            ),
            Ok(Instruction::ISelf(Entrypoint::default()))
        );
        assert_eq!(stk, &tc_stk![Type::new_contract(Type::Nat)]);
    }

    #[test]
    fn self_instr_ep() {
        let stk = &mut tc_stk![];
        assert_eq!(
            super::typecheck_instruction(
                &parse("SELF %foo").unwrap(),
                &mut Gas::default(),
                Some(
                    &[
                        (Entrypoint::default(), Type::Nat),
                        (Entrypoint::try_from("foo").unwrap(), Type::ChainId)
                    ]
                    .into()
                ),
                stk,
                false,
            ),
            Ok(Instruction::ISelf(Entrypoint::try_from("foo").unwrap()))
        );
        assert_eq!(stk, &tc_stk![Type::new_contract(Type::ChainId)]);
    }

    #[test]
    fn self_instr_no_ep() {
        let stk = &mut tc_stk![];
        assert_eq!(
            super::typecheck_instruction(
                &parse("SELF %bar").unwrap(),
                &mut Gas::default(),
                Some(&[(Entrypoint::default(), Type::Nat)].into()),
                stk,
                false,
            ),
            Err(TcError::NoSuchEntrypoint("bar".try_into().unwrap()))
        );
    }

    #[test]
    fn self_instr_duplicate_ann() {
        let stk = &mut tc_stk![];
        assert_eq!(
            super::typecheck_instruction(
                &parse("SELF %bar %baz").unwrap(),
                &mut Gas::default(),
                Some(&[(Entrypoint::default(), Type::Nat)].into()),
                stk,
                false,
            ),
            Err(AnnotationError::TooManyFieldAnns("baz".into()).into())
        );
    }

    #[test]
    fn self_instr_contract() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %foo) (unit %default));",
                "storage unit;",
                "code { DROP; SELF %foo; UNIT; FAILWITH };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::new_or(Type::Int, Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![
                    Drop(None),
                    ISelf("foo".try_into().unwrap()),
                    Unit,
                    Failwith(Type::Unit)
                ]),
                annotations: HashMap::from([
                    (
                        FieldAnnotation::default(),
                        (vec![Direction::Right], Type::Unit)
                    ),
                    (
                        FieldAnnotation::from_str_unchecked("foo"),
                        (vec![Direction::Left], Type::Int)
                    )
                ]),
                views: HashMap::new(),
            })
        );
    }

    #[test]
    fn self_instr_contract_overlong_ep() {
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq) (unit %default));",
                "storage unit;",
                "code { DROP; SELF %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq; UNIT; FAILWITH };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Err(TcError::EntrypointError(ByteReprError::WrongFormat(
                "entrypoint name must be at most 31 characters long, but it is 32 characters long"
                    .into()
            )))
        );
    }

    #[test]
    fn self_instr_contract_overlong_field_ann() {
        // NB: overlong field annotations are OK, but they don't work as
        // entrypoints.
        let mut gas = Gas::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq) (unit %default));",
                "storage unit;",
                "code { DROP; SELF; UNIT; FAILWITH };",
            ))
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true),
            Ok(ContractScript {
                parameter: Type::new_or(Type::Int, Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![
                    Drop(None),
                    ISelf("default".try_into().unwrap()),
                    Unit,
                    Failwith(Type::Unit)
                ]),
                annotations: HashMap::from([
                    (
                        FieldAnnotation::from_str_unchecked("qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq"),
                        (vec![Direction::Left], Type::Int)
                    ),
                    (
                        FieldAnnotation::default(),
                        (vec![Direction::Right], Type::Unit)
                    ),
                ]),
                views: HashMap::new(),
            })
        );
    }

    #[test]
    fn address_instr() {
        let stk = &mut tc_stk![Type::new_contract(Type::Nat)];
        assert_eq!(
            typecheck_instruction(&parse("ADDRESS").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Address)
        );
        assert_eq!(stk, &tc_stk![Type::Address]);
    }

    #[test]
    fn test_address_short() {
        too_short_test(&parse("ADDRESS").unwrap(), Prim::ADDRESS, 1);
    }

    #[test]
    fn test_address_mismatch() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("ADDRESS").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Unit]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::ADDRESS,
                stack: stk![Type::Unit],
                reason: None,
            })
        );
    }

    #[test]
    fn left() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("LEFT nat").unwrap(), &mut Gas::default(), &mut stack),
            Ok(Instruction::Left)
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Nat)]);
    }

    #[test]
    fn left_too_short() {
        too_short_test(&parse("LEFT nat").unwrap(), Prim::LEFT, 1);
    }

    #[test]
    fn right() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(
                &parse("RIGHT nat").unwrap(),
                &mut Gas::default(),
                &mut stack
            ),
            Ok(Instruction::Right)
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Nat, Type::Int)]);
    }

    #[test]
    fn right_too_short() {
        too_short_test(&parse("RIGHT nat").unwrap(), Prim::RIGHT, 1);
    }

    #[test]
    fn read_top_level() {
        use crate::lexer::Prim::{code, parameter, storage};
        use TcError as Err;

        let go = |s| {
            parse_contract_script(s)
                .unwrap()
                .split_script()?
                .typecheck_script(&mut Gas::default(), true, true)
        };

        // duplicate
        assert_eq!(
            go("parameter unit; parameter int; storage unit; code FAILWITH"),
            Err(Err::DuplicateTopLevelElt(parameter))
        );
        assert_eq!(
            go("parameter unit; storage unit; storage int; code FAILWITH"),
            Err(Err::DuplicateTopLevelElt(storage))
        );
        assert_eq!(
            go("code INT; parameter unit; storage unit; code FAILWITH"),
            Err(Err::DuplicateTopLevelElt(code))
        );
        // missing
        assert_eq!(
            go("storage unit; code FAILWITH"),
            Err(Err::MissingTopLevelElt(parameter))
        );
        assert_eq!(
            go("parameter unit; code FAILWITH"),
            Err(Err::MissingTopLevelElt(storage))
        );
        assert_eq!(
            go("parameter unit; storage unit"),
            Err(Err::MissingTopLevelElt(code))
        );
    }

    #[test]
    fn dip_dup_arg_too_large() {
        assert_eq!(
            parse("DROP 1025")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::ExpectedU10(1025.into()))
        );
        assert_eq!(
            parse("DIP 1024 {}")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::ExpectedU10(1024.into()))
        );
        assert_eq!(
            parse("DUP 65536")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::ExpectedU10(65536.into()))
        );
    }

    #[test]
    fn push_bytes() {
        assert_eq!(
            parse("PUSH bytes 0xdeadf00d")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Bytes(
                hex::decode("deadf00d").unwrap()
            ))))
        );
    }

    #[test]
    fn push_key() {
        assert_eq!(
            parse("PUSH key \"p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB\"")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Key(
                "p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB"
                    .try_into()
                    .unwrap()
            ))))
        );
        assert_eq!(
            parse(
                "PUSH key 0x01022c380cd1ff286a0a1a7c3aad6e891d237fa82e2a7cdeec08ccb55e90fdef995f"
            )
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Key(
                "sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD"
                    .try_into()
                    .unwrap()
            ))))
        );
    }

    #[test]
    fn push_key_hash() {
        assert_eq!(
            parse("PUSH key_hash \"tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw\"")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::KeyHash(
                "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw".try_into().unwrap()
            ))))
        );
        assert_eq!(
            parse("PUSH key_hash 0x036342f30484dd46b6074373aa6ddca9dfb70083d6")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::KeyHash(
                "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN".try_into().unwrap()
            ))))
        );
    }

    #[test]
    fn push_signature() {
        assert_eq!(
            parse("PUSH signature \"p2sigRmXDp38VNVaEQH28LYukfLPn8QB5hPEberhvQrrUpRscDZJrrApbRh2u46PTVTwKXjxTLKNN9dyLhPQU6U6jWPGxe4d9v\"")
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Signature(
                        "p2sigRmXDp38VNVaEQH28LYukfLPn8QB5hPEberhvQrrUpRscDZJrrApbRh2u46PTVTwKXjxTLKNN9dyLhPQU6U6jWPGxe4d9v"
                        .try_into()
                        .unwrap()
                        ))))
            );
        // NB: bytes are always treated as a generic signature
        assert_eq!(
            parse(
                "PUSH signature 0x22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
                )
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Signature(
                        "sigSTJNiwaPuZXmU2FscxNy9scPjjwpbxpPD5rY1QRBbyb4gHXYU7jN9Wcbs9sE4GMzuiSSG5S2egeyJhUjW1uJEgw4AWAXj"
                        .try_into()
                        .unwrap()
                        ))))
            );
    }

    #[test]
    fn check_signature() {
        assert_eq!(
            parse("CHECK_SIGNATURE").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("bytes").unwrap(),
                    parse("signature").unwrap(),
                    parse("key").unwrap()
                ]
            ),
            Ok(CheckSignature)
        );
    }

    #[test]
    fn check_signature_wrong_type() {
        assert_eq!(
            parse("CHECK_SIGNATURE").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[
                    parse("bytes").unwrap(),
                    parse("key").unwrap(),
                    parse("key").unwrap()
                ]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CHECK_SIGNATURE,
                stack: stk![Type::Bytes, Type::Key, Type::Key],
                reason: None
            })
        );
    }

    #[test]
    fn check_signature_too_short() {
        too_short_test(&parse("CHECK_SIGNATURE").unwrap(), Prim::CHECK_SIGNATURE, 3)
    }

    #[test]
    fn pairing_check() {
        assert_eq!(
            parse("PAIRING_CHECK").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[parse("list (pair bls12_381_g1 bls12_381_g2)").unwrap()]
            ),
            Ok(PairingCheck)
        );
    }

    #[test]
    fn pairing_check_wrong_type() {
        assert_eq!(
            parse("PAIRING_CHECK").unwrap().typecheck_instruction(
                &mut Gas::default(),
                None,
                &[parse("list (pair bls12_381_g1 bls12_381_g1)").unwrap()]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::PAIRING_CHECK,
                stack: stk![Type::new_list(Type::new_pair(
                    Type::Bls12381G1,
                    Type::Bls12381G1
                ))],
                reason: Some(
                    TypesNotEqual(
                        Type::new_list(Type::new_pair(Type::Bls12381G1, Type::Bls12381G2)),
                        Type::new_list(Type::new_pair(Type::Bls12381G1, Type::Bls12381G1))
                    )
                    .into()
                )
            })
        );
    }

    #[test]
    fn pairing_check_too_short() {
        too_short_test(&parse("PAIRING_CHECK").unwrap(), Prim::PAIRING_CHECK, 1)
    }

    mod mul {
        use super::*;
        use Type as T;

        #[track_caller]
        fn test_mul(
            mut stack: FailingTypeStack,
            expected_stack: FailingTypeStack,
            overload: overloads::Mul,
        ) {
            assert_eq!(
                typecheck_instruction(&parse("MUL").unwrap(), &mut Gas::default(), &mut stack),
                Ok(Mul(overload))
            );
            assert_eq!(stack, expected_stack);
        }
        macro_rules! test {
            ($overload:ident, $i1:expr, $i2:expr, $out:expr $(,)*) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test_mul(tc_stk![$i2, $i1], tc_stk![$out], overloads::Mul::$overload);
                }
            };
        }
        test!(
            Bls12381G1Bls12381Fr,
            T::Bls12381G1,
            T::Bls12381Fr,
            T::Bls12381G1,
        );
        test!(
            Bls12381G2Bls12381Fr,
            T::Bls12381G2,
            T::Bls12381Fr,
            T::Bls12381G2,
        );
        test!(
            Bls12381FrBls12381Fr,
            T::Bls12381Fr,
            T::Bls12381Fr,
            T::Bls12381Fr,
        );
        test!(NatBls12381Fr, T::Nat, T::Bls12381Fr, T::Bls12381Fr);
        test!(IntBls12381Fr, T::Int, T::Bls12381Fr, T::Bls12381Fr);
        test!(Bls12381FrNat, T::Bls12381Fr, T::Nat, T::Bls12381Fr);
        test!(Bls12381FrInt, T::Bls12381Fr, T::Int, T::Bls12381Fr);

        test!(NatNat, T::Nat, T::Nat, T::Nat);
        test!(NatInt, T::Nat, T::Int, T::Int);
        test!(IntNat, T::Int, T::Nat, T::Int);
        test!(IntInt, T::Int, T::Int, T::Int);
        test!(MutezNat, T::Mutez, T::Nat, T::Mutez);
        test!(NatMutez, T::Nat, T::Mutez, T::Mutez);

        #[test]
        fn wrong_type() {
            assert_eq!(
                parse("MUL").unwrap().typecheck_instruction(
                    &mut Gas::default(),
                    None,
                    &[parse("unit").unwrap(), parse("unit").unwrap()]
                ),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::MUL,
                    stack: stk![Type::Unit, Type::Unit],
                    reason: None
                })
            );
        }

        #[test]
        fn too_short() {
            too_short_test(&parse("MUL").unwrap(), Prim::MUL, 2)
        }
    }

    mod neg {
        use super::*;
        use Type as T;

        #[track_caller]
        fn test_neg(
            mut stack: FailingTypeStack,
            expected_stack: FailingTypeStack,
            overload: overloads::Neg,
        ) {
            assert_eq!(
                typecheck_instruction(&parse("NEG").unwrap(), &mut Gas::default(), &mut stack),
                Ok(Neg(overload))
            );
            assert_eq!(stack, expected_stack)
        }
        macro_rules! test {
            ($overload:ident, $expected:ident) => {
                #[test]
                #[allow(non_snake_case)]
                fn $overload() {
                    test_neg(
                        tc_stk![T::$overload],
                        tc_stk![T::$expected],
                        overloads::Neg::$overload,
                    );
                }
            };
        }
        test!(Bls12381G1, Bls12381G1);
        test!(Bls12381G2, Bls12381G2);
        test!(Bls12381Fr, Bls12381Fr);

        test!(Nat, Int);
        test!(Int, Int);

        #[test]
        fn wrong_type() {
            assert_eq!(
                parse("NEG").unwrap().typecheck_instruction(
                    &mut Gas::default(),
                    None,
                    &[parse("unit").unwrap()]
                ),
                Err(TcError::NoMatchingOverload {
                    instr: Prim::NEG,
                    stack: stk![Type::Unit],
                    reason: None
                })
            );
        }

        #[test]
        fn too_short() {
            too_short_test(&parse("NEG").unwrap(), Prim::NEG, 1)
        }
    }

    #[test]
    fn transfer_tokens() {
        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Mutez, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Gas::default(), stk),
            Err(TypesNotEqual(Type::Nat, Type::Int).into())
        );

        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::new_contract(Type::Nat), Type::Int, Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::Nat, Type::Mutez, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::Nat, Type::Mutez, Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::Nat],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 3 })
            })
        );

        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Mutez, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::TransferTokens)
        );
        assert_eq!(stk, &tc_stk![Type::Operation]);
    }

    #[test]
    fn set_delegate() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SET_DELEGATE,
                stack: stk![Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::new_option(Type::Nat)];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SET_DELEGATE,
                stack: stk![Type::new_option(Type::Nat)],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::new_option(Type::KeyHash)];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::SetDelegate)
        );
        assert_eq!(stk, &tc_stk![Type::Operation]);
    }

    #[test]
    fn slice_string() {
        let stk = &mut tc_stk![Type::String, Type::Nat, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SLICE").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Slice(overloads::Slice::String))
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn slice_bytes() {
        let stk = &mut tc_stk![Type::Bytes, Type::Nat, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SLICE").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Slice(overloads::Slice::Bytes))
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::Bytes)]);
    }

    #[test]
    fn slice_too_short() {
        too_short_test(&parse("SLICE").unwrap(), Prim::SLICE, 3);
    }

    #[test]
    fn slice_wrong_type() {
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(
                &parse("SLICE").unwrap(),
                &mut gas,
                &mut tc_stk![Type::Bool, Type::Nat, Type::Nat]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SLICE,
                stack: stk![Type::Bool, Type::Nat, Type::Nat],
                reason: None,
            })
        );
    }

    #[test]
    fn push_lambda_wrong_type() {
        assert_eq!(
            parse("PUSH (lambda unit unit) Unit")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::InvalidValueForType(
                "App(Unit, [], [])".to_owned(),
                Type::new_lambda(Type::Unit, Type::Unit)
            ))
        );
    }

    #[test]
    fn push_lambda() {
        assert_eq!(
            parse("PUSH (lambda unit unit) { DROP ; UNIT }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Lambda(Closure::Lambda(
                Lambda::Lambda {
                    micheline_code: parse("{DROP; UNIT}").unwrap(),
                    code: vec![Drop(None), Unit].into()
                }
            )))))
        );
        assert_eq!(
            parse("LAMBDA unit unit { DROP ; UNIT }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Lambda(Lambda::Lambda {
                micheline_code: parse("{DROP; UNIT}").unwrap(),
                code: vec![Drop(None), Unit].into()
            }))
        );
    }

    #[test]
    fn push_lambda_bad_result() {
        assert_eq!(
            parse("PUSH (lambda unit int) { DROP ; UNIT }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::StacksNotEqual(
                stk![Type::Unit],
                stk![Type::Int],
                TypesNotEqual(Type::Unit, Type::Int).into()
            ))
        );
        assert_eq!(
            parse("LAMBDA unit int { DROP ; UNIT }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::StacksNotEqual(
                stk![Type::Unit],
                stk![Type::Int],
                TypesNotEqual(Type::Unit, Type::Int).into()
            ))
        );
    }

    #[test]
    fn push_lambda_bad_input() {
        assert_eq!(
            parse("PUSH (lambda int unit) { IF { UNIT } { UNIT } }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::Int],
                reason: Some(TypesNotEqual(Type::Bool, Type::Int).into())
            })
        );
        assert_eq!(
            parse("LAMBDA int unit { IF { UNIT } { UNIT } }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::Int],
                reason: Some(TypesNotEqual(Type::Bool, Type::Int).into())
            })
        );
    }

    #[test]
    fn push_lambda_with_self() {
        assert_eq!(
            parse("PUSH (lambda unit unit) { SELF }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), Some(&parse("unit").unwrap()), &[]),
            Err(TcError::SelfForbidden)
        );
        assert_eq!(
            parse("LAMBDA unit unit { SELF }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), Some(&parse("unit").unwrap()), &[]),
            Err(TcError::SelfForbidden)
        );
    }

    #[test]
    fn push_lambda_rec_with_self() {
        assert_eq!(
            parse("PUSH (lambda unit unit) (Lambda_rec { SELF })")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), Some(&parse("unit").unwrap()), &[]),
            Err(TcError::SelfForbidden)
        );
        assert_eq!(
            parse("LAMBDA_REC unit unit { SELF }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), Some(&parse("unit").unwrap()), &[]),
            Err(TcError::SelfForbidden)
        );
    }

    #[test]
    fn push_lambda_rec() {
        assert_eq!(
            parse("PUSH (lambda unit unit) (Lambda_rec { SWAP ; DROP })")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(Rc::new(TypedValue::Lambda(Closure::Lambda(
                Lambda::LambdaRec {
                    micheline_code: parse("{SWAP; DROP}").unwrap(),
                    code: vec![Swap, Drop(None)].into(),
                    in_ty: Type::Unit,
                    out_ty: Type::Unit
                }
            )))))
        );
        assert_eq!(
            parse("LAMBDA_REC unit unit { SWAP ; DROP }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Lambda(Lambda::LambdaRec {
                micheline_code: parse("{SWAP; DROP}").unwrap(),
                code: vec![Swap, Drop(None)].into(),
                in_ty: Type::Unit,
                out_ty: Type::Unit,
            }))
        );
    }

    #[test]
    fn push_lambda_rec_bad_result() {
        assert_eq!(
            parse("PUSH (lambda unit unit) (Lambda_rec { DROP })")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::StacksNotEqual(
                stk![Type::new_lambda(Type::Unit, Type::Unit)],
                stk![Type::Unit],
                TypesNotEqual(Type::new_lambda(Type::Unit, Type::Unit), Type::Unit).into()
            ))
        );
        assert_eq!(
            parse("LAMBDA_REC unit unit { DROP }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::StacksNotEqual(
                stk![Type::new_lambda(Type::Unit, Type::Unit)],
                stk![Type::Unit],
                TypesNotEqual(Type::new_lambda(Type::Unit, Type::Unit), Type::Unit).into()
            ))
        );
    }

    #[test]
    fn push_lambda_rec_bad_input() {
        assert_eq!(
            parse("PUSH (lambda int unit) (Lambda_rec { IF { UNIT } { UNIT }; DIP { DROP } })")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::new_lambda(Type::Int, Type::Unit), Type::Int],
                reason: Some(TypesNotEqual(Type::Bool, Type::Int).into())
            })
        );
        assert_eq!(
            parse("LAMBDA_REC int unit { IF { UNIT } { UNIT }; DIP { DROP } }")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::new_lambda(Type::Int, Type::Unit), Type::Int],
                reason: Some(TypesNotEqual(Type::Bool, Type::Int).into())
            })
        );
    }

    #[test]
    fn typecheck_forged_ticket() {
        let mut ctx = Ctx::default();
        let ticket: super::Ticket = super::Ticket {
            ticketer: AddressHash::try_from("tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h").unwrap(),
            content_type: Type::Unit,
            content: TypedValue::Unit,
            amount: 5u32.into(),
        };
        assert_eq!(
            typecheck_value(
                &parse("Pair \"tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h\" (Pair Unit 5)").unwrap(),
                &mut ctx,
                &Type::new_ticket(Type::Unit)
            ),
            Ok(TypedValue::new_ticket(ticket))
        );
    }

    #[test]
    fn typecheck_contract_value() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_value(
                &parse("\"tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h\"").unwrap(),
                &mut ctx,
                &Type::new_contract(Type::Unit)
            ),
            Ok(TypedValue::Contract(
                addr::Address::try_from("tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h").unwrap()
            ))
        );

        assert_eq!(
            typecheck_value(
                &parse("\"tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h\"").unwrap(),
                &mut ctx,
                &Type::new_contract(Type::new_ticket(Type::Unit))
            ),
            Ok(TypedValue::Contract(
                addr::Address::try_from("tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h").unwrap()
            ))
        );

        assert_eq!(
            typecheck_value(
                &parse("\"tz1T1K14rZ46m1GT1kPVwZWkSHxNSDZgM71h\"").unwrap(),
                &mut ctx,
                &Type::new_contract(Type::Int)
            ),
            Err(TcError::UnexpectedImplicitAccountType(Type::Int))
        );
    }

    /// Helper: builds and typechecks a `PUSH (lambda unit unit) {<inner>}
    /// ; DROP` body of the given depth on the current thread. Each layer
    /// keeps its body's stack at `[unit]` so the lambda's `out_ty = unit`
    /// unifies. Returns the typecheck result; the caller is responsible
    /// for asserting the expected shape. Drops are skipped via
    /// `mem::forget` so the still-recursive `Type` / `Instruction`
    /// `Drop` (gated on MR4) does not overflow on scope exit.
    ///
    /// The kernel call path uses `Micheline::decode_raw` (the iterative
    /// byte-decoder), so building via the arena rather than the source
    /// parser mirrors the actual kernel exposure surface.
    #[cfg(test)]
    fn typecheck_nested_push_lambda<'a>(
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        depth: usize,
    ) -> Result<(), TcError> {
        let unit_ty = Micheline::App(Prim::unit, &[], NO_ANNS);
        let lambda_ty = Micheline::App(
            Prim::lambda,
            arena.alloc_extend([unit_ty.clone(), unit_ty.clone()]),
            NO_ANNS,
        );
        let drop_instr = Micheline::App(Prim::DROP, &[], NO_ANNS);
        let mut current_body: &[Micheline<'_>] = &[];
        for _ in 0..depth {
            let lambda_value = Micheline::Seq(current_body);
            let push = Micheline::App(
                Prim::PUSH,
                arena.alloc_extend([lambda_ty.clone(), lambda_value]),
                NO_ANNS,
            );
            current_body = arena.alloc_extend([push, drop_instr.clone()]);
        }
        let mut gas = Gas::new(u32::MAX);
        let stk = &mut tc_stk![];
        let result = typecheck(current_body, &mut gas, None, stk, false, TypecheckViews::Disabled);
        std::mem::forget(std::mem::replace(stk, tc_stk![]));
        match result {
            Ok(body) => {
                std::mem::forget(body);
                Ok(())
            }
            Err(e) => Err(e),
        }
    }

    /// Below-limit case: a single PUSH-lambda value typechecks cleanly
    /// with the depth guard installed; the guard increments to 1 and
    /// decrements back to 0 on scope exit, leaving the counter clean
    /// for the next test.
    #[test]
    fn push_lambda_literal_below_l1_depth_limit() {
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
        typecheck_nested_push_lambda(&arena, 1)
            .expect("below the L1 depth cap, typecheck must succeed");
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            assert_eq!(d.get(), 0, "depth counter must return to 0 after Ok");
        });
    }

    /// L1-parity guard at the recursion limit. Mirrors
    /// `script_ir_translator.ml:571` (`stack_depth > 10000` →
    /// `Typechecking_too_many_recursive_calls`). Pre-load the counter
    /// to `MAX_LAMBDA_TYPECHECK_DEPTH` so the next entry trips the
    /// guard — verifies the check fires *without* needing to recurse
    /// 10000 times (which would overflow any test thread's stack on
    /// debug builds at ~160 KB/layer).
    #[test]
    fn push_lambda_literal_at_l1_depth_limit_returns_clean_error() {
        // Pre-load past the L1-parity threshold: L1 rejects on
        // `stack_depth > 10000`, MIR rejects on `cur > 10000` after
        // increment. Pre-loading to 10001 makes the next `enter()`
        // trip exactly as L1 would on stack_depth=10001.
        LAMBDA_TYPECHECK_DEPTH.with(|d| d.set(MAX_LAMBDA_TYPECHECK_DEPTH + 1));
        // Hand-construct a single PUSH-lambda value and typecheck it.
        // The very first DepthGuard::enter() must trip on the
        // pre-loaded counter and return a clean Err.
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
        let result = typecheck_nested_push_lambda(&arena, 1);
        // Restore the counter immediately so a leak doesn't bias the
        // next test (the guard's Drop already decremented from its
        // failed enter() — but defensive reset is cheap).
        LAMBDA_TYPECHECK_DEPTH.with(|d| d.set(0));
        match result {
            Err(TcError::TypecheckingTooManyRecursiveCalls) => {}
            other => panic!(
                "expected TypecheckingTooManyRecursiveCalls at L1 limit; got {other:?}",
            ),
        }
    }

    /// The RAII guard restores the counter on both Ok and Err paths.
    /// Pre-load to a sentinel `K`, exercise each path, verify the
    /// counter ends back at `K`. Covers the Drop-on-`?` propagation
    /// contract that the guard's purpose hinges on.
    #[test]
    fn lambda_typecheck_depth_guard_balances_on_ok_and_err() {
        const K: u16 = 7;
        let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();

        // Ok path: counter starts at K, increments to K+1 during typecheck,
        // Drop fires on Ok scope exit, ends at K.
        LAMBDA_TYPECHECK_DEPTH.with(|d| d.set(K));
        typecheck_nested_push_lambda(&arena, 1).expect("Ok path");
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            assert_eq!(d.get(), K, "guard must restore counter on Ok");
        });

        // Err path: pre-load above the cap; enter() returns Err *before*
        // constructing the guard, so Drop never runs for the failed
        // entry. The counter therefore stays at the pre-loaded value.
        LAMBDA_TYPECHECK_DEPTH.with(|d| d.set(MAX_LAMBDA_TYPECHECK_DEPTH + 1));
        let err = typecheck_nested_push_lambda(&arena, 1).unwrap_err();
        assert!(
            matches!(err, TcError::TypecheckingTooManyRecursiveCalls),
            "Err path must return TypecheckingTooManyRecursiveCalls; got {err:?}",
        );
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            assert_eq!(
                d.get(),
                MAX_LAMBDA_TYPECHECK_DEPTH + 1,
                "counter must be unchanged after Err — Drop must not fire on a failed enter()",
            );
            // Restore so subsequent tests aren't biased by the residual.
            d.set(0);
        });
    }

    /// Regression: when a `PUSH (lambda T1 T2) <body>` typecheck fails after
    /// `LAMBDA_TYPECHECK_DEPTH` was bumped at PUSH step time -- whether the
    /// failure originates in the body (e.g. `unify_stacks` Err on the lambda
    /// output type, or an ill-typed instruction inside the body) -- the
    /// counter must still return to 0 by the time the outer typecheck call
    /// returns. The kernel runs a single thread, so a leaked increment
    /// poisons every subsequent operation on that thread and eventually
    /// trips `TypecheckingTooManyRecursiveCalls` for legitimate input. With
    /// the RAII guard threaded through the `AfterLambda` frame, the Drop
    /// fires on both normal finalize and on Vec teardown on the Err path.
    #[test]
    fn lambda_typecheck_depth_counter_balances_on_push_lambda_body_err() {
        // Defensive: reset thread-local in case a prior test on the same
        // pool worker left it nonzero.
        LAMBDA_TYPECHECK_DEPTH.with(|d| d.set(0));
        // `PUSH (lambda unit int) { DROP ; UNIT }`: body produces Unit, the
        // declared output is Int -> unify_stacks at AfterLambda finalize
        // returns StacksNotEqual. Pre-fix this leaked one increment because
        // the manual decrement at AfterLambda was reached only via the Ok
        // path.
        let err = crate::parser::test_helpers::parse("PUSH (lambda unit int) { DROP ; UNIT }")
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[])
            .unwrap_err();
        assert!(
            matches!(err, TcError::StacksNotEqual(..)),
            "expected unify failure, got {err:?}",
        );
        LAMBDA_TYPECHECK_DEPTH.with(|d| {
            assert_eq!(
                d.get(),
                0,
                "depth counter leaked across an Err return -- subsequent operations on this kernel thread would mis-trip TypecheckingTooManyRecursiveCalls",
            );
        });
    }

    // Regression for L2-1431 revalidation (francois.thire 2026-05-29). The
    // sibling `deeply_nested_lambda_typechecks_without_stack_overflow` test
    // below covers the *instruction-level* `LAMBDA T1 T2 { … }`, which has
    // been iterative since !21983. The *value-level* `PUSH (lambda T1 T2)
    // <body>` path went through `typecheck_value`'s Lambda arm →
    // `typecheck_lambda` → `typecheck` → step → PUSH → `typecheck_value`,
    // adding one round-trip of Rust frames per nesting layer. On the
    // kernel's ~1 MiB Rust stack a depth-100 chain reliably aborted with
    // `thread '<unknown>' has overflowed its stack` (gas/operation-size
    // would admit far more), far below the L1-parity 10 000 depth cap. With
    // PUSH-lambda routed through OpenLambda/AfterLambda the body becomes a
    // worklist frame, so depth is bounded by gas/heap only. `mem::forget`
    // skips the still-recursive `Type`/`Instruction` Drop, gated on !21985.
    #[test]
    fn deeply_nested_push_lambda_value_typechecks_without_stack_overflow() {
        use std::thread;
        const DEPTH: usize = 5_000;
        thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
                typecheck_nested_push_lambda(&arena, DEPTH)
                    .expect("PUSH-lambda chain must typecheck iteratively");
                LAMBDA_TYPECHECK_DEPTH.with(|d| {
                    assert_eq!(
                        d.get(),
                        0,
                        "depth counter must return to 0 after the iterative PUSH-lambda chain",
                    );
                });
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    // Regression: pre-fix, nested LAMBDAs grew the Rust call stack by one
    // frame per nesting level (`typecheck_instruction_step` → `typecheck_lambda`
    // → `typecheck` recursion). With the OpenLambda/AfterLambda worklist
    // refactor, depth is bounded by gas/heap only. Each body is
    // `LAMBDA unit unit { inner } ; DROP` so the body's residual stack
    // ends at `[unit]` and unifies with `out_ty = unit`. Run on a 1 MiB
    // worker thread matching the kernel budget; `mem::forget` skips the
    // recursive Drop on `Instruction`/`Type` (gated on MR4).
    #[test]
    fn deeply_nested_lambda_typechecks_without_stack_overflow() {
        use std::thread;
        const DEPTH: usize = 8_000;
        thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let arena: typed_arena::Arena<Micheline<'_>> = typed_arena::Arena::new();
                let unit_ty = Micheline::App(Prim::unit, &[], NO_ANNS);
                let drop_instr = Micheline::App(Prim::DROP, &[], NO_ANNS);
                // Innermost body is just empty (stack stays [unit]).
                let mut current: &[Micheline<'_>] = &[];
                for _ in 0..DEPTH {
                    let lambda = Micheline::App(
                        Prim::LAMBDA,
                        arena.alloc_extend([
                            unit_ty.clone(),
                            unit_ty.clone(),
                            Micheline::Seq(current),
                        ]),
                        NO_ANNS,
                    );
                    // Wrap: `LAMBDA unit unit { current } ; DROP` keeps
                    // the body's stack at [unit] for the surrounding
                    // lambda's out_ty unification.
                    current = arena.alloc_extend([lambda, drop_instr.clone()]);
                }
                let mut gas = Gas::new(u32::MAX);
                let stk = &mut tc_stk![];
                let result = typecheck(
                    current,
                    &mut gas,
                    None,
                    stk,
                    false,
                    TypecheckViews::Disabled,
                );
                std::mem::forget(std::mem::replace(stk, tc_stk![]));
                let body = result.expect("deep lambda nesting must typecheck");
                std::mem::forget(body);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    #[test]
    fn map_key_comparable_check_fires_before_value_parsing() {
        // Single-arg `pair int` is a valid Micheline but rejected by
        // parse_ty (pair needs at least 2 type args). With the bug, the
        // value's parse_ty error surfaces first; with the fix, the key's
        // `Comparable` failure (list isn't comparable) surfaces first,
        // matching the recursive code.
        let err = parse_ty(&mut Gas::default(), &parse("map (list unit) (pair int)").unwrap())
            .expect_err("must reject");
        match err {
            TcError::InvalidTypeProperty(TypeProperty::Comparable, t) => {
                assert_eq!(t, Type::new_list(Type::Unit),
                    "must surface the key Comparable error, not the value error");
            }
            other => panic!(
                "expected InvalidTypeProperty(Comparable, list unit); got {other:?}",
            ),
        }
    }

    #[test]
    fn big_map_value_check_fires_after_value_parsing() {
        // big_map is fine on `int -> nat`, but `big_map int big_map int nat`
        // must reject the value as not-BigMapValue. The check fires after
        // the value is parsed — same as the recursive order.
        let err = parse_ty(
            &mut Gas::default(),
            &parse("big_map int (big_map int nat)").unwrap(),
        )
        .expect_err("nested big_map must reject");
        match err {
            TcError::InvalidTypeProperty(TypeProperty::BigMapValue, _) => {}
            other => panic!("expected BigMapValue rejection; got {other:?}"),
        }
    }

    // Locks in the wording of `InvalidValueForType` when a legacy-ticket
    // value (the 3-tuple form) is malformed. The recursive code wrapped
    // the synthetic pair typecheck in a match whose `_ =>` arm remapped
    // any failure to `InvalidValueForType(value, ticket_ty)`. The new
    // worklist must do the same via the recursive `typecheck_value` call
    // with an explicit Err remap.
    #[test]
    fn invalid_value_for_type_legacy_ticket_malformed() {
        let mut ctx = Ctx::default();
        // A string value against `ticket int`: the synthetic pair-typecheck
        // fails on the outer Pair, and the error must be remapped to
        // reference the ticket type, not the synthetic pair type.
        let result = typecheck_value(
            &parse("\"not_a_pair\"").unwrap(),
            &mut ctx,
            &Type::new_ticket(Type::Int),
        );
        match result {
            Err(TcError::InvalidValueForType(_, t)) => {
                assert_eq!(t, Type::new_ticket(Type::Int),
                    "ticket Err must reference the ticket type, not the synthetic pair");
            }
            other => panic!("expected InvalidValueForType(_, ticket int), got {other:?}"),
        }
    }

    // Locks in the wording of `InvalidValueForType` when typechecking an
    // over-long Pair literal against a shorter Pair type. The new iterative
    // worklist hand-formats the synthetic-pair tail; this test guards that
    // the format stays byte-identical with `format!("{:?}", App(Pair, vs,
    // NO_ANNS))`, which is what the recursive code emitted.
    #[test]
    fn invalid_value_for_type_n_ary_pair_tail_wording() {
        let mut ctx = Ctx::default();
        let result = typecheck_value(
            &parse("Pair 1 2 3").unwrap(),
            &mut ctx,
            &Type::new_pair(Type::Int, Type::Int),
        );
        // Three values for a 2-tuple: the third value (3) hits the
        // VisitPairTail fallback against the inner Type::Int.
        let msg = match result {
            Err(TcError::InvalidValueForType(s, _)) => s,
            other => panic!("expected InvalidValueForType, got {other:?}"),
        };
        // Format must match what `format!("{:?}", App(Pair, [Int(2), Int(3)], NO_ANNS))`
        // produces under the auto-derived Debug — same as the recursive code.
        let expected = format!(
            "{:?}",
            Micheline::App(
                Prim::Pair,
                &[Micheline::Int(2.into()), Micheline::Int(3.into())],
                NO_ANNS,
            ),
        );
        assert_eq!(msg, expected, "n-ary pair tail wording diverged");
    }

    #[test]
    fn ticket() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TICKET").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TICKET,
                stack: stk![Type::Nat],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 2 })
            })
        );

        let stk = &mut tc_stk![Type::Nat, Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("TICKET").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Ticket(Type::Unit))
        );
        assert_eq!(
            stk,
            &tc_stk![Type::new_option(Type::new_ticket(Type::Unit))]
        );

        // Trying to build a `ticket (ticket string)` fails because
        // `ticket string` is not comparable.
        let stk = &mut tc_stk![Type::Nat, Type::new_ticket(Type::String)];
        assert_eq!(
            typecheck_instruction(&parse("TICKET").unwrap(), &mut Gas::default(), stk),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_ticket(Type::String)
            ))
        );
    }

    #[test]
    fn read_ticket() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("READ_TICKET").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::READ_TICKET,
                stack: stk![Type::Nat],
                reason: None
            })
        );

        let test_ticket = Type::new_ticket(Type::Int);
        let stk = &mut tc_stk![test_ticket.clone()];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("READ_TICKET").unwrap(), &mut gas, stk),
            Ok(ReadTicket)
        );
        assert_eq!(
            stk,
            &tc_stk![
                test_ticket,
                Type::new_pair(Type::Address, Type::new_pair(Type::Int, Type::Nat)),
            ]
        );
    }

    #[test]
    fn split_ticket() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SPLIT_TICKET").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SPLIT_TICKET,
                stack: stk![Type::Nat],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 2 })
            })
        );
        let stk = &mut tc_stk![Type::Nat, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SPLIT_TICKET").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SPLIT_TICKET,
                stack: stk![Type::Nat, Type::Nat],
                reason: None
            })
        );
        let stk = &mut tc_stk![
            Type::new_pair(Type::Nat, Type::Nat),
            Type::new_ticket(Type::Unit)
        ];
        assert_eq!(
            typecheck_instruction(&parse("SPLIT_TICKET").unwrap(), &mut Gas::default(), stk),
            Ok(SplitTicket)
        );
        assert_eq!(
            stk,
            &tc_stk![Type::new_option(Type::new_pair(
                Type::new_ticket(Type::Unit),
                Type::new_ticket(Type::Unit)
            ))]
        );
    }

    #[test]
    fn join_tickets() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("JOIN_TICKETS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::JOIN_TICKETS,
                stack: stk![Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("JOIN_TICKETS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::JOIN_TICKETS,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        let stk = &mut tc_stk![Type::new_pair(
            Type::new_ticket(Type::Int),
            Type::new_ticket(Type::Unit)
        )];
        assert_eq!(
            typecheck_instruction(&parse("JOIN_TICKETS").unwrap(), &mut Gas::default(), stk),
            Err(TcError::TypesNotEqual(TypesNotEqual(Type::Int, Type::Unit)))
        );

        let stk = &mut tc_stk![Type::new_pair(
            Type::new_ticket(Type::Unit),
            Type::new_ticket(Type::Unit)
        )];
        assert_eq!(
            typecheck_instruction(&parse("JOIN_TICKETS").unwrap(), &mut Gas::default(), stk),
            Ok(JoinTickets)
        );

        assert_eq!(
            stk,
            &tc_stk![Type::new_option(Type::new_ticket(Type::Unit))]
        );
    }

    #[test]
    fn hash_key() {
        let stk = &mut tc_stk![Type::Key];
        let gas = &mut Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("HASH_KEY").unwrap(), gas, stk),
            Ok(HashKey)
        )
    }

    #[test]
    fn hash_key_bad_arg() {
        let stk = &mut tc_stk![Type::Bytes];
        let gas = &mut Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("HASH_KEY").unwrap(), gas, stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::HASH_KEY,
                stack: stk![Type::Bytes],
                reason: Some(TypesNotEqual(Type::Key, Type::Bytes).into()),
            })
        )
    }

    #[test]
    fn hash_key_too_short() {
        too_short_test(&parse("HASH_KEY").unwrap(), Prim::HASH_KEY, 1);
    }

    mod hash_instructions {
        use super::*;

        // hash instructions are all basically the same as far as typechecking
        // is concerned, so instead of duplicating a bunch of tests 5 times, a
        // macro. -- @lierdakl
        macro_rules! test {
            ($instr_prim:ident, $expected_instruction:expr) => {
                #[allow(non_snake_case)]
                mod $instr_prim {
                    use super::*;

                    #[test]
                    fn ok() {
                        let mut stack = tc_stk![Type::Bytes];
                        assert_eq!(
                            typecheck_instruction(
                                &parse(stringify!($instr_prim)).unwrap(),
                                &mut Gas::default(),
                                &mut stack,
                            ),
                            Ok($expected_instruction),
                        );
                        assert_eq!(stack, tc_stk![Type::Bytes]);
                    }

                    #[test]
                    fn too_short() {
                        too_short_test(
                            &parse(stringify!($instr_prim)).unwrap(),
                            Prim::$instr_prim,
                            1,
                        );
                    }

                    #[test]
                    fn bad_input() {
                        let mut stack = tc_stk![Type::Unit];
                        assert_eq!(
                            typecheck_instruction(
                                &parse(stringify!($instr_prim)).unwrap(),
                                &mut Gas::default(),
                                &mut stack,
                            ),
                            Err(TcError::NoMatchingOverload {
                                instr: Prim::$instr_prim,
                                stack: stk![Type::Unit],
                                reason: Some(TypesNotEqual(Type::Bytes, Type::Unit).into()),
                            }),
                        );
                    }

                    #[test]
                    fn bad_micheline() {
                        assert!(matches!(
                            typecheck_instruction(
                                // instruction with unit argument
                                &parse(&format!("{} unit", stringify!($instr_prim))).unwrap(),
                                &mut Gas::default(),
                                &mut tc_stk![],
                            ),
                            Err(TcError::UnexpectedMicheline(_)),
                        ));
                    }
                }
            };
        }
        test!(BLAKE2B, Blake2b);
        test!(KECCAK, Keccak);
        test!(SHA256, Sha256);
        test!(SHA3, Sha3);
        test!(SHA512, Sha512);
    }

    #[test]
    fn balance() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("BALANCE").unwrap(), &mut Gas::default(), stk),
            Ok(Balance)
        );

        assert_eq!(stk, &tc_stk![Type::Mutez]);
    }

    #[test]
    fn contract() {
        let stk = &mut tc_stk![Type::Address];
        assert_eq!(
            typecheck_instruction(&parse("CONTRACT int").unwrap(), &mut Gas::default(), stk),
            Ok(Contract(Type::Int, Entrypoint::default()))
        );
        assert_eq!(
            stk,
            &tc_stk![Type::new_option(Type::new_contract(Type::Int))]
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("CONTRACT int").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CONTRACT,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        let stk = &mut tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("CONTRACT int").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CONTRACT,
                stack: stk![Type::Unit],
                reason: None
            })
        );

        // `CONTRACT operation` is rejected because `operation` is not passable.
        let stk = &mut tc_stk![Type::Address];
        assert_eq!(
            typecheck_instruction(
                &parse("CONTRACT operation").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Passable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn level() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("LEVEL").unwrap(), &mut Gas::default(), stk),
            Ok(Level)
        );

        assert_eq!(stk, &tc_stk![Type::Nat]);
    }

    #[test]
    fn min_block_time() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("MIN_BLOCK_TIME").unwrap(), &mut Gas::default(), stk),
            Ok(MinBlockTime)
        );

        assert_eq!(stk, &tc_stk![Type::Nat]);
    }

    #[test]
    fn self_address() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("SELF_ADDRESS").unwrap(), &mut Gas::default(), stk),
            Ok(SelfAddress)
        );

        assert_eq!(stk, &tc_stk![Type::Address]);
    }

    #[test]
    fn sender() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("SENDER").unwrap(), &mut Gas::default(), stk),
            Ok(Sender)
        );

        assert_eq!(stk, &tc_stk![Type::Address]);
    }

    #[test]
    fn source() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("SOURCE").unwrap(), &mut Gas::default(), stk),
            Ok(Source)
        );

        assert_eq!(stk, &tc_stk![Type::Address]);
    }

    #[test]
    fn timestamp_value_tc() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp 1571659294").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(1571659294))))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"2019-10-21T12:01:34Z\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(1571659294))))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"1571659294\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(1571659294))))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"ABCD\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::InvalidValueForType(
                "String(\"ABCD\")".to_string(),
                Type::Timestamp
            ))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"3.5\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::InvalidValueForType(
                "String(\"3.5\")".to_string(),
                Type::Timestamp
            ))
        );
    }

    /// L2-1375: `PUSH timestamp "<bigint>"` where `<bigint>` exceeds the
    /// `i64` range must be accepted (matching L1's `Z.of_string` fallback in
    /// `Script_timestamp.of_string`).
    #[test]
    fn timestamp_value_string_bigint() {
        // ~10x i64::MAX — the exact case from the L2-1375 reproduction.
        let big = "99999999999999999999";
        let expected = BigInt::parse_bytes(big.as_bytes(), 10).unwrap();
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH timestamp \"{big}\"")).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::Timestamp(expected.clone()))))
        );

        // Sibling control: same numeric literal as a `Micheline` int — already
        // accepted on master — must still decode to the same value.
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH timestamp {big}")).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::Timestamp(expected))))
        );

        // Negative big-int string is also accepted by `Z.of_string`.
        let neg = "-99999999999999999999";
        let neg_expected = BigInt::parse_bytes(neg.as_bytes(), 10).unwrap();
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(&format!("PUSH timestamp \"{neg}\"")).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::Timestamp(neg_expected))))
        );
    }

    /// L2-1375: an RFC3339 leap-second string (`...T00:00:60Z`) must decode to
    /// the same epoch second as L1 (Ptime folds the leap second into the
    /// following POSIX second; on master MIR was returning the prior second).
    #[test]
    fn timestamp_value_string_leap_second() {
        // L1 (Ptime) maps `1970-01-01T00:00:60Z` to epoch second 60.
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"1970-01-01T00:00:60Z\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(60))))
        );

        // Sibling control: `:00` and `:59` must keep the obvious values.
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"1970-01-01T00:00:00Z\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(0))))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"1970-01-01T00:00:59Z\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(Rc::new(TypedValue::timestamp(59))))
        );
    }

    #[test]
    fn implicit_account() {
        let stk = &mut tc_stk![Type::KeyHash];
        assert_eq!(
            typecheck_instruction(
                &parse("IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(ImplicitAccount)
        );
        assert_eq!(stk, &tc_stk![Type::new_contract(Type::Unit)]);

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IMPLICIT_ACCOUNT,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        let stk = &mut tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(
                &parse("IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IMPLICIT_ACCOUNT,
                stack: stk![Type::Unit],
                reason: None
            })
        );
    }

    #[test]
    fn is_implicit_account() {
        let stk = &mut tc_stk![Type::Address];
        assert_eq!(
            typecheck_instruction(
                &parse("IS_IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(IsImplicitAccount)
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::KeyHash)]);

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("IS_IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IS_IMPLICIT_ACCOUNT,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        let stk = &mut tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(
                &parse("IS_IMPLICIT_ACCOUNT").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IS_IMPLICIT_ACCOUNT,
                stack: stk![Type::Unit],
                reason: None
            })
        );
    }

    #[test]
    fn now() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("NOW").unwrap(), &mut Gas::default(), stk),
            Ok(Now)
        );

        assert_eq!(stk, &tc_stk![Type::Timestamp]);
    }

    #[test]
    fn voting_power() {
        let stk = &mut tc_stk![Type::KeyHash];
        assert_eq!(
            typecheck_instruction(&parse("VOTING_POWER").unwrap(), &mut Gas::default(), stk),
            Ok(VotingPower)
        );

        assert_eq!(stk, &tc_stk![Type::Nat]);

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("VOTING_POWER").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::VOTING_POWER,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        let stk = &mut tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("VOTING_POWER").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::VOTING_POWER,
                stack: stk![Type::Unit],
                reason: None
            })
        );
    }

    #[test]
    fn total_voting_power() {
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("TOTAL_VOTING_POWER").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(TotalVotingPower)
        );

        assert_eq!(stk, &tc_stk![Type::Nat]);
    }

    #[test]
    fn dig() {
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DIG 3").unwrap(), &mut Gas::default(), stk),
            Ok(Dig(3))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Int, Type::Nat, Type::String, Type::Unit]
        );

        // DIG 0
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DIG 0").unwrap(), &mut Gas::default(), stk),
            Ok(Dig(0))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String]
        );

        // DIG 1
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DIG 1").unwrap(), &mut Gas::default(), stk),
            Ok(Dig(1))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Unit, Type::Int, Type::String, Type::Nat]
        );
    }

    #[test]
    fn dug() {
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DUG 2").unwrap(), &mut Gas::default(), stk),
            Ok(Dug(2))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Unit, Type::String, Type::Int, Type::Nat,]
        );

        // DUG 0
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DUG 0").unwrap(), &mut Gas::default(), stk),
            Ok(Dug(0))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String]
        );

        // DUG 1
        let stk = &mut tc_stk![Type::Unit, Type::Int, Type::Nat, Type::String];
        assert_eq!(
            typecheck_instruction(&parse("DUG 1").unwrap(), &mut Gas::default(), stk),
            Ok(Dug(1))
        );

        assert_eq!(
            stk,
            &tc_stk![Type::Unit, Type::Int, Type::String, Type::Nat]
        );
    }

    #[test]
    fn unpack() {
        let stk = &mut tc_stk![Type::Bytes];
        assert_eq!(
            typecheck_instruction(&parse("UNPACK int").unwrap(), &mut Gas::default(), stk),
            Ok(Unpack(Type::Int))
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn unpack_contract() {
        let stk = &mut tc_stk![Type::Bytes];
        assert_eq!(
            typecheck_instruction(
                &parse("UNPACK (contract unit)").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    fn unpack_bad_stack() {
        let stk = &mut tc_stk![Type::Unit];
        assert_eq!(
            typecheck_instruction(&parse("UNPACK int").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UNPACK,
                stack: stk![Type::Unit],
                reason: Some(TypesNotEqual(Type::Bytes, Type::Unit).into())
            })
        );
    }

    #[test]
    fn unpack_short_stack() {
        too_short_test(&parse("UNPACK unit").unwrap(), Prim::UNPACK, 1)
    }

    #[test]
    fn emit() {
        use crate::ast::annotations::FieldAnnotation;

        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("EMIT %mytag nat").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Emit {
                tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                arg_ty: Right(parse("nat").unwrap())
            })
        );
        assert_eq!(stk, &tc_stk![Type::Operation]);

        // Instruction with tag only
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("EMIT %mytag").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Emit {
                tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                arg_ty: Left(Type::Nat)
            })
        );

        // Only pushable types can be emitted.
        let stk = &mut tc_stk![Type::Operation];
        assert_eq!(
            typecheck_instruction(&parse("EMIT").unwrap(), &mut Gas::default(), stk),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::Operation
            ))
        );

        // Only pushable types can be emitted with type explicitly provided.
        let stk = &mut tc_stk![Type::Operation];
        assert_eq!(
            typecheck_instruction(&parse("EMIT operation").unwrap(), &mut Gas::default(), stk),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Pushable,
                Type::Operation
            ))
        );

        // Instruction with type arg only
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("EMIT nat").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Emit {
                tag: None,
                arg_ty: Right(parse("nat").unwrap())
            })
        );

        // Instruction with type no args
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("EMIT").unwrap(), &mut Gas::default(), stk),
            Ok(Instruction::Emit {
                tag: None,
                arg_ty: Left(Type::Nat)
            })
        );

        // Type from stack and from argument should be same.
        let stk = &mut tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("EMIT %mytag nat").unwrap(), &mut Gas::default(), stk),
            Err(TcError::TypesNotEqual(TypesNotEqual(Type::Nat, Type::Int)))
        );

        // too short stack
        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("EMIT %mytag nat").unwrap(), &mut Gas::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::EMIT,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );

        // Unexpected argument count
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(
                &parse("EMIT %mytag nat nat").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::UnexpectedMicheline(
                "App(EMIT, [App(nat, [], []), App(nat, [], [])], [Field(\"mytag\")])".to_string()
            ))
        );

        // Too long tag
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(
                &parse("EMIT %mytagmytagmytagmytagmytagmytagmytag").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::EntrypointError(ByteReprError::WrongFormat(
                "entrypoint name must be at most 31 characters long, but it is 35 characters long"
                    .into()
            )))
        );

        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(
                &parse("EMIT %mytagmytagmytagmytagmytagmytagmytag nat").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::EntrypointError(ByteReprError::WrongFormat(
                "entrypoint name must be at most 31 characters long, but it is 35 characters long"
                    .into()
            )))
        );
    }

    #[test]
    fn create_contract() {
        let stk = &mut tc_stk![Type::Unit, Type::Mutez, Type::new_option(Type::KeyHash)];
        let mut gas = Gas::default();
        let create_contract_src = "CREATE_CONTRACT { parameter unit; storage unit; code { DROP; UNIT; NIL operation; PAIR; }}";
        let cs_mich =
            parse("{ parameter unit; storage unit; code { DROP; UNIT; NIL operation; PAIR; }}")
                .unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut gas, true, true)
            .unwrap();
        assert_eq!(
            typecheck_instruction(
                &parse(create_contract_src).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(CreateContract(Rc::new(cs), &cs_mich))
        );
        assert_eq!(stk, &tc_stk![Type::Address, Type::Operation]);

        // Stack too short tests
        let stk = &mut tc_stk![Type::Mutez, Type::new_option(Type::KeyHash)];
        assert_eq!(
            typecheck_instruction(
                &parse(create_contract_src).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CREATE_CONTRACT,
                stack: stk![Type::Mutez, Type::new_option(Type::KeyHash)],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 3 })
            })
        );

        let stk = &mut tc_stk![Type::Mutez];
        assert_eq!(
            typecheck_instruction(
                &parse(create_contract_src).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CREATE_CONTRACT,
                stack: stk![Type::Mutez],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 3 })
            })
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(create_contract_src).unwrap(),
                &mut Gas::default(),
                stk
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CREATE_CONTRACT,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 3 })
            })
        );
    }

    #[test]
    fn test_cast() {
        let mut stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("CAST nat").unwrap(), &mut gas, &mut stack),
            Ok(Seq(Vec::new()))
        );
        assert_eq!(stack, tc_stk![Type::Nat]);
        let nat_size = Type::Nat.size_for_gas();
        assert_eq!(
            Gas::default().milligas().unwrap() - gas.milligas().unwrap(),
            tc_cost::INSTR_STEP
                + tc_cost::PARSE_TYPE_STEP
                + tc_cost::ty_eq(nat_size, nat_size).unwrap()
        );
    }

    #[test]
    fn test_cast_wrong_type() {
        let mut stack = tc_stk![Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("CAST nat").unwrap(), &mut gas, &mut stack),
            Err(TcError::TypesNotEqual(TypesNotEqual(Type::Nat, Type::Int)))
        );
    }

    #[test]
    fn test_cast_with_stack_to_small() {
        let mut stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("CAST nat").unwrap(), &mut gas, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::CAST,
                stack: stk![],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 })
            })
        );
    }

    #[test]
    fn test_cast_too_much_arg() {
        let mut stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("CAST nat nat").unwrap(), &mut gas, &mut stack),
            Err(TcError::UnexpectedMicheline(
                "App(CAST, [App(nat, [], []), App(nat, [], [])], [])".to_string()
            ))
        );
    }

    #[test]
    fn test_cast_not_enough_arg() {
        let mut stack = tc_stk![Type::Nat];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("CAST").unwrap(), &mut gas, &mut stack),
            Err(TcError::UnexpectedMicheline(
                "App(CAST, [], [])".to_string()
            ))
        );
    }

    #[test]
    fn test_rename() {
        let mut stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("RENAME").unwrap(), &mut gas, &mut stack),
            Ok(Seq(Vec::new()))
        );
        assert_eq!(stack, tc_stk![]);
        assert_eq!(
            Gas::default().milligas().unwrap() - gas.milligas().unwrap(),
            tc_cost::INSTR_STEP
        );
    }

    #[test]
    fn test_rename_too_much_arg() {
        let mut stack = tc_stk![];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("RENAME bool").unwrap(), &mut gas, &mut stack),
            Err(TcError::UnexpectedMicheline(
                "App(RENAME, [App(bool, [], [])], [])".to_string()
            ))
        );
    }

    // -- View restriction tests (L2-1049) --
    // Ref: https://octez.tezos.com/docs/active/views.html

    fn typecheck_script_with_view(view_body: &str) -> Result<(), TcError> {
        let src = format!(
            "parameter unit; storage unit; code {{ CAR; NIL operation; PAIR }}; \
             view \"v\" unit unit {{ {view_body} }};",
        );
        parse_contract_script(&src)
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)?;
        Ok(())
    }

    // -- Forbidden direct in view body --
    // in_view is checked before stack type, so minimal stacks suffice.

    #[test]
    fn transfer_tokens_forbidden_in_view() {
        assert_eq!(
            typecheck_script_with_view("TRANSFER_TOKENS"),
            Err(TcError::ForbiddenInView(Prim::TRANSFER_TOKENS))
        );
    }

    #[test]
    fn set_delegate_forbidden_in_view() {
        assert_eq!(
            typecheck_script_with_view("SET_DELEGATE"),
            Err(TcError::ForbiddenInView(Prim::SET_DELEGATE))
        );
    }

    #[test]
    fn create_contract_forbidden_in_view() {
        assert_eq!(
            typecheck_script_with_view("CREATE_CONTRACT { parameter unit; storage unit; code { CDR; NIL operation; PAIR } }"),
            Err(TcError::ForbiddenInView(Prim::CREATE_CONTRACT))
        );
    }

    // -- Allowed in view body --

    #[test]
    fn balance_allowed_in_view() {
        assert!(typecheck_script_with_view("DROP; BALANCE; DROP; UNIT").is_ok());
    }

    #[test]
    fn amount_allowed_in_view() {
        assert!(typecheck_script_with_view("DROP; AMOUNT; DROP; UNIT").is_ok());
    }

    // -- Lambda escape: forbidden instruction inside LAMBDA in view is ok --

    #[test]
    fn set_delegate_in_lambda_in_view_ok() {
        assert!(typecheck_script_with_view(
            "DROP; LAMBDA (option key_hash) operation { SET_DELEGATE }; DROP; UNIT"
        )
        .is_ok());
    }

    #[test]
    fn create_contract_in_lambda_in_view_ok() {
        assert!(typecheck_script_with_view(
            "DROP; LAMBDA unit operation { \
               DROP; UNIT; PUSH mutez 0; NONE key_hash; \
               CREATE_CONTRACT { parameter unit; storage unit; code { CDR; NIL operation; PAIR } }; \
               SWAP; DROP \
             }; DROP; UNIT",
        )
        .is_ok());
    }

    // -- Nested: forbidden in IF inside view --

    #[test]
    fn set_delegate_in_if_in_view_forbidden() {
        assert_eq!(
            typecheck_script_with_view("DROP; PUSH bool True; IF { SET_DELEGATE } { UNIT }"),
            Err(TcError::ForbiddenInView(Prim::SET_DELEGATE))
        );
    }

    // -- Code after lambda in view still has restriction --

    #[test]
    fn forbidden_after_lambda_in_view() {
        assert_eq!(
            typecheck_script_with_view(
                "DROP; \
                 LAMBDA unit unit { }; DROP; \
                 SET_DELEGATE"
            ),
            Err(TcError::ForbiddenInView(Prim::SET_DELEGATE))
        );
    }

    // -- Nested lambda: forbidden in nested lambda escape is still ok --

    #[test]
    fn forbidden_in_nested_lambda_in_view_ok() {
        assert!(typecheck_script_with_view(
            "DROP; \
             LAMBDA unit operation { \
               DROP; LAMBDA (option key_hash) operation { SET_DELEGATE }; \
               NONE key_hash; EXEC \
             }; DROP; UNIT"
        )
        .is_ok());
    }

    // -- Conversion impls for the formerly-panicking error paths --

    #[test]
    fn stack_oob_converts_to_stack_oob_variant() {
        let err: TcError = StackOob.into();
        assert_eq!(err, TcError::StackOob(StackOob));
    }

    #[test]
    fn compare_error_converts_to_compare_error_variant() {
        // No flattening: CompareError is preserved structure-intact
        // inside TcError via the #[from] variant.
        let err: TcError = CompareError::Incomparable.into();
        assert_eq!(err, TcError::CompareError(CompareError::Incomparable));
    }

    #[test]
    fn cost_overflow_converts_to_cost_overflow_variant() {
        let ovf: TcError = CostOverflow.into();
        assert_eq!(ovf, TcError::CostOverflow(CostOverflow));
    }

    // -- View-name validation: lock in byte-loop replacement of the regex --

    #[test]
    fn check_view_name_accepts_empty() {
        assert_eq!(check_view_name(""), Ok(()));
    }

    #[test]
    fn check_view_name_accepts_all_allowed_chars() {
        assert_eq!(
            check_view_name("aZ0_.%@"),
            Ok(()),
            "all allowed character classes must be accepted"
        );
    }

    #[test]
    fn check_view_name_accepts_31_chars() {
        let name = "a".repeat(31);
        assert_eq!(check_view_name(&name), Ok(()));
    }

    #[test]
    fn check_view_name_rejects_32_chars() {
        let name = "a".repeat(32);
        assert_eq!(check_view_name(&name), Err(TcError::InvalidViewName(name)));
    }

    #[test]
    fn check_view_name_rejects_disallowed_ascii() {
        for c in ['?', '!', '-', '+', '/', ' ', '\n', '*', '#'] {
            let name = format!("name{c}");
            assert_eq!(
                check_view_name(&name),
                Err(TcError::InvalidViewName(name.clone())),
                "expected {c:?} to be rejected"
            );
        }
    }

    #[test]
    fn check_view_name_rejects_non_ascii() {
        // Multibyte UTF-8 must be rejected at the byte level.
        let name = "café";
        assert_eq!(
            check_view_name(name),
            Err(TcError::InvalidViewName(name.to_owned()))
        );
    }

    // -- Macro error-message contract --
    //
    // The pop!, stack_get!, stack_top_mut! macros live inside
    // typecheck_instruction and their error arms are unreachable for any
    // valid Micheline (the outer match patterns guarantee the
    // preconditions). The tests below lock in the exact InternalError
    // wording each macro will produce if its arm is ever reached, so a
    // future refactor that drifts the strings is caught here instead of
    // shipping a silent diagnostic change.

    #[test]
    fn pop_empty_internal_error_message() {
        // pop!() on an empty type stack produces this exact error.
        let err = TcError::InternalError(TcInvariant::EmptyTypeStackPop);
        assert_eq!(
            format!("{err}"),
            "internal typechecker error: attempted to pop from an empty type stack"
        );
    }

    #[test]
    fn stack_top_mut_empty_internal_error_message() {
        // stack_top_mut() on an empty type stack returns this exact error.
        // Unreachable for valid Micheline, so exercised directly here.
        let mut stack = crate::stack::TypeStack::new();
        let err = super::stack_top_mut(&mut stack).expect_err("empty stack must error");
        assert_eq!(
            format!("{err}"),
            "internal typechecker error: type stack unexpectedly empty"
        );
    }

    #[test]
    fn pop_type_mismatch_internal_error_message() {
        // pop!(T::Foo) on a value that isn't T::Foo produces this exact error,
        // with the expected variant name stringified at the macro call site.
        let err = TcError::InternalError(TcInvariant::TypeMismatchOnPop {
            expected: "Type::Nat",
        });
        assert_eq!(
            format!("{err}"),
            "internal typechecker error: type mismatch when popping the type stack: expected Type::Nat"
        );
    }

    #[test]
    fn stack_get_oob_internal_error_format() {
        // stack_get(idx) out-of-bounds returns this exact error, carrying
        // the requested index and the actual stack length for forensics.
        // Unreachable for valid Micheline, so exercised directly here.
        let stack = crate::stack::TypeStack::new();
        let err = super::stack_get(&stack, 3).expect_err("out-of-range access must error");
        assert_eq!(
            format!("{err}"),
            "internal typechecker error: type stack too short for indexed access: index 3, len 0"
        );
    }
}
