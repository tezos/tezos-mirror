// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Michelson typechecker definitions. Most functions defined as associated
//! functions on [Micheline], see there for more.

use chrono::prelude::DateTime;
use num_bigint::{BigInt, BigUint, TryFromBigIntError};
use num_traits::{Signed, Zero};
use regex::Regex;
use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;
use tezos_crypto_rs::{base58::FromBase58CheckError, hash::FromBytesError, public_key::PublicKey};
use tezos_data_encoding::nom::{error::convert_error, NomReader};
use tezos_protocol::entrypoint;

pub mod type_props;

use type_props::TypeProperty;

use crate::ast::annotations::{AnnotationError, NO_ANNS};
use crate::ast::big_map::BigMap;
use crate::ast::micheline::{
    micheline_fields, micheline_instructions, micheline_literals, micheline_types,
    micheline_unsupported_instructions, micheline_unsupported_types, micheline_values,
};
use crate::ast::michelson_address::AddressHash;
use crate::ast::*;
#[cfg(feature = "bls")]
use crate::bls;
use crate::context::TypecheckingCtx;
use crate::gas::OutOfGas;
use crate::gas::{self, tc_cost, Gas};
use crate::irrefutable_match::irrefutable_match;
use crate::lexer::Prim;
use crate::stack::*;

/// Typechecker error type.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TcError {
    /// Two stacks didn't compare equal when they should have.
    #[error("type stacks not equal: {0:?} != {1:?}")]
    StacksNotEqual(TypeStack, TypeStack, StacksNotEqualReason),
    /// Ran out of gas during typechecking.
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
    /// The type didn't satisfy a given [TypeProperty].
    #[error("type is not {0}: {1:?}")]
    InvalidTypeProperty(TypeProperty, Type),
    /// Encountered FAIL instruction not in tail position.
    #[error("FAIL instruction is not in tail position")]
    FailNotInTail,
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
    let re = Regex::new(r"^[a-zA-Z0-9_.%@]*$").expect("view name regex should be valid");
    if name.len() > 31 || !re.is_match(name) {
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
        for (name, view) in mich_views {
            let input_type = view.input_type.parse_ty(gas)?;
            let output_type = view.output_type.parse_ty(gas)?;
            if typecheck_views {
                input_type.ensure_prop(gas, TypeProperty::ViewInput)?;
                output_type.ensure_prop(gas, TypeProperty::ViewOutput)?;
                match view.code {
                    Micheline::Seq(instrs) => {
                        typecheck_lambda(
                            instrs,
                            gas,
                            Type::Pair(Rc::new((input_type.clone(), storage.clone()))),
                            output_type.clone(),
                            false,
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
        let code = typecheck_instruction(code, gas, Some(&entrypoints), &mut stack)?;
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
        typecheck_instruction(self, gas, entrypoints.as_ref(), &mut opt_stack)
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

fn parse_ty_with_entrypoints<'a>(
    gas: &mut Gas,
    ty: &Micheline<'a>,
    mut entrypoints: Option<&mut Entrypoints>,
    routed_annotations: &mut HashMap<FieldAnnotation<'a>, (Vec<Direction>, Type)>,
    path: Vec<Direction>,
) -> Result<Type, TcError> {
    use Micheline::*;
    use Prim::*;
    gas.consume(gas::tc_cost::PARSE_TYPE_STEP)?;
    fn make_pair<'a>(
        gas: &mut Gas,
        args: (&Micheline<'a>, &Micheline<'a>, &[Micheline<'a>]),
        // NB: the tuple models a slice of at least 2 elements
    ) -> Result<Type, TcError> {
        Ok(match args {
            (ty1, ty2, []) => Type::new_pair(parse_ty(gas, ty1)?, parse_ty(gas, ty2)?),
            (ty1, ty2, [ty3, rest @ ..]) => {
                Type::new_pair(parse_ty(gas, ty1)?, make_pair(gas, (ty2, ty3, rest))?)
            }
        })
    }
    let unexpected = || Err(TcError::UnexpectedMicheline(format!("{ty:?}")));
    let parsed_ty = match ty {
        App(int, [], _) => Type::Int,
        App(int, ..) => unexpected()?,

        App(nat, [], _) => Type::Nat,
        App(nat, ..) => unexpected()?,

        App(bool, [], _) => Type::Bool,
        App(bool, ..) => unexpected()?,

        App(mutez, [], _) => Type::Mutez,
        App(mutez, ..) => unexpected()?,

        App(string, [], _) => Type::String,
        App(string, ..) => unexpected()?,

        App(operation, [], _) => Type::Operation,
        App(operation, ..) => unexpected()?,

        App(never, [], _) => Type::Never,
        App(never, ..) => unexpected()?,

        App(unit, [], _) => Type::Unit,
        App(unit, ..) => unexpected()?,

        App(address, [], _) => Type::Address,
        App(address, ..) => unexpected()?,

        App(chain_id, [], _) => Type::ChainId,
        App(chain_id, ..) => unexpected()?,

        App(ticket, [t], _) => {
            let t = parse_ty(gas, t)?;
            // NB: The inner type of ticket only needs to be comparable.
            // See https://tezos.gitlab.io/michelson-reference/#type-ticket
            t.ensure_prop(gas, TypeProperty::Comparable)?;
            Type::new_ticket(t)
        }
        App(ticket, ..) => unexpected()?,

        App(timestamp, [], _) => Type::Timestamp,
        App(timestamp, ..) => unexpected()?,

        App(pair, [ty1, ty2, rest @ ..], _) => make_pair(gas, (ty1, ty2, rest))?,
        App(pair, ..) => unexpected()?,

        App(or, [l, r], _) => Type::new_or(
            parse_ty_with_entrypoints(gas, l, entrypoints.as_deref_mut(), routed_annotations, {
                let mut new_path = path.clone();
                new_path.push(Direction::Left);
                new_path
            })?,
            parse_ty_with_entrypoints(gas, r, entrypoints.as_deref_mut(), routed_annotations, {
                let mut new_path = path.clone();
                new_path.push(Direction::Right);
                new_path
            })?,
        ),

        App(or, ..) => unexpected()?,

        App(option, [t], _) => Type::new_option(parse_ty(gas, t)?),
        App(option, ..) => unexpected()?,

        App(list, [t], _) => Type::new_list(parse_ty(gas, t)?),
        App(list, ..) => unexpected()?,

        App(lambda, [ty1, ty2], _) => Type::new_lambda(parse_ty(gas, ty1)?, parse_ty(gas, ty2)?),
        App(lambda, ..) => unexpected()?,

        App(contract, [t], _) => {
            let t = parse_ty(gas, t)?;
            // NB: despite `contract` type being duplicable and packable, its
            // argument doesn't need to be. The only constraint is that it needs
            // to be passable, as it represents the contract's parameter type.
            // See https://tezos.gitlab.io/michelson-reference/#type-contract
            t.ensure_prop(gas, TypeProperty::Passable)?;
            Type::new_contract(t)
        }
        App(contract, ..) => unexpected()?,

        App(set, [k], _) => {
            let k = parse_ty(gas, k)?;
            k.ensure_prop(gas, TypeProperty::Comparable)?;
            Type::new_set(k)
        }
        App(set, ..) => unexpected()?,

        App(map, [k, v], _) => {
            let k = parse_ty(gas, k)?;
            k.ensure_prop(gas, TypeProperty::Comparable)?;
            let v = parse_ty(gas, v)?;
            Type::new_map(k, v)
        }
        App(map, ..) => unexpected()?,

        App(big_map, [k, v], _) => {
            let k = parse_ty(gas, k)?;
            k.ensure_prop(gas, TypeProperty::Comparable)?;
            let v = parse_ty(gas, v)?;
            v.ensure_prop(gas, TypeProperty::BigMapValue)?;
            Type::new_big_map(k, v)
        }
        App(big_map, ..) => unexpected()?,

        App(bytes, [], _) => Type::Bytes,
        App(bytes, ..) => unexpected()?,

        App(key, [], _) => Type::Key,
        App(key, ..) => unexpected()?,

        App(key_hash, [], _) => Type::KeyHash,
        App(key_hash, ..) => unexpected()?,

        App(signature, [], _) => Type::Signature,
        App(signature, ..) => unexpected()?,

        #[cfg(feature = "bls")]
        App(bls12_381_fr, [], _) => Type::Bls12381Fr,
        #[cfg(feature = "bls")]
        App(bls12_381_fr, ..) => unexpected()?,

        #[cfg(feature = "bls")]
        App(bls12_381_g1, [], _) => Type::Bls12381G1,
        #[cfg(feature = "bls")]
        App(bls12_381_g1, ..) => unexpected()?,

        #[cfg(feature = "bls")]
        App(bls12_381_g2, [], _) => Type::Bls12381G2,
        #[cfg(feature = "bls")]
        App(bls12_381_g2, ..) => unexpected()?,

        Seq(..)
        | micheline_fields!()
        | micheline_instructions!()
        | micheline_literals!()
        | micheline_values!() => unexpected()?,

        App(prim @ micheline_unsupported_types!(), ..) => Err(TcError::TodoType(*prim))?,
    };

    if let Option::Some(eps) = entrypoints {
        // we just ensured it's an application of some type primitive
        irrefutable_match!(ty; App, _prim, _args, anns);
        if let Option::Some(field_ann) = anns.get_single_field_ann()? {
            // NB: field annotations may be longer than entrypoints; however
            // it's not an error to have an overly-long field annotation, it
            // just doesn't count as an entrypoint.
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
    Ok(parsed_ty)
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
fn typecheck<'a>(
    ast: &[Micheline<'a>],
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
) -> Result<Vec<Instruction<'a>>, TcError> {
    ast.iter()
        .map(|i| typecheck_instruction(i, gas, self_entrypoints, opt_stack))
        .collect()
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
pub(crate) fn typecheck_instruction<'a>(
    i: &Micheline<'a>,
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
) -> Result<Instruction<'a>, TcError> {
    use Instruction as I;
    use NoMatchingOverloadReason as NMOR;
    use Type as T;

    let stack = opt_stack.access_mut(TcError::FailNotInTail)?;

    // helper to reduce boilerplate. Usage:
    // `pop!()` force-pops the top elements from the stack (panics if nothing to
    // pop), returning it
    // `let x = pop!()` is roughly equivalent to `let x = stack.pop().unwrap()`
    // but with a clear error message
    // `pop!(T::Foo)` force-pops the stack and unwraps `T::Foo(x)`, returning
    // `x`
    // `let x = pop!(T::Foo)` is roughly equivalent to `let T::Foo(x) = pop!()
    // else {panic!()}` but with a clear error message
    // `pop!(T::Bar, x, y, z)` force-pops the stack, unwraps `T::Bar(x, y, z)`
    // and binds those to `x, y, z` in the surrounding scope
    // `pop!(T::Bar, x, y, z)` is roughly equivalent to `let T::Bar(x, y, z) =
    // pop!() else {panic!()}` but with a clear error message.
    macro_rules! pop {
        ($($args:tt)*) => {
            irrefutable_match!(
                stack.pop().expect("pop from empty stack!");
                $($args)*
                )
        };
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
    Ok(match (i, stack_slice.as_slice()) {
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
            stack[0] = T::Int;
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
            stack[0] = T::Timestamp;
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
            stack.drop_top(2);
            stack.push(T::Int);
            I::Mul(overloads::Mul::IntNat)
        }
        (App(MUL, [], _), [.., T::Nat, T::Mutez]) => {
            stack.drop_top(2);
            stack.push(T::Mutez);
            I::Mul(overloads::Mul::MutezNat)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Bls12381G1]) => {
            stack.drop_top(2);
            stack.push(T::Bls12381G1);
            I::Mul(overloads::Mul::Bls12381G1Bls12381Fr)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Bls12381Fr, T::Bls12381G2]) => {
            stack.drop_top(2);
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
            stack.drop_top(2);
            stack.push(T::Bls12381Fr);
            I::Mul(overloads::Mul::Bls12381FrNat)
        }
        #[cfg(feature = "bls")]
        (App(MUL, [], _), [.., T::Int, T::Bls12381Fr]) => {
            stack.drop_top(2);
            stack.push(T::Bls12381Fr);
            I::Mul(overloads::Mul::Bls12381FrInt)
        }
        (App(MUL, [], _), [.., _, _]) => no_overload!(MUL),
        (App(MUL, [], _), [_] | []) => no_overload!(MUL, len 2),
        (App(MUL, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EDIV, [], _), [.., T::Nat, T::Nat]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Nat, T::Nat));
            I::EDiv(overloads::EDiv::NatNat)
        }
        (App(EDIV, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::NatInt)
        }
        (App(EDIV, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::IntNat)
        }
        (App(EDIV, [], _), [.., T::Int, T::Int]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Int, T::Nat));
            I::EDiv(overloads::EDiv::IntInt)
        }
        (App(EDIV, [], _), [.., T::Nat, T::Mutez]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Mutez, T::Mutez));
            I::EDiv(overloads::EDiv::MutezNat)
        }
        (App(EDIV, [], _), [.., T::Mutez, T::Mutez]) => {
            pop!();
            stack[0] = T::new_option(T::new_pair(T::Nat, T::Mutez));
            I::EDiv(overloads::EDiv::MutezMutez)
        }
        (App(EDIV, [], _), [.., _, _]) => no_overload!(EDIV),
        (App(EDIV, [], _), [_] | []) => no_overload!(EDIV, len 2),
        (App(EDIV, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NEG, [], _), [.., T::Nat]) => {
            stack[0] = T::Int;
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
            stack[0] = T::Int;
            I::Sub(overloads::Sub::NatNat)
        }
        (App(SUB, [], _), [.., T::Int, T::Nat]) => {
            pop!();
            I::Sub(overloads::Sub::NatInt)
        }
        (App(SUB, [], _), [.., T::Nat, T::Int]) => {
            pop!();
            stack[0] = T::Int;
            I::Sub(overloads::Sub::IntNat)
        }
        (App(SUB, [], _), [.., T::Int, T::Int]) => {
            pop!();
            I::Sub(overloads::Sub::IntInt)
        }
        (App(SUB, [], _), [.., T::Int, T::Timestamp]) => {
            pop!();
            stack[0] = T::Timestamp;
            I::Sub(overloads::Sub::TimestampInt)
        }
        (App(SUB, [], _), [.., T::Timestamp, T::Timestamp]) => {
            pop!();
            stack[0] = T::Int;
            I::Sub(overloads::Sub::TimestampTimestamp)
        }
        (App(SUB, [], _), [.., _, _]) => no_overload!(SUB),
        (App(SUB, [], _), [] | [_]) => no_overload!(SUB, len 2),
        (App(SUB, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SUB_MUTEZ, [], _), [.., T::Mutez, T::Mutez]) => {
            pop!();
            stack[0] = Type::new_option(T::Mutez);
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
            stack[0] = T::Bytes;
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
            stack[0] = T::Bytes;
            I::Lsr(overloads::Lsr::Bytes)
        }
        (App(LSR, [], _), [.., _, _]) => no_overload!(LSR),
        (App(LSR, [], _), [] | [_]) => no_overload!(LSR, len 2),
        (App(LSR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NOT, [], _), [.., T::Bool]) => I::Not(overloads::Not::Bool),
        (App(NOT, [], _), [.., T::Int]) => I::Not(overloads::Not::Int),
        (App(NOT, [], _), [.., T::Nat]) => {
            stack[0] = T::Int;
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
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, gas, self_entrypoints, opt_stack)?;
            opt_stack
                .access_mut(TcError::FailNotInTail)?
                .append(&mut protected);
            I::Dip(opt_height, nested)
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
            stack.drop_top(drop_height);
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
            let ty = &stack[dup_height - 1];
            ty.ensure_prop(gas, TypeProperty::Duplicable)?;
            stack.push(ty.clone());
            I::Dup(opt_height)
        }

        (App(DIG, [Int(height)], _), ..) => {
            let dig_height = validate_u10(height)?;
            ensure_stack_len(Prim::DIG, stack, dig_height as usize)?;
            gas.consume(gas::tc_cost::dig_n(dig_height as usize)?)?;
            if dig_height > 0 {
                let e = stack.remove(dig_height as usize);
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
                stack.insert(dug_height as usize, e);
            }
            I::Dug(dug_height)
        }
        (App(DUG, [_], _), ..) => unexpected_micheline!(),
        (App(DUG, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(GT, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Gt
        }
        (App(GT, [], _), [.., t]) => no_overload!(GT, TypesNotEqual(T::Int, (*t).clone())),
        (App(GT, [], _), []) => no_overload!(GT, len 1),
        (App(GT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(GE, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Ge
        }
        (App(GE, [], _), [.., t]) => no_overload!(GE, TypesNotEqual(T::Int, (*t).clone())),
        (App(GE, [], _), []) => no_overload!(GE, len 1),
        (App(GE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EQ, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Eq
        }
        (App(EQ, [], _), [.., t]) => no_overload!(EQ, TypesNotEqual(T::Int, (*t).clone())),
        (App(EQ, [], _), []) => no_overload!(EQ, len 1),
        (App(EQ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NEQ, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Neq
        }
        (App(NEQ, [], _), [.., t]) => no_overload!(NEQ, TypesNotEqual(T::Int, (*t).clone())),
        (App(NEQ, [], _), []) => no_overload!(NEQ, len 1),
        (App(NEQ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LE, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Le
        }
        (App(LE, [], _), [.., t]) => no_overload!(LE, TypesNotEqual(T::Int, (*t).clone())),
        (App(LE, [], _), []) => no_overload!(LE, len 1),
        (App(LE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LT, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Lt
        }
        (App(LT, [], _), [.., t]) => no_overload!(LT, TypesNotEqual(T::Int, (*t).clone())),
        (App(LT, [], _), []) => no_overload!(LT, len 1),
        (App(LT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IF, [Seq(nested_t), Seq(nested_f)], _), [.., T::Bool]) => {
            // pop the bool off the stack
            pop!();
            // Clone the stack so that we have a copy to run one branch on.
            // We can run the other branch on the live stack.
            let mut f_opt_stack = opt_stack.clone();
            let nested_t = typecheck(nested_t, gas, self_entrypoints, opt_stack)?;
            let nested_f = typecheck(nested_f, gas, self_entrypoints, &mut f_opt_stack)?;
            // If stacks unify after typecheck, all is good.
            unify_stacks(gas, opt_stack, f_opt_stack)?;
            I::If(nested_t, nested_f)
        }
        (App(IF, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF, TypesNotEqual(T::Bool, (*t).clone()))
        }
        (App(IF, [Seq(_), Seq(_)], _), []) => no_overload!(IF, len 1),
        (App(IF, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_NONE, [Seq(when_none), Seq(when_some)], _), [.., T::Option(..)]) => {
            // Extract option type
            let ty = pop!(T::Option);
            // Clone the some_stack as we need to push a type on top of it
            let mut some_stack: TypeStack = stack.clone();
            some_stack.push(ty.as_ref().clone());
            let mut some_opt_stack = FailingTypeStack::Ok(some_stack);
            let when_none = typecheck(when_none, gas, self_entrypoints, opt_stack)?;
            let when_some = typecheck(when_some, gas, self_entrypoints, &mut some_opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(gas, opt_stack, some_opt_stack)?;
            I::IfNone(when_none, when_some)
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_NONE, NMOR::ExpectedOption((*t).clone()))
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), []) => no_overload!(IF_NONE, len 1),
        (App(IF_NONE, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_CONS, [Seq(when_cons), Seq(when_nil)], _), [.., T::List(..)]) => {
            // Clone the cons_stack as we need to push a type on top of it
            let mut cons_stack: TypeStack = stack.clone();
            // get the list element type
            let ty = pop!(T::List);
            // push it to the cons stack
            cons_stack.push(ty.as_ref().clone());
            let mut cons_opt_stack = FailingTypeStack::Ok(cons_stack);
            let when_cons = typecheck(when_cons, gas, self_entrypoints, &mut cons_opt_stack)?;
            let when_nil = typecheck(when_nil, gas, self_entrypoints, opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(gas, opt_stack, cons_opt_stack)?;
            I::IfCons(when_cons, when_nil)
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_CONS, NMOR::ExpectedList((*t).clone()))
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), []) => no_overload!(IF_CONS, len 1),
        (App(IF_CONS, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_LEFT, [Seq(when_left), Seq(when_right)], _), [.., T::Or(..)]) => {
            // get the list element type
            let (tl, tr) = pop!(T::Or).as_ref().clone();
            // use main stack as left branch, cloned stack as right
            let mut right_stack = stack.clone();
            stack.push(tl);
            right_stack.push(tr);
            let mut opt_right_stack = FailingTypeStack::Ok(right_stack);
            let when_left = typecheck(when_left, gas, self_entrypoints, opt_stack)?;
            let when_right = typecheck(when_right, gas, self_entrypoints, &mut opt_right_stack)?;
            // If stacks unify, all is good
            unify_stacks(gas, opt_stack, opt_right_stack)?;
            I::IfLeft(when_left, when_right)
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_LEFT, NMOR::ExpectedOr((*t).clone()))
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), []) => no_overload!(IF_LEFT, len 1),
        (App(IF_LEFT, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(INT, [], _), [.., T::Nat]) => {
            stack[0] = Type::Int;
            I::Int(overloads::Int::Nat)
        }
        #[cfg(feature = "bls")]
        (App(INT, [], _), [.., T::Bls12381Fr]) => {
            stack[0] = Type::Int;
            I::Int(overloads::Int::Bls12381Fr)
        }
        (App(INT, [], _), [.., T::Bytes]) => {
            stack[0] = Type::Int;
            I::Int(overloads::Int::Bytes)
        }
        (App(INT, [], _), [.., _]) => no_overload!(INT),
        (App(INT, [], _), []) => no_overload!(INT, len 1),
        (App(INT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NAT, [], _), [.., T::Bytes]) => {
            stack[0] = Type::Nat;
            I::Nat
        }
        (App(NAT, [], _), [.., _]) => no_overload!(NAT),
        (App(NAT, [], _), []) => no_overload!(NAT, len 1),
        (App(NAT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(BYTES, [], _), [.., T::Int]) => {
            stack[0] = Type::Bytes;
            I::Bytes(overloads::Bytes::Int)
        }
        (App(BYTES, [], _), [.., T::Nat]) => {
            stack[0] = Type::Bytes;
            I::Bytes(overloads::Bytes::Nat)
        }
        (App(BYTES, [], _), [.., _]) => no_overload!(BYTES),
        (App(BYTES, [], _), []) => no_overload!(BYTES, len 1),
        (App(BYTES, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(ABS, [], _), [.., T::Int]) => {
            stack[0] = Type::Nat;
            I::Abs
        }
        (App(ABS, [], _), [.., t]) => no_overload!(ABS, TypesNotEqual(T::Int, (*t).clone())),
        (App(ABS, [], _), []) => no_overload!(ABS, len 1),
        (App(ABS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(ISNAT, [], _), [.., T::Int]) => {
            stack[0] = Type::new_option(Type::Nat);
            I::IsNat
        }
        (App(ISNAT, [], _), [.., t]) => no_overload!(ISNAT, TypesNotEqual(T::Int, (*t).clone())),
        (App(ISNAT, [], _), []) => no_overload!(ISNAT, len 1),
        (App(ISNAT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LOOP, [Seq(nested)], _), [.., T::Bool]) => {
            // copy stack for unifying with it later
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            // Pop the bool off the top
            pop!();
            // Typecheck body with the current stack
            let nested = typecheck(nested, gas, self_entrypoints, opt_stack)?;
            // If the starting stack and result stack unify, all is good.
            unify_stacks(gas, opt_stack, opt_copy)?;
            // pop the remaining bool off (if not failed)
            opt_stack.access_mut(()).ok().map(Stack::pop);
            I::Loop(nested)
        }
        (App(LOOP, [Seq(_)], _), [.., ty]) => {
            no_overload!(LOOP, TypesNotEqual(T::Bool, (*ty).clone()))
        }
        (App(LOOP, [Seq(_)], _), []) => no_overload!(LOOP, len 1),
        (App(LOOP, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(LOOP_LEFT, [Seq(nested)], _), [.., T::Or(_)]) => {
            // copy current stack to unify with later
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            let (l_ty, r_ty) = pop!(T::Or).as_ref().clone();
            // loop body consumes left leaf and returns `or` again
            stack.push(l_ty);
            let nested = typecheck(nested, gas, self_entrypoints, opt_stack)?;
            unify_stacks(gas, opt_stack, opt_copy)?;
            // the loop leaves the right leaf of `or` on the stack at the end
            // this FailNotInTail should be impossible to get
            opt_stack.access_mut(TcError::FailNotInTail)?[0] = r_ty;
            I::LoopLeft(nested)
        }
        (App(LOOP_LEFT, [Seq(_)], _), [.., ty]) => {
            no_overload!(LOOP_LEFT, NMOR::ExpectedOr((*ty).clone()))
        }
        (App(LOOP_LEFT, [Seq(_)], _), []) => no_overload!(LOOP_LEFT, len 1),
        (App(LOOP_LEFT, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(ITER, [Seq(nested)], ..), [.., T::List(..)]) => {
            // get the list element type
            let ty = pop!(T::List);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(ty.as_ref().clone());
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, gas, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans list) and result stack unify, all is good.
            unify_stacks(gas, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::List, nested)
        }
        (App(ITER, [Seq(nested)], _), [.., T::Set(..)]) => {
            // get the set element type
            let ty = pop!(T::Set);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(ty.as_ref().clone());
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, gas, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans set) and result stack unify, all is good.
            unify_stacks(gas, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::Set, nested)
        }
        (App(ITER, [Seq(nested)], _), [.., T::Map(..)]) => {
            // get the map element type
            let kty_vty_box = pop!(T::Map);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(T::Pair(kty_vty_box));
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, gas, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans map) and result stack unify, all is good.
            unify_stacks(gas, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::Map, nested)
        }
        (App(ITER, [Seq(_)], _), [.., _]) => no_overload!(ITER),
        (App(ITER, [Seq(_)], _), []) => no_overload!(ITER, len 1),
        (App(ITER, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(MAP, [Seq(nested_instrs)], ..), [.., T::List(..)]) => {
            // Get the element type
            let ty1 = pop!(T::List).as_ref().clone();

            // Typecheck the nested instructions.
            let (nested_instrs, ty2) =
                typecheck_map_block(nested_instrs, ty1, gas, self_entrypoints, stack)?;

            stack.push(Type::new_list(ty2));
            I::Map(overloads::Map::List, nested_instrs)
        }
        (App(MAP, [Seq(nested_instrs)], ..), [.., T::Option(..)]) => {
            // Get the element type
            let ty1 = pop!(T::Option).as_ref().clone();

            // Typecheck the nested instructions.
            let (nested_instrs, ty2) =
                typecheck_map_block(nested_instrs, ty1, gas, self_entrypoints, stack)?;

            stack.push(Type::new_option(ty2));
            I::Map(overloads::Map::Option, nested_instrs)
        }
        (App(MAP, [Seq(nested_instrs)], ..), [.., T::Map(..)]) => {
            // Get the element type
            let (kty, ty1) = pop!(T::Map).as_ref().clone();

            // Typecheck the nested instructions.
            let (nested_instrs, ty2) = typecheck_map_block(
                nested_instrs,
                T::new_pair(kty.clone(), ty1),
                gas,
                self_entrypoints,
                stack,
            )?;

            stack.push(Type::new_map(kty, ty2));
            I::Map(overloads::Map::Map, nested_instrs)
        }
        (App(MAP, [Seq(_)], _), [.., _]) => no_overload!(MAP),
        (App(MAP, [Seq(_)], _), []) => no_overload!(MAP, len 1),
        (App(MAP, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(PUSH, [t, v], _), ..) => {
            let t = parse_ty(gas, t)?;
            t.ensure_prop(gas, TypeProperty::Pushable)?;
            // contracts and big maps are not pushable so it's OK to typecheck values using default
            let mut ctx = crate::context::PushableTypecheckingContext { gas };
            let v = typecheck_value(v, &mut ctx, &t)?;
            stack.push(t);
            I::Push(v)
        }
        (App(PUSH, expect_args!(2), _), _) => unexpected_micheline!(),

        (App(SWAP, [], _), [.., _, _]) => {
            stack.swap(0, 1);
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
            // unwrap is fine, n is non-zero.
            let res = stack
                .drain_top(n as usize)
                .rev()
                .reduce(|acc, e| Type::new_pair(e, acc))
                .unwrap();
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
            fn fill(n: u16, stack: &mut Stack<Type>, p: &Type) -> Result<(), TcError> {
                if n == 0 {
                    stack.push(p.clone());
                } else if let Type::Pair(p) = p {
                    fill(n - 1, stack, &p.1)?;
                    stack.push(p.0.clone());
                } else {
                    return Err(TcError::NoMatchingOverload {
                        instr: UNPAIR,
                        stack: stack.clone(),
                        reason: Option::Some(NMOR::ExpectedPair(p.clone())),
                    });
                }
                Ok(())
            }
            stack.reserve(n as usize);
            let p = pop!();
            fill(n - 1, stack, &p)?;
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
            stack[0] = T::Int;
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
            stack[0] = ty.clone(); // cheap clone, `ty` is either `String` or `Bytes`
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
            let ty = &mut stack[0];
            let n = validate_u10(n)?;
            gas.consume(tc_cost::get_n(n as usize)?)?;
            let res = match get_nth_field_ref(n, ty) {
                Ok(res) => res,
                Err(ty) => no_overload!(GET, NMOR::ExpectedPair(ty)),
            };
            // this is a bit hacky, but borrow rules leave few other options
            stack[0] = std::mem::replace(res, T::Unit);
            I::GetN(n)
        }
        (App(GET, [Micheline::Int(_)], _), []) => no_overload!(GET, len 1),

        (App(GET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UPDATE, [], _), [.., T::Set(ty), T::Bool, ty_]) => {
            ensure_ty_eq(gas, ty, ty_)?;
            stack.drop_top(2);
            I::Update(overloads::Update::Set)
        }
        (App(UPDATE, [], _), [.., T::Map(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            stack.drop_top(2);
            I::Update(overloads::Update::Map)
        }
        (App(UPDATE, [], _), [.., T::BigMap(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(gas, kty, kty_)?;
            ensure_ty_eq(gas, vty, vty_new)?;
            stack.drop_top(2);
            I::Update(overloads::Update::BigMap)
        }
        (App(UPDATE, [], _), [.., _, _, _]) => no_overload!(UPDATE),
        (App(UPDATE, [], _), [] | [_] | [_, _]) => no_overload!(UPDATE, len 3),

        (App(UPDATE, [Micheline::Int(n)], _), [.., _, _]) => {
            let n = validate_u10(n)?;
            let new_val = pop!();
            let old_val = match get_nth_field_ref(n, &mut stack[0]) {
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
            stack[0] = T::Nat;
            I::Size(overloads::Size::String)
        }
        (App(SIZE, [], _), [.., T::Bytes]) => {
            stack[0] = T::Nat;
            I::Size(overloads::Size::Bytes)
        }
        (App(SIZE, [], _), [.., T::List(_)]) => {
            stack[0] = T::Nat;
            I::Size(overloads::Size::List)
        }
        (App(SIZE, [], _), [.., T::Set(_)]) => {
            stack[0] = T::Nat;
            I::Size(overloads::Size::Set)
        }
        (App(SIZE, [], _), [.., T::Map(..)]) => {
            stack[0] = T::Nat;
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
            stack[0] = T::new_option(ty.clone());
            I::Unpack(ty)
        }
        (App(UNPACK, [_], _), [.., ty]) => {
            no_overload!(UNPACK, TypesNotEqual(T::Bytes, (*ty).clone()))
        }
        (App(UNPACK, [_], _), []) => no_overload!(UNPACK, len 1),
        (App(UNPACK, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(TRANSFER_TOKENS, [], _), [.., T::Contract(ct), T::Mutez, arg_t]) => {
            ensure_ty_eq(gas, ct, arg_t)?;
            stack.drop_top(3);
            stack.push(T::Operation);
            I::TransferTokens
        }
        (App(TRANSFER_TOKENS, [], _), [.., _, _, _]) => no_overload!(TRANSFER_TOKENS),
        (App(TRANSFER_TOKENS, [], _), [] | [_] | [_, _]) => no_overload!(TRANSFER_TOKENS, len 3),
        (App(TRANSFER_TOKENS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SET_DELEGATE, [], _), [.., T::Option(ot)]) if matches!(ot.as_ref(), T::KeyHash) => {
            pop!();
            stack.push(T::Operation);
            I::SetDelegate
        }
        (App(SET_DELEGATE, [], _), [.., _]) => no_overload!(SET_DELEGATE),
        (App(SET_DELEGATE, [], _), []) => no_overload!(SET_DELEGATE, len 1),
        (App(SET_DELEGATE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CHECK_SIGNATURE, [], _), [.., T::Bytes, T::Signature, T::Key]) => {
            stack.drop_top(2);
            stack[0] = T::Bool;
            I::CheckSignature
        }
        (App(CHECK_SIGNATURE, [], _), [.., _, _, _]) => no_overload!(CHECK_SIGNATURE),
        (App(CHECK_SIGNATURE, [], _), [] | [_] | [_, _]) => no_overload!(CHECK_SIGNATURE, len 3),
        (App(CHECK_SIGNATURE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SLICE, [], _), [.., T::String, T::Nat, T::Nat]) => {
            stack.drop_top(2);
            stack[0] = T::new_option(T::String);
            I::Slice(overloads::Slice::String)
        }
        (App(SLICE, [], _), [.., T::Bytes, T::Nat, T::Nat]) => {
            stack.drop_top(2);
            stack[0] = T::new_option(T::Bytes);
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
            stack.push(Type::new_lambda(in_ty.clone(), out_ty.clone()));
            let res = typecheck_lambda(instrs, gas, in_ty, out_ty, matches!(prim, LAMBDA_REC))?;
            I::Lambda(res)
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
            stack[0] = T::KeyHash;
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
            stack.push(T::new_lambda(pair_ty.1.clone(), lam_ty.1.clone()));
            I::Apply { arg_ty: ty }
        }
        (App(APPLY, [], _), [.., _, _]) => no_overload!(APPLY),
        (App(APPLY, [], _), [] | [_]) => no_overload!(APPLY, len 2),
        (App(APPLY, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(TICKET, [], _), [.., T::Nat, _]) => {
            let content_ty = pop!();
            stack[0] = T::new_option(T::new_ticket(content_ty.clone()));
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
            stack[0] = Type::new_option(Type::new_pair(typ.clone(), typ));
            I::SplitTicket
        }
        (App(SPLIT_TICKET, [], _), [.., _, _]) => no_overload!(SPLIT_TICKET),
        (App(SPLIT_TICKET, [], _), [] | [_]) => no_overload!(SPLIT_TICKET, len 2),
        (App(SPLIT_TICKET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(JOIN_TICKETS, [], _), [.., T::Pair(tickets)])
            if matches!(tickets.as_ref(), (Type::Ticket(_), Type::Ticket(_))) =>
        {
            let lt = irrefutable_match!(&tickets.0; Type::Ticket);
            let rt = irrefutable_match!(&tickets.1; Type::Ticket);

            ensure_ty_eq(gas, lt, rt)?;
            stack[0] = Type::new_option(tickets.0.clone());
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
            stack[0] = T::new_contract(T::Unit);
            I::ImplicitAccount
        }
        (App(IMPLICIT_ACCOUNT, [], _), [.., _]) => no_overload!(IMPLICIT_ACCOUNT),
        (App(IMPLICIT_ACCOUNT, [], _), []) => no_overload!(IMPLICIT_ACCOUNT, len 1),
        (App(IMPLICIT_ACCOUNT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IS_IMPLICIT_ACCOUNT, [], _), [.., T::Address]) => {
            stack[0] = T::new_option(T::KeyHash);
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
            stack[0] = T::Nat;
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
            stack[0] = T::Bool;
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

        (App(CREATE_CONTRACT, [cs], _), [.., new_storage, T::Mutez, T::Option(opt_keyhash)])
            if matches!(opt_keyhash.as_ref(), Type::KeyHash) =>
        {
            #[cfg(feature = "allow_lazy_storage_transfer")]
            let allow_lazy_storage_in_storage = true;
            #[cfg(not(feature = "allow_lazy_storage_transfer"))]
            let allow_lazy_storage_in_storage = false;
            let contract_script =
                cs.split_script()?
                    .typecheck_script(gas, allow_lazy_storage_in_storage, false)?;
            ensure_ty_eq(gas, &contract_script.storage, new_storage)?;
            stack.drop_top(3);
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
            let _arg_type = pop!();
            pop!();
            let return_type = parse_ty(gas, return_ty)?;
            return_type.ensure_prop(gas, TypeProperty::ViewOutput)?;
            stack.push(Type::new_option(return_type.clone()));
            I::IView { name, return_type }
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

        (Seq(nested), _) => I::Seq(typecheck(nested, gas, self_entrypoints, opt_stack)?),
    })
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
            (1, Type::Pair(p)) => break &mut Rc::make_mut(p).0,
            (_, Type::Pair(p)) => {
                ty = &mut Rc::make_mut(p).1;
                m -= 2;
            }
            (_, ty) => {
                let ty = ty.clone();
                return Err(ty);
            }
        }
    })
}

/// Typecheck a value. Assumes passed the type is valid, i.e. doesn't contain
/// illegal types like `set operation` or `contract operation`.
pub fn typecheck_value<'a>(
    v: &Micheline<'a>,
    ctx: &mut impl TypecheckingCtx<'a>,
    t: &Type,
) -> Result<TypedValue<'a>, TcError> {
    use Micheline as V;
    use Type as T;
    use TypedValue as TV;
    ctx.gas().consume(gas::tc_cost::VALUE_STEP)?;
    macro_rules! invalid_value_for_type {
        () => {
            TcError::InvalidValueForType(format!("{v:?}"), t.clone())
        };
    }
    Ok(match (t, v) {
        (T::Nat, V::Int(n)) => TV::Nat(BigUint::try_from(n)?),
        (T::Int, V::Int(n)) => TV::Int(n.clone()),
        (T::Bool, V::App(Prim::True, [], _)) => TV::Bool(true),
        (T::Bool, V::App(Prim::False, [], _)) => TV::Bool(false),
        (T::Mutez, V::Int(n)) if !n.is_negative() => TV::Mutez(i64::try_from(n)?),
        (T::String, V::String(s)) => TV::String(s.clone()),
        (T::Unit, V::App(Prim::Unit, [], _)) => TV::Unit,
        (T::Pair(pt), V::App(Prim::Pair, [vl, rest @ ..], _) | V::Seq([vl, rest @ ..]))
            if !rest.is_empty() =>
        {
            let (tl, tr) = pt.as_ref();
            let l = typecheck_value(vl, ctx, tl)?;
            let r = match rest {
                [vr] => typecheck_value(vr, ctx, tr)?,
                vrs => typecheck_value(&V::App(Prim::Pair, vrs, NO_ANNS), ctx, tr)?,
            };
            TV::new_pair(l, r)
        }
        (T::Or(ot), V::App(prim @ (Prim::Left | Prim::Right), [val], _)) => {
            let (tl, tr) = ot.as_ref();
            let typed_val = match prim {
                Prim::Left => crate::ast::Or::Left(typecheck_value(val, ctx, tl)?),
                Prim::Right => crate::ast::Or::Right(typecheck_value(val, ctx, tr)?),
                _ => unreachable!(),
            };
            TV::new_or(typed_val)
        }
        (T::Option(ty), V::App(Prim::Some, [v], _)) => {
            let v = typecheck_value(v, ctx, ty)?;
            TV::new_option(Some(v))
        }
        (T::Option(_), V::App(Prim::None, [], _)) => TV::new_option(None),
        (T::List(ty), V::Seq(vs)) => TV::List(MichelsonList::from(
            vs.iter()
                .map(|v| typecheck_value(v, ctx, ty).map(Rc::new))
                .collect::<Result<Vec<_>, TcError>>()?,
        )),
        (T::Set(ty), V::Seq(vs)) => {
            let set = typecheck_set(ctx, t, ty, vs)?;
            TV::Set(set.into_iter().map(Rc::new).collect())
        }
        (T::Map(m), V::Seq(vs)) => {
            let (tk, tv) = m.as_ref();
            let map = typecheck_map(ctx, t, tk, tv, vs)?;
            TV::Map(
                map.into_iter()
                    .map(|(k, v)| (Rc::new(k), Rc::new(v)))
                    .collect(),
            )
        }
        // All valid instantiations of big map are mentioned in
        // https://tezos.gitlab.io/michelson-reference/#type-big_map
        (T::BigMap(m), V::Seq(vs)) => {
            // In-memory big maps have the same syntax as regular maps
            let (tk, tv) = m.as_ref();
            let map = typecheck_map(ctx, t, tk, tv, vs)?;
            TV::BigMap(BigMap::new(tk.clone(), tv.clone(), map))
        }
        (T::BigMap(m), v) => {
            let (id, vs_opt, diff) = match v {
                V::Int(i) => (i, None, false),
                V::App(Prim::Pair, [V::Int(i), V::Seq(vs)], _) => (i, Some(vs), true),
                _ => return Err(invalid_value_for_type!()),
            };

            let (tk, tv) = m.as_ref();

            let big_map_id = id.clone().into();
            let (key_type, value_type) = ctx
                .big_map_get_type(&big_map_id)
                .map_err(|e| TcError::LazyStorageError(e.to_string()))?
                .ok_or(TcError::BigMapNotFound(id.clone()))?;

            ensure_ty_eq(ctx.gas(), &key_type, tk)?;
            ensure_ty_eq(ctx.gas(), &value_type, tv)?;

            let overlay = if let Some(vs) = vs_opt {
                typecheck_big_map(ctx, t, tk, tv, vs, diff)?
            } else {
                BTreeMap::default()
            };
            let content = big_map::BigMapContent::FromId(big_map::BigMapFromId {
                id: big_map_id,
                overlay,
            });
            TV::BigMap(BigMap {
                content,
                key_type: tk.clone(),
                value_type: tv.clone(),
            })
        }
        (T::Address, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_READABLE)?;
            TV::Address(
                Address::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::Address, e))?,
            )
        }
        (T::Address, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            TV::Address(Address::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::Address, e))?)
        }
        (T::Contract(ty), addr) => {
            let t_addr = irrefutable_match!(typecheck_value(addr, ctx, &T::Address)?; TV::Address);
            typecheck_contract_address(ctx, t_addr, Entrypoint::default(), ty)
                .map(TypedValue::Contract)?
        }
        (T::ChainId, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::CHAIN_ID_READABLE)?;
            TV::ChainId(
                ChainId::from_base58_check(str).map_err(|x| TcError::ChainIdError(x.into()))?,
            )
        }
        (T::ChainId, V::Bytes(bs)) => {
            use tezos_crypto_rs::hash::HashTrait;
            ctx.gas().consume(gas::tc_cost::CHAIN_ID_OPTIMIZED)?;
            TV::ChainId(ChainId::try_from_bytes(bs).map_err(|x| TcError::ChainIdError(x.into()))?)
        }
        (T::Bytes, V::Bytes(bs)) => TV::Bytes(bs.clone()),
        (T::Key, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_READABLE)?;
            TV::Key(
                PublicKey::from_b58check(str)
                    .map_err(|e| TcError::ByteReprError(T::Key, e.into()))?,
            )
        }
        (T::Key, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_OPTIMIZED)?;
            TV::Key(
                PublicKey::nom_read_exact(bs)
                    .map_err(|e| TcError::ByteReprError(T::Key, e.into()))?,
            )
        }
        (T::Signature, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_READABLE)?;
            TV::Signature(
                Signature::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::Signature, e.into()))?,
            )
        }
        (T::Signature, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_OPTIMIZED)?;
            TV::Signature(
                Signature::try_from(bs.clone())
                    .map_err(|e| TcError::ByteReprError(T::Signature, e.into()))?,
            )
        }
        (T::KeyHash, V::String(str)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_READABLE)?;
            TV::KeyHash(
                PublicKeyHash::from_b58check(str)
                    .map_err(|e| TcError::ByteReprError(T::KeyHash, e.into()))?,
            )
        }
        (T::KeyHash, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            TV::KeyHash(PublicKeyHash::nom_read_exact(bs).map_err(|err| {
                TcError::ByteReprError(
                    T::KeyHash,
                    ByteReprError::WrongFormat(format!(
                        "public key hash, optimized {}",
                        convert_error(bs, err)
                    )),
                )
            })?)
        }
        (T::Timestamp, V::Int(n)) => TV::Timestamp(n.clone()),
        (T::Timestamp, V::String(n)) => {
            ctx.gas()
                .consume(gas::tc_cost::timestamp_decoding(n.len())?)?;
            // First, try to parse the string as an integer
            if let Ok(int_value) = n.parse::<i64>() {
                TV::Timestamp(int_value.into())
            } else {
                // If integer parsing fails, try to parse as RFC3339 datetime
                let dt = DateTime::parse_from_rfc3339(n);
                match dt {
                    Ok(dt) => TV::Timestamp(dt.timestamp().into()),
                    Err(_) => return Err(invalid_value_for_type!()),
                }
            }
        }
        (
            T::Lambda(tys),
            raw @ (V::Seq(instrs) | V::App(Prim::Lambda_rec, [V::Seq(instrs)], _)),
        ) => {
            let (in_ty, out_ty) = tys.as_ref();
            TV::Lambda(Closure::Lambda(typecheck_lambda(
                instrs,
                ctx.gas(),
                in_ty.clone(),
                out_ty.clone(),
                matches!(raw, V::App(Prim::Lambda_rec, ..)),
            )?))
        }
        (
            T::Ticket(content_type),
            V::App(Prim::Ticket, [ticketer, content_type_bis, content, amount], _),
        ) => {
            let content_type_bis = parse_ty(ctx.gas(), content_type_bis)?;
            ensure_ty_eq(ctx.gas(), content_type, &content_type_bis)?;
            let ticketer = typecheck_value(ticketer, ctx, &T::Address)?;
            let ticketer = irrefutable_match!(ticketer; TV::Address);
            let content = typecheck_value(content, ctx, content_type)?;
            let amount = typecheck_value(amount, ctx, &T::Nat)?;
            let amount = irrefutable_match!(amount; TV::Nat);
            TV::new_ticket(Ticket {
                ticketer: ticketer.hash,
                content_type: content_type.as_ref().clone(),
                content,
                amount,
            })
        }
        (T::Ticket(content_type), m) => {
            let content_type = content_type.as_ref();
            match typecheck_value(
                m,
                ctx,
                &Type::new_pair(
                    Type::Address,
                    Type::new_pair(content_type.clone(), Type::Nat),
                ),
            ) {
                Ok(TV::Pair(left, right)) => {
                    let address = irrefutable_match!(TypedValue::unwrap_rc(left); TV::Address);
                    irrefutable_match!(TypedValue::unwrap_rc(right); TV::Pair, content, amount);
                    TV::new_ticket(Ticket {
                        ticketer: address.hash,
                        content_type: content_type.clone(),
                        content: TypedValue::unwrap_rc(content),
                        amount: irrefutable_match!(TypedValue::unwrap_rc(amount); TV::Nat),
                    })
                }
                _ => return Err(invalid_value_for_type!()),
            }
        }
        #[cfg(feature = "bls")]
        (T::Bls12381Fr, V::Int(i)) => {
            ctx.gas().consume(gas::tc_cost::BLS_FR)?;
            TV::Bls12381Fr(bls::Fr::from_big_int(i))
        }
        #[cfg(feature = "bls")]
        (T::Bls12381Fr, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_FR)?;
            TV::Bls12381Fr(bls::Fr::from_bytes(bs).ok_or_else(|| invalid_value_for_type!())?)
        }
        #[cfg(feature = "bls")]
        (T::Bls12381G1, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_G1)?;
            TV::new_bls12381_g1(bls::G1::from_bytes(bs).ok_or_else(|| invalid_value_for_type!())?)
        }
        #[cfg(feature = "bls")]
        (T::Bls12381G2, V::Bytes(bs)) => {
            ctx.gas().consume(gas::tc_cost::BLS_G2)?;
            TV::new_bls12381_g2(bls::G2::from_bytes(bs).ok_or_else(|| invalid_value_for_type!())?)
        }
        (_, _) => return Err(invalid_value_for_type!()),
    })
}

#[allow(missing_docs)]
pub fn typecheck_lambda<'a>(
    instrs: &'a [Micheline<'a>],
    gas: &mut Gas,
    in_ty: Type,
    out_ty: Type,
    recursive: bool,
) -> Result<Lambda<'a>, TcError> {
    let stk = &mut if recursive {
        let self_ty = Type::new_lambda(in_ty.clone(), out_ty.clone());
        tc_stk![self_ty, in_ty.clone()]
    } else {
        tc_stk![in_ty.clone()]
    };
    let code = Rc::from(typecheck(instrs, gas, None, stk)?);
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

/// Typechecks the instruction block for a `MAP` instruction.
///
/// Given a `ty1` and a stack of type `A`,
/// the instruction block is expected to have the type `ty1 : A => ty2 : A` for some type `ty2`.
///
/// Returns the typechecked instructions and the inferred type `ty2`
fn typecheck_map_block<'a>(
    nested: &[Micheline<'a>],
    ty1: Type,
    gas: &mut Gas,
    self_entrypoints: Option<&Entrypoints>,
    stack: &TypeStack,
) -> Result<(Vec<Instruction<'a>>, Type), TcError> {
    let mut nested_stack = stack.clone();
    nested_stack.push(ty1);
    let mut opt_nested_stack = FailingTypeStack::Ok(nested_stack);

    // Types:
    //   stack :: A
    //   opt_nested_stack :: ty1 : A

    // Typecheck the nested instructions.
    let nested: Vec<Instruction<'a>> =
        typecheck(nested, gas, self_entrypoints, &mut opt_nested_stack)?;

    // Assert that the `opt_nested_stack` now has the type `ty2 : A`, for some `ty2`.
    // NB: the nested instruction block cannot fail, otherwise we cannot infer `ty2`.
    let nested_stack = opt_nested_stack.access_mut(TcError::MapBlockFail)?;
    let ty2 = nested_stack.pop().ok_or(TcError::MapBlockEmptyStack)?;
    ensure_stacks_eq(gas, stack, nested_stack)?;
    Ok((nested, ty2))
}

fn validate_u10(n: &BigInt) -> Result<u16, TcError> {
    let res = u16::try_from(n).map_err(|_| TcError::ExpectedU10(n.clone()))?;
    if res >= 1024 {
        return Err(TcError::ExpectedU10(n.clone()));
    }
    Ok(res)
}

fn typecheck_set<'a>(
    ctx: &mut impl TypecheckingCtx<'a>,
    set_ty: &Type,
    elem_ty: &Type,
    vs: &[Micheline<'a>],
) -> Result<BTreeSet<TypedValue<'a>>, TcError> {
    ctx.gas().consume(gas::tc_cost::construct_set(
        elem_ty.size_for_gas(),
        vs.len(),
    )?)?;

    let mut prev_elt = None;
    let mut set = BTreeSet::new();

    for elt in vs.iter() {
        let elt = typecheck_value(elt, ctx, elem_ty)?;
        if let Some(prev_elt) = prev_elt {
            ctx.gas()
                .consume(gas::interpret_cost::compare(&prev_elt, &elt)?)?;
            match prev_elt.cmp(&elt) {
                std::cmp::Ordering::Less => (),
                std::cmp::Ordering::Equal => {
                    return Err(TcError::DuplicateElements(set_ty.clone()))
                }
                std::cmp::Ordering::Greater => {
                    return Err(TcError::ElementsNotSorted(set_ty.clone()))
                }
            };
        };
        prev_elt = Some(elt.clone());
        set.insert(elt);
    }
    Ok(set)
}

fn typecheck_map<'a>(
    ctx: &mut impl TypecheckingCtx<'a>,
    map_ty: &Type,
    key_type: &Type,
    value_type: &Type,
    vs: &[Micheline<'a>],
) -> Result<BTreeMap<TypedValue<'a>, TypedValue<'a>>, TcError> {
    ctx.gas().consume(gas::tc_cost::construct_map(
        key_type.size_for_gas(),
        vs.len(),
    )?)?;

    let mut prev_key = None;
    let mut map = BTreeMap::new();

    for mich in vs.iter() {
        let (key, val) = match mich {
            Micheline::App(Prim::Elt, [k_expr, v_expr], _) => {
                let k = typecheck_value(k_expr, ctx, key_type)?;
                let v = typecheck_value(v_expr, ctx, value_type)?;
                (k, v)
            }
            _ => {
                return Err(TcError::InvalidEltForMap(
                    format!("{mich:?}"),
                    map_ty.clone(),
                ))
            }
        };
        if let Some(prev_key) = prev_key {
            ctx.gas()
                .consume(gas::interpret_cost::compare(&prev_key, &key)?)?;
            match prev_key.cmp(&key) {
                std::cmp::Ordering::Less => (),
                std::cmp::Ordering::Equal => {
                    return Err(TcError::DuplicateElements(map_ty.clone()))
                }
                std::cmp::Ordering::Greater => {
                    return Err(TcError::ElementsNotSorted(map_ty.clone()))
                }
            };
        };
        prev_key = Some(key.clone());
        map.insert(key, val);
    }
    Ok(map)
}

fn typecheck_big_map<'a>(
    ctx: &mut impl TypecheckingCtx<'a>,
    map_ty: &Type,
    key_type: &Type,
    value_type: &Type,
    vs: &[Micheline<'a>],
    diff: bool,
) -> Result<BTreeMap<TypedValue<'a>, Option<TypedValue<'a>>>, TcError> {
    ctx.gas().consume(gas::tc_cost::construct_map(
        key_type.size_for_gas(),
        vs.len(),
    )?)?;

    let mut prev_key = None;
    let mut map = BTreeMap::new();

    for mich in vs.iter() {
        let (key, val) = match mich {
            Micheline::App(Prim::Elt, [k_expr, v_expr], _) => {
                let k = typecheck_value(k_expr, ctx, key_type)?;
                let v = if diff {
                    match v_expr {
                        Micheline::App(Prim::Some, [inner_val], _) => {
                            let v = typecheck_value(inner_val, ctx, value_type)?;
                            Some(v)
                        }
                        Micheline::App(Prim::None, [], _) => None,
                        _ => {
                            return Err(TcError::InvalidEltForMap(
                                format!("{v_expr:?}"),
                                map_ty.clone(),
                            ))
                        }
                    }
                } else {
                    Some(typecheck_value(v_expr, ctx, value_type)?)
                };
                (k, v)
            }
            _ => {
                return Err(TcError::InvalidEltForMap(
                    format!("{mich:?}"),
                    map_ty.clone(),
                ))
            }
        };
        if let Some(prev_key) = prev_key {
            ctx.gas()
                .consume(gas::interpret_cost::compare(&prev_key, &key)?)?;
            match prev_key.cmp(&key) {
                std::cmp::Ordering::Less => (),
                std::cmp::Ordering::Equal => {
                    return Err(TcError::DuplicateElements(map_ty.clone()))
                }
                std::cmp::Ordering::Greater => {
                    return Err(TcError::ElementsNotSorted(map_ty.clone()))
                }
            };
        };
        prev_key = Some(key.clone());
        map.insert(key, val);
    }
    Ok(map)
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

fn ensure_ty_eq(gas: &mut Gas, ty1: &Type, ty2: &Type) -> Result<(), TcError> {
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
    use crate::gas::Gas;
    use crate::parser::test_helpers::*;
    use crate::typechecker::*;
    use std::collections::HashMap;
    use std::rc::Rc;
    use Instruction::*;
    use Option::None;

    /// hack to simplify syntax in tests
    fn typecheck_instruction<'a>(
        i: &Micheline<'a>,
        gas: &mut Gas,
        opt_stack: &mut FailingTypeStack,
    ) -> Result<Instruction<'a>, TcError> {
        super::typecheck_instruction(i, gas, None, opt_stack)
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
        assert!(gas.milligas() < Gas::default().milligas());
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
        assert!(gas.milligas() < Gas::default().milligas());
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert!(gas.milligas() < Gas::default().milligas());
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
        assert!(gas.milligas() < Gas::default().milligas());
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
            assert!(gas.milligas() < Gas::default().milligas());
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
            assert!(gas.milligas() < Gas::default().milligas());
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
            assert!(gas.milligas() < Gas::default().milligas());
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440 - 2 * 50);
    }

    #[test]
    fn test_push() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Int];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("PUSH int 1").unwrap(), &mut gas, &mut stack),
            Ok(Push(TypedValue::int(1)))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_dip() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Bool];
        let mut gas = Gas::default();
        assert_eq!(
            typecheck_instruction(&parse("DIP 1 {PUSH nat 6}").unwrap(), &mut gas, &mut stack),
            Ok(Dip(Some(1), vec![Push(TypedValue::nat(6))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas() < Gas::default().milligas());
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
            assert!(gas.milligas() < Gas::default().milligas());
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(gas.milligas(), Gas::default().milligas() - 440);
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
            Ok(Loop(vec![Push(TypedValue::Bool(true))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas() < Gas::default().milligas());
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
                Push(TypedValue::new_or(Or::Right(TypedValue::nat(123))))
            ]))
        );
        assert_eq!(stack, expected_stack);
        assert!(gas.milligas() < Gas::default().milligas());
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
                    Push(TypedValue::Bool(true)),
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
                    Push(TypedValue::Bool(true)),
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
            Ok(Push(TypedValue::String("foo".to_owned())))
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
            Ok(Push(TypedValue::Bls12381Fr(bls::Fr::from_big_int(
                &100500.into()
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
            Ok(Push(TypedValue::Bls12381Fr(
                bls::Fr::from_bytes(&[1]).unwrap()
            )))
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
            Ok(Push(TypedValue::new_bls12381_g1(
                bls::G1::from_bytes(&hex::decode(hex_val).unwrap()).unwrap()
            )))
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
            Ok(Push(TypedValue::new_bls12381_g2(
                bls::G2::from_bytes(&hex::decode(hex_val).unwrap()).unwrap()
            )))
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
            Ok(Push(TypedValue::Unit))
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
            Ok(Push(TypedValue::new_pair(
                TypedValue::int(-5),
                TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
            )))
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
            Ok(Push(TypedValue::new_or(or::Or::Left(TypedValue::int(1)))))
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
            Ok(Push(TypedValue::new_or(or::Or::Right(TypedValue::Bool(
                false
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
            Ok(Push(TypedValue::new_option(Some(TypedValue::nat(3)))))
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
            Ok(Push(TypedValue::new_option(None)))
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
                Push(TypedValue::new_pair(
                    TypedValue::int(-5),
                    TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
                )),
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
                Push(TypedValue::new_pair(
                    TypedValue::int(-5),
                    TypedValue::new_pair(TypedValue::nat(3), TypedValue::Bool(false))
                )),
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
            Ok(IfNone(vec![Push(TypedValue::int(5))], vec![]))
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
            Ok(Push(TypedValue::List(
                vec![TypedValue::int(1), TypedValue::int(2), TypedValue::int(3),].into()
            )))
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
            Ok(Push(TypedValue::Set(
                [TypedValue::int(1), TypedValue::int(2)]
                    .into_iter()
                    .map(Rc::new)
                    .collect()
            )))
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
            Ok(Push(TypedValue::Map(
                [
                    (TypedValue::int(1), TypedValue::String("foo".to_owned())),
                    (TypedValue::int(2), TypedValue::String("bar".to_owned()))
                ]
                .into_iter()
                .map(|(key, value)| (Rc::new(key), Rc::new(value)))
                .collect()
            )))
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
                        assert!(gas.milligas() < Gas::default().milligas());
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
                Type::BigMap((Type::Int, Type::Int).into())
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
                Type::BigMap(Rc::new((Type::String, Type::Nat)))
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
                Type::BigMap(Rc::new((Type::String, Type::Nat)))
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
                    Push(TypedValue::Address(addr::Address
                        { hash: AddressHash::try_from("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq").unwrap(),
                          entrypoint: Entrypoint::default() }
                        )),
                    Unit,
                    IView { name: "hello_view".into(), return_type: Type::String }, Drop(None)]),
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
            let exp = Ok(Push(TypedValue::Address(exp)));
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

    #[test]
    fn test_push_chain_id() {
        let bytes = "f3d48554";
        let exp = hex::decode(bytes).unwrap();
        let exp = Ok(Push(TypedValue::ChainId(
            super::ChainId::try_from(exp).unwrap(),
        )));
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
            super::typecheck_instruction(&parse("PACK").unwrap(), &mut Gas::default(), None, stk),
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
                stk
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
                stk
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
                stk
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
                stk
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
            Ok(Push(TypedValue::Bytes(hex::decode("deadf00d").unwrap())))
        );
    }

    #[test]
    fn push_key() {
        assert_eq!(
            parse("PUSH key \"p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB\"")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::Key(
                "p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB"
                    .try_into()
                    .unwrap()
            )))
        );
        assert_eq!(
            parse(
                "PUSH key 0x01022c380cd1ff286a0a1a7c3aad6e891d237fa82e2a7cdeec08ccb55e90fdef995f"
            )
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::Key(
                "sppk7Ze7NMs6EHF2uB8qq8GrEgJvE9PWYkUijN3LcesafzQuGyniHBD"
                    .try_into()
                    .unwrap()
            )))
        );
    }

    #[test]
    fn push_key_hash() {
        assert_eq!(
            parse("PUSH key_hash \"tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw\"")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::KeyHash(
                "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw".try_into().unwrap()
            )))
        );
        assert_eq!(
            parse("PUSH key_hash 0x036342f30484dd46b6074373aa6ddca9dfb70083d6")
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::KeyHash(
                "tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN".try_into().unwrap()
            )))
        );
    }

    #[test]
    fn push_signature() {
        assert_eq!(
            parse("PUSH signature \"p2sigRmXDp38VNVaEQH28LYukfLPn8QB5hPEberhvQrrUpRscDZJrrApbRh2u46PTVTwKXjxTLKNN9dyLhPQU6U6jWPGxe4d9v\"")
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::Signature(
                        "p2sigRmXDp38VNVaEQH28LYukfLPn8QB5hPEberhvQrrUpRscDZJrrApbRh2u46PTVTwKXjxTLKNN9dyLhPQU6U6jWPGxe4d9v"
                        .try_into()
                        .unwrap()
                        )))
            );
        // NB: bytes are always treated as a generic signature
        assert_eq!(
            parse(
                "PUSH signature 0x22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222"
                )
            .unwrap()
            .typecheck_instruction(&mut Gas::default(), None, &[]),
            Ok(Push(TypedValue::Signature(
                        "sigSTJNiwaPuZXmU2FscxNy9scPjjwpbxpPD5rY1QRBbyb4gHXYU7jN9Wcbs9sE4GMzuiSSG5S2egeyJhUjW1uJEgw4AWAXj"
                        .try_into()
                        .unwrap()
                        )))
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
            Ok(Push(TypedValue::Lambda(Closure::Lambda(Lambda::Lambda {
                micheline_code: parse("{DROP; UNIT}").unwrap(),
                code: vec![Drop(None), Unit].into()
            }))))
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
            Ok(Push(TypedValue::Lambda(Closure::Lambda(
                Lambda::LambdaRec {
                    micheline_code: parse("{SWAP; DROP}").unwrap(),
                    code: vec![Swap, Drop(None)].into(),
                    in_ty: Type::Unit,
                    out_ty: Type::Unit
                }
            ))))
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
            Ok(Push(TypedValue::timestamp(1571659294)))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"2019-10-21T12:01:34Z\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(TypedValue::timestamp(1571659294)))
        );

        let stk = &mut tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH timestamp \"1571659294\"").unwrap(),
                &mut Gas::default(),
                stk
            ),
            Ok(Push(TypedValue::timestamp(1571659294)))
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
            Gas::default().milligas() - gas.milligas(),
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
            Gas::default().milligas() - gas.milligas(),
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
}
