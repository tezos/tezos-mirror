/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::hash_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::num::TryFromIntError;
use tezos_crypto_rs::{base58::FromBase58CheckError, hash::FromBytesError};

pub mod type_props;

use type_props::TypeProperty;

use crate::ast::annotations::{AnnotationError, NO_ANNS};
use crate::ast::micheline::{
    micheline_fields, micheline_instructions, micheline_literals, micheline_types, micheline_values,
};
use crate::ast::michelson_address::AddressHash;
use crate::ast::*;
use crate::context::Ctx;
use crate::gas;
use crate::gas::OutOfGas;
use crate::irrefutable_match::irrefutable_match;
use crate::lexer::Prim;
use crate::stack::*;

/// Typechecker error type.
#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum TcError {
    #[error("type stacks not equal: {0:?} != {1:?}")]
    StacksNotEqual(TypeStack, TypeStack, StacksNotEqualReason),
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
    #[error("type is not {0}: {1:?}")]
    InvalidTypeProperty(TypeProperty, Type),
    #[error("FAIL instruction is not in tail position")]
    FailNotInTail,
    #[error("numeric conversion failed: {0}")]
    NumericConversion(#[from] TryFromIntError),
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    #[error("DUP 0 is forbidden")]
    Dup0,
    #[error("value {0} is invalid for type {1:?}")]
    InvalidValueForType(String, Type),
    #[error("value {0:?} is invalid element for container type {1:?}")]
    InvalidEltForMap(String, Type),
    #[error("sequence elements must be in strictly ascending order for type {0:?}")]
    ElementsNotSorted(Type),
    #[error("sequence elements must contain no duplicate keys for type {0:?}")]
    DuplicateElements(Type),
    #[error("no matching overload for {instr} on stack {stack:?}{}", .reason.as_ref().map_or("".to_owned(), |x| format!(", reason: {}", x)))]
    NoMatchingOverload {
        instr: Prim,
        stack: TypeStack,
        reason: Option<NoMatchingOverloadReason>,
    },
    #[error("invalid value for type {0:?}: {1}")]
    ByteReprError(Type, ByteReprError),
    #[error("invalid entrypoint: {0}")]
    EntrypointError(ByteReprError),
    #[error("invalid value for chain_id: {0}")]
    ChainIdError(#[from] ChainIdError),
    #[error("SELF instruction is forbidden in this context")]
    SelfForbidden,
    #[error("no such entrypoint: {0}")]
    NoSuchEntrypoint(Entrypoint),
    #[error("unexpected implicit account parameter type: {0:?}")]
    UnexpectedImplicitAccountType(Type),
    #[error("unexpected syntax: {0}")]
    UnexpectedMicheline(String),
    #[error("duplicate top-level element: {0}")]
    DuplicateTopLevelElt(Prim),
    #[error("missing top-level element: {0}")]
    MissingTopLevelElt(Prim),
    #[error("expected a natural between 0 and 1023, but got {0}")]
    ExpectedU10(i128),
    #[error(transparent)]
    AnnotationError(#[from] AnnotationError),
    #[error("duplicate entrypoint: {0}")]
    DuplicateEntrypoint(Entrypoint),
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum ChainIdError {
    #[error("{0}")]
    FromBase58CheckError(String),
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

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum NoMatchingOverloadReason {
    #[error("stack too short, expected {expected}")]
    StackTooShort { expected: usize },
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    #[error("expected pair 'a 'b, but got {0:?}")]
    ExpectedPair(Type),
    #[error("expected option 'a, but got {0:?}")]
    ExpectedOption(Type),
    #[error("expected list 'a, but got {0:?}")]
    ExpectedList(Type),
    #[error("expected or 'a 'b, but got {0:?}")]
    ExpectedOr(Type),
    #[error("type not comparable: {0:?}")]
    TypeNotComparable(Type),
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum StacksNotEqualReason {
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    #[error("lengths are different: {0} != {1}")]
    LengthsDiffer(usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
#[error("types not equal: {0:?} != {1:?}")]
pub struct TypesNotEqual(Type, Type);

pub type Entrypoints = HashMap<Entrypoint, Type>;

impl Micheline<'_> {
    /// Typechecks `Micheline` as a value, given its type (also as `Micheline`).
    /// Validates the type.
    pub fn typecheck_value(
        &self,
        ctx: &mut Ctx,
        value_type: &Micheline,
    ) -> Result<TypedValue, TcError> {
        let ty = parse_ty(ctx, value_type)?;
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
        ctx: &mut Ctx,
        self_type: Option<&Micheline>,
        stack: &[Micheline],
    ) -> Result<Instruction, TcError> {
        let entrypoints = self_type
            .map(|ty| {
                let (entrypoints, ty) = parse_parameter_ty_with_entrypoints(ctx, ty)?;
                ty.ensure_prop(&mut ctx.gas, TypeProperty::Passable)?;
                Ok::<_, TcError>(entrypoints)
            })
            .transpose()?;

        let TopIsLast(checked_stack) = stack
            .iter()
            .map(|ty| parse_ty(ctx, ty))
            .collect::<Result<_, TcError>>()?;
        let mut opt_stack = FailingTypeStack::Ok(checked_stack);
        typecheck_instruction(self, ctx, entrypoints.as_ref(), &mut opt_stack)
    }

    /// Parse `Micheline` as a type. Validates the type.
    pub fn parse_ty(&self, ctx: &mut Ctx) -> Result<Type, TcError> {
        parse_ty(ctx, self)
    }

    pub fn get_entrypoints(&self, ctx: &mut Ctx) -> Result<Entrypoints, TcError> {
        let (entrypoints, _) = parse_parameter_ty_with_entrypoints(ctx, self)?;
        Ok(entrypoints)
    }

    /// Typecheck the contract script. Validates the script's types, then
    /// typechecks the code and checks the result stack is as expected. Returns
    /// typechecked script.
    pub fn typecheck_script(&self, ctx: &mut Ctx) -> Result<ContractScript, TcError> {
        let seq = match self {
            // top-level allows one level of nesting
            Micheline::Seq([Micheline::Seq(seq)]) => seq,
            Micheline::Seq(seq) => seq,
            x => return Err(TcError::UnexpectedMicheline(format!("{x:?}"))),
        };
        let mut parameter_ty = None;
        let mut storage_ty = None;
        let mut code = None;
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
                Micheline::Seq(..)
                | micheline_instructions!()
                | micheline_literals!()
                | micheline_types!()
                | micheline_fields!()
                | micheline_values!() => {
                    return Err(TcError::UnexpectedMicheline(format!("{elt:?}")))
                }
            }
        }
        let (entrypoints, parameter) = parse_parameter_ty_with_entrypoints(
            ctx,
            parameter_ty.ok_or(TcError::MissingTopLevelElt(Prim::parameter))?,
        )?;
        let storage = storage_ty
            .ok_or(TcError::MissingTopLevelElt(Prim::storage))?
            .parse_ty(ctx)?;
        parameter.ensure_prop(&mut ctx.gas, TypeProperty::Passable)?;
        storage.ensure_prop(&mut ctx.gas, TypeProperty::Storable)?;
        let mut stack = tc_stk![Type::new_pair(parameter.clone(), storage.clone())];
        let code = typecheck_instruction(
            code.ok_or(TcError::MissingTopLevelElt(Prim::code))?,
            ctx,
            Some(&entrypoints),
            &mut stack,
        )?;
        unify_stacks(
            ctx,
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
        })
    }
}

pub(crate) fn parse_ty(ctx: &mut Ctx, ty: &Micheline) -> Result<Type, TcError> {
    parse_ty_with_entrypoints(ctx, ty, None)
}

fn parse_ty_with_entrypoints(
    ctx: &mut Ctx,
    ty: &Micheline,
    mut entrypoints: Option<&mut Entrypoints>,
) -> Result<Type, TcError> {
    use Micheline::*;
    use Prim::*;
    ctx.gas.consume(gas::tc_cost::PARSE_TYPE_STEP)?;
    fn make_pair(
        ctx: &mut Ctx,
        args: (&Micheline, &Micheline, &[Micheline]),
        // NB: the tuple models a slice of at least 2 elements
    ) -> Result<Type, TcError> {
        Ok(match args {
            (ty1, ty2, []) => Type::new_pair(parse_ty(ctx, ty1)?, parse_ty(ctx, ty2)?),
            (ty1, ty2, [ty3, rest @ ..]) => {
                Type::new_pair(parse_ty(ctx, ty1)?, make_pair(ctx, (ty2, ty3, rest))?)
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

        App(pair, [ty1, ty2, rest @ ..], _) => make_pair(ctx, (ty1, ty2, rest))?,
        App(pair, ..) => unexpected()?,

        App(or, [l, r], _) => Type::new_or(
            parse_ty_with_entrypoints(ctx, l, entrypoints.as_deref_mut())?,
            parse_ty_with_entrypoints(ctx, r, entrypoints.as_deref_mut())?,
        ),

        App(or, ..) => unexpected()?,

        App(option, [t], _) => Type::new_option(parse_ty(ctx, t)?),
        App(option, ..) => unexpected()?,

        App(list, [t], _) => Type::new_list(parse_ty(ctx, t)?),
        App(list, ..) => unexpected()?,

        App(contract, [t], _) => {
            let t = parse_ty(ctx, t)?;
            // NB: despite `contract` type being duplicable and packable, its
            // argument doesn't need to be. The only constraint is that it needs
            // to be passable, as it represents the contract's parameter type.
            // See https://tezos.gitlab.io/michelson-reference/#type-contract
            t.ensure_prop(&mut ctx.gas, TypeProperty::Passable)?;
            Type::new_contract(t)
        }
        App(contract, ..) => unexpected()?,

        App(set, [k], _) => {
            let k = parse_ty(ctx, k)?;
            k.ensure_prop(&mut ctx.gas, TypeProperty::Comparable)?;
            Type::new_set(k)
        }
        App(set, ..) => unexpected()?,

        App(map, [k, v], _) => {
            let k = parse_ty(ctx, k)?;
            k.ensure_prop(&mut ctx.gas, TypeProperty::Comparable)?;
            let v = parse_ty(ctx, v)?;
            Type::new_map(k, v)
        }
        App(map, ..) => unexpected()?,

        App(bytes, [], _) => Type::Bytes,
        App(bytes, ..) => unexpected()?,

        App(key, [], _) => Type::Key,
        App(key, ..) => unexpected()?,

        App(key_hash, [], _) => Type::KeyHash,
        App(key_hash, ..) => unexpected()?,

        App(signature, [], _) => Type::Signature,
        App(signature, ..) => unexpected()?,

        Seq(..)
        | micheline_fields!()
        | micheline_instructions!()
        | micheline_literals!()
        | micheline_values!() => unexpected()?,

        App(other, ..) => todo!("Unhandled type {other}"),
    };
    if let Option::Some(eps) = entrypoints {
        // we just ensured it's an application of some type primitive
        irrefutable_match!(ty; App, _prim, _args, anns);
        if let Option::Some(field_ann) = anns.get_single_field_ann()? {
            // NB: field annotations may be longer than entrypoints; however
            // it's not an error to have an overly-long field annotation, it
            // just doesn't count as an entrypoint.
            if let Ok(entrypoint) = Entrypoint::try_from(field_ann) {
                let entry = eps.entry(entrypoint);
                match entry {
                    Entry::Occupied(e) => {
                        return Err(TcError::DuplicateEntrypoint(e.key().clone()))
                    }
                    Entry::Vacant(e) => e.insert(parsed_ty.clone()),
                };
            }
        }
    }
    Ok(parsed_ty)
}

fn parse_parameter_ty_with_entrypoints(
    ctx: &mut Ctx,
    parameter_ty: &Micheline,
) -> Result<(Entrypoints, Type), TcError> {
    let mut entrypoints = Entrypoints::new();
    let parameter = parse_ty_with_entrypoints(ctx, parameter_ty, Some(&mut entrypoints))?;
    entrypoints
        .entry(Entrypoint::default())
        .or_insert_with(|| parameter.clone());
    Ok((entrypoints, parameter))
}

/// Typecheck a sequence of instructions. Assumes the passed stack is valid, i.e.
/// doesn't contain illegal types like `set operation` or `contract operation`.
///
/// When `self_entrypoints` is `None`, `SELF` instruction is forbidden (e.g.
/// like in lambdas).
///
/// Entrypoint map is carried as an argument, not as part of context, because it
/// has to be locally overridden during typechecking.
fn typecheck(
    ast: &[Micheline],
    ctx: &mut Ctx,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
) -> Result<Vec<Instruction>, TcError> {
    ast.iter()
        .map(|i| typecheck_instruction(i, ctx, self_entrypoints, opt_stack))
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
pub(crate) fn typecheck_instruction(
    i: &Micheline,
    ctx: &mut Ctx,
    self_entrypoints: Option<&Entrypoints>,
    opt_stack: &mut FailingTypeStack,
) -> Result<Instruction, TcError> {
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
        ($instr:ident, len $expected_len:expr) => {
            {
                return Err(TcError::NoMatchingOverload {
                    instr: Prim::$instr,
                    stack: stack.clone(),
                    reason: Option::Some(NoMatchingOverloadReason::StackTooShort {
                        expected: $expected_len
                    }),
                })
            }
        };
        ($instr:ident$(, $reason:expr)?) => {
            {
                return Err(TcError::NoMatchingOverload {
                    instr: Prim::$instr,
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

    /// Pattern synonym matching any number of slice elements _except_ the number
    /// passed as the argument. If the number is suffixed with `seq`, the pattern
    /// will also match the number of elements equal to the argument iff either of
    /// those isn't `Micheline::Seq`.
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
        (1 seq) => {
            expect_args!(1) | [micheline_non_seq!()]
        };
        (2 seq) => {
            expect_args!(2)
                | [micheline_non_seq!(), micheline_non_seq!()]
                | [Micheline::Seq(..), micheline_non_seq!()]
                | [micheline_non_seq!(), Micheline::Seq(..)]
        };
    }

    ctx.gas.consume(gas::tc_cost::INSTR_STEP)?;

    use Micheline::*;
    use Prim::*;

    macro_rules! unexpected_micheline {
        () => {
            return Err(TcError::UnexpectedMicheline(format!("{i:?}")))
        };
    }

    Ok(match (i, stack.as_slice()) {
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
        (App(ADD, [], _), [.., _, _]) => no_overload!(ADD),
        (App(ADD, [], _), [_] | []) => no_overload!(ADD, len 2),
        (App(ADD, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(DIP, args, _), ..) => {
            let (opt_height, nested) = match args {
                [Int(height), Seq(nested)] => (Option::Some(validate_u10(*height)?), nested),
                [Seq(nested)] => (Option::None, nested),
                _ => unexpected_micheline!(),
            };
            let protected_height = opt_height.unwrap_or(1) as usize;

            ctx.gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;

            ensure_stack_len(Prim::DIP, stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, ctx, self_entrypoints, opt_stack)?;
            opt_stack
                .access_mut(TcError::FailNotInTail)?
                .append(&mut protected);
            I::Dip(opt_height, nested)
        }

        (App(DROP, args, _), ..) => {
            let opt_height = match args {
                [Int(height)] => Option::Some(validate_u10(*height)?),
                [] => Option::None,
                _ => unexpected_micheline!(),
            };
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            ctx.gas.consume(gas::tc_cost::drop_n(&opt_height)?)?;
            ensure_stack_len(Prim::DROP, stack, drop_height)?;
            stack.drop_top(drop_height);
            I::Drop(opt_height)
        }

        // DUP instruction requires an argument that is > 0.
        (App(DUP, [Int(0)], _), ..) => return Err(TcError::Dup0),
        (App(DUP, args, _), ..) => {
            let opt_height = match args {
                [Int(height)] => Option::Some(validate_u10(*height)?),
                [] => Option::None,
                _ => unexpected_micheline!(),
            };
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            ensure_stack_len(Prim::DUP, stack, dup_height)?;
            let ty = &stack[dup_height - 1];
            ty.ensure_prop(&mut ctx.gas, TypeProperty::Duplicable)?;
            stack.push(ty.clone());
            I::Dup(opt_height)
        }

        (App(GT, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Gt
        }
        (App(GT, [], _), [.., t]) => no_overload!(GT, TypesNotEqual(T::Int, t.clone())),
        (App(GT, [], _), []) => no_overload!(GT, len 1),
        (App(GT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(EQ, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Eq
        }
        (App(EQ, [], _), [.., t]) => no_overload!(EQ, TypesNotEqual(T::Int, t.clone())),
        (App(EQ, [], _), []) => no_overload!(EQ, len 1),
        (App(EQ, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LE, [], _), [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Le
        }
        (App(LE, [], _), [.., t]) => no_overload!(LE, TypesNotEqual(T::Int, t.clone())),
        (App(LE, [], _), []) => no_overload!(LE, len 1),
        (App(LE, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(IF, [Seq(nested_t), Seq(nested_f)], _), [.., T::Bool]) => {
            // pop the bool off the stack
            pop!();
            // Clone the stack so that we have a copy to run one branch on.
            // We can run the other branch on the live stack.
            let mut f_opt_stack = opt_stack.clone();
            let nested_t = typecheck(nested_t, ctx, self_entrypoints, opt_stack)?;
            let nested_f = typecheck(nested_f, ctx, self_entrypoints, &mut f_opt_stack)?;
            // If stacks unify after typecheck, all is good.
            unify_stacks(ctx, opt_stack, f_opt_stack)?;
            I::If(nested_t, nested_f)
        }
        (App(IF, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF, TypesNotEqual(T::Bool, t.clone()))
        }
        (App(IF, [Seq(_), Seq(_)], _), []) => no_overload!(IF, len 1),
        (App(IF, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_NONE, [Seq(when_none), Seq(when_some)], _), [.., T::Option(..)]) => {
            // Extract option type
            let ty = pop!(T::Option);
            // Clone the some_stack as we need to push a type on top of it
            let mut some_stack: TypeStack = stack.clone();
            some_stack.push(*ty);
            let mut some_opt_stack = FailingTypeStack::Ok(some_stack);
            let when_none = typecheck(when_none, ctx, self_entrypoints, opt_stack)?;
            let when_some = typecheck(when_some, ctx, self_entrypoints, &mut some_opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, some_opt_stack)?;
            I::IfNone(when_none, when_some)
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_NONE, NMOR::ExpectedOption(t.clone()))
        }
        (App(IF_NONE, [Seq(_), Seq(_)], _), []) => no_overload!(IF_NONE, len 1),
        (App(IF_NONE, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_CONS, [Seq(when_cons), Seq(when_nil)], _), [.., T::List(..)]) => {
            // Clone the cons_stack as we need to push a type on top of it
            let mut cons_stack: TypeStack = stack.clone();
            // get the list element type
            let ty = pop!(T::List);
            // push it to the cons stack
            cons_stack.push(*ty);
            let mut cons_opt_stack = FailingTypeStack::Ok(cons_stack);
            let when_cons = typecheck(when_cons, ctx, self_entrypoints, &mut cons_opt_stack)?;
            let when_nil = typecheck(when_nil, ctx, self_entrypoints, opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, cons_opt_stack)?;
            I::IfCons(when_cons, when_nil)
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_CONS, NMOR::ExpectedList(t.clone()))
        }
        (App(IF_CONS, [Seq(_), Seq(_)], _), []) => no_overload!(IF_CONS, len 1),
        (App(IF_CONS, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(IF_LEFT, [Seq(when_left), Seq(when_right)], _), [.., T::Or(..)]) => {
            // get the list element type
            let (tl, tr) = *pop!(T::Or);
            // use main stack as left branch, cloned stack as right
            let mut right_stack = stack.clone();
            stack.push(tl);
            right_stack.push(tr);
            let mut opt_right_stack = FailingTypeStack::Ok(right_stack);
            let when_left = typecheck(when_left, ctx, self_entrypoints, opt_stack)?;
            let when_right = typecheck(when_right, ctx, self_entrypoints, &mut opt_right_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, opt_right_stack)?;
            I::IfLeft(when_left, when_right)
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), [.., t]) => {
            no_overload!(IF_LEFT, NMOR::ExpectedOr(t.clone()))
        }
        (App(IF_LEFT, [Seq(_), Seq(_)], _), []) => no_overload!(IF_LEFT, len 1),
        (App(IF_LEFT, expect_args!(2 seq), _), _) => unexpected_micheline!(),

        (App(INT, [], _), [.., T::Nat]) => {
            stack[0] = Type::Int;
            I::Int
        }
        (App(INT, [], _), [.., _]) => no_overload!(INT),
        (App(INT, [], _), []) => no_overload!(INT, len 1),
        (App(INT, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(LOOP, [Seq(nested)], _), [.., T::Bool]) => {
            // copy stack for unifying with it later
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            // Pop the bool off the top
            pop!();
            // Typecheck body with the current stack
            let nested = typecheck(nested, ctx, self_entrypoints, opt_stack)?;
            // If the starting stack and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_copy)?;
            // pop the remaining bool off (if not failed)
            opt_stack.access_mut(()).ok().map(Stack::pop);
            I::Loop(nested)
        }
        (App(LOOP, [Seq(_)], _), [.., ty]) => {
            no_overload!(LOOP, TypesNotEqual(T::Bool, ty.clone()))
        }
        (App(LOOP, [Seq(_)], _), []) => no_overload!(LOOP, len 1),
        (App(LOOP, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(ITER, [Seq(nested)], ..), [.., T::List(..)]) => {
            // get the list element type
            let ty = *pop!(T::List);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(ty);
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, ctx, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans list) and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::List, nested)
        }
        (App(ITER, [Seq(nested)], _), [.., T::Set(..)]) => {
            // get the set element type
            let ty = pop!(T::Set);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(*ty);
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, ctx, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans set) and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_inner_stack)?;
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
            let nested = typecheck(nested, ctx, self_entrypoints, &mut opt_inner_stack)?;
            // If the starting stack (sans map) and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::Map, nested)
        }
        (App(ITER, [Seq(_)], _), [.., _]) => no_overload!(ITER),
        (App(ITER, [Seq(_)], _), []) => no_overload!(ITER, len 1),
        (App(ITER, expect_args!(1 seq), _), _) => unexpected_micheline!(),

        (App(PUSH, [t, v], _), ..) => {
            let t = parse_ty(ctx, t)?;
            t.ensure_prop(&mut ctx.gas, TypeProperty::Pushable)?;
            let v = typecheck_value(v, ctx, &t)?;
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
            ty.ensure_prop(&mut ctx.gas, TypeProperty::Pushable)?;
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
            no_overload!(NEVER, TypesNotEqual(T::Never, t.clone()))
        }
        (App(NEVER, [], _), []) => no_overload!(NEVER, len 1),
        (App(NEVER, ..), _) => unexpected_micheline!(),

        (App(UNIT, [], _), ..) => {
            stack.push(T::Unit);
            I::Unit
        }
        (App(UNIT, ..), _) => unexpected_micheline!(),

        (App(CAR, [], _), [.., T::Pair(..)]) => {
            let (l, _) = *pop!(T::Pair);
            stack.push(l);
            I::Car
        }
        (App(CAR, [], _), [.., ty]) => no_overload!(CAR, NMOR::ExpectedPair(ty.clone())),
        (App(CAR, [], _), []) => no_overload!(CAR, len 1),
        (App(CAR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(CDR, [], _), [.., T::Pair(..)]) => {
            let (_, r) = *pop!(T::Pair);
            stack.push(r);
            I::Cdr
        }
        (App(CDR, [], _), [.., ty]) => no_overload!(CDR, NMOR::ExpectedPair(ty.clone())),
        (App(CDR, [], _), []) => no_overload!(CDR, len 1),
        (App(CDR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(PAIR, [], _), [.., _, _]) => {
            let (l, r) = (pop!(), pop!());
            stack.push(Type::new_pair(l, r));
            I::Pair
        }
        (App(PAIR, [], _), [] | [_]) => no_overload!(PAIR, len 2),
        (App(PAIR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UNPAIR, [], _), [.., T::Pair(..)]) => {
            let (l, r) = *pop!(T::Pair);
            stack.push(r);
            stack.push(l);
            I::Unpair
        }
        (App(UNPAIR, [], _), [.., ty]) => no_overload!(UNPAIR, NMOR::ExpectedPair(ty.clone())),
        (App(UNPAIR, [], _), []) => no_overload!(UNPAIR, len 1),
        (App(UNPAIR, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(SOME, [], _), [.., _]) => {
            let ty = pop!();
            stack.push(T::new_option(ty));
            I::ISome
        }
        (App(SOME, [], _), []) => no_overload!(SOME, len 1),
        (App(SOME, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(NONE, [ty], _), _) => {
            let ty = parse_ty(ctx, ty)?;
            stack.push(T::new_option(ty));
            I::None
        }
        (App(NONE, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(COMPARE, [], _), [.., u, t]) => {
            ensure_ty_eq(ctx, t, u).map_err(|e| match e {
                TcError::TypesNotEqual(e) => TcError::NoMatchingOverload {
                    instr: Prim::COMPARE,
                    stack: stack.clone(),
                    reason: Option::Some(e.into()),
                },
                e => e,
            })?;
            t.ensure_prop(&mut ctx.gas, TypeProperty::Comparable)?;
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
            let ty = parse_ty(ctx, ty)?;
            stack.push(T::new_list(ty));
            I::Nil
        }
        (App(NIL, ..), _) => unexpected_micheline!(),

        (App(CONS, [], _), [.., T::List(ty1), ty2]) => {
            ensure_ty_eq(ctx, ty1, ty2)?;
            pop!();
            I::Cons
        }
        (App(CONS, [], _), [.., ty, _]) => no_overload!(CONS, NMOR::ExpectedList(ty.clone())),
        (App(CONS, [], _), [] | [_]) => no_overload!(CONS, len 2),
        (App(CONS, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(GET, [], _), [.., T::Map(..), _]) => {
            let kty_ = pop!();
            let (kty, vty) = *pop!(T::Map);
            ensure_ty_eq(ctx, &kty, &kty_)?;
            stack.push(T::new_option(vty));
            I::Get(overloads::Get::Map)
        }
        (App(GET, [], _), [.., _, _]) => no_overload!(GET),
        (App(GET, [], _), [] | [_]) => no_overload!(GET, len 2),
        (App(GET, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(UPDATE, [], _), [.., T::Set(ty), T::Bool, ty_]) => {
            ensure_ty_eq(ctx, ty, ty_)?;
            stack.drop_top(2);
            I::Update(overloads::Update::Set)
        }
        (App(UPDATE, [], _), [.., T::Map(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(ctx, kty, kty_)?;
            ensure_ty_eq(ctx, vty, vty_new)?;
            stack.drop_top(2);
            I::Update(overloads::Update::Map)
        }
        (App(UPDATE, [], _), [.., _, _, _]) => no_overload!(UPDATE),
        (App(UPDATE, [], _), [] | [_] | [_, _]) => no_overload!(UPDATE, len 3),
        (App(UPDATE, expect_args!(0), _), _) => unexpected_micheline!(),

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
            t.ensure_prop(&mut ctx.gas, TypeProperty::Packable)?;
            stack.push(T::Bytes);
            I::Pack
        }
        (App(PACK, [], _), []) => no_overload!(PACK, len 1),
        (App(PACK, expect_args!(0), _), _) => unexpected_micheline!(),

        (App(TRANSFER_TOKENS, [], _), [.., T::Contract(ct), T::Mutez, arg_t]) => {
            ensure_ty_eq(ctx, ct, arg_t)?;
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
            let ty_right = parse_ty(ctx, ty_right)?;
            stack.push(T::new_or(ty_left, ty_right));
            I::Left
        }
        (App(LEFT, [_ty_right], _), []) => no_overload!(LEFT, len 1),
        (App(LEFT, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(RIGHT, [ty_left], _), [.., _]) => {
            let ty_right = pop!();
            let ty_left = parse_ty(ctx, ty_left)?;
            stack.push(T::new_or(ty_left, ty_right));
            I::Right
        }
        (App(RIGHT, [_ty_left], _), []) => no_overload!(RIGHT, len 1),
        (App(RIGHT, expect_args!(1), _), _) => unexpected_micheline!(),

        (App(other, ..), _) => todo!("Unhandled instruction {other}"),

        (Seq(nested), _) => I::Seq(typecheck(nested, ctx, self_entrypoints, opt_stack)?),
    })
}

/// Typecheck a value. Assumes passed the type is valid, i.e. doesn't contain
/// illegal types like `set operation` or `contract operation`.
pub(crate) fn typecheck_value(
    v: &Micheline,
    ctx: &mut Ctx,
    t: &Type,
) -> Result<TypedValue, TcError> {
    use Micheline as V;
    use Type as T;
    use TypedValue as TV;
    ctx.gas.consume(gas::tc_cost::VALUE_STEP)?;
    Ok(match (t, v) {
        (T::Nat, V::Int(n)) => TV::Nat((*n).try_into()?),
        (T::Int, V::Int(n)) => TV::Int(*n),
        (T::Bool, V::App(Prim::True, [], _)) => TV::Bool(true),
        (T::Bool, V::App(Prim::False, [], _)) => TV::Bool(false),
        (T::Mutez, V::Int(n)) if *n >= 0 => TV::Mutez((*n).try_into()?),
        (T::String, V::String(s)) => TV::String(s.clone()),
        (T::Unit, V::App(Prim::Unit, [], _)) => TV::Unit,
        (T::Pair(pt), V::App(Prim::Pair, [vl, rest @ ..], _)) if !rest.is_empty() => {
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
        (T::List(ty), V::Seq(vs)) => TV::List(
            vs.iter()
                .map(|v| typecheck_value(v, ctx, ty))
                .collect::<Result<_, TcError>>()?,
        ),
        (T::Set(ty), V::Seq(vs)) => {
            let vs = vs
                .iter()
                .map(|v| typecheck_value(v, ctx, ty))
                .collect::<Result<Vec<TypedValue>, TcError>>()?;
            validate_ordered_by(ctx, t, &vs, |x| x)?;
            ctx.gas
                .consume(gas::tc_cost::construct_set(ty.size_for_gas(), vs.len())?)?;
            // See the same concern about constructing from ordered sequence as in Map
            let set: BTreeSet<TypedValue> = vs.into_iter().collect();
            TV::Set(set)
        }
        (T::Map(m), V::Seq(vs)) => {
            let (tk, tv) = m.as_ref();
            let tc_elt = |v: &Micheline| -> Result<(TypedValue, TypedValue), TcError> {
                match v {
                    Micheline::App(Prim::Elt, [k, v], _) => {
                        let k = typecheck_value(k, ctx, tk)?;
                        let v = typecheck_value(v, ctx, tv)?;
                        Ok((k, v))
                    }
                    _ => Err(TcError::InvalidEltForMap(format!("{v:?}"), t.clone())),
                }
            };
            let elts: Vec<(TypedValue, TypedValue)> =
                vs.iter().map(tc_elt).collect::<Result<_, TcError>>()?;
            validate_ordered_by(ctx, t, &elts, |(k, _)| k)?;
            ctx.gas
                .consume(gas::tc_cost::construct_map(tk.size_for_gas(), elts.len())?)?;
            // Unfortunately, `BTreeMap` doesn't expose methods to build from an already-sorted
            // slice/vec/iterator. FWIW, Rust docs claim that its sorting algorithm is "designed to
            // be very fast in cases where the slice is nearly sorted", so hopefully it doesn't add
            // too much overhead.
            let map: BTreeMap<TypedValue, TypedValue> = elts.into_iter().collect();
            TV::Map(map)
        }
        (T::Address, V::String(str)) => {
            ctx.gas.consume(gas::tc_cost::KEY_HASH_READABLE)?;
            TV::Address(
                Address::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::Address, e))?,
            )
        }
        (T::Address, V::Bytes(bs)) => {
            ctx.gas.consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            TV::Address(Address::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::Address, e))?)
        }
        (T::Contract(ty), addr) => {
            let t_addr = irrefutable_match!(typecheck_value(addr, ctx, &T::Address)?; TV::Address);
            match t_addr.hash {
                AddressHash::Implicit(_) => {
                    if !t_addr.is_default_ep() {
                        return Err(TcError::NoSuchEntrypoint(t_addr.entrypoint));
                    }
                    ctx.gas.consume(gas::tc_cost::ty_eq(
                        ty.size_for_gas(),
                        T::Unit.size_for_gas(),
                    )?)?;
                    match ty.as_ref() {
                        T::Unit => {}
                        ty => return Err(TcError::UnexpectedImplicitAccountType(ty.clone())),
                    }
                }
                AddressHash::Kt1(_) | AddressHash::Sr1(_) => {
                    // TODO: verify against ctx
                }
            }
            TV::Contract(t_addr)
        }
        (T::ChainId, V::String(str)) => {
            ctx.gas.consume(gas::tc_cost::CHAIN_ID_READABLE)?;
            TV::ChainId(
                ChainId::from_base58_check(str).map_err(|x| TcError::ChainIdError(x.into()))?,
            )
        }
        (T::ChainId, V::Bytes(bs)) => {
            use tezos_crypto_rs::hash::HashTrait;
            ctx.gas.consume(gas::tc_cost::CHAIN_ID_OPTIMIZED)?;
            TV::ChainId(ChainId::try_from_bytes(bs).map_err(|x| TcError::ChainIdError(x.into()))?)
        }
        (T::Bytes, V::Bytes(bs)) => TV::Bytes(bs.clone()),
        (T::Key, V::String(str)) => {
            ctx.gas.consume(gas::tc_cost::KEY_READABLE)?;
            TV::Key(Key::from_base58_check(str).map_err(|e| TcError::ByteReprError(T::Key, e))?)
        }
        (T::Key, V::Bytes(bs)) => {
            ctx.gas.consume(gas::tc_cost::KEY_OPTIMIZED)?;
            TV::Key(Key::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::Key, e))?)
        }
        (T::Signature, V::String(str)) => {
            ctx.gas.consume(gas::tc_cost::KEY_READABLE)?;
            TV::Signature(
                Signature::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::Signature, e))?,
            )
        }
        (T::Signature, V::Bytes(bs)) => {
            ctx.gas.consume(gas::tc_cost::KEY_OPTIMIZED)?;
            TV::Signature(
                Signature::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::Signature, e))?,
            )
        }
        (T::KeyHash, V::String(str)) => {
            ctx.gas.consume(gas::tc_cost::KEY_HASH_READABLE)?;
            TV::KeyHash(
                KeyHash::from_base58_check(str)
                    .map_err(|e| TcError::ByteReprError(T::KeyHash, e))?,
            )
        }
        (T::KeyHash, V::Bytes(bs)) => {
            ctx.gas.consume(gas::tc_cost::KEY_HASH_OPTIMIZED)?;
            TV::KeyHash(KeyHash::from_bytes(bs).map_err(|e| TcError::ByteReprError(T::KeyHash, e))?)
        }
        (t, v) => return Err(TcError::InvalidValueForType(format!("{v:?}"), t.clone())),
    })
}

fn validate_u10(n: i128) -> Result<u16, TcError> {
    let res = u16::try_from(n).map_err(|_| TcError::ExpectedU10(n))?;
    if res >= 1024 {
        return Err(TcError::ExpectedU10(n));
    }
    Ok(res)
}

/// Ensures given elements have their keys in strictly ascending order
/// (where you specify a getter to obtain the key from an element).
///
/// Also charges gas for this check.
fn validate_ordered_by<T>(
    ctx: &mut Ctx,
    container_t: &Type,
    vs: &[T],
    to_key: impl Fn(&T) -> &TypedValue,
) -> Result<(), TcError> {
    if vs.len() > 1 {
        let mut prev = to_key(&vs[0]);
        for i in &vs[1..] {
            ctx.gas
                .consume(gas::interpret_cost::compare(prev, to_key(i))?)?;
            match prev.cmp(to_key(i)) {
                std::cmp::Ordering::Less => (),
                std::cmp::Ordering::Equal => {
                    return Err(TcError::DuplicateElements(container_t.clone()))
                }
                std::cmp::Ordering::Greater => {
                    return Err(TcError::ElementsNotSorted(container_t.clone()))
                }
            }
            prev = to_key(i);
        }
    }
    Ok(())
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
    ctx: &mut Ctx,
    dest: &mut FailingTypeStack,
    aux: FailingTypeStack,
) -> Result<(), TcError> {
    match &dest {
        FailingTypeStack::Ok(stack1) => {
            if let FailingTypeStack::Ok(stack2) = aux {
                if stack1.len() != stack2.len() {
                    return Err(TcError::StacksNotEqual(
                        stack1.clone(),
                        stack2.clone(),
                        StacksNotEqualReason::LengthsDiffer(stack1.len(), stack2.len()),
                    ));
                }
                for (ty1, ty2) in stack1.iter().zip(stack2.iter()) {
                    ensure_ty_eq(ctx, ty1, ty2).map_err(|e| match e {
                        TcError::TypesNotEqual(e) => {
                            TcError::StacksNotEqual(stack1.clone(), stack2.clone(), e.into())
                        }
                        err => err,
                    })?;
                }
            }
        }
        FailingTypeStack::Failed => {
            // if main stack is failing, assign aux to main, as aux may be OK
            *dest = aux;
        }
    }
    Ok(())
}

fn ensure_ty_eq(ctx: &mut Ctx, ty1: &Type, ty2: &Type) -> Result<(), TcError> {
    ctx.gas
        .consume(gas::tc_cost::ty_eq(ty1.size_for_gas(), ty2.size_for_gas())?)?;
    if ty1 != ty2 {
        Err(TypesNotEqual(ty1.clone(), ty2.clone()).into())
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod typecheck_tests {
    use crate::ast::micheline::test_helpers::*;
    use crate::ast::michelson_address as addr;
    use crate::gas::Gas;
    use crate::parser::test_helpers::*;
    use crate::typechecker::*;
    use Instruction::*;
    use Option::None;

    /// hack to simplify syntax in tests
    fn typecheck_instruction(
        i: &Micheline,
        ctx: &mut Ctx,
        opt_stack: &mut FailingTypeStack,
    ) -> Result<Instruction, TcError> {
        super::typecheck_instruction(i, ctx, None, opt_stack)
    }

    #[test]
    fn test_dup() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Nat];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(DUP[1]), &mut ctx, &mut stack),
            Ok(Dup(Some(1)))
        );
        assert_eq!(stack, expected_stack);
        assert!(ctx.gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_dup_n() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(DUP[2]), &mut ctx, &mut stack),
            Ok(Dup(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert!(ctx.gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_swap() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        let expected_stack = tc_stk![Type::Int, Type::Nat];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(SWAP), &mut ctx, &mut stack),
            Ok(Swap)
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_int() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(INT), &mut ctx, &mut stack),
            Ok(Int)
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_drop() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(DROP), &mut ctx, &mut stack),
            Ok(Drop(None))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_drop_n() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        let expected_stack = tc_stk![];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(DROP[2]), &mut ctx, &mut stack),
            Ok(Drop(Some(2)))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440 - 2 * 50);
    }

    #[test]
    fn test_push() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(PUSH[app!(int), 1]), &mut ctx, &mut stack),
            Ok(Push(TypedValue::Int(1)))
        );
        assert_eq!(stack, expected_stack);
        assert!(ctx.gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_gt() {
        let mut stack = tc_stk![Type::Int];
        let expected_stack = tc_stk![Type::Bool];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(GT), &mut ctx, &mut stack),
            Ok(Gt)
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_dip() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Bool];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("DIP 1 {PUSH nat 6}").unwrap(), &mut ctx, &mut stack),
            Ok(Dip(Some(1), vec![Push(TypedValue::Nat(6))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(ctx.gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_add_int_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        let expected_stack = tc_stk![Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(ADD), &mut ctx, &mut stack),
            Ok(Add(overloads::Add::IntInt))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_add_nat_nat() {
        let mut stack = tc_stk![Type::Nat, Type::Nat];
        let expected_stack = tc_stk![Type::Nat];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(ADD), &mut ctx, &mut stack),
            Ok(Add(overloads::Add::NatNat))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_add_mutez_mutez() {
        let mut stack = tc_stk![Type::Mutez, Type::Mutez];
        let expected_stack = tc_stk![Type::Mutez];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(ADD), &mut ctx, &mut stack),
            Ok(Add(overloads::Add::MutezMutez))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_loop() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {PUSH bool True}").unwrap(),
                &mut ctx,
                &mut stack
            ),
            Ok(Loop(vec![Push(TypedValue::Bool(true))]))
        );
        assert_eq!(stack, expected_stack);
        assert!(ctx.gas.milligas() < Gas::default().milligas());
    }

    #[test]
    fn test_loop_stacks_not_equal_length() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {PUSH int 1; PUSH bool True}").unwrap(),
                &mut ctx,
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &parse("LOOP {DROP; PUSH bool False; PUSH bool True}").unwrap(),
                &mut ctx,
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
    fn test_iter_list() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_too_short() {
        too_short_test(&app!(ITER[Micheline::Seq(&[])]), Prim::ITER, 1)
    }

    #[test]
    fn test_iter_list_inner_mismatch() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::Set, vec![Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_set_inner_mismatch() {
        let mut stack = tc_stk![Type::new_set(Type::Int)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { CAR; DROP }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::Map, vec![Car, Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_map_inner_mismatch() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::Nat)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { }").unwrap(), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { DROP }").unwrap(), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&parse("ITER { FAILWITH }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Failwith(Type::Int)]))
        );
        assert_eq!(stack, tc_stk![])
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            typecheck_instruction(
                &app!(FAILWITH),
                &mut Ctx::default(),
                &mut tc_stk![Type::Int]
            ),
            Ok(Failwith(Type::Int))
        );
    }

    #[test]
    fn test_never() {
        assert_eq!(
            typecheck_instruction(&app!(NEVER), &mut Ctx::default(), &mut tc_stk![Type::Never]),
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
                        &mut Ctx::default(),
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
                    &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::String("foo".to_owned())))
        );
        assert_eq!(stack, tc_stk![Type::String]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH unit Unit").unwrap(),
                &mut Ctx::default(),
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
            typecheck_instruction(&parse("UNIT").unwrap(), &mut Ctx::default(), &mut stack),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::new_pair(
                TypedValue::Int(-5),
                TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::new_or(Or::Left(TypedValue::Int(1)))))
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Bool)]);
    }

    #[test]
    fn push_or_value_right() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (or int bool) (Right False)").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::new_or(Or::Right(TypedValue::Bool(false)))))
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Bool)]);
    }

    #[test]
    fn push_option_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (option nat) (Some 3)").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::new_option(Some(TypedValue::Nat(3)))))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Nat)]);
    }

    #[test]
    fn push_option_none_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH (option nat) None").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Seq(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Seq(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
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
            typecheck_instruction(&parse("CAR").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(&parse("CDR").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(&parse("PAIR").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Pair)
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Int)]);
    }

    #[test]
    fn unpair() {
        let mut stack = tc_stk![Type::new_pair(Type::Nat, Type::Int)];
        assert_eq!(
            typecheck_instruction(&parse("UNPAIR").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Unpair)
        );
        assert_eq!(stack, tc_stk![Type::Int, Type::Nat]);
    }

    #[test]
    fn pair_car() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(
                &parse("{ PAIR; CAR }").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(IfNone(vec![Push(TypedValue::Int(5))], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_cons() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_CONS { DROP 2 } {}").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(IfCons(vec![Drop(Some(2))], vec![]))
        );
        assert_eq!(stack, tc_stk![]);
        too_short_test(&app!(IF_CONS[seq!{}, seq!{}]), Prim::IF_CONS, 1)
    }

    #[test]
    fn if_cons_mismatch() {
        let mut stack = tc_stk![Type::String];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_CONS { DROP 2 } {}").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(IfLeft(vec![Gt], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn if_left_too_short() {
        too_short_test(&app!(IF_LEFT[seq!{}, seq!{}]), Prim::IF_LEFT, 1)
    }

    #[test]
    fn if_left_same_branch_ty() {
        let mut stack = tc_stk![Type::new_or(Type::Int, Type::Int)];
        assert_eq!(
            typecheck_instruction(
                &parse("IF_LEFT {} {}").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
            typecheck_instruction(&parse("SOME").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(ISome)
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn none() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(&parse("NONE int").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Instruction::None)
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn compare_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("COMPARE").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Compare)
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn compare_int_fail() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("COMPARE").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(&parse("AMOUNT").unwrap(), &mut Ctx::default(), &mut stack),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::List(
                vec![TypedValue::Int(1), TypedValue::Int(2), TypedValue::Int(3),].into()
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
                &mut Ctx::default(),
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
            typecheck_instruction(&parse("NIL int").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Nil)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Cons)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons_too_short() {
        too_short_test(&app!(CONS), Prim::CONS, 2);
    }

    #[test]
    fn cons_mismatch_elt() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into())
        );
    }

    #[test]
    fn cons_mismatch_list() {
        let mut stack = tc_stk![Type::String, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
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
                &mut Ctx::default(),
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
            typecheck_instruction(&parse("FAILWITH").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(&parse("NEVER").unwrap(), &mut Ctx::default(), &mut stack),
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
    fn push_set() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { 1; 2 }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::Set(BTreeSet::from([
                TypedValue::Int(1),
                TypedValue::Int(2)
            ]))))
        );
        assert_eq!(stack, tc_stk![Type::new_set(Type::Int)]);
    }

    #[test]
    fn push_set_unsorted() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (set int) { 2; 1 }"#).unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Push(TypedValue::Map(BTreeMap::from([
                (TypedValue::Int(1), TypedValue::String("foo".to_owned())),
                (TypedValue::Int(2), TypedValue::String("bar".to_owned()))
            ]))))
        );
        assert_eq!(stack, tc_stk![Type::new_map(Type::Int, Type::String)]);
    }

    #[test]
    fn push_map_unsorted() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                &parse(r#"PUSH (map int string) { Elt 2 "foo"; Elt 1 "bar" }"#).unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::DuplicateElements(Type::new_map(
                Type::Int,
                Type::String
            )))
        );
    }

    #[test]
    fn get_map() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("GET").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Get(overloads::Get::Map))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn get_map_incomparable() {
        assert_eq!(
            parse("GET").unwrap().typecheck_instruction(
                &mut Ctx::default(),
                None,
                &[
                    app!(map[app!(list[app!(int)]), app!(string)]),
                    app!(list[app!(int)]),
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
            typecheck_instruction(&parse("GET").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into()),
        );
    }

    #[test]
    fn update_set() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Bool, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Update(overloads::Update::Set))
        );
        assert_eq!(stack, tc_stk![Type::new_set(Type::Int)]);
    }

    #[test]
    fn update_set_wrong_ty() {
        let mut stack = tc_stk![Type::new_set(Type::Int), Type::Bool, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into())
        );
    }

    #[test]
    fn update_set_incomparable() {
        assert_eq!(
            parse("UPDATE").unwrap().typecheck_instruction(
                &mut Ctx::default(),
                None,
                &[
                    app!(set[app!(list[app!(int)])]),
                    app!(bool),
                    app!(list[app!(int)])
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
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Update(overloads::Update::Map))
        );
        assert_eq!(stack, tc_stk![Type::new_map(Type::Int, Type::String)]);
    }

    #[test]
    fn update_map_wrong_ty() {
        let mut stack = tc_stk![
            Type::new_map(Type::Int, Type::String),
            Type::new_option(Type::Nat),
            Type::Int
        ];
        assert_eq!(
            typecheck_instruction(&parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::String, Type::Nat).into())
        );
    }

    #[test]
    fn update_map_incomparable() {
        assert_eq!(
            parse("UPDATE").unwrap().typecheck_instruction(
                &mut Ctx::default(),
                None,
                &[
                    app!(map[app!(list[app!(int)]), app!(string)]),
                    app!(option[app!(string)]),
                    app!(list[app!(int)]),
                ]
            ),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Int)
            ))
        );
    }

    #[test]
    fn seq() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(
                &parse("{ { PAIR }; {{ CAR; }}; {}; {{{}}}; {{{{{DROP}}}}} }").unwrap(),
                &mut Ctx::default(),
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
            typecheck_instruction(&parse("ADD").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Add(overloads::Add::IntNat))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("ADD").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Add(overloads::Add::NatInt))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[track_caller]
    fn too_short_test(instr: &Micheline, prim: Prim, len: usize) {
        for n in 0..len {
            let mut ctx = Ctx::default();
            assert_eq!(
                typecheck_instruction(instr, &mut ctx, &mut tc_stk![Type::Unit; n]),
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
        too_short_test(&app!(ADD), Prim::ADD, 2);
    }

    #[test]
    fn test_add_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(ADD), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(DUP[0]), &mut ctx, &mut stack),
            Err(TcError::Dup0)
        );
    }

    #[test]
    fn test_gt_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(GT), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::GT,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Int, Type::String).into())
            })
        );
    }

    #[test]
    fn test_eq_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(EQ), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::EQ,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Int, Type::String).into())
            })
        );
    }

    #[test]
    fn test_le_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(LE), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::LE,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Int, Type::String).into())
            })
        );
    }

    #[test]
    fn test_gt_short() {
        too_short_test(&app!(GT), Prim::GT, 1);
    }

    #[test]
    fn test_eq_short() {
        too_short_test(&app!(EQ), Prim::EQ, 1);
    }

    #[test]
    fn test_le_short() {
        too_short_test(&app!(LE), Prim::LE, 1);
    }

    #[test]
    fn test_if_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(IF[seq!{}, seq!{}]), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Bool, Type::String).into())
            })
        );
    }

    #[test]
    fn test_if_short() {
        too_short_test(&app!(IF[seq![], seq![]]), Prim::IF, 1);
    }

    #[test]
    fn test_if_none_short() {
        too_short_test(&app!(IF_NONE[seq![], seq![]]), Prim::IF_NONE, 1);
    }

    #[test]
    fn test_int_short() {
        too_short_test(&app!(INT), Prim::INT, 1);
    }

    #[test]
    fn test_int_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(INT), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::INT,
                stack: stk![Type::String],
                reason: None,
            })
        );
    }

    #[test]
    fn test_loop_short() {
        too_short_test(&app!(LOOP[seq![]]), Prim::LOOP, 1);
    }

    #[test]
    fn test_loop_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(LOOP[seq![]]), &mut ctx, &mut stack),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(UNPAIR), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UNPAIR,
                stack: stk![Type::String],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::String)),
            })
        );
    }

    #[test]
    fn test_swap_short() {
        too_short_test(&app!(SWAP), Prim::SWAP, 2);
    }

    #[test]
    fn test_pair_short() {
        too_short_test(&app!(PAIR), Prim::PAIR, 2);
    }

    #[test]
    fn test_get_short() {
        too_short_test(&app!(GET), Prim::GET, 2);
    }

    #[test]
    fn test_update_short() {
        too_short_test(&app!(UPDATE), Prim::UPDATE, 3);
    }

    #[test]
    fn test_failwith_short() {
        too_short_test(&app!(FAILWITH), Prim::FAILWITH, 1);
    }

    #[test]
    fn test_never_short() {
        too_short_test(&app!(NEVER), Prim::NEVER, 1);
    }

    #[test]
    fn test_car_short() {
        too_short_test(&app!(CAR), Prim::CAR, 1);
    }

    #[test]
    fn test_cdr_short() {
        too_short_test(&app!(CDR), Prim::CDR, 1);
    }

    #[test]
    fn test_some_short() {
        too_short_test(&app!(SOME), Prim::SOME, 1);
    }

    #[test]
    fn test_compare_short() {
        too_short_test(&app!(COMPARE), Prim::COMPARE, 2);
    }

    #[test]
    fn test_unpair_short() {
        too_short_test(&app!(UNPAIR), Prim::UNPAIR, 1);
    }

    #[test]
    fn test_compare_gas_exhaustion() {
        let mut ctx = &mut Ctx::default();
        ctx.gas = Gas::new(gas::tc_cost::INSTR_STEP);
        assert_eq!(
            typecheck_instruction(
                &app!(COMPARE),
                &mut ctx,
                &mut tc_stk![Type::Unit, Type::Unit]
            ),
            Err(TcError::OutOfGas(OutOfGas))
        );
    }

    #[test]
    fn test_compare_incomparable() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &app!(COMPARE),
                &mut ctx,
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(GET), &mut ctx, &mut tc_stk![Type::Unit, Type::Unit]),
            Err(TcError::NoMatchingOverload {
                instr: Prim::GET,
                stack: stk![Type::Unit, Type::Unit],
                reason: None,
            })
        );
    }

    #[test]
    fn test_update_mismatch() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &app!(UPDATE),
                &mut ctx,
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
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &app!(PUSH[app!(operation), app!(Unit)]),
                &mut ctx,
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
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter operation;",
                "storage nat;",
                "code FAILWITH"
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Passable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_non_storable_storage() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter nat;",
                "storage operation;",
                "code FAILWITH"
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Storable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_invalid_map() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (map (list unit) unit);",
                "storage nat;",
                "code FAILWITH;",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
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
            Micheline::Seq(&[])
                .typecheck_value(&mut ctx, &app!(map[app!(list[app!(unit)]), app!(unit)])),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (pair unit (option (map (list unit) unit)));",
                "storage nat;",
                "code FAILWITH",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Comparable,
                Type::new_list(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_contract_not_storable() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter unit;",
                "storage (contract unit);",
                "code FAILWITH;",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Storable,
                Type::new_contract(Type::Unit)
            ))
        );
    }

    #[test]
    fn test_contract_not_pushable() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &app!(PUSH[app!(contract[app!(unit)]), app!(Unit)]),
                &mut ctx,
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
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract operation);",
                "storage unit;",
                "code FAILWITH;"
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Passable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn test_contract_is_passable() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract unit);",
                "storage unit;",
                "code { DROP; UNIT; FAILWITH };",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Ok(ContractScript {
                parameter: Type::new_contract(Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![Drop(None), Unit, Failwith(Type::Unit)])
            })
        );
    }

    #[test]
    fn test_fail_with_contract_should_fail() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (contract unit);",
                "storage unit;",
                "code FAILWITH;",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
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
                    &mut Ctx::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
            assert_eq!(
                &typecheck_instruction(
                    &parse(&format!("PUSH address {bytes}")).unwrap(),
                    &mut Ctx::default(),
                    &mut tc_stk![],
                ),
                &exp
            );
        }
        fn hex<T: Into<AddressHash>>(con: fn(Vec<u8>) -> T, hex: &str, ep: &str) -> addr::Address {
            addr::Address {
                hash: con(hex::decode(hex).unwrap()).into(),
                entrypoint: Entrypoint::try_from(ep).unwrap(),
            }
        }
        use tezos_crypto_rs::hash::*;
        // hex representations are obtained via `octez-client hash data`
        test_ok(
            r#""tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j""#,
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef",
            hex(
                ContractTz1Hash,
                "7b09f782e0bcd67739510afa819d85976119d5ef",
                "default",
            ),
        );
        test_ok(
            r#""tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH""#,
            "0x00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
            hex(
                ContractTz2Hash,
                "0a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
                "default",
            ),
        );
        test_ok(
            r#""tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r""#,
            "0x00025cfa532f50de3e12befc0ad21603835dd7698d35",
            hex(
                ContractTz3Hash,
                "5cfa532f50de3e12befc0ad21603835dd7698d35",
                "default",
            ),
        );
        test_ok(
            r#""tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN""#,
            "0x00036342f30484dd46b6074373aa6ddca9dfb70083d6",
            hex(
                ContractTz4Hash,
                "6342f30484dd46b6074373aa6ddca9dfb70083d6",
                "default",
            ),
        );
        test_ok(
            r#""KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye""#,
            "0x011f2d825fdd9da219235510335e558520235f4f5400",
            hex(
                ContractKt1Hash,
                "1f2d825fdd9da219235510335e558520235f4f54",
                "default",
            ),
        );
        test_ok(
            r#""sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf""#,
            "0x03d601f22256d2ad1faec0c64374e527c6e62f2e5a00",
            hex(
                SmartRollupHash,
                "d601f22256d2ad1faec0c64374e527c6e62f2e5a",
                "default",
            ),
        );
        // with entrypoints
        test_ok(
            r#""tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j%foo""#,
            "0x00007b09f782e0bcd67739510afa819d85976119d5ef666f6f",
            hex(
                ContractTz1Hash,
                "7b09f782e0bcd67739510afa819d85976119d5ef",
                "foo",
            ),
        );
        test_ok(
            r#""tz29EDhZ4D3XueHxm5RGZsJLHRtj3qSA2MzH%foo""#,
            "0x00010a053e3d8b622a993d3182e3f6cc5638ff5f12fe666f6f",
            hex(
                ContractTz2Hash,
                "0a053e3d8b622a993d3182e3f6cc5638ff5f12fe",
                "foo",
            ),
        );
        test_ok(
            r#""tz3UoffC7FG7zfpmvmjUmUeAaHvzdcUvAj6r%foo""#,
            "0x00025cfa532f50de3e12befc0ad21603835dd7698d35666f6f",
            hex(
                ContractTz3Hash,
                "5cfa532f50de3e12befc0ad21603835dd7698d35",
                "foo",
            ),
        );
        test_ok(
            r#""tz4J46gb6DxDFYxkex8k9sKiYZwjuiaoNSqN%foo""#,
            "0x00036342f30484dd46b6074373aa6ddca9dfb70083d6666f6f",
            hex(
                ContractTz4Hash,
                "6342f30484dd46b6074373aa6ddca9dfb70083d6",
                "foo",
            ),
        );
        test_ok(
            r#""KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo""#,
            "0x011f2d825fdd9da219235510335e558520235f4f5400666f6f",
            hex(
                ContractKt1Hash,
                "1f2d825fdd9da219235510335e558520235f4f54",
                "foo",
            ),
        );
        test_ok(
            r#""sr1RYurGZtN8KNSpkMcCt9CgWeUaNkzsAfXf%foo""#,
            "0x03d601f22256d2ad1faec0c64374e527c6e62f2e5a00666f6f",
            hex(
                SmartRollupHash,
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(Type::Address, ByteReprError::UnknownPrefix(s))) if s == "tz9"
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address \"tz\"").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(Type::Address, ByteReprError::UnknownPrefix(p))) if p == "0xff"
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x00fffe0000000000000000000000000000000000000000").unwrap(),
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ByteReprError(Type::Address, ByteReprError::UnknownPrefix(p))) if p == "0xff"
        );
        assert_matches!(
            typecheck_instruction(
                &parse("PUSH address 0x00").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
        let exp = Ok(Push(TypedValue::ChainId(super::ChainId(exp))));
        let lit = "NetXynUjJNZm7wi";
        assert_eq!(
            &typecheck_instruction(
                &parse(&format!("PUSH chain_id \"{}\"", lit)).unwrap(),
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            &exp
        );
        assert_eq!(
            &typecheck_instruction(
                &parse(&format!("PUSH chain_id 0x{}", bytes)).unwrap(),
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            &exp
        );
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH chain_id \"foobar\"").unwrap(),
                &mut Ctx::default(),
                &mut tc_stk![],
            ),
            Err(TcError::ChainIdError(
                tezos_crypto_rs::base58::FromBase58CheckError::InvalidChecksum.into()
            ))
        );
        assert_eq!(
            typecheck_instruction(
                &parse("PUSH chain_id 0xbeef").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                &mut tc_stk![]
            ),
            Ok(Instruction::ChainId)
        );
    }

    #[test]
    fn pack_instr() {
        let stk = &mut tc_stk![Type::new_pair(Type::Int, Type::Unit)];
        assert_eq!(
            super::typecheck_instruction(&parse("PACK").unwrap(), &mut Ctx::default(), None, stk),
            Ok(Instruction::Pack)
        );
        assert_eq!(stk, &tc_stk![Type::Bytes]);
    }

    #[test]
    fn pack_instr_non_packable() {
        assert_eq!(
            typecheck_instruction(
                &parse("PACK").unwrap(),
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
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
                &mut Ctx::default(),
                Some(&[(Entrypoint::default(), Type::Nat)].into()),
                stk
            ),
            Err(AnnotationError::TooManyFieldAnns("baz".into()).into())
        );
    }

    #[test]
    fn self_instr_contract() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %foo) (unit %default));",
                "storage unit;",
                "code { DROP; SELF %foo; UNIT; FAILWITH };",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Ok(ContractScript {
                parameter: Type::new_or(Type::Int, Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![
                    Drop(None),
                    ISelf("foo".try_into().unwrap()),
                    Unit,
                    Failwith(Type::Unit)
                ])
            })
        );
    }

    #[test]
    fn self_instr_contract_overlong_ep() {
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq) (unit %default));",
                "storage unit;",
                "code { DROP; SELF %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq; UNIT; FAILWITH };",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
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
        let mut ctx = Ctx::default();
        assert_eq!(
            parse_contract_script(concat!(
                "parameter (or (int %qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq) (unit %default));",
                "storage unit;",
                "code { DROP; SELF; UNIT; FAILWITH };",
            ))
            .unwrap()
            .typecheck_script(&mut ctx),
            Ok(ContractScript {
                parameter: Type::new_or(Type::Int, Type::Unit),
                storage: Type::Unit,
                code: Seq(vec![
                    Drop(None),
                    ISelf("default".try_into().unwrap()),
                    Unit,
                    Failwith(Type::Unit)
                ])
            })
        );
    }

    #[test]
    fn address_instr() {
        let stk = &mut tc_stk![Type::new_contract(Type::Nat)];
        assert_eq!(
            typecheck_instruction(&parse("ADDRESS").unwrap(), &mut Ctx::default(), stk),
            Ok(Instruction::Address)
        );
        assert_eq!(stk, &tc_stk![Type::Address]);
    }

    #[test]
    fn test_address_short() {
        too_short_test(&app!(ADDRESS), Prim::ADDRESS, 1);
    }

    #[test]
    fn test_address_mismatch() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(&app!(ADDRESS), &mut ctx, &mut tc_stk![Type::Unit]),
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
            typecheck_instruction(&parse("LEFT nat").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Left)
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Int, Type::Nat)]);
    }

    #[test]
    fn left_too_short() {
        too_short_test(&app!(LEFT["nat"]), Prim::LEFT, 1);
    }

    #[test]
    fn right() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck_instruction(
                &parse("RIGHT nat").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Right)
        );
        assert_eq!(stack, tc_stk![Type::new_or(Type::Nat, Type::Int)]);
    }

    #[test]
    fn right_too_short() {
        too_short_test(&app!(RIGHT["nat"]), Prim::RIGHT, 1);
    }

    #[test]
    fn read_top_level() {
        use crate::lexer::Prim::{code, parameter, storage};
        use TcError as Err;

        let go = |s| {
            parse_contract_script(s)
                .unwrap()
                .typecheck_script(&mut Ctx::default())
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
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
            Err(TcError::ExpectedU10(1025))
        );
        assert_eq!(
            parse("DIP 1024 {}")
                .unwrap()
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
            Err(TcError::ExpectedU10(1024))
        );
        assert_eq!(
            parse("DUP 65536")
                .unwrap()
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
            Err(TcError::ExpectedU10(65536))
        );
    }

    #[test]
    fn push_bytes() {
        assert_eq!(
            parse("PUSH bytes 0xdeadf00d")
                .unwrap()
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
            Ok(Push(TypedValue::Bytes(hex::decode("deadf00d").unwrap())))
        );
    }

    #[test]
    fn push_key() {
        assert_eq!(
            parse("PUSH key \"p2pk67K1dwkDFPB63RZU5H3SoMCvmJdKZDZszc7U4FiGKN2YypKdDCB\"")
                .unwrap()
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
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
            .typecheck_instruction(&mut Ctx::default(), None, &[]),
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
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
            Ok(Push(TypedValue::KeyHash(
                "tz1Nw5nr152qddEjKT2dKBH8XcBMDAg72iLw".try_into().unwrap()
            )))
        );
        assert_eq!(
            parse("PUSH key_hash 0x036342f30484dd46b6074373aa6ddca9dfb70083d6")
                .unwrap()
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
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
                .typecheck_instruction(&mut Ctx::default(), None, &[]),
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
            .typecheck_instruction(&mut Ctx::default(), None, &[]),
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
                &mut Ctx::default(),
                None,
                &[app!(bytes), app!(signature), app!(key)]
            ),
            Ok(CheckSignature)
        );
    }

    #[test]
    fn check_signature_wrong_type() {
        assert_eq!(
            parse("CHECK_SIGNATURE").unwrap().typecheck_instruction(
                &mut Ctx::default(),
                None,
                &[app!(bytes), app!(key), app!(key)]
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
        too_short_test(&app!(CHECK_SIGNATURE), Prim::CHECK_SIGNATURE, 3)
    }

    #[test]
    fn transfer_tokens() {
        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Mutez, Type::Int];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Ctx::default(), stk),
            Err(TypesNotEqual(Type::Nat, Type::Int).into())
        );

        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Ctx::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::new_contract(Type::Nat), Type::Int, Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::Nat, Type::Mutez, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Ctx::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::Nat, Type::Mutez, Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Ctx::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::TRANSFER_TOKENS,
                stack: stk![Type::Nat],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 3 })
            })
        );

        let stk = &mut tc_stk![Type::new_contract(Type::Nat), Type::Mutez, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("TRANSFER_TOKENS").unwrap(), &mut Ctx::default(), stk),
            Ok(Instruction::TransferTokens)
        );
        assert_eq!(stk, &tc_stk![Type::Operation]);
    }

    #[test]
    fn set_delegate() {
        let stk = &mut tc_stk![Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Ctx::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SET_DELEGATE,
                stack: stk![Type::Nat],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::new_option(Type::Nat)];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Ctx::default(), stk),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SET_DELEGATE,
                stack: stk![Type::new_option(Type::Nat)],
                reason: None
            })
        );

        let stk = &mut tc_stk![Type::new_option(Type::KeyHash)];
        assert_eq!(
            typecheck_instruction(&parse("SET_DELEGATE").unwrap(), &mut Ctx::default(), stk),
            Ok(Instruction::SetDelegate)
        );
        assert_eq!(stk, &tc_stk![Type::Operation]);
    }

    #[test]
    fn slice_string() {
        let stk = &mut tc_stk![Type::String, Type::Nat, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SLICE").unwrap(), &mut Ctx::default(), stk),
            Ok(Instruction::Slice(overloads::Slice::String))
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn slice_bytes() {
        let stk = &mut tc_stk![Type::Bytes, Type::Nat, Type::Nat];
        assert_eq!(
            typecheck_instruction(&parse("SLICE").unwrap(), &mut Ctx::default(), stk),
            Ok(Instruction::Slice(overloads::Slice::Bytes))
        );
        assert_eq!(stk, &tc_stk![Type::new_option(Type::Bytes)]);
    }

    #[test]
    fn slice_too_short() {
        too_short_test(&app!(SLICE), Prim::SLICE, 3);
    }

    #[test]
    fn slice_wrong_type() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                &app!(SLICE),
                &mut ctx,
                &mut tc_stk![Type::Bool, Type::Nat, Type::Nat]
            ),
            Err(TcError::NoMatchingOverload {
                instr: Prim::SLICE,
                stack: stk![Type::Bool, Type::Nat, Type::Nat],
                reason: None,
            })
        );
    }
}
