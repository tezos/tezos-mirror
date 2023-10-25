/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::BTreeMap;
use std::num::TryFromIntError;

pub mod type_props;

use type_props::TypeProperty;

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
    #[error("value {0:?} is invalid for type {1:?}")]
    InvalidValueForType(Value, Type),
    #[error("value {0:?} is invalid element for container type {1:?}")]
    InvalidEltForMap(Value, Type),
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

impl ContractScript<ParsedStage> {
    /// Typecheck the contract script. Validates the script's types, then
    /// typechecks the code and checks the result stack is as expected. Returns
    /// typechecked script.
    pub fn typecheck(
        self,
        ctx: &mut crate::context::Ctx,
    ) -> Result<ContractScript<TypecheckedStage>, crate::typechecker::TcError> {
        let ContractScript {
            parameter, storage, ..
        } = self;
        parameter.ensure_prop(&mut ctx.gas, TypeProperty::Passable)?;
        storage.ensure_prop(&mut ctx.gas, TypeProperty::Storable)?;
        let mut stack = tc_stk![Type::new_pair(parameter.clone(), storage.clone())];
        let code = self.code.typecheck(ctx, &mut stack)?;
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

impl ParsedInstruction {
    /// Typecheck an individual instruction. Validates the passed stack types.
    pub fn typecheck(
        self,
        ctx: &mut Ctx,
        opt_stack: &mut FailingTypeStack,
    ) -> Result<TypecheckedInstruction, TcError> {
        if let Ok(stack) = opt_stack.access_mut(()) {
            stack.iter().try_for_each(|ty| verify_ty(ctx, ty))?;
        }
        typecheck_instruction(self, ctx, opt_stack)
    }
}

impl Value {
    /// Typecheck a value. Validates the input type.
    pub fn typecheck(self, ctx: &mut Ctx, t: &Type) -> Result<TypedValue, TcError> {
        verify_ty(ctx, t)?;
        typecheck_value(ctx, t, self)
    }
}

/// Checks type invariants, e.g. that `map` key is comparable.
fn verify_ty(ctx: &mut Ctx, t: &Type) -> Result<(), TcError> {
    use Type::*;
    ctx.gas.consume(gas::tc_cost::VERIFY_TYPE_STEP)?;
    match t {
        Nat | Int | Bool | Mutez | String | Operation | Unit => Ok(()),
        Pair(tys) | Or(tys) => {
            verify_ty(ctx, &tys.0)?;
            verify_ty(ctx, &tys.1)
        }
        Option(ty) | List(ty) => verify_ty(ctx, ty),
        Map(tys) => {
            tys.0.ensure_prop(&mut ctx.gas, TypeProperty::Comparable)?;
            verify_ty(ctx, &tys.0)?;
            verify_ty(ctx, &tys.1)
        }
        Contract(ty) => {
            // NB: despite `contract` type being duplicable and packable, its
            // argument doesn't need to be. The only constraint is that it needs
            // to be passable, as it represents the contract's parameter type.
            // See https://tezos.gitlab.io/michelson-reference/#type-contract
            ty.ensure_prop(&mut ctx.gas, TypeProperty::Passable)?;
            verify_ty(ctx, ty)
        }
    }
}

/// Typecheck a sequence of instructions. Assumes the passed stack is valid, i.e.
/// doesn't contain illegal types like `set operation` or `contract operation`.
fn typecheck(
    ast: ParsedAST,
    ctx: &mut Ctx,
    opt_stack: &mut FailingTypeStack,
) -> Result<TypecheckedAST, TcError> {
    ast.into_iter()
        .map(|i| typecheck_instruction(i, ctx, opt_stack))
        .collect()
}

macro_rules! nothing_to_none {
    () => {
        None
    };
    ($e:expr) => {
        Some($e)
    };
}

/// Typecheck a single instruction. Assumes passed stack is valid, i.e. doesn't
/// contain illegal types like `set operation` or `contract operation`.
fn typecheck_instruction(
    i: ParsedInstruction,
    ctx: &mut Ctx,
    opt_stack: &mut FailingTypeStack,
) -> Result<TypecheckedInstruction, TcError> {
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
                    reason: Some(NoMatchingOverloadReason::StackTooShort {
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

    ctx.gas.consume(gas::tc_cost::INSTR_STEP)?;

    Ok(match (i, stack.as_slice()) {
        (I::Add(..), [.., T::Nat, T::Nat]) => {
            pop!();
            I::Add(overloads::Add::NatNat)
        }
        (I::Add(..), [.., T::Int, T::Int]) => {
            pop!();
            I::Add(overloads::Add::IntInt)
        }
        (I::Add(..), [.., T::Nat, T::Int]) => {
            pop!();
            stack[0] = T::Int;
            I::Add(overloads::Add::IntNat)
        }
        (I::Add(..), [.., T::Int, T::Nat]) => {
            pop!();
            I::Add(overloads::Add::NatInt)
        }
        (I::Add(..), [.., T::Mutez, T::Mutez]) => {
            pop!();
            I::Add(overloads::Add::MutezMutez)
        }
        (I::Add(..), [.., _, _]) => no_overload!(ADD),
        (I::Add(..), [_] | []) => no_overload!(ADD, len 2),

        (I::Dip(opt_height, nested), ..) => {
            let protected_height = opt_height.unwrap_or(1) as usize;

            ctx.gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;

            ensure_stack_len(Prim::DIP, stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, ctx, opt_stack)?;
            opt_stack
                .access_mut(TcError::FailNotInTail)?
                .append(&mut protected);
            I::Dip(opt_height, nested)
        }

        (I::Drop(opt_height), ..) => {
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            ctx.gas.consume(gas::tc_cost::drop_n(&opt_height)?)?;
            ensure_stack_len(Prim::DROP, stack, drop_height)?;
            stack.drop_top(drop_height);
            I::Drop(opt_height)
        }

        // DUP instruction requires an argument that is > 0.
        (I::Dup(Some(0)), ..) => return Err(TcError::Dup0),
        (I::Dup(opt_height), ..) => {
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            ensure_stack_len(Prim::DUP, stack, dup_height)?;
            let ty = &stack[dup_height - 1];
            ty.ensure_prop(&mut ctx.gas, TypeProperty::Duplicable)?;
            stack.push(ty.clone());
            I::Dup(opt_height)
        }

        (I::Gt, [.., T::Int]) => {
            stack[0] = T::Bool;
            I::Gt
        }
        (I::Gt, [.., t]) => no_overload!(GT, TypesNotEqual(T::Int, t.clone())),
        (I::Gt, []) => no_overload!(GT, len 1),

        (I::If(nested_t, nested_f), [.., T::Bool]) => {
            // pop the bool off the stack
            pop!();
            // Clone the stack so that we have a copy to run one branch on.
            // We can run the other branch on the live stack.
            let mut f_opt_stack = opt_stack.clone();
            let nested_t = typecheck(nested_t, ctx, opt_stack)?;
            let nested_f = typecheck(nested_f, ctx, &mut f_opt_stack)?;
            // If stacks unify after typecheck, all is good.
            unify_stacks(ctx, opt_stack, f_opt_stack)?;
            I::If(nested_t, nested_f)
        }
        (I::If(..), [.., t]) => no_overload!(IF, TypesNotEqual(T::Bool, t.clone())),
        (I::If(..), []) => no_overload!(IF, len 1),

        (I::IfNone(when_none, when_some), [.., T::Option(..)]) => {
            // Extract option type
            let ty = pop!(T::Option);
            // Clone the some_stack as we need to push a type on top of it
            let mut some_stack: TypeStack = stack.clone();
            some_stack.push(*ty);
            let mut some_opt_stack = FailingTypeStack::Ok(some_stack);
            let when_none = typecheck(when_none, ctx, opt_stack)?;
            let when_some = typecheck(when_some, ctx, &mut some_opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, some_opt_stack)?;
            I::IfNone(when_none, when_some)
        }
        (I::IfNone(..), [.., t]) => no_overload!(IF_NONE, NMOR::ExpectedOption(t.clone())),
        (I::IfNone(..), []) => no_overload!(IF_NONE, len 1),

        (I::IfCons(when_cons, when_nil), [.., T::List(..)]) => {
            // Clone the cons_stack as we need to push a type on top of it
            let mut cons_stack: TypeStack = stack.clone();
            // get the list element type
            let ty = pop!(T::List);
            // push it to the cons stack
            cons_stack.push(*ty);
            let mut cons_opt_stack = FailingTypeStack::Ok(cons_stack);
            let when_cons = typecheck(when_cons, ctx, &mut cons_opt_stack)?;
            let when_nil = typecheck(when_nil, ctx, opt_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, cons_opt_stack)?;
            I::IfCons(when_cons, when_nil)
        }
        (I::IfCons(..), [.., t]) => no_overload!(IF_CONS, NMOR::ExpectedList(t.clone())),
        (I::IfCons(..), []) => no_overload!(IF_CONS, len 1),

        (I::IfLeft(when_left, when_right), [.., T::Or(..)]) => {
            // get the list element type
            let (tl, tr) = *pop!(T::Or);
            // use main stack as left branch, cloned stack as right
            let mut right_stack = stack.clone();
            stack.push(tl);
            right_stack.push(tr);
            let mut opt_right_stack = FailingTypeStack::Ok(right_stack);
            let when_left = typecheck(when_left, ctx, opt_stack)?;
            let when_right = typecheck(when_right, ctx, &mut opt_right_stack)?;
            // If stacks unify, all is good
            unify_stacks(ctx, opt_stack, opt_right_stack)?;
            I::IfLeft(when_left, when_right)
        }
        (I::IfLeft(..), [.., t]) => no_overload!(IF_LEFT, NMOR::ExpectedOr(t.clone())),
        (I::IfLeft(..), []) => no_overload!(IF_LEFT, len 1),

        (I::Int, [.., T::Nat]) => {
            stack[0] = Type::Int;
            I::Int
        }
        (I::Int, [.., _]) => no_overload!(INT),
        (I::Int, []) => no_overload!(INT, len 1),

        (I::Loop(nested), [.., T::Bool]) => {
            // copy stack for unifying with it later
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            // Pop the bool off the top
            pop!();
            // Typecheck body with the current stack
            let nested = typecheck(nested, ctx, opt_stack)?;
            // If the starting stack and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_copy)?;
            // pop the remaining bool off (if not failed)
            opt_stack.access_mut(()).ok().map(Stack::pop);
            I::Loop(nested)
        }
        (I::Loop(..), [.., ty]) => no_overload!(LOOP, TypesNotEqual(T::Bool, ty.clone())),
        (I::Loop(..), []) => no_overload!(LOOP, len 1),

        (I::Iter(.., nested), [.., T::List(..)]) => {
            // get the list element type
            let ty = *pop!(T::List);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(ty);
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, ctx, &mut opt_inner_stack)?;
            // If the starting stack (sans list) and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::List, nested)
        }
        (I::Iter(.., nested), [.., T::Map(..)]) => {
            // get the map element type
            let kty_vty_box = pop!(T::Map);
            // clone the rest of the stack
            let mut inner_stack = stack.clone();
            // push the element type to the top of the inner stack and typecheck
            inner_stack.push(T::Pair(kty_vty_box));
            let mut opt_inner_stack = FailingTypeStack::Ok(inner_stack);
            let nested = typecheck(nested, ctx, &mut opt_inner_stack)?;
            // If the starting stack (sans map) and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_inner_stack)?;
            I::Iter(overloads::Iter::Map, nested)
        }
        (I::Iter(..), [.., _]) => no_overload!(ITER),
        (I::Iter(..), []) => no_overload!(ITER, len 1),

        (I::Push((t, v)), ..) => {
            t.ensure_prop(&mut ctx.gas, TypeProperty::Pushable)?;
            verify_ty(ctx, &t)?;
            let v = typecheck_value(ctx, &t, v)?;
            stack.push(t);
            I::Push(v)
        }

        (I::Swap, [.., _, _]) => {
            stack.swap(0, 1);
            I::Swap
        }
        (I::Swap, [] | [_]) => no_overload!(SWAP, len 2),

        (I::Failwith(..), [.., _]) => {
            let ty = pop!();
            ty.ensure_prop(&mut ctx.gas, TypeProperty::Packable)?;
            // mark stack as failed
            *opt_stack = FailingTypeStack::Failed;
            I::Failwith(ty)
        }
        (I::Failwith(..), []) => no_overload!(FAILWITH, len 1),

        (I::Unit, ..) => {
            stack.push(T::Unit);
            I::Unit
        }

        (I::Car, [.., T::Pair(..)]) => {
            let (l, _) = *pop!(T::Pair);
            stack.push(l);
            I::Car
        }
        (I::Car, [.., ty]) => no_overload!(CAR, NMOR::ExpectedPair(ty.clone())),
        (I::Car, []) => no_overload!(CAR, len 1),

        (I::Cdr, [.., T::Pair(..)]) => {
            let (_, r) = *pop!(T::Pair);
            stack.push(r);
            I::Cdr
        }
        (I::Cdr, [.., ty]) => no_overload!(CDR, NMOR::ExpectedPair(ty.clone())),
        (I::Cdr, []) => no_overload!(CDR, len 1),

        (I::Pair, [.., _, _]) => {
            let (l, r) = (pop!(), pop!());
            stack.push(Type::new_pair(l, r));
            I::Pair
        }
        (I::Pair, [] | [_]) => no_overload!(PAIR, len 2),

        (I::Unpair, [.., T::Pair(..)]) => {
            let (l, r) = *pop!(T::Pair);
            stack.push(r);
            stack.push(l);
            I::Unpair
        }
        (I::Unpair, [.., ty]) => no_overload!(UNPAIR, NMOR::ExpectedPair(ty.clone())),
        (I::Unpair, []) => no_overload!(UNPAIR, len 1),

        (I::ISome, [.., _]) => {
            let ty = pop!();
            stack.push(T::new_option(ty));
            I::ISome
        }
        (I::ISome, []) => no_overload!(SOME, len 1),

        (I::Compare, [.., u, t]) => {
            ensure_ty_eq(ctx, t, u).map_err(|e| match e {
                TcError::TypesNotEqual(e) => TcError::NoMatchingOverload {
                    instr: Prim::COMPARE,
                    stack: stack.clone(),
                    reason: Some(e.into()),
                },
                e => e,
            })?;
            t.ensure_prop(&mut ctx.gas, TypeProperty::Comparable)?;
            pop!();
            stack[0] = T::Int;
            I::Compare
        }
        (I::Compare, [] | [_]) => no_overload!(COMPARE, len 2),

        (I::Amount, ..) => {
            stack.push(T::Mutez);
            I::Amount
        }

        (I::Nil(ty), ..) => {
            verify_ty(ctx, &ty)?;
            stack.push(T::new_list(ty));
            I::Nil(())
        }

        (I::Cons, [.., T::List(ty1), ty2]) => {
            ensure_ty_eq(ctx, ty1, ty2)?;
            pop!();
            I::Cons
        }
        (I::Cons, [.., ty, _]) => no_overload!(CONS, NMOR::ExpectedList(ty.clone())),
        (I::Cons, [] | [_]) => no_overload!(CONS, len 2),

        (I::Get(..), [.., T::Map(..), _]) => {
            let kty_ = pop!();
            let (kty, vty) = *pop!(T::Map);
            ensure_ty_eq(ctx, &kty, &kty_)?;
            stack.push(T::new_option(vty));
            I::Get(overloads::Get::Map)
        }
        (I::Get(..), [.., _, _]) => no_overload!(GET),
        (I::Get(..), [] | [_]) => no_overload!(GET, len 2),

        (I::Update(..), [.., T::Map(m), T::Option(vty_new), kty_]) => {
            let (kty, vty) = m.as_ref();
            ensure_ty_eq(ctx, kty, kty_)?;
            ensure_ty_eq(ctx, vty, vty_new)?;
            pop!();
            pop!();
            I::Update(overloads::Update::Map)
        }
        (I::Update(..), [.., _, _, _]) => no_overload!(UPDATE),
        (I::Update(..), [] | [_] | [_, _]) => no_overload!(UPDATE, len 3),

        (I::Seq(nested), ..) => I::Seq(typecheck(nested, ctx, opt_stack)?),
    })
}

/// Typecheck a value. Assumes passed the type is valid, i.e. doesn't contain
/// illegal types like `set operation` or `contract operation`.
fn typecheck_value(ctx: &mut Ctx, t: &Type, v: Value) -> Result<TypedValue, TcError> {
    use Type::*;
    use TypedValue as TV;
    use Value as V;
    ctx.gas.consume(gas::tc_cost::VALUE_STEP)?;
    Ok(match (t, v) {
        (Nat, V::Number(n)) => TV::Nat(n.try_into()?),
        (Int, V::Number(n)) => TV::Int(n),
        (Bool, V::Boolean(b)) => TV::Bool(b),
        (Mutez, V::Number(n)) if n >= 0 => TV::Mutez(n.try_into()?),
        (String, V::String(s)) => TV::String(s),
        (Unit, V::Unit) => TV::Unit,
        (Pair(pt), V::Pair(pv)) => {
            let (tl, tr) = pt.as_ref();
            let (vl, vr) = *pv;
            let l = typecheck_value(ctx, tl, vl)?;
            let r = typecheck_value(ctx, tr, vr)?;
            TV::new_pair(l, r)
        }
        (Or(ot), V::Or(val)) => {
            let (tl, tr) = ot.as_ref();
            let typed_val = match *val {
                crate::ast::Or::Left(lv) => crate::ast::Or::Left(typecheck_value(ctx, tl, lv)?),
                crate::ast::Or::Right(rv) => crate::ast::Or::Right(typecheck_value(ctx, tr, rv)?),
            };
            TV::new_or(typed_val)
        }
        (Option(ty), V::Option(v)) => match v {
            Some(v) => {
                let v = typecheck_value(ctx, ty, *v)?;
                TV::new_option(Some(v))
            }
            None => TV::new_option(None),
        },
        (List(ty), V::Seq(vs)) => TV::List(
            vs.into_iter()
                .map(|v| typecheck_value(ctx, ty, v))
                .collect::<Result<_, TcError>>()?,
        ),
        (Map(m), V::Seq(vs)) => {
            let (tk, tv) = m.as_ref();
            let tc_elt = |v: Value| -> Result<(TypedValue, TypedValue), TcError> {
                match v {
                    Value::Elt(e) => {
                        let (k, v) = *e;
                        let k = typecheck_value(ctx, tk, k)?;
                        let v = typecheck_value(ctx, tv, v)?;
                        Ok((k, v))
                    }
                    _ => Err(TcError::InvalidEltForMap(v, t.clone())),
                }
            };
            let elts: Vec<(TypedValue, TypedValue)> =
                vs.into_iter().map(tc_elt).collect::<Result<_, TcError>>()?;
            if elts.len() > 1 {
                let mut prev = &elts[0].0;
                for i in &elts[1..] {
                    ctx.gas.consume(gas::interpret_cost::compare(prev, &i.0)?)?;
                    match prev.cmp(&i.0) {
                        std::cmp::Ordering::Less => (),
                        std::cmp::Ordering::Equal => {
                            return Err(TcError::DuplicateElements(t.clone()))
                        }
                        std::cmp::Ordering::Greater => {
                            return Err(TcError::ElementsNotSorted(t.clone()))
                        }
                    }
                    prev = &i.0;
                }
            }
            ctx.gas
                .consume(gas::tc_cost::construct_map(tk.size_for_gas(), elts.len())?)?;
            // Unfortunately, `BTreeMap` doesn't expose methods to build from an already-sorted
            // slice/vec/iterator. FWIW, Rust docs claim that its sorting algorithm is "designed to
            // be very fast in cases where the slice is nearly sorted", so hopefully it doesn't add
            // too much overhead.
            let map: BTreeMap<TypedValue, TypedValue> = elts.into_iter().collect();
            TV::Map(map)
        }
        (t, v) => return Err(TcError::InvalidValueForType(v, t.clone())),
    })
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
    use crate::gas::Gas;
    use crate::parser::*;
    use crate::typechecker::*;
    use Instruction::*;

    #[test]
    fn test_dup() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Nat, Type::Nat];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(Dup(Some(1)), &mut ctx, &mut stack),
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
            typecheck_instruction(Dup(Some(2)), &mut ctx, &mut stack),
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
        assert_eq!(typecheck_instruction(Swap, &mut ctx, &mut stack), Ok(Swap));
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_int() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![Type::Int];
        let mut ctx = Ctx::default();
        assert_eq!(typecheck_instruction(Int, &mut ctx, &mut stack), Ok(Int));
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_drop() {
        let mut stack = tc_stk![Type::Nat];
        let expected_stack = tc_stk![];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck(vec![Drop(None)], &mut ctx, &mut stack),
            Ok(vec![Drop(None)])
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
            typecheck_instruction(Drop(Some(2)), &mut ctx, &mut stack),
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
            typecheck_instruction(Push((Type::Int, Value::Number(1))), &mut ctx, &mut stack),
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
        assert_eq!(typecheck_instruction(Gt, &mut ctx, &mut stack), Ok(Gt));
        assert_eq!(stack, expected_stack);
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
    }

    #[test]
    fn test_dip() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let expected_stack = tc_stk![Type::Int, Type::Nat, Type::Bool];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(parse("DIP 1 {PUSH nat 6}").unwrap(), &mut ctx, &mut stack),
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
            typecheck_instruction(Add(()), &mut ctx, &mut stack),
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
            typecheck_instruction(Add(()), &mut ctx, &mut stack),
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
            typecheck_instruction(Add(()), &mut ctx, &mut stack),
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
                parse("LOOP {PUSH bool True}").unwrap(),
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
                parse("LOOP {PUSH int 1; PUSH bool True}").unwrap(),
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
                parse("LOOP {DROP; PUSH bool False; PUSH bool True}").unwrap(),
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
            typecheck_instruction(parse("ITER { DROP }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_too_short() {
        too_short_test(Iter((), vec![]), Prim::ITER, 1)
    }

    #[test]
    fn test_iter_list_inner_mismatch() {
        let mut stack = tc_stk![Type::new_list(Type::Int)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(parse("ITER { }").unwrap(), &mut ctx, &mut stack),
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
            typecheck_instruction(parse("ITER { CAR; DROP }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::Map, vec![Car, Drop(None)]))
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn test_iter_map_inner_mismatch() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::Nat)];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(parse("ITER { }").unwrap(), &mut ctx, &mut stack),
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
            typecheck_instruction(parse("ITER { DROP }").unwrap(), &mut ctx, &mut stack),
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
            typecheck_instruction(parse("ITER { FAILWITH }").unwrap(), &mut ctx, &mut stack),
            Ok(Iter(overloads::Iter::List, vec![Failwith(Type::Int)]))
        );
        assert_eq!(stack, tc_stk![])
    }

    #[test]
    fn test_failwith() {
        assert_eq!(
            typecheck_instruction(Failwith(()), &mut Ctx::default(), &mut tc_stk![Type::Int]),
            Ok(Failwith(Type::Int))
        );
    }

    #[test]
    fn test_failed_stacks() {
        macro_rules! test_fail {
            ($code:expr) => {
                assert_eq!(
                    typecheck_instruction(
                        parse($code).unwrap(),
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
        macro_rules! test_ok {
            ($code:expr) => {
                assert!(typecheck_instruction(
                    parse($code).unwrap(),
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
    }

    #[test]
    fn string_values() {
        assert_eq!(
            typecheck_value(
                &mut Ctx::default(),
                &Type::String,
                Value::String("foo".to_owned())
            ),
            Ok(TypedValue::String("foo".to_owned()))
        )
    }

    #[test]
    fn push_string_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse(r#"PUSH string "foo""#).unwrap(),
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
                parse("PUSH unit Unit").unwrap(),
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
            typecheck_instruction(parse("UNIT").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Unit)
        );
        assert_eq!(stack, tc_stk![Type::Unit]);
    }

    #[test]
    fn push_pair_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse("PUSH (pair int nat bool) (Pair -5 3 False)").unwrap(),
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
                parse("PUSH (or int bool) (Left 1)").unwrap(),
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
                parse("PUSH (or int bool) (Right False)").unwrap(),
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
                parse("PUSH (option nat) (Some 3)").unwrap(),
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
                parse("PUSH (option nat) None").unwrap(),
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
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CAR }").unwrap(),
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
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CDR }").unwrap(),
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
            typecheck_instruction(parse("CAR").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(parse("CDR").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(parse("PAIR").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Pair)
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Int)]);
    }

    #[test]
    fn unpair() {
        let mut stack = tc_stk![Type::new_pair(Type::Nat, Type::Int)];
        assert_eq!(
            typecheck_instruction(parse("UNPAIR").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Unpair)
        );
        assert_eq!(stack, tc_stk![Type::Int, Type::Nat]);
    }

    #[test]
    fn pair_car() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck_instruction(
                parse("{ PAIR; CAR }").unwrap(),
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
                parse("{ PAIR; CDR }").unwrap(),
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
                parse("IF_NONE { PUSH int 5; } {}").unwrap(),
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
                parse("IF_CONS { DROP 2 } {}").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(IfCons(vec![Drop(Some(2))], vec![]))
        );
        assert_eq!(stack, tc_stk![]);
        too_short_test(IfCons(vec![], vec![]), Prim::IF_CONS, 1)
    }

    #[test]
    fn if_cons_mismatch() {
        let mut stack = tc_stk![Type::String];
        assert_eq!(
            typecheck_instruction(
                parse("IF_CONS { DROP 2 } {}").unwrap(),
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
                parse("IF_CONS {} {}").unwrap(),
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
                parse("IF_LEFT { GT } {}").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(IfLeft(vec![Gt], vec![]))
        );
        assert_eq!(stack, tc_stk![Type::Bool]);
    }

    #[test]
    fn if_left_too_short() {
        too_short_test(IfLeft(vec![], vec![]), Prim::IF_LEFT, 1)
    }

    #[test]
    fn if_left_same_branch_ty() {
        let mut stack = tc_stk![Type::new_or(Type::Int, Type::Int)];
        assert_eq!(
            typecheck_instruction(
                parse("IF_LEFT {} {}").unwrap(),
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
                parse("IF_LEFT {} {}").unwrap(),
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
                parse("IF_LEFT {} {}").unwrap(),
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
                parse("IF_NONE { PUSH int 5; } {}").unwrap(),
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
            typecheck_instruction(parse("SOME").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(ISome)
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn compare_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        assert_eq!(
            typecheck_instruction(parse("COMPARE").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Compare)
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn compare_int_fail() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(parse("COMPARE").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(parse("AMOUNT").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Amount)
        );
        assert_eq!(stack, tc_stk![Type::Mutez]);
    }

    #[test]
    fn push_int_list() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse("PUSH (list int) { 1; 2; 3 }").unwrap(),
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
                parse("PUSH (list int) { 1; Unit; 3 }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(Value::Unit, Type::Int))
        );
    }

    #[test]
    fn nil() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(parse("NIL int").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Nil(()))
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Int];
        assert_eq!(
            typecheck_instruction(parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Cons)
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn cons_too_short() {
        too_short_test(Cons, Prim::CONS, 2);
    }

    #[test]
    fn cons_mismatch_elt() {
        let mut stack = tc_stk![Type::new_list(Type::Int), Type::Nat];
        assert_eq!(
            typecheck_instruction(parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into())
        );
    }

    #[test]
    fn cons_mismatch_list() {
        let mut stack = tc_stk![Type::String, Type::Nat];
        assert_eq!(
            typecheck_instruction(parse("CONS").unwrap(), &mut Ctx::default(), &mut stack),
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
                parse("NIL operation").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(Nil(()))
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Operation)]);
    }

    #[test]
    fn failwith_operation() {
        let mut stack = tc_stk![Type::new_list(Type::Operation)];
        assert_eq!(
            typecheck_instruction(parse("FAILWITH").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TcError::InvalidTypeProperty(
                TypeProperty::Packable,
                Type::Operation
            ))
        );
    }

    #[test]
    fn push_map() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse(r#"PUSH (map int string) { Elt 1 "foo"; Elt 2 "bar" }"#).unwrap(),
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
                parse(r#"PUSH (map int string) { Elt 2 "foo"; Elt 1 "bar" }"#).unwrap(),
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
                parse(r#"PUSH (map (list int) string) { Elt { 2 } "foo"; Elt { 1 } "bar" }"#)
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
                parse(r#"PUSH (map (list int) string) { }"#).unwrap(),
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
                parse(r#"PUSH (map int string) { Elt "1" "foo"; Elt 2 "bar" }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::InvalidValueForType(
                Value::String("1".to_owned()),
                Type::Int
            ))
        );
    }

    #[test]
    fn push_map_wrong_elt() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse(r#"PUSH (map int string) { Elt 1 "foo"; "bar" }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::InvalidEltForMap(
                Value::String("bar".to_owned()),
                Type::new_map(Type::Int, Type::String)
            ))
        );
    }

    #[test]
    fn push_map_duplicate_key() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck_instruction(
                parse(r#"PUSH (map int string) { Elt 1 "foo"; Elt 1 "bar" }"#).unwrap(),
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
            typecheck_instruction(parse("GET").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Get(overloads::Get::Map))
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::String)]);
    }

    #[test]
    fn get_map_incomparable() {
        let mut stack = tc_stk![
            Type::new_map(Type::new_list(Type::Int), Type::String),
            Type::new_list(Type::Int)
        ];
        assert_eq!(
            parse("GET")
                .unwrap()
                .typecheck(&mut Ctx::default(), &mut stack),
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
            typecheck_instruction(parse("GET").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::Int, Type::Nat).into()),
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
            typecheck_instruction(parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck_instruction(parse("UPDATE").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TypesNotEqual(Type::String, Type::Nat).into())
        );
    }

    #[test]
    fn update_map_incomparable() {
        let mut stack = tc_stk![
            Type::new_map(Type::new_list(Type::Int), Type::String),
            Type::new_option(Type::String),
            Type::new_list(Type::Int)
        ];
        assert_eq!(
            parse("UPDATE")
                .unwrap()
                .typecheck(&mut Ctx::default(), &mut stack),
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
                parse("{ { PAIR }; {{ CAR; }}; {}; {{{}}}; {{{{{DROP}}}}} }").unwrap(),
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
            typecheck_instruction(parse("ADD").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Add(overloads::Add::IntNat))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck_instruction(parse("ADD").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(Add(overloads::Add::NatInt))
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[track_caller]
    fn too_short_test(instr: ParsedInstruction, prim: Prim, len: usize) {
        for n in 0..len {
            let mut ctx = Ctx::default();
            assert_eq!(
                typecheck_instruction(instr.clone(), &mut ctx, &mut tc_stk![Type::Unit; n]),
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
        too_short_test(Add(()), Prim::ADD, 2);
    }

    #[test]
    fn test_add_mismatch() {
        let mut stack = tc_stk![Type::String, Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(Add(()), &mut ctx, &mut stack),
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
            typecheck_instruction(Dup(Some(0)), &mut ctx, &mut stack),
            Err(TcError::Dup0)
        );
    }

    #[test]
    fn test_gt_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(Gt, &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::GT,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Int, Type::String).into())
            })
        );
    }

    #[test]
    fn test_gt_short() {
        too_short_test(Gt, Prim::GT, 1);
    }

    #[test]
    fn test_if_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(If(vec![], vec![]), &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::IF,
                stack: stk![Type::String],
                reason: Some(TypesNotEqual(Type::Bool, Type::String).into())
            })
        );
    }

    #[test]
    fn test_if_short() {
        too_short_test(If(vec![], vec![]), Prim::IF, 1);
    }

    #[test]
    fn test_if_none_short() {
        too_short_test(IfNone(vec![], vec![]), Prim::IF_NONE, 1);
    }

    #[test]
    fn test_int_short() {
        too_short_test(Int, Prim::INT, 1);
    }

    #[test]
    fn test_int_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(Int, &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::INT,
                stack: stk![Type::String],
                reason: None,
            })
        );
    }

    #[test]
    fn test_loop_short() {
        too_short_test(Loop(vec![]), Prim::LOOP, 1);
    }

    #[test]
    fn test_loop_mismatch() {
        let mut stack = tc_stk![Type::String];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(Loop(vec![]), &mut ctx, &mut stack),
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
            typecheck_instruction(Unpair, &mut ctx, &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: Prim::UNPAIR,
                stack: stk![Type::String],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::String)),
            })
        );
    }

    #[test]
    fn test_swap_short() {
        too_short_test(Swap, Prim::SWAP, 2);
    }

    #[test]
    fn test_pair_short() {
        too_short_test(Pair, Prim::PAIR, 2);
    }

    #[test]
    fn test_get_short() {
        too_short_test(Get(()), Prim::GET, 2);
    }

    #[test]
    fn test_update_short() {
        too_short_test(Update(()), Prim::UPDATE, 3);
    }

    #[test]
    fn test_failwith_short() {
        too_short_test(Failwith(()), Prim::FAILWITH, 1);
    }

    #[test]
    fn test_car_short() {
        too_short_test(Car, Prim::CAR, 1);
    }

    #[test]
    fn test_cdr_short() {
        too_short_test(Cdr, Prim::CDR, 1);
    }

    #[test]
    fn test_some_short() {
        too_short_test(ISome, Prim::SOME, 1);
    }

    #[test]
    fn test_compare_short() {
        too_short_test(Compare, Prim::COMPARE, 2);
    }

    #[test]
    fn test_unpair_short() {
        too_short_test(Unpair, Prim::UNPAIR, 1);
    }

    #[test]
    fn test_compare_gas_exhaustion() {
        let mut ctx = Ctx {
            gas: Gas::new(gas::tc_cost::INSTR_STEP),
            ..Ctx::default()
        };
        assert_eq!(
            typecheck_instruction(Compare, &mut ctx, &mut tc_stk![Type::Unit, Type::Unit]),
            Err(TcError::OutOfGas(OutOfGas))
        );
    }

    #[test]
    fn test_compare_incomparable() {
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                Compare,
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
            typecheck_instruction(Get(()), &mut ctx, &mut tc_stk![Type::Unit, Type::Unit]),
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
                Update(()),
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
                Push((Type::Operation, Value::Unit)),
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
            ContractScript {
                parameter: Type::Operation,
                storage: Type::Nat,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
            ContractScript {
                parameter: Type::Nat,
                storage: Type::Operation,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
            ContractScript {
                parameter: Type::new_map(Type::new_list(Type::Unit), Type::Unit),
                storage: Type::Nat,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
            Value::from(vec![] as Vec<()>).typecheck(
                &mut ctx,
                &Type::new_map(Type::new_list(Type::Unit), Type::Unit)
            ),
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
            ContractScript {
                parameter: Type::new_pair(
                    Type::Unit,
                    Type::new_option(Type::new_map(Type::new_list(Type::Unit), Type::Unit))
                ),
                storage: Type::Nat,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
            ContractScript {
                parameter: Type::Unit,
                storage: Type::new_contract(Type::Unit),
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
                Push((Type::new_contract(Type::Unit), Value::Unit)),
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
            ContractScript {
                parameter: Type::new_contract(Type::Operation),
                storage: Type::Unit,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
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
            ContractScript {
                parameter: Type::new_contract(Type::Unit),
                storage: Type::Unit,
                code: Failwith(())
            }
            .typecheck(&mut ctx),
            Ok(ContractScript {
                parameter: Type::new_contract(Type::Unit),
                storage: Type::Unit,
                code: Failwith(Type::new_pair(Type::new_contract(Type::Unit), Type::Unit))
            })
        );
    }
}
