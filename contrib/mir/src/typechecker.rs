/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use std::collections::BTreeMap;
use std::num::TryFromIntError;

use crate::ast::*;
use crate::context::Ctx;
use crate::gas;
use crate::gas::OutOfGas;
use crate::irrefutable_match::irrefutable_match;
use crate::stack::*;

/// Typechecker error type.
#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum TcError {
    #[error("type stacks not equal: {0:?} != {1:?}")]
    StacksNotEqual(TypeStack, TypeStack, StacksNotEqualReason),
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
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
    #[error("type not comparable: {0:?}")]
    TypeNotComparable(Type),
    #[error("sequence elements must contain no duplicate keys for type {0:?}")]
    DuplicateElements(Type),
    #[error("no matching overload for {instr} on stack {stack:?}{}", .reason.as_ref().map_or("".to_owned(), |x| format!(", reason: {}", x)))]
    NoMatchingOverload {
        instr: &'static str,
        stack: TypeStack,
        reason: Option<NoMatchingOverloadReason>,
    },
    #[error("type not packable: {0:?}")]
    TypeNotPackable(Type),
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum NoMatchingOverloadReason {
    #[error("stack too short, expected {expected}")]
    StackTooShort { expected: usize },
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    #[error("expected pair 'a 'b, but got {0:?}")]
    ExpectedPair(Type),
    #[error("expected option 'a, but got {0:?}")]
    ExpectedOption(Type),
    #[error("type not comparable: {0:?}")]
    TypeNotComparable(Type),
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum StacksNotEqualReason {
    #[error(transparent)]
    TypesNotEqual(#[from] TypesNotEqual),
    #[error("lengths are different: {0} != {1}")]
    LengthsDiffer(usize, usize),
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
#[error("types not equal: {0:?} != {1:?}")]
pub struct TypesNotEqual(Type, Type);

#[allow(dead_code)]
impl ContractScript<ParsedStage> {
    pub fn typecheck(
        self,
        ctx: &mut crate::context::Ctx,
    ) -> Result<ContractScript<TypecheckedStage>, crate::typechecker::TcError> {
        let ContractScript {
            parameter, storage, ..
        } = self;
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

#[allow(dead_code)]
pub fn typecheck(
    ast: ParsedAST,
    ctx: &mut Ctx,
    opt_stack: &mut FailingTypeStack,
) -> Result<TypecheckedAST, TcError> {
    ast.into_iter()
        .map(|i| i.typecheck(ctx, opt_stack))
        .collect()
}

impl ParsedInstruction {
    pub fn typecheck(
        self,
        ctx: &mut Ctx,
        opt_stack: &mut FailingTypeStack,
    ) -> Result<TypecheckedInstruction, TcError> {
        typecheck_instruction(self, ctx, opt_stack)
    }
}

macro_rules! nothing_to_none {
    () => {
        None
    };
    ($e:expr) => {
        Some($e)
    };
}

// This is a bit more complex than I'd like to, but if we want to capture the
// offending stack in its entirety, we _have_ to match on it and handle errors
// before popping. The alternative is to clone whole stack on each instruction,
// which doesn't sound good. -- @lierdakil
macro_rules! checked_pop {
    ($instr:literal, $stack:expr, $pat:pat => $res:expr $(, $reason:expr)?) => {
        match $stack.as_slice() {
            #[allow(unused_variables)]
            [.., $pat] => match $stack.pop().unwrap() {
                $pat => Ok($res),
                #[allow(unreachable_patterns)]
                _ => unreachable!(),
            }
            [] => Err(TcError::NoMatchingOverload {
                instr: $instr,
                stack: $stack.clone(),
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 1 }),
            }),
            #[allow(unused_variables, unreachable_patterns)]
            [.., t] => Err(TcError::NoMatchingOverload {
                instr: $instr,
                stack: $stack.clone(),
                reason: nothing_to_none!($($reason($stack.pop().unwrap()))?),
            }),
        }
    };
}

fn typecheck_instruction(
    i: ParsedInstruction,
    ctx: &mut Ctx,
    opt_stack: &mut FailingTypeStack,
) -> Result<TypecheckedInstruction, TcError> {
    use Instruction as I;
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

    ctx.gas.consume(gas::tc_cost::INSTR_STEP)?;

    Ok(match i {
        I::Add(..) => {
            ensure_stack_len("ADD", stack, 2)?;
            match stack.as_slice() {
                [.., T::Nat, T::Nat] => {
                    stack.pop();
                    I::Add(overloads::Add::NatNat)
                }
                [.., T::Int, T::Int] => {
                    stack.pop();
                    I::Add(overloads::Add::IntInt)
                }
                [.., T::Nat, T::Int] => {
                    stack.pop();
                    stack[0] = T::Int;
                    I::Add(overloads::Add::IntNat)
                }
                [.., T::Int, T::Nat] => {
                    stack.pop();
                    I::Add(overloads::Add::NatInt)
                }
                [.., T::Mutez, T::Mutez] => {
                    stack.pop();
                    I::Add(overloads::Add::MutezMutez)
                }
                _ => {
                    return Err(TcError::NoMatchingOverload {
                        instr: "ADD",
                        stack: stack.clone(),
                        reason: None,
                    })
                }
            }
        }
        I::Dip(opt_height, nested) => {
            let protected_height = opt_height.unwrap_or(1) as usize;

            ctx.gas.consume(gas::tc_cost::dip_n(&opt_height)?)?;

            ensure_stack_len("DIP", stack, protected_height)?;
            // Here we split off the protected portion of the stack, typecheck the code with the
            // remaining unprotected part, then append the protected portion back on top.
            let mut protected = stack.split_off(protected_height);
            let nested = typecheck(nested, ctx, opt_stack)?;
            opt_stack
                .access_mut(TcError::FailNotInTail)?
                .append(&mut protected);
            I::Dip(opt_height, nested)
        }
        I::Drop(opt_height) => {
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            ctx.gas.consume(gas::tc_cost::drop_n(&opt_height)?)?;
            ensure_stack_len("DROP", stack, drop_height)?;
            stack.drop_top(drop_height);
            I::Drop(opt_height)
        }
        // DUP instruction requires an argument that is > 0.
        I::Dup(Some(0)) => return Err(TcError::Dup0),
        I::Dup(opt_height) => {
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            ensure_stack_len("DUP", stack, dup_height)?;
            stack.push(stack[dup_height - 1].clone());
            I::Dup(opt_height)
        }
        I::Gt => {
            checked_pop!("GT", stack, T::Int => (), |t| TypesNotEqual(T::Int, t).into())?;
            stack.push(T::Bool);
            I::Gt
        }
        I::If(nested_t, nested_f) => {
            checked_pop!("IF", stack, T::Bool => (), |t| TypesNotEqual(T::Bool, t).into())?;
            // Check if top is bool
            // Clone the stack so that we have a copy to run one branch on.
            // We can run the other branch on the live stack.
            let mut f_opt_stack = opt_stack.clone();
            let nested_t = typecheck(nested_t, ctx, opt_stack)?;
            let nested_f = typecheck(nested_f, ctx, &mut f_opt_stack)?;
            // If stacks unify after typecheck, all is good.
            unify_stacks(ctx, opt_stack, f_opt_stack)?;
            I::If(nested_t, nested_f)
        }
        I::IfNone(when_none, when_some) => {
            // Check if top is option 'ty
            let ty = checked_pop!("IF_NONE", stack, T::Option(ty) => ty, NoMatchingOverloadReason::ExpectedOption)?;
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
        I::Int => {
            checked_pop!("INT", stack, T::Nat => ())?;
            stack.push(Type::Int);
            I::Int
        }
        I::Loop(nested) => {
            // copy stack for unifying with it later
            let opt_copy = FailingTypeStack::Ok(stack.clone());
            // Check if top is bool
            checked_pop!("LOOP", stack, T::Bool => (), |t| TypesNotEqual(T::Bool, t).into())?;
            // Typecheck with the current stack
            let nested = typecheck(nested, ctx, opt_stack)?;
            // If the starting stack and result stack unify, all is good.
            unify_stacks(ctx, opt_stack, opt_copy)?;
            // pop the remaining bool off (if not failed)
            opt_stack.access_mut(()).ok().map(Stack::pop);
            I::Loop(nested)
        }
        I::Push((t, v)) => {
            let v = typecheck_value(ctx, &t, v)?;
            stack.push(t.to_owned());
            I::Push(v)
        }
        I::Swap => {
            ensure_stack_len("SWAP", stack, 2)?;
            stack.swap(0, 1);
            I::Swap
        }
        I::Failwith => {
            ensure_stack_len("FAILWITH", stack, 1)?;
            let ty = pop!();
            ensure_packable(ty)?;
            // mark stack as failed
            *opt_stack = FailingTypeStack::Failed;
            I::Failwith
        }
        I::Unit => {
            stack.push(T::Unit);
            I::Unit
        }
        I::Car => {
            let l = checked_pop!("CAR", stack, T::Pair(l, _) => *l, NoMatchingOverloadReason::ExpectedPair)?;
            stack.push(l);
            I::Car
        }
        I::Cdr => {
            let r = checked_pop!("CDR", stack, T::Pair(_, r) => *r, NoMatchingOverloadReason::ExpectedPair)?;
            stack.push(r);
            I::Cdr
        }
        I::Pair => {
            ensure_stack_len("PAIR", stack, 2)?;
            let (l, r) = (pop!(), pop!());
            stack.push(Type::new_pair(l, r));
            I::Pair
        }
        I::ISome => {
            let ty = checked_pop!("SOME", stack, ty => ty)?;
            stack.push(T::new_option(ty));
            I::ISome
        }
        I::Compare => {
            ensure_stack_len("COMPARE", stack, 2)?;
            let (t, u) = (&stack[0], &stack[1]);
            ensure_ty_eq(ctx, t, u).map_err(|e| match e {
                TcError::TypesNotEqual(e) => TcError::NoMatchingOverload {
                    instr: "COMPARE",
                    stack: stack.clone(),
                    reason: Some(e.into()),
                },
                e => e,
            })?;
            if !t.is_comparable() {
                return Err(TcError::NoMatchingOverload {
                    instr: "COMPARE",
                    stack: stack.clone(),
                    reason: Some(NoMatchingOverloadReason::TypeNotComparable(t.clone())),
                });
            }
            stack.pop();
            stack[0] = T::Int;
            I::Compare
        }
        I::Amount => {
            stack.push(T::Mutez);
            I::Amount
        }
        I::Nil(ty) => {
            stack.push(T::new_list(ty));
            I::Nil(())
        }
        I::Get(..) => {
            ensure_stack_len("GET", stack, 2)?;
            match stack.as_slice() {
                [.., T::Map(..), _] => {
                    let kty_ = pop!();
                    pop!(T::Map, kty, vty);
                    ensure_ty_eq(ctx, &kty, &kty_)?;
                    // can only fail if the typechecker is invoked on invalid
                    // types, i.e. where `map` key is non-comparable
                    ensure_comparable(&kty)?;
                    stack.push(T::new_option(*vty));
                    I::Get(overloads::Get::Map)
                }
                _ => {
                    return Err(TcError::NoMatchingOverload {
                        instr: "GET",
                        stack: stack.clone(),
                        reason: None,
                    })
                }
            }
        }
        I::Update(..) => {
            ensure_stack_len("UPDATE", stack, 3)?;
            match stack.as_slice() {
                [.., T::Map(..), _, _] => {
                    let kty_ = pop!();
                    let vty_new = pop!();
                    pop!(T::Map, kty, vty);
                    ensure_ty_eq(ctx, &kty, &kty_)?;
                    ensure_ty_eq(ctx, &T::new_option(*vty), &vty_new)?;
                    // can only fail if the typechecker is invoked on invalid
                    // types, i.e. where `map` key is non-comparable
                    ensure_comparable(&kty)?;
                    let boxed_vty = irrefutable_match!(vty_new; T::Option);
                    stack.push(T::Map(kty, boxed_vty));
                    I::Update(overloads::Update::Map)
                }
                _ => {
                    return Err(TcError::NoMatchingOverload {
                        instr: "UPDATE",
                        stack: stack.clone(),
                        reason: None,
                    })
                }
            }
        }
        I::Seq(nested) => I::Seq(typecheck(nested, ctx, opt_stack)?),
    })
}

impl Value {
    pub fn typecheck(self, ctx: &mut Ctx, t: &Type) -> Result<TypedValue, TcError> {
        typecheck_value(ctx, t, self)
    }
}

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
        (Pair(tl, tr), V::Pair(vl, vr)) => {
            let l = typecheck_value(ctx, tl, *vl)?;
            let r = typecheck_value(ctx, tr, *vr)?;
            TV::new_pair(l, r)
        }
        (Option(ty), V::Option(v)) => match v {
            Some(v) => {
                let v = typecheck_value(ctx, ty, *v)?;
                TV::new_option(Some(v))
            }
            None => TV::new_option(None),
        },
        (List(ty), V::Seq(vs)) => {
            let lst: Result<Vec<TypedValue>, TcError> = vs
                .into_iter()
                .map(|v| typecheck_value(ctx, ty, v))
                .collect();
            TV::List(lst?)
        }
        (Map(tk, tv), V::Seq(vs)) => {
            // can only fail if the typechecker is invoked on invalid
            // types, i.e. where `map` key is non-comparable
            ensure_comparable(tk)?;
            let tc_elt = |v: Value| -> Result<(TypedValue, TypedValue), TcError> {
                match v {
                    Value::Elt(k, v) => {
                        let k = typecheck_value(ctx, tk, *k)?;
                        let v = typecheck_value(ctx, tv, *v)?;
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
fn ensure_stack_len(instr: &'static str, stack: &TypeStack, l: usize) -> Result<(), TcError> {
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

fn ensure_packable(ty: Type) -> Result<(), TcError> {
    if ty.is_packable() {
        Ok(())
    } else {
        Err(TcError::TypeNotPackable(ty))
    }
}

fn ensure_comparable(ty: &Type) -> Result<(), TcError> {
    if ty.is_comparable() {
        Ok(())
    } else {
        Err(TcError::TypeNotComparable(ty.clone()))
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
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440);
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
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 440 - 100);
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
            typecheck_instruction(
                Dip(Some(1), parse("{PUSH nat 6}").unwrap()),
                &mut ctx,
                &mut stack
            ),
            Ok(Dip(Some(1), vec![Push(TypedValue::Nat(6))]))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - 440 - 440 - 100 - 50
        );
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
                Loop(parse("{PUSH bool True}").unwrap()),
                &mut ctx,
                &mut stack
            ),
            Ok(Loop(vec![Push(TypedValue::Bool(true))]))
        );
        assert_eq!(stack, expected_stack);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - 440 - 440 - 100 - 60 * 2
        );
    }

    #[test]
    fn test_loop_stacks_not_equal_length() {
        let mut stack = tc_stk![Type::Int, Type::Bool];
        let mut ctx = Ctx::default();
        assert_eq!(
            typecheck_instruction(
                Loop(parse("{PUSH int 1; PUSH bool True}").unwrap()),
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
                Loop(parse("{DROP; PUSH bool False; PUSH bool True}").unwrap()),
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
    fn test_failwith() {
        assert_eq!(
            typecheck_instruction(Failwith, &mut Ctx::default(), &mut tc_stk![Type::Int]),
            Ok(Failwith)
        );
    }

    #[test]
    fn test_failed_stacks() {
        macro_rules! test_fail {
            ($code:expr) => {
                assert_eq!(
                    typecheck(parse($code).unwrap(), &mut Ctx::default(), &mut tc_stk![]),
                    Err(TcError::FailNotInTail)
                );
            };
        }
        test_fail!("{ PUSH int 1; FAILWITH; PUSH int 1 }");
        test_fail!("{ PUSH int 1; DIP { PUSH int 1; FAILWITH } }");
        test_fail!("{ PUSH bool True; IF { PUSH int 1; FAILWITH } { PUSH int 1; FAILWITH }; GT }");
        macro_rules! test_ok {
            ($code:expr) => {
                assert!(
                    typecheck(parse($code).unwrap(), &mut Ctx::default(), &mut tc_stk![]).is_ok()
                );
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
            typecheck(
                parse(r#"{ PUSH string "foo"; }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::String("foo".to_owned()))])
        );
        assert_eq!(stack, tc_stk![Type::String]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH unit Unit; }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::Unit)])
        );
        assert_eq!(stack, tc_stk![Type::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(parse("{ UNIT }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![Unit])
        );
        assert_eq!(stack, tc_stk![Type::Unit]);
    }

    #[test]
    fn push_pair_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False) }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::new_pair(
                TypedValue::Int(-5),
                TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
            ))])
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
    fn push_option_value() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (option nat) (Some 3) }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::new_option(Some(TypedValue::Nat(3))))])
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Nat)]);
    }

    #[test]
    fn car() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CAR }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
                )),
                Car
            ])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn cdr() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (pair int nat bool) (Pair -5 3 False); CDR }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![
                Push(TypedValue::new_pair(
                    TypedValue::Int(-5),
                    TypedValue::new_pair(TypedValue::Nat(3), TypedValue::Bool(false))
                )),
                Cdr
            ])
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Bool)]);
    }

    #[test]
    fn car_fail() {
        let mut stack = tc_stk![Type::Unit];
        assert_eq!(
            typecheck(parse("{ CAR }").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: "CAR",
                stack: stk![Type::Unit],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Unit)),
            })
        );
    }

    #[test]
    fn cdr_fail() {
        let mut stack = tc_stk![Type::Unit];
        assert_eq!(
            typecheck(parse("{ CDR }").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: "CDR",
                stack: stk![Type::Unit],
                reason: Some(NoMatchingOverloadReason::ExpectedPair(Type::Unit)),
            })
        );
    }

    #[test]
    fn pair() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(parse("{ PAIR }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![Pair])
        );
        assert_eq!(stack, tc_stk![Type::new_pair(Type::Nat, Type::Int)]);
    }

    #[test]
    fn pair_car() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(
                parse("{ PAIR; CAR }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Pair, Car])
        );
        assert_eq!(stack, tc_stk![Type::Nat]);
    }

    #[test]
    fn pair_cdr() {
        let mut stack = tc_stk![Type::Int, Type::Nat]; // NB: nat is top
        assert_eq!(
            typecheck(
                parse("{ PAIR; CDR }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Pair, Cdr])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_none() {
        let mut stack = tc_stk![Type::new_option(Type::Int)];
        assert_eq!(
            typecheck(
                parse("{ IF_NONE { PUSH int 5; } {} }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![IfNone(vec![Push(TypedValue::Int(5))], vec![])])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn if_none_fail() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck(
                parse("{ IF_NONE { PUSH int 5; } {} }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::NoMatchingOverload {
                instr: "IF_NONE",
                stack: stk![Type::Int],
                reason: Some(NoMatchingOverloadReason::ExpectedOption(Type::Int)),
            })
        );
    }

    #[test]
    fn some() {
        let mut stack = tc_stk![Type::Int];
        assert_eq!(
            typecheck(parse("{ SOME }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![ISome])
        );
        assert_eq!(stack, tc_stk![Type::new_option(Type::Int)]);
    }

    #[test]
    fn compare_int() {
        let mut stack = tc_stk![Type::Int, Type::Int];
        assert_eq!(
            typecheck(
                parse("{ COMPARE }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Compare])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn compare_int_fail() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck(
                parse("{ COMPARE }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::NoMatchingOverload {
                instr: "COMPARE",
                stack: stk![Type::Int, Type::Nat],
                reason: Some(TypesNotEqual(Type::Nat, Type::Int).into())
            })
        );
    }

    #[test]
    fn amount() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ AMOUNT }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Amount])
        );
        assert_eq!(stack, tc_stk![Type::Mutez]);
    }

    #[test]
    fn push_int_list() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (list int) { 1; 2; 3 }}").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::List(vec![
                TypedValue::Int(1),
                TypedValue::Int(2),
                TypedValue::Int(3),
            ]))])
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn push_int_list_fail() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ PUSH (list int) { 1; Unit; 3 }}").unwrap(),
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
            typecheck(
                parse("{ NIL int }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Nil(())])
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Int)]);
    }

    #[test]
    fn nil_operation() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse("{ NIL operation }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Nil(())])
        );
        assert_eq!(stack, tc_stk![Type::new_list(Type::Operation)]);
    }

    #[test]
    fn failwith_operation() {
        let mut stack = tc_stk![Type::new_list(Type::Operation)];
        assert_eq!(
            typecheck(
                parse("{ FAILWITH }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::TypeNotPackable(Type::new_list(Type::Operation)))
        );
    }

    #[test]
    fn push_map() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse(r#"{ PUSH (map int string) { Elt 1 "foo"; Elt 2 "bar" } }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Push(TypedValue::Map(BTreeMap::from([
                (TypedValue::Int(1), TypedValue::String("foo".to_owned())),
                (TypedValue::Int(2), TypedValue::String("bar".to_owned()))
            ])))])
        );
        assert_eq!(stack, tc_stk![Type::new_map(Type::Int, Type::String)]);
    }

    #[test]
    fn push_map_unsorted() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse(r#"{ PUSH (map int string) { Elt 2 "foo"; Elt 1 "bar" } }"#).unwrap(),
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
            typecheck(
                parse(r#"{ PUSH (map (list int) string) { Elt { 2 } "foo"; Elt { 1 } "bar" } }"#)
                    .unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::TypeNotComparable(Type::new_list(Type::Int)))
        );
    }

    #[test]
    fn push_map_incomparable_empty() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse(r#"{ PUSH (map (list int) string) { } }"#).unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::TypeNotComparable(Type::new_list(Type::Int)))
        );
    }

    #[test]
    fn push_map_wrong_key_type() {
        let mut stack = tc_stk![];
        assert_eq!(
            typecheck(
                parse(r#"{ PUSH (map int string) { Elt "1" "foo"; Elt 2 "bar" } }"#).unwrap(),
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
            typecheck(
                parse(r#"{ PUSH (map int string) { Elt 1 "foo"; "bar" } }"#).unwrap(),
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
            typecheck(
                parse(r#"{ PUSH (map int string) { Elt 1 "foo"; Elt 1 "bar" } }"#).unwrap(),
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
            typecheck(parse("{ GET }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![Get(overloads::Get::Map)])
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
            typecheck(parse("{ GET }").unwrap(), &mut Ctx::default(), &mut stack),
            Err(TcError::TypeNotComparable(Type::new_list(Type::Int))),
        );
    }

    #[test]
    fn get_map_wrong_type() {
        let mut stack = tc_stk![Type::new_map(Type::Int, Type::String), Type::Nat];
        assert_eq!(
            typecheck(parse("{ GET }").unwrap(), &mut Ctx::default(), &mut stack),
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
            typecheck(
                parse("{ UPDATE }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![Update(overloads::Update::Map)])
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
            typecheck(
                parse("{ UPDATE }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TypesNotEqual(Type::new_option(Type::String), Type::new_option(Type::Nat)).into())
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
            typecheck(
                parse("{ UPDATE }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Err(TcError::TypeNotComparable(Type::new_list(Type::Int),))
        );
    }

    #[test]
    fn seq() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck(
                parse("{ { PAIR }; {{ CAR; }}; {}; {{{}}}; {{{{{DROP}}}}} }").unwrap(),
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(vec![
                Seq(vec![Pair]),
                Seq(vec![Seq(vec![Car])]),
                Seq(vec![]),
                Seq(vec![Seq(vec![Seq(vec![])])]),
                Seq(vec![Seq(vec![Seq(vec![Seq(vec![Seq(vec![Drop(None)])])])])])
            ])
        );
        assert_eq!(stack, tc_stk![]);
    }

    #[test]
    fn add_int_nat() {
        let mut stack = tc_stk![Type::Nat, Type::Int];
        assert_eq!(
            typecheck(parse("{ ADD }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![Add(overloads::Add::IntNat)])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }

    #[test]
    fn add_nat_int() {
        let mut stack = tc_stk![Type::Int, Type::Nat];
        assert_eq!(
            typecheck(parse("{ ADD }").unwrap(), &mut Ctx::default(), &mut stack),
            Ok(vec![Add(overloads::Add::NatInt)])
        );
        assert_eq!(stack, tc_stk![Type::Int]);
    }
}
