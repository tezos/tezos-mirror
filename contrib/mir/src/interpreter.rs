/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

use checked::Checked;
use cryptoxide::hashing::{blake2b_256, keccak256, sha256, sha3_256, sha512};
use num_bigint::{BigInt, BigUint, Sign};
use num_traits::{Signed, Zero};
use std::rc::Rc;
use typed_arena::Arena;

use crate::ast::*;
use crate::bls;
use crate::context::Ctx;
use crate::gas::{interpret_cost, OutOfGas};
use crate::irrefutable_match::irrefutable_match;
use crate::stack::*;
use crate::typechecker::{typecheck_contract_address, typecheck_value};

#[derive(Debug, PartialEq, Eq, Clone, thiserror::Error)]
pub enum InterpretError<'a> {
    #[error(transparent)]
    OutOfGas(#[from] OutOfGas),
    #[error("mutez overflow")]
    MutezOverflow,
    #[error("failed with: {1:?} of type {0:?}")]
    FailedWith(Type, TypedValue<'a>),
}

#[derive(Debug, PartialEq, Eq, thiserror::Error)]
pub enum ContractInterpretError<'a> {
    #[error("failed typechecking input: {0}")]
    TcError(#[from] crate::typechecker::TcError),
    #[error("runtime failure while running the contract: {0}")]
    InterpretError(InterpretError<'a>),
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
        ctx: &mut crate::context::Ctx,
        arena: &'a Arena<Micheline<'a>>,
        parameter: Micheline<'a>,
        storage: Micheline<'a>,
    ) -> Result<(impl Iterator<Item = OperationInfo<'a>>, TypedValue<'a>), ContractInterpretError<'a>>
    {
        let parameter = typecheck_value(&parameter, ctx, &self.parameter)?;
        let storage = typecheck_value(&storage, ctx, &self.storage)?;
        let tc_val = TypedValue::new_pair(parameter, storage);
        let mut stack = stk![tc_val];
        self.code.interpret(ctx, arena, &mut stack)?;
        use TypedValue as V;
        match stack.pop().expect("empty execution stack") {
            V::Pair(p) => match *p {
                (V::List(vec), storage) => Ok((
                    vec.into_iter()
                        .map(|x| (*irrefutable_match!(x; V::Operation))),
                    storage,
                )),
                (v, _) => panic!("expected `list operation`, got {:?}", v),
            },
            v => panic!("expected `pair 'a 'b`, got {:?}", v),
        }
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
        ctx: &mut Ctx,
        arena: &'a Arena<Micheline<'a>>,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        interpret_one(self, ctx, arena, stack)
    }
}

fn interpret<'a>(
    ast: &[Instruction<'a>],
    ctx: &mut Ctx,
    arena: &'a Arena<Micheline<'a>>,
    stack: &mut IStack<'a>,
) -> Result<(), InterpretError<'a>> {
    for i in ast {
        i.interpret(ctx, arena, stack)?;
    }
    ctx.gas.consume(interpret_cost::INTERPRET_RET)?;
    Ok(())
}

#[track_caller]
fn unreachable_state() -> ! {
    // If the typechecking of the program being interpreted was successful and if this is reached
    // during interpreting, then the typechecking should be broken, and needs to be fixed.
    panic!("Unreachable state reached during interpreting, possibly broken typechecking!")
}

fn interpret_one<'a>(
    i: &Instruction<'a>,
    ctx: &mut Ctx,
    arena: &'a Arena<Micheline<'a>>,
    stack: &mut IStack<'a>,
) -> Result<(), InterpretError<'a>> {
    use Instruction as I;
    use TypedValue as V;

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
            crate::irrefutable_match::irrefutable_match!(
                stack.pop().unwrap_or_else(|| unreachable_state());
                $($args)*
                )
        };
    }

    match i {
        I::Add(overload) => match overload {
            overloads::Add::IntInt => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + o2;
                stack.push(V::Nat(sum));
            }
            overloads::Add::IntNat => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = o1 + BigInt::from(o2);
                stack.push(V::Int(sum));
            }
            overloads::Add::NatInt => {
                let o1 = pop!(V::Nat);
                let o2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::add_num(&o1, &o2)?)?;
                let sum = BigInt::from(o1) + o2;
                stack.push(V::Int(sum));
            }
            overloads::Add::MutezMutez => {
                let o1 = pop!(V::Mutez);
                let o2 = pop!(V::Mutez);
                ctx.gas.consume(interpret_cost::ADD_TEZ)?;
                let sum = o1.checked_add(o2).ok_or(InterpretError::MutezOverflow)?;
                stack.push(V::Mutez(sum));
            }
            overloads::Add::Bls12381Fr => {
                let o1 = pop!(V::Bls12381Fr);
                let o2 = pop!(V::Bls12381Fr);
                ctx.gas.consume(interpret_cost::ADD_BLS_FR)?;
                stack.push(V::Bls12381Fr(o1 + o2));
            }
            overloads::Add::Bls12381G1 => {
                let o1 = pop!(V::Bls12381G1);
                let o2 = pop!(V::Bls12381G1);
                ctx.gas.consume(interpret_cost::ADD_BLS_G1)?;
                stack.push(V::new_bls12381_g1(o1.as_ref() + o2.as_ref()));
            }
            overloads::Add::Bls12381G2 => {
                let o1 = pop!(V::Bls12381G2);
                let o2 = pop!(V::Bls12381G2);
                ctx.gas.consume(interpret_cost::ADD_BLS_G2)?;
                stack.push(V::new_bls12381_g2(o1.as_ref() + o2.as_ref()));
            }
        },
        I::Mul(overload) => match overload {
            overloads::Mul::NatNat => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * x2;
                stack.push(V::Nat(res));
            }
            overloads::Mul::NatInt => {
                let x1 = pop!(V::Nat);
                let x2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = BigInt::from(x1) * x2;
                stack.push(V::Int(res));
            }
            overloads::Mul::IntNat => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * BigInt::from(x2);
                stack.push(V::Int(res));
            }
            overloads::Mul::IntInt => {
                let x1 = pop!(V::Int);
                let x2 = pop!(V::Int);
                ctx.gas.consume(interpret_cost::mul_int(&x1, &x2)?)?;
                let res = x1 * x2;
                stack.push(V::Int(res));
            }
            overloads::Mul::MutezNat => {
                ctx.gas.consume(interpret_cost::MUL_TEZ_NAT)?;
                let x1 = pop!(V::Mutez);
                let x2 = i64::try_from(pop!(V::Nat)).map_err(|_| InterpretError::MutezOverflow)?;
                let res = x1.checked_mul(x2).ok_or(InterpretError::MutezOverflow)?;
                stack.push(V::Mutez(res));
            }
            overloads::Mul::NatMutez => {
                ctx.gas.consume(interpret_cost::MUL_NAT_TEZ)?;
                let x1 = i64::try_from(pop!(V::Nat)).map_err(|_| InterpretError::MutezOverflow)?;
                let x2 = pop!(V::Mutez);
                let res = x1.checked_mul(x2).ok_or(InterpretError::MutezOverflow)?;
                stack.push(V::Mutez(res));
            }
            overloads::Mul::Bls12381G1Bls12381Fr => {
                ctx.gas.consume(interpret_cost::MUL_BLS_G1)?;
                let x1 = pop!(V::Bls12381G1);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::new_bls12381_g1(x1.as_ref() * x2));
            }
            overloads::Mul::Bls12381G2Bls12381Fr => {
                ctx.gas.consume(interpret_cost::MUL_BLS_G2)?;
                let x1 = pop!(V::Bls12381G2);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::new_bls12381_g2(x1.as_ref() * x2));
            }
            overloads::Mul::Bls12381FrBls12381Fr => {
                ctx.gas.consume(interpret_cost::MUL_BLS_FR)?;
                let x1 = pop!(V::Bls12381Fr);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            overloads::Mul::NatBls12381Fr => {
                let nat = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::mul_bls_fr_big_int(&nat)?)?;
                let x1 = bls::Fr::from_big_int(&nat.into());
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            overloads::Mul::IntBls12381Fr => {
                let int = pop!(V::Int);
                ctx.gas.consume(interpret_cost::mul_bls_fr_big_int(&int)?)?;
                let x1 = bls::Fr::from_big_int(&int);
                let x2 = pop!(V::Bls12381Fr);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            overloads::Mul::Bls12381FrNat => {
                let x1 = pop!(V::Bls12381Fr);
                let nat = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::mul_bls_fr_big_int(&nat)?)?;
                let x2 = bls::Fr::from_big_int(&nat.into());
                stack.push(V::Bls12381Fr(x1 * x2));
            }
            overloads::Mul::Bls12381FrInt => {
                let x1 = pop!(V::Bls12381Fr);
                let int = pop!(V::Int);
                ctx.gas.consume(interpret_cost::mul_bls_fr_big_int(&int)?)?;
                let x2 = bls::Fr::from_big_int(&int);
                stack.push(V::Bls12381Fr(x1 * x2));
            }
        },
        I::Neg(overload) => match overload {
            overloads::Neg::Nat => {
                let v = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::neg_int(&v)?)?;
                stack.push(V::Int(BigInt::from_biguint(Sign::Minus, v)));
            }
            overloads::Neg::Int => {
                let v = pop!(V::Int);
                ctx.gas.consume(interpret_cost::neg_int(&v)?)?;
                stack.push(V::Int(-v));
            }
            overloads::Neg::Bls12381G1 => {
                ctx.gas.consume(interpret_cost::NEG_G1)?;
                let v = irrefutable_match!(&mut stack[0]; V::Bls12381G1).as_mut();
                *v = -(v as &bls::G1);
            }
            overloads::Neg::Bls12381G2 => {
                ctx.gas.consume(interpret_cost::NEG_G2)?;
                let v = irrefutable_match!(&mut stack[0]; V::Bls12381G2).as_mut();
                *v = -(v as &bls::G2);
            }
            overloads::Neg::Bls12381Fr => {
                ctx.gas.consume(interpret_cost::NEG_FR)?;
                let v = irrefutable_match!(&mut stack[0]; V::Bls12381Fr);
                *v = -(v as &bls::Fr);
            }
        },
        I::SubMutez => {
            ctx.gas.consume(interpret_cost::SUB_MUTEZ)?;
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
                let o2 = irrefutable_match!(&mut stack[0]; V::Bool);
                ctx.gas.consume(interpret_cost::AND_BOOL)?;
                *o2 &= o1;
            }
            overloads::And::NatNat => {
                let o1 = pop!(V::Nat);
                let o2 = irrefutable_match!(&mut stack[0]; V::Nat);
                ctx.gas.consume(interpret_cost::and_num(&o1, o2)?)?;
                *o2 &= o1;
            }
            overloads::And::IntNat => {
                let o1 = pop!(V::Int);
                let o2 = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::and_num(&o1, &o2)?)?;
                let res = BigUint::try_from(o1 & BigInt::from(o2))
                    // safe, `neg` & `pos` = `pos`
                    .unwrap();
                stack.push(V::Nat(res));
            }
            overloads::And::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = irrefutable_match!(&mut stack[0]; V::Bytes);
                ctx.gas.consume(interpret_cost::and_bytes(&o1, o2)?)?;

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
                let o2 = irrefutable_match!(&mut stack[0]; V::Bool);
                ctx.gas.consume(interpret_cost::OR_BOOL)?;
                *o2 |= o1;
            }
            overloads::Or::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = irrefutable_match!(&mut stack[0]; V::Nat);
                ctx.gas.consume(interpret_cost::or_num(&o1, o2)?)?;
                *o2 |= o1;
            }
            overloads::Or::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = irrefutable_match!(&mut stack[0]; V::Bytes);
                ctx.gas.consume(interpret_cost::or_bytes(&o1, o2)?)?;

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
                let o2 = irrefutable_match!(&mut stack[0]; V::Bool);
                ctx.gas.consume(interpret_cost::XOR_BOOL)?;
                *o2 ^= o1;
            }
            overloads::Xor::Nat => {
                let o1 = pop!(V::Nat);
                let o2 = irrefutable_match!(&mut stack[0]; V::Nat);
                ctx.gas.consume(interpret_cost::xor_nat(&o1, o2)?)?;
                *o2 ^= o1;
            }
            overloads::Xor::Bytes => {
                let mut o1 = pop!(V::Bytes);
                let o2 = irrefutable_match!(&mut stack[0]; V::Bytes);

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
                let o = irrefutable_match!(&mut stack[0]; V::Bool);
                ctx.gas.consume(interpret_cost::NOT_BOOL)?;
                *o = !*o;
            }
            overloads::Not::Int => {
                let o = pop!(V::Int);
                ctx.gas.consume(interpret_cost::not_num(&o)?)?;
                stack.push(V::Int(!o));
            }
            overloads::Not::Nat => {
                let o = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::not_num(&o)?)?;
                stack.push(V::Int(!BigInt::from(o)))
            }
            overloads::Not::Bytes => {
                let o = irrefutable_match!(&mut stack[0]; V::Bytes);
                ctx.gas.consume(interpret_cost::not_bytes(o)?)?;
                for b in o.iter_mut() {
                    *b = !*b
                }
            }
        },
        I::Dip(opt_height, nested) => {
            ctx.gas.consume(interpret_cost::dip(*opt_height)?)?;
            let protected_height: u16 = opt_height.unwrap_or(1);
            let mut protected = stack.split_off(protected_height as usize);
            interpret(nested, ctx, arena, stack)?;
            ctx.gas.consume(interpret_cost::undip(protected_height)?)?;
            stack.append(&mut protected);
        }
        I::Drop(opt_height) => {
            ctx.gas.consume(interpret_cost::drop(*opt_height)?)?;
            let drop_height: usize = opt_height.unwrap_or(1) as usize;
            stack.drop_top(drop_height);
        }
        I::Dup(opt_height) => {
            ctx.gas.consume(interpret_cost::dup(*opt_height)?)?;
            let dup_height: usize = opt_height.unwrap_or(1) as usize;
            stack.push(stack[dup_height - 1].clone());
        }
        I::Dig(dig_height) => {
            ctx.gas.consume(interpret_cost::dig(*dig_height)?)?;
            if *dig_height > 0 {
                let e = stack.remove(*dig_height as usize);
                stack.push(e);
            }
        }
        I::Dug(dug_height) => {
            ctx.gas.consume(interpret_cost::dug(*dug_height)?)?;
            if *dug_height > 0 {
                let e = pop!();
                stack.insert(*dug_height as usize, e);
            }
        }
        I::Gt => {
            ctx.gas.consume(interpret_cost::GT)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_positive()));
        }
        I::Ge => {
            ctx.gas.consume(interpret_cost::GE)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_negative()));
        }
        I::Eq => {
            ctx.gas.consume(interpret_cost::EQ)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_zero()));
        }
        I::Neq => {
            ctx.gas.consume(interpret_cost::NEQ)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_zero()));
        }
        I::Le => {
            ctx.gas.consume(interpret_cost::LE)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(!i.is_positive()));
        }
        I::Lt => {
            ctx.gas.consume(interpret_cost::LT)?;
            let i = pop!(V::Int);
            stack.push(V::Bool(i.is_negative()));
        }
        I::If(nested_t, nested_f) => {
            ctx.gas.consume(interpret_cost::IF)?;
            if pop!(V::Bool) {
                interpret(nested_t, ctx, arena, stack)?;
            } else {
                interpret(nested_f, ctx, arena, stack)?;
            }
        }
        I::IfNone(when_none, when_some) => {
            ctx.gas.consume(interpret_cost::IF_NONE)?;
            match pop!(V::Option) {
                Some(x) => {
                    stack.push(*x);
                    interpret(when_some, ctx, arena, stack)?
                }
                None => interpret(when_none, ctx, arena, stack)?,
            }
        }
        I::IfCons(when_cons, when_nil) => {
            ctx.gas.consume(interpret_cost::IF_CONS)?;
            let lst = irrefutable_match!(&mut stack[0]; V::List);
            match lst.uncons() {
                Some(x) => {
                    stack.push(x);
                    interpret(when_cons, ctx, arena, stack)?
                }
                None => {
                    pop!();
                    interpret(when_nil, ctx, arena, stack)?;
                }
            }
        }
        I::IfLeft(when_left, when_right) => {
            ctx.gas.consume(interpret_cost::IF_LEFT)?;
            let or = *pop!(V::Or);
            match or {
                Or::Left(x) => {
                    stack.push(x);
                    interpret(when_left, ctx, arena, stack)?
                }
                Or::Right(x) => {
                    stack.push(x);
                    interpret(when_right, ctx, arena, stack)?;
                }
            }
        }
        I::Abs => {
            let i = pop!(V::Int);
            ctx.gas.consume(interpret_cost::abs(&i)?)?;
            stack.push(V::Nat(i.into_parts().1));
        }
        I::IsNat => {
            let i = pop!(V::Int);
            ctx.gas.consume(interpret_cost::ISNAT)?;
            stack.push(V::new_option(i.try_into().ok().map(V::Nat)));
        }
        I::Int(overload) => match overload {
            overloads::Int::Nat => {
                let i = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::INT_NAT)?;
                stack.push(V::Int(i.into()));
            }
            overloads::Int::Bls12381Fr => {
                let i = pop!(V::Bls12381Fr);
                ctx.gas.consume(interpret_cost::INT_BLS_FR)?;
                stack.push(V::Int(i.to_big_int()))
            }
            overloads::Int::Bytes => {
                let i = pop!(V::Bytes);
                ctx.gas.consume(interpret_cost::int_bytes(i.len())?)?;
                stack.push(V::Int(BigInt::from_signed_bytes_be(&i)))
            }
        },
        I::Nat => {
            let i = pop!(V::Bytes);
            ctx.gas.consume(interpret_cost::int_bytes(i.len())?)?;
            stack.push(V::Nat(BigUint::from_bytes_be(&i)))
        }
        I::Bytes(overload) => match overload {
            overloads::Bytes::Nat => {
                let i = pop!(V::Nat);
                ctx.gas.consume(interpret_cost::bytes_nat(&i)?)?;
                stack.push(V::Bytes(if i.is_zero() {
                    Vec::new() // empty
                } else {
                    i.to_bytes_be()
                }));
            }
            overloads::Bytes::Int => {
                let i = pop!(V::Int);
                ctx.gas.consume(interpret_cost::bytes_int(&i)?)?;
                stack.push(V::Bytes(if i.is_zero() {
                    Vec::new() // empty
                } else {
                    i.to_signed_bytes_be()
                }));
            }
        },
        I::Loop(nested) => {
            ctx.gas.consume(interpret_cost::LOOP_ENTER)?;
            loop {
                ctx.gas.consume(interpret_cost::LOOP)?;
                if pop!(V::Bool) {
                    interpret(nested, ctx, arena, stack)?;
                } else {
                    ctx.gas.consume(interpret_cost::LOOP_EXIT)?;
                    break;
                }
            }
        }
        I::LoopLeft(nested) => {
            ctx.gas.consume(interpret_cost::LOOP_LEFT_ENTER)?;
            loop {
                ctx.gas.consume(interpret_cost::LOOP)?;
                match *pop!(V::Or) {
                    Or::Left(x) => {
                        stack.push(x);
                        interpret(nested, ctx, arena, stack)?;
                    }
                    Or::Right(x) => {
                        stack.push(x);
                        ctx.gas.consume(interpret_cost::LOOP_EXIT)?;
                        break;
                    }
                }
            }
        }
        I::Iter(overload, nested) => {
            ctx.gas.consume(interpret_cost::ITER)?;
            match overload {
                overloads::Iter::List => {
                    let lst = pop!(V::List);
                    for i in lst {
                        ctx.gas.consume(interpret_cost::PUSH)?;
                        stack.push(i);
                        interpret(nested, ctx, arena, stack)?;
                    }
                }
                overloads::Iter::Set => {
                    let set = pop!(V::Set);
                    for v in set {
                        ctx.gas.consume(interpret_cost::PUSH)?;
                        stack.push(v);
                        interpret(nested, ctx, arena, stack)?;
                    }
                }
                overloads::Iter::Map => {
                    let map = pop!(V::Map);
                    for (k, v) in map {
                        ctx.gas.consume(interpret_cost::PUSH)?;
                        stack.push(V::new_pair(k, v));
                        interpret(nested, ctx, arena, stack)?;
                    }
                }
            }
        }
        I::Push(v) => {
            ctx.gas.consume(interpret_cost::PUSH)?;
            stack.push(v.clone());
        }
        I::Swap => {
            ctx.gas.consume(interpret_cost::SWAP)?;
            stack.swap(0, 1);
        }
        I::Failwith(ty) => {
            let x = pop!();
            return Err(InterpretError::FailedWith(ty.clone(), x));
        }
        I::Never => unreachable_state(),
        I::Unit => {
            ctx.gas.consume(interpret_cost::UNIT)?;
            stack.push(V::Unit);
        }
        I::Car => {
            ctx.gas.consume(interpret_cost::CAR)?;
            let (l, _) = *pop!(V::Pair);
            stack.push(l);
        }
        I::Cdr => {
            ctx.gas.consume(interpret_cost::CDR)?;
            let (_, r) = *pop!(V::Pair);
            stack.push(r);
        }
        I::Pair => {
            ctx.gas.consume(interpret_cost::PAIR)?;
            let l = pop!();
            let r = pop!();
            stack.push(V::new_pair(l, r));
        }
        I::Unpair => {
            ctx.gas.consume(interpret_cost::UNPAIR)?;
            let (l, r) = *pop!(V::Pair);
            stack.push(r);
            stack.push(l);
        }
        I::ISome => {
            ctx.gas.consume(interpret_cost::SOME)?;
            let v = pop!();
            stack.push(V::new_option(Some(v)));
        }
        I::None => {
            ctx.gas.consume(interpret_cost::NONE)?;
            stack.push(V::new_option(None));
        }
        I::Compare => {
            let l = pop!();
            let r = pop!();
            ctx.gas.consume(interpret_cost::compare(&l, &r)?)?;
            let cmp = l.partial_cmp(&r).expect("comparison failed") as i8;
            stack.push(V::Int(cmp.into()));
        }
        I::Amount => {
            ctx.gas.consume(interpret_cost::AMOUNT)?;
            stack.push(V::Mutez(ctx.amount));
        }
        I::Nil => {
            ctx.gas.consume(interpret_cost::NIL)?;
            stack.push(V::List(MichelsonList::new()));
        }
        I::Cons => {
            ctx.gas.consume(interpret_cost::CONS)?;
            let elt = pop!();
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
                ctx.gas
                    .consume(interpret_cost::concat_string_pair(s1.len(), s2.len())?)?;
                s1.push_str(&s2);
                stack.push(V::String(s1));
            }
            overloads::Concat::TwoBytes => {
                let mut bs1 = pop!(V::Bytes);
                let bs2 = pop!(V::Bytes);
                ctx.gas
                    .consume(interpret_cost::concat_bytes_pair(bs1.len(), bs2.len())?)?;
                bs1.extend_from_slice(&bs2);
                stack.push(V::Bytes(bs1))
            }
            overloads::Concat::ListOfStrings => {
                let list = pop!(V::List);
                ctx.gas
                    .consume(interpret_cost::concat_list_precheck(list.len())?)?;

                let mut total_len = Checked::zero();
                for val in &list {
                    let s = irrefutable_match!(val; V::String);
                    total_len += s.len()
                }
                ctx.gas
                    .consume(interpret_cost::concat_string_list(total_len)?)?;

                let mut result = String::with_capacity(total_len.ok_or(OutOfGas)?);
                for val in list {
                    let s = irrefutable_match!(val; V::String);
                    result.push_str(&s);
                }
                stack.push(V::String(result))
            }
            overloads::Concat::ListOfBytes => {
                let list = pop!(V::List);
                ctx.gas
                    .consume(interpret_cost::concat_list_precheck(list.len())?)?;

                let mut total_len = Checked::zero();
                for val in &list {
                    let bs = irrefutable_match!(val; V::Bytes);
                    total_len += bs.len()
                }
                ctx.gas
                    .consume(interpret_cost::concat_bytes_list(total_len)?)?;

                let mut result = Vec::with_capacity(total_len.ok_or(OutOfGas)?);
                for val in &list {
                    let bs = irrefutable_match!(val; V::Bytes);
                    result.extend_from_slice(bs);
                }
                stack.push(V::Bytes(result))
            }
        },
        I::EmptySet => {
            use std::collections::BTreeSet;
            ctx.gas.consume(interpret_cost::EMPTY_SET)?;
            stack.push(V::Set(BTreeSet::new()))
        }
        I::Mem(overload) => match overload {
            overloads::Mem::Set => {
                let key = pop!();
                let set = pop!(V::Set);
                ctx.gas.consume(interpret_cost::set_mem(&key, set.len())?)?;
                let result = set.contains(&key);
                stack.push(V::Bool(result));
            }
            overloads::Mem::Map => {
                let key = pop!();
                let map = pop!(V::Map);
                ctx.gas.consume(interpret_cost::map_mem(&key, map.len())?)?;
                let result = map.contains_key(&key);
                stack.push(V::Bool(result));
            }
        },
        I::Get(overload) => match overload {
            overloads::Get::Map => {
                let key = pop!();
                let map = pop!(V::Map);
                ctx.gas.consume(interpret_cost::map_get(&key, map.len())?)?;
                let result = map.get(&key);
                stack.push(V::new_option(result.cloned()));
            }
        },
        I::Update(overload) => match overload {
            overloads::Update::Set => {
                let key = pop!();
                let new_present = pop!(V::Bool);
                let set = irrefutable_match!(&mut stack[0]; V::Set);
                ctx.gas
                    .consume(interpret_cost::set_update(&key, set.len())?)?;
                if new_present {
                    set.insert(key)
                } else {
                    set.remove(&key)
                };
            }
            overloads::Update::Map => {
                let key = pop!();
                let opt_new_val = pop!(V::Option);
                let map = irrefutable_match!(&mut stack[0]; V::Map);
                ctx.gas
                    .consume(interpret_cost::map_update(&key, map.len())?)?;
                match opt_new_val {
                    None => map.remove(&key),
                    Some(val) => map.insert(key, *val),
                };
            }
        },
        I::Size(overload) => {
            macro_rules! run_size {
                ($ctor:tt, $gas:ident) => {{
                    let e = pop!(V::$ctor);
                    ctx.gas.consume(interpret_cost::$gas)?;
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
        I::ChainId => {
            ctx.gas.consume(interpret_cost::CHAIN_ID)?;
            stack.push(V::ChainId(ctx.chain_id.clone()));
        }
        I::ISelf(entrypoint) => {
            ctx.gas.consume(interpret_cost::SELF)?;
            stack.push(V::Contract(Address {
                hash: ctx.self_address.clone(),
                entrypoint: entrypoint.clone(),
            }));
        }
        I::Pack => {
            ctx.gas.consume(interpret_cost::PACK)?;
            let v = pop!();
            let arena = Arena::new();
            // In the Tezos implementation they also charge gas for the pass
            // that strips locations. We don't have it.
            let mich = v.into_micheline_optimized_legacy(&arena);
            ctx.gas
                .consume(interpret_cost::micheline_encoding(&mich)?)?;
            let encoded = mich.encode_for_pack();
            stack.push(V::Bytes(encoded));
        }
        I::Unpack(ty) => {
            let bytes = pop!(V::Bytes);
            ctx.gas.consume(interpret_cost::unpack(bytes.as_slice())?)?;
            let mut try_unpack = || -> Option<TypedValue> {
                let mich = Micheline::decode_packed(arena, &bytes).ok()?;
                crate::interpreter::typecheck_value(&mich, ctx, ty).ok()
            };
            stack.push(V::new_option(try_unpack()));
        }
        I::CheckSignature => {
            let key = pop!(V::Key);
            let sig = pop!(V::Signature);
            let msg = pop!(V::Bytes);
            ctx.gas
                .consume(interpret_cost::check_signature(&key, &msg)?)?;
            stack.push(V::Bool(sig.check(&key, &msg)));
        }
        I::TransferTokens => {
            let param = pop!();
            let mutez_amount = pop!(V::Mutez);
            let contract_address = pop!(V::Contract);
            let counter = ctx.operation_counter();
            ctx.gas.consume(interpret_cost::TRANSFER_TOKENS)?;
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
            let opt_keyhash = pop!(V::Option).map(|kh| irrefutable_match!(*kh; V::KeyHash));
            let counter: u128 = ctx.operation_counter();
            ctx.gas.consume(interpret_cost::SET_DELEGATE)?;
            stack.push(V::new_operation(
                Operation::SetDelegate(SetDelegate(opt_keyhash)),
                counter,
            ))
        }
        I::Address => {
            ctx.gas.consume(interpret_cost::ADDRESS)?;
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
                    let str = pop!(V::String);

                    ctx.gas.consume(interpret_cost::slice(str.len())?)?;
                    validate_bounds(offset, length, str.len())
                        .and_then(|range| str.get(range))
                        .map(|str| V::String(str.to_string()))
                }
                overloads::Slice::Bytes => {
                    let bytes = pop!(V::Bytes);

                    ctx.gas.consume(interpret_cost::slice(bytes.len())?)?;
                    validate_bounds(offset, length, bytes.len())
                        .and_then(|range| bytes.get(range))
                        .map(|bytes| V::Bytes(bytes.to_owned()))
                }
            };
            stack.push(V::new_option(result));
        }
        I::Left => {
            ctx.gas.consume(interpret_cost::LEFT)?;
            let left = pop!();
            stack.push(V::new_or(Or::Left(left)));
        }
        I::Right => {
            ctx.gas.consume(interpret_cost::RIGHT)?;
            let right = pop!();
            stack.push(V::new_or(Or::Right(right)));
        }
        I::Lambda(lam) => {
            ctx.gas.consume(interpret_cost::LAMBDA)?;
            stack.push(V::Lambda(Closure::Lambda(lam.clone())));
        }
        I::Exec => {
            ctx.gas.consume(interpret_cost::EXEC)?;
            let mut arg = pop!();
            let mut closure = pop!(V::Lambda);
            loop {
                match closure {
                    Closure::Lambda(ref lam) => {
                        let mut res_stk = match &lam {
                            Lambda::LambdaRec { code, .. } => {
                                // NB: this `clone` is constant-time as `code` is Rc
                                // See Note: Rc in lambdas
                                let code = Rc::clone(code);
                                let mut stk = stk![V::Lambda(closure), arg];
                                interpret(&code, ctx, arena, &mut stk)?;
                                stk
                            }
                            Lambda::Lambda { code, .. } => {
                                let mut stk = stk![arg];
                                interpret(code, ctx, arena, &mut stk)?;
                                stk
                            }
                        };
                        stack.push(res_stk.pop().unwrap_or_else(|| unreachable_state()));
                        break;
                    }
                    Closure::Apply {
                        arg_val,
                        closure: inner,
                        ..
                    } => {
                        ctx.gas.consume(interpret_cost::PAIR)?; // reasonable estimation
                        arg = V::new_pair(*arg_val, arg);
                        closure = *inner;
                    }
                }
            }
        }
        I::Apply { arg_ty } => {
            let arg_val = pop!();
            let closure = pop!(V::Lambda);
            ctx.gas.consume(interpret_cost::APPLY)?;
            stack.push(V::Lambda(Closure::Apply {
                arg_ty: arg_ty.clone(),
                arg_val: Box::new(arg_val),
                closure: Box::new(closure),
            }))
        }
        I::HashKey => {
            ctx.gas.consume(interpret_cost::HASH_KEY)?;
            let key = pop!(V::Key);
            stack.push(TypedValue::KeyHash(key.hash()))
        }
        I::Ticket => {
            let content = pop!();
            let amount = pop!(V::Nat);
            ctx.gas.consume(interpret_cost::TICKET)?;
            if amount.is_zero() {
                // If the amount is zero, then we push a None value
                // as per the specified instruction behavior.
                stack.push(V::new_option(None));
            } else {
                let ticket = Ticket {
                    ticketer: ctx.self_address.clone(),
                    content,
                    amount,
                };
                stack.push(V::new_option(Some(V::new_ticket(ticket))));
            }
        }
        I::ReadTicket => {
            ctx.gas.consume(interpret_cost::READ_TICKET)?;
            stack.push(unwrap_ticket(
                irrefutable_match!(&stack[0]; V::Ticket).as_ref().clone(),
            ));
        }
        I::SplitTicket => {
            let ticket = *pop!(V::Ticket);
            let amounts = pop!(V::Pair);
            let amount_left = irrefutable_match!(amounts.0; V::Nat);
            let amount_right = irrefutable_match!(amounts.1; V::Nat);

            ctx.gas
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
            let tickets = pop!(V::Pair);
            let mut ticket_left = irrefutable_match!(tickets.0; V::Ticket);
            let ticket_right = irrefutable_match!(tickets.1; V::Ticket);
            ctx.gas
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
            let msg = irrefutable_match!(&mut stack[0]; V::Bytes);
            ctx.gas.consume(interpret_cost::blake2b(msg)?)?;
            *msg = blake2b_256(msg).to_vec();
        }
        I::Keccak => {
            let msg = irrefutable_match!(&mut stack[0]; V::Bytes);
            ctx.gas.consume(interpret_cost::keccak(msg)?)?;
            *msg = keccak256(msg).to_vec();
        }
        I::Sha256 => {
            let msg = irrefutable_match!(&mut stack[0]; V::Bytes);
            ctx.gas.consume(interpret_cost::sha256(msg)?)?;
            *msg = sha256(msg).to_vec();
        }
        I::Sha3 => {
            let msg = irrefutable_match!(&mut stack[0]; V::Bytes);
            ctx.gas.consume(interpret_cost::sha3(msg)?)?;
            *msg = sha3_256(msg).to_vec();
        }
        I::Sha512 => {
            let msg = irrefutable_match!(&mut stack[0]; V::Bytes);
            ctx.gas.consume(interpret_cost::sha512(msg)?)?;
            *msg = sha512(msg).to_vec();
        }
        I::Balance => {
            ctx.gas.consume(interpret_cost::BALANCE)?;
            stack.push(V::Mutez(ctx.balance));
        }
        I::Contract(typ, ep) => {
            ctx.gas.consume(interpret_cost::CONTRACT)?;
            let address = pop!(V::Address);
            stack.push(TypedValue::new_option(
                typecheck_contract_address(ctx, address, ep.clone(), typ)
                    .ok()
                    .map(TypedValue::Contract),
            ));
        }
        I::Level => {
            ctx.gas.consume(interpret_cost::LEVEL)?;
            stack.push(TypedValue::Nat(ctx.level.clone()));
        }
        I::MinBlockTime => {
            ctx.gas.consume(interpret_cost::MIN_BLOCK_TIME)?;
            stack.push(TypedValue::Nat(ctx.min_block_time.clone()));
        }
        I::SelfAddress => {
            ctx.gas.consume(interpret_cost::SELF_ADDRESS)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.self_address.clone(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Sender => {
            ctx.gas.consume(interpret_cost::SENDER)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.sender.clone(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Source => {
            ctx.gas.consume(interpret_cost::SOURCE)?;
            stack.push(TypedValue::Address(Address {
                hash: ctx.source.clone(),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::Now => {
            ctx.gas.consume(interpret_cost::NOW)?;
            stack.push(TypedValue::Timestamp(ctx.now.clone()));
        }
        I::ImplicitAccount => {
            ctx.gas.consume(interpret_cost::IMPLICIT_ACCOUNT)?;
            let keyhash = pop!(V::KeyHash);
            stack.push(TypedValue::Contract(Address {
                hash: AddressHash::Implicit(keyhash),
                entrypoint: Entrypoint::default(),
            }));
        }
        I::VotingPower => {
            ctx.gas.consume(interpret_cost::VOTING_POWER)?;
            let keyhash = pop!(V::KeyHash);
            stack.push(TypedValue::Nat((ctx.voting_powers)(&keyhash)))
        }
        I::TotalVotingPower => {
            ctx.gas.consume(interpret_cost::TOTAL_VOTING_POWER)?;
            stack.push(TypedValue::Nat(ctx.total_voting_power.clone()))
        }
        I::PairingCheck => {
            let list = pop!(V::List);
            ctx.gas
                .consume(interpret_cost::pairing_check(list.len())?)?;
            let it = list.iter().map(|elt| {
                let (g1, g2) = irrefutable_match!(elt; V::Pair).as_ref();
                (
                    irrefutable_match!(g1; V::Bls12381G1).as_ref(),
                    irrefutable_match!(g2; V::Bls12381G2).as_ref(),
                )
            });
            let res = bls::pairing::pairing_check(it);
            stack.push(V::Bool(res));
        }
        I::Seq(nested) => interpret(nested, ctx, arena, stack)?,
    }
    Ok(())
}

#[cfg(test)]
mod interpreter_tests {
    use std::collections::{BTreeMap, BTreeSet, HashMap};

    use super::*;
    use super::{Lambda, Or};
    use crate::ast::michelson_address as addr;
    use crate::bls;
    use crate::gas::Gas;
    use num_bigint::BigUint;
    use Instruction::*;
    use Option::None;
    use TypedValue as V;

    #[track_caller]
    fn mk_0x(hex: &str) -> TypedValue {
        V::Bytes(hex::decode(hex).unwrap_or_else(|e| panic!("Invalid hex: {e}")))
    }

    fn interpret<'a>(
        ast: &[Instruction<'a>],
        ctx: &mut Ctx,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        let temp = Box::leak(Box::default());
        super::interpret(ast, ctx, temp, stack)
    }

    fn interpret_one<'a>(
        i: &Instruction<'a>,
        ctx: &mut Ctx,
        stack: &mut IStack<'a>,
    ) -> Result<(), InterpretError<'a>> {
        let temp = Box::leak(Box::default());
        super::interpret_one(i, ctx, temp, stack)
    }

    #[test]
    fn test_add() {
        let mut stack = stk![V::nat(10), V::nat(20)];
        let expected_stack = stk![V::nat(30)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(&Add(overloads::Add::NatNat), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
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
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 20);
        assert_eq!(stack, stk![V::Mutez(2i64.pow(62) + 20)]);
        assert_eq!(
            interpret_one(
                &Add(overloads::Add::MutezMutez),
                &mut ctx,
                &mut stk![V::Mutez(2i64.pow(62)), V::Mutez(2i64.pow(62))]
            ),
            Err(InterpretError::MutezOverflow)
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
            Err(InterpretError::MutezOverflow)
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
            Err(InterpretError::MutezOverflow)
        );
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
        assert!(interpret_one(
            &Dip(None, vec![Add(overloads::Add::NatNat)]),
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
        assert!(interpret_one(&Dip(Some(2), vec![Drop(None)]), &mut ctx, &mut stack).is_ok());
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
            assert!(ctx.gas.milligas() < Ctx::default().gas.milligas());
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
        assert!(interpret_one(
            &If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)]),
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
        assert!(interpret_one(
            &If(vec![Drop(None)], vec![Add(overloads::Add::IntInt)]),
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
            assert!(ctx.gas.milligas() < Ctx::default().gas.milligas());
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
            assert!(ctx.gas.milligas() < Ctx::default().gas.milligas());
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
        assert!(interpret_one(&Push(V::nat(0)), &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, expected_stack);
    }

    #[test]
    fn test_loop_0() {
        let mut stack = stk![V::nat(20), V::nat(10), V::Bool(false)];
        let expected_stack = stk![V::nat(20), V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret_one(
            &Loop(vec![
                Push(V::nat(1)),
                Add(overloads::Add::NatNat),
                Push(V::Bool(false))
            ]),
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
        assert!(interpret_one(
            &Loop(vec![
                Push(V::nat(1)),
                Add(overloads::Add::NatNat),
                Push(V::Bool(false))
            ]),
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
        assert!(interpret_one(
            &Loop(vec![
                Push(V::int(-1)),
                Add(overloads::Add::IntInt),
                Dup(None),
                Gt
            ]),
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
            interpret_one(&LoopLeft(vec![Failwith(Type::Nat)]), &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::nat(0)])
    }

    #[test]
    fn loop_left_1() {
        let mut stack = stk![V::new_or(Or::Left(V::nat(0)))];
        let mut ctx = Ctx::default();
        assert_eq!(
            interpret_one(
                &LoopLeft(vec![Drop(None), Push(V::new_or(Or::Right(V::int(1))))]),
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
            V::List(vec![].into()),
            V::List((1..5).map(V::int).collect())
        ];
        assert!(interpret_one(
            &Iter(overloads::Iter::List, vec![Cons]),
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
        let mut stack = stk![V::Unit, V::List(vec![].into())];
        assert!(interpret_one(
            &Iter(overloads::Iter::List, vec![Drop(None)]),
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::Unit]);
    }

    #[test]
    fn test_iter_set_many() {
        let mut stack = stk![
            V::List(vec![].into()),
            V::Set(
                vec![(V::int(1)), (V::int(2)), (V::int(3)),]
                    .into_iter()
                    .collect()
            )
        ];
        assert!(interpret_one(
            &Iter(overloads::Iter::Set, vec![Cons]),
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
        assert!(interpret_one(
            &Iter(overloads::Iter::Set, vec![Add(overloads::Add::IntInt)]),
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(0)]);
    }

    #[test]
    fn test_iter_map_many() {
        let mut stack = stk![
            V::List(vec![].into()),
            V::Map(
                vec![
                    (V::int(1), V::nat(1)),
                    (V::int(2), V::nat(2)),
                    (V::int(3), V::nat(3)),
                ]
                .into_iter()
                .collect()
            )
        ];
        assert!(interpret_one(
            &Iter(overloads::Iter::Map, vec![Cons]),
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
        assert!(interpret_one(
            &Iter(overloads::Iter::Map, vec![Car, Add(overloads::Add::IntInt)]),
            &mut Ctx::default(),
            &mut stack,
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(0)]);
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
        let mut stack = stk![];
        assert_eq!(
            interpret(
                &[Push(V::String("foo".to_owned()))],
                &mut Ctx::default(),
                &mut stack
            ),
            Ok(())
        );
        assert_eq!(stack, stk![V::String("foo".to_owned())]);
    }

    #[test]
    fn push_unit_value() {
        let mut stack = stk![];
        assert_eq!(
            interpret(&[Push(V::Unit)], &mut Ctx::default(), &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Unit]);
    }

    #[test]
    fn unit_instruction() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Unit], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::Unit]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::UNIT - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_pair() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Push(V::new_pair(
                V::int(-5),
                V::new_pair(V::nat(3), V::Bool(false))
            ))],
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn push_option() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[Push(V::new_option(Some(V::int(-5))))],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::new_option(Some(V::int(-5)))]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn car() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[
                Push(V::new_pair(
                    V::int(-5),
                    V::new_pair(V::nat(3), V::Bool(false))
                )),
                Car
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(-5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::PUSH
                - interpret_cost::CAR
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn cdr() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(
            &[
                Push(V::new_pair(
                    V::new_pair(V::nat(3), V::Bool(false)),
                    V::int(-5),
                )),
                Cdr
            ],
            &mut ctx,
            &mut stack
        )
        .is_ok());
        assert_eq!(stack, stk![V::int(-5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
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
    fn unpair() {
        let mut stack = stk![V::new_pair(V::Bool(false), V::nat(42))];
        assert!(interpret(&[Unpair], &mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![V::nat(42), V::Bool(false)]);
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
        let code = vec![IfNone(vec![Push(V::int(5))], vec![])];
        // with Some
        let mut stack = stk![V::new_option(Some(V::int(42)))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(42)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::IF_NONE - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_none_2() {
        let code = vec![IfNone(vec![Push(V::int(5))], vec![])];
        // with None
        let mut stack = stk![V::new_option(None)];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(5)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::IF_NONE
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_cons_cons() {
        let code = vec![IfCons(vec![Swap, Drop(None)], vec![Push(V::int(0))])];
        let mut stack = stk![V::List(vec![V::int(1), V::int(2)].into())];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(1)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::IF_CONS
                - interpret_cost::SWAP
                - interpret_cost::DROP
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_cons_nil() {
        let code = vec![IfCons(vec![Swap, Drop(None)], vec![Push(V::int(0))])];
        let mut stack = stk![V::List(vec![].into())];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(0)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::IF_CONS
                - interpret_cost::PUSH
                - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_left_left() {
        let code = vec![IfLeft(vec![], vec![Drop(None), Push(V::int(0))])];
        let mut stack = stk![V::new_or(or::Or::Left(V::int(1)))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(1)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::IF_LEFT - interpret_cost::INTERPRET_RET * 2
        );
    }

    #[test]
    fn if_left_right() {
        let code = vec![IfLeft(vec![], vec![Drop(None), Push(V::int(0))])];
        let mut stack = stk![V::new_or(or::Or::Right(V::Unit))];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&code, &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::int(0)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::IF_LEFT
                - interpret_cost::DROP
                - interpret_cost::PUSH
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::SOME - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn none() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Instruction::None], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_option(None)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::NONE - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn compare() {
        macro_rules! test {
            ($expr:tt, $res:tt) => {
                let mut stack = stk!$expr;
                let expected_cost = interpret_cost::compare(&stack[0], &stack[1]).unwrap()
                    + interpret_cost::INTERPRET_RET;
                let mut ctx = Ctx::default();
                assert!(interpret(&[Compare], &mut ctx, &mut stack).is_ok());
                assert_eq!(stack, stk!$res);
                assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - expected_cost);
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
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        ctx.amount = 100500;
        assert_eq!(interpret(&[Amount], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Mutez(100500)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::INTERPRET_RET - interpret_cost::AMOUNT,
        )
    }

    #[test]
    fn push_int_list() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert_eq!(
            interpret(
                &[Push(V::List(vec![V::int(1), V::int(2), V::int(3),].into()))],
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn nil() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&[Nil], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::List(vec![].into())]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::NIL - interpret_cost::INTERPRET_RET,
        )
    }

    #[test]
    fn cons() {
        let mut stack = stk![V::List(vec![V::int(321)].into()), V::int(123)];
        let mut ctx = Ctx::default();
        assert_eq!(interpret(&[Cons], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::List(vec![V::int(123), V::int(321)].into())]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::CONS - interpret_cost::INTERPRET_RET,
        )
    }

    #[test]
    fn push_map() {
        let mut stack = stk![];
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
            (V::int(1), V::String("foo".to_owned())),
            (V::int(2), V::String("bar".to_owned())),
        ]);
        assert_eq!(
            interpret(&[Push(V::Map(map.clone()))], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Map(map)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::PUSH - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_map() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
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
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_get(&V::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn get_map_none() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
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
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_get(&V::int(100500), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn mem_map() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
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
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_mem(&TypedValue::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn mem_map_absent() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([
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
        let set = BTreeSet::from([TypedValue::int(1), TypedValue::int(2)]);
        let mut stack = stk![TypedValue::Set(set), TypedValue::int(1)];
        assert_eq!(
            interpret(&[Mem(overloads::Mem::Set)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![TypedValue::Bool(true)]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::set_mem(&TypedValue::int(1), 2).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn mem_set_absent() {
        let mut ctx = Ctx::default();
        let set = BTreeSet::from([TypedValue::int(1), TypedValue::int(2)]);
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
        let mut stack = stk![];
        assert_eq!(interpret(&[EmptySet], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![TypedValue::Set(BTreeSet::new())]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::EMPTY_SET - interpret_cost::INTERPRET_RET
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
        assert_eq!(
            stack,
            stk![TypedValue::Set(BTreeSet::from([TypedValue::int(1)])),]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::set_update(&TypedValue::int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_set_remove() {
        let mut ctx = Ctx::default();
        let set = BTreeSet::from([TypedValue::int(1)]);
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
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::set_update(&TypedValue::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_set_insert_when_exists() {
        let mut ctx = Ctx::default();
        let set = BTreeSet::from([TypedValue::int(1)]);
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
            ctx.gas.milligas(),
            Gas::default().milligas()
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
            ctx.gas.milligas(),
            Gas::default().milligas()
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
            stk![V::Map(BTreeMap::from([(
                V::int(1),
                V::String("foo".to_owned())
            )])),]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_update(&V::int(1), 0).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_update() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([(V::int(1), V::String("bar".to_owned()))]);
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
            stk![V::Map(BTreeMap::from([(
                V::int(1),
                V::String("foo".to_owned())
            )])),]
        );
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
                - interpret_cost::map_update(&V::int(1), 1).unwrap()
                - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn update_map_remove() {
        let mut ctx = Ctx::default();
        let map = BTreeMap::from([(V::int(1), V::String("bar".to_owned()))]);
        let mut stack = stk![V::Map(map), V::new_option(None), V::int(1)];
        assert_eq!(
            interpret(&[Update(overloads::Update::Map)], &mut ctx, &mut stack),
            Ok(())
        );
        assert_eq!(stack, stk![V::Map(BTreeMap::new())]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas()
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::SIZE_STRING - interpret_cost::INTERPRET_RET
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::SIZE_LIST - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn size_set() {
        let mut ctx = Ctx::default();
        let set = (1..=3).map(TypedValue::nat).collect();
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
        let map = BTreeMap::from([
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
        let mut stack = stk![TypedValue::List(vec![].into()),];
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
        let mut stack = stk![TypedValue::List(vec![].into())];
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
    #[should_panic(
        expected = "Unreachable state reached during interpreting, possibly broken typechecking!"
    )]
    fn trigger_unreachable_state() {
        let mut stack = stk![];
        interpret(
            &[Add(overloads::Add::NatInt)],
            &mut Ctx::default(),
            &mut stack,
        )
        .unwrap(); // panics
    }

    #[test]
    fn chain_id_instr() {
        let chain_id = super::ChainId::from_base58_check("NetXynUjJNZm7wi").unwrap();
        let ctx = &mut Ctx::default();
        ctx.chain_id = chain_id.clone();
        let start_milligas = ctx.gas.milligas();
        let stk = &mut stk![];
        assert_eq!(interpret(&[Instruction::ChainId], ctx, stk), Ok(()));
        assert_eq!(stk, &stk![V::ChainId(chain_id)]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
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
    fn self_instr() {
        let stk = &mut stk![];
        let ctx = &mut Ctx::default();
        ctx.self_address = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT".try_into().unwrap();
        assert_eq!(interpret(&[ISelf(Entrypoint::default())], ctx, stk), Ok(()));
        assert_eq!(
            stk,
            &stk![V::Contract(
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
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[TransferTokens], ctx, stk), Ok(()));
        assert_eq!(
            stk,
            &stk![V::new_operation(Operation::TransferTokens(tt), 101)]
        );
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::TRANSFER_TOKENS + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn set_delegate() {
        use Instruction as I;
        let sd = super::SetDelegate(Some(
            KeyHash::try_from("tz3h4mjmMieZKSaSBWBC7XmeL6JQ3hucFDcP").unwrap(),
        ));
        let stk = &mut stk![V::new_option(Some(V::KeyHash(sd.0.clone().unwrap())))];
        let ctx = &mut Ctx::default();
        ctx.set_operation_counter(100);
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[I::SetDelegate], ctx, stk), Ok(()));
        assert_eq!(
            stk,
            &stk![V::new_operation(Operation::SetDelegate(sd), 101)]
        );
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
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
            KeyHash::try_from("tz3h4mjmMieZKSaSBWBC7XmeL6JQ3hucFDcP").unwrap(),
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
        let stk = &mut stk![];
        let ctx = &mut Ctx::default();
        ctx.self_address = "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT".try_into().unwrap();
        assert_eq!(
            interpret(&[ISelf(Entrypoint::try_from("foo").unwrap())], ctx, stk),
            Ok(())
        );
        assert_eq!(
            stk,
            &stk![V::Contract(
                "KT18amZmM5W7qDWVt2pH6uj7sCEd3kbzLrHT%foo"
                    .try_into()
                    .unwrap()
            )]
        );
    }

    #[test]
    fn check_signature() {
        for (key, msg, sig, res) in michelson_signature::tests::signature_fixtures() {
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::ADDRESS - interpret_cost::INTERPRET_RET
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
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::LEFT - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn right() {
        let mut stack = stk![V::nat(10)];
        let mut ctx = Ctx::default();
        assert!(interpret(&[Instruction::Right], &mut ctx, &mut stack).is_ok());
        assert_eq!(stack, stk![V::new_or(or::Or::Right(V::nat(10)))]);
        assert_eq!(
            ctx.gas.milligas(),
            Gas::default().milligas() - interpret_cost::RIGHT - interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn ticket() {
        let mut stack = stk![V::nat(100), TypedValue::Unit];
        let mut ctx = Ctx::default();
        let expected_ticket = V::new_ticket(crate::ast::Ticket {
            ticketer: ctx.self_address.clone(),
            amount: 100u32.into(),
            content: V::Unit,
        });

        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Ticket], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::new_option(Some(expected_ticket))]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
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
            content: V::Unit,
        };
        let ticket_val = V::new_ticket(ticket.clone());
        let mut stack = stk![ticket_val.clone()];
        let start_milligas = ctx.gas.milligas();
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
            start_milligas - ctx.gas.milligas(),
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
                        Push(TypedValue::Bool(false)),
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
                        Push(TypedValue::Bool(false)),
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
            ctx.gas.milligas(),
            Ctx::default().gas.milligas() - interpret_cost::HASH_KEY
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
                        Push(TypedValue::Bool(false)),
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
                        Push(TypedValue::Bool(false)),
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
            interpret_one(&Exec, &mut Ctx::default(), &mut stack),
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
            content: V::Unit,
        };
        let ticket_val = V::new_ticket(ticket.clone());
        let mut stack = stk![V::new_pair(V::nat(20), V::nat(80)), ticket_val.clone(),];

        let start_milligas = ctx.gas.milligas();
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
            start_milligas - ctx.gas.milligas(),
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
            content: V::nat(10),
        };
        let ticket_left = V::new_ticket(ticket.clone());
        let ticket_right_ = Ticket {
            amount: 20u32.into(),
            ..ticket.clone()
        };
        let ticket_right = V::new_ticket(ticket_right_.clone());
        let mut stack = stk![V::new_pair(ticket_left, ticket_right),];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[JoinTickets], &mut ctx, &mut stack), Ok(()));
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
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
            let out = irrefutable_match!(&stack[0]; V::Bytes);
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
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Balance], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Mutez(70),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
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
            use crate::ast::michelson_address::entrypoint::Entrypoints;
            let mut ctx = Ctx::default();
            if let Some(e) = opt_entrypoints {
                ctx.set_known_contracts({
                    let mut x: HashMap<AddressHash, Entrypoints> = HashMap::new();
                    x.insert(address.hash, HashMap::from_iter(Vec::from(e)));
                    x
                })
            }

            let start_milligas = ctx.gas.milligas();
            assert_eq!(interpret(&[instr], &mut ctx, &mut start_stack), Ok(()));
            assert_eq!(start_stack, stack_expect);
            if let Some(exp_gas) = opt_exp_gas {
                assert_eq!(start_milligas - ctx.gas.milligas(), exp_gas);
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
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Level], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(70),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::LEVEL + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn min_block_time() {
        let mut ctx = Ctx::default();
        ctx.min_block_time = 70u32.into();
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[MinBlockTime], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(70),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::MIN_BLOCK_TIME + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn self_address() {
        let addr = super::Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let mut ctx = Ctx::default();
        ctx.self_address = addr.hash.clone();
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[SelfAddress], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Address(addr),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::SELF_ADDRESS + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn sender() {
        let addr = super::Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let mut ctx = Ctx::default();
        ctx.sender = addr.hash.clone();
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Sender], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Address(addr),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::SENDER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn source() {
        let addr = super::Address::try_from("tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP").unwrap();
        let mut ctx = Ctx::default();
        ctx.source = addr.hash.clone();
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Source], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::Address(addr),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::SOURCE + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn now() {
        let mut ctx = Ctx::default();
        ctx.now = 7000i32.into();
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[Now], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::timestamp(7000),]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::NOW + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn implicit_account() {
        let mut ctx = Ctx::default();
        let key_hash = KeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let mut stack = stk![V::KeyHash(key_hash)];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[ImplicitAccount], &mut ctx, &mut stack), Ok(()));
        assert_eq!(
            stack,
            stk![V::Contract(
                super::Address::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap()
            ),]
        );
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::NOW + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn voting_power() {
        let key_hash_1 = KeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = KeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        let key_hash_3 = KeyHash::try_from("tz3hpojUX9dYL5KLusv42SCBiggB77a2QLGx").unwrap();

        let mut ctx = Ctx::default();
        ctx.set_voting_powers([
            (key_hash_1.clone(), 30u32.into()),
            (key_hash_2.clone(), 50u32.into()),
        ]);
        let mut stack = stk![TypedValue::KeyHash(key_hash_2.clone())];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[VotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(50)]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::VOTING_POWER + interpret_cost::INTERPRET_RET
        );

        // When key_hash is not in context.
        let mut ctx = Ctx::default();
        ctx.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
        let mut stack = stk![TypedValue::KeyHash(key_hash_3)];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[VotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(0)]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::VOTING_POWER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
    fn total_voting_power() {
        let key_hash_1 = KeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = KeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        let mut ctx = Ctx::default();
        ctx.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
        let mut stack = stk![];
        let start_milligas = ctx.gas.milligas();
        assert_eq!(interpret(&[TotalVotingPower], &mut ctx, &mut stack), Ok(()));
        assert_eq!(stack, stk![V::nat(80)]);
        assert_eq!(
            start_milligas - ctx.gas.milligas(),
            interpret_cost::TOTAL_VOTING_POWER + interpret_cost::INTERPRET_RET
        );
    }

    #[test]
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
        assert!(Ctx::default().gas.milligas() > ctx.gas.milligas());
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
            assert!(Ctx::default().gas.milligas() > ctx.gas.milligas());
        }

        use crate::bls::*;
        use TypedValue as V;

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

        test!(
            Bls12381G1Bls12381Fr,
            V::new_bls12381_g1(G1::one()),
            V::Bls12381Fr(Fr::one()),
            V::new_bls12381_g1(G1::one()),
        );

        test!(
            Bls12381G2Bls12381Fr,
            V::new_bls12381_g2(G2::one()),
            V::Bls12381Fr(Fr::one()),
            V::new_bls12381_g2(G2::one()),
        );

        test!(
            Bls12381FrBls12381Fr,
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        test!(
            NatBls12381Fr,
            V::Nat(1u8.into()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        test!(
            IntBls12381Fr,
            V::Int(1.into()),
            V::Bls12381Fr(Fr::one()),
            V::Bls12381Fr(Fr::one()),
        );

        test!(
            Bls12381FrNat,
            V::Bls12381Fr(Fr::one()),
            V::Nat(1u8.into()),
            V::Bls12381Fr(Fr::one()),
        );

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

    mod neg {
        use super::*;

        #[track_caller]
        fn test_neg(overload: overloads::Neg, input: TypedValue, output: TypedValue) {
            let mut stack = stk![input];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&Neg(overload), ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![output]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas() > ctx.gas.milligas());
        }

        use crate::bls::*;
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

        test!(
            Bls12381G1,
            V::new_bls12381_g1(G1::one()),
            V::new_bls12381_g1(G1::neg_one()),
        );

        test!(
            Bls12381G2,
            V::new_bls12381_g2(G2::one()),
            V::new_bls12381_g2(G2::neg_one()),
        );

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

    #[test]
    fn test_sub_mutez() {
        fn test(v1: i64, v2: i64, res: Option<i64>) {
            let mut stack = stk![V::Mutez(v2), V::Mutez(v1)];
            let ctx = &mut Ctx::default();
            assert_eq!(interpret_one(&SubMutez, ctx, &mut stack), Ok(()));
            assert_eq!(stack, stk![V::new_option(res.map(V::Mutez))]);
            // assert some gas is consumed, exact values are subject to change
            assert!(Ctx::default().gas.milligas() > ctx.gas.milligas());
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
        assert!(ctx.gas.milligas() < Ctx::default().gas.milligas());
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
}
