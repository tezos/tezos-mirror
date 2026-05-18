// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `lambda 'a 'b` values.

use std::rc::Rc;

use crate::gas::{Gas, OutOfGas};
use crate::lexer::Prim;

use super::{Instruction, IntoMicheline, Micheline, Type, TypedValue};

/// Michelson lambda. Can be either non-recursive or recursive. Michelson
/// lambdas carry their own raw [Micheline] representation to ensure consistent
/// roundtripping through `PACK`/`UNPACK`.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Lambda<'a> {
    /// Non-recursive lambda.
    Lambda {
        /// Raw [Micheline] representation.
        micheline_code: Micheline<'a>,
        /// Typechecked code.
        code: Rc<[Instruction<'a>]>, // see Note: Rc in lambdas
    },
    /// Recursive lambda.
    LambdaRec {
        /// Lambda argument type
        in_ty: Type,
        /// Lambda result type
        out_ty: Type,
        /// Raw [Micheline] representation.
        micheline_code: Micheline<'a>,
        /// Typechecked code.
        code: Rc<[Instruction<'a>]>, // see Note: Rc in lambdas
    },
}

/* *** Note: Rc in lambdas ***

When interpreting recursive lambdas, the lambda itself is used as the argument
of its body, which means we'd need two copies of the body if we used `Vec`. To
avoid potentially costly cloning, we use an `Rc` instead.

For non-recursive Lambda the same approach is used to make `LAMBDA` instruction
independent of the length of the code (there's a Lambda::clone call in the
implementation)
*/

/// Either a simple [Lambda], or a partially-applied one, the result of the
/// `APPLY` instruction.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Closure<'a> {
    /// Simple [Lambda].
    Lambda(Lambda<'a>),
    /// Partially-applied [Lambda].
    Apply {
        /// Captured argument type
        arg_ty: Type,
        /// Captured argument value
        arg_val: Box<TypedValue<'a>>,
        /// Inner closure
        closure: Box<Closure<'a>>,
    },
}

impl<'a> IntoMicheline<'a> for Closure<'a> {
    /// Unwinds the `Closure::Apply` chain into a heap allocated `Vec` and
    /// assembles the result bottom up, so deeply nested `APPLY` chains do
    /// not blow the WASM call stack on the previous recursive call to
    /// `closure.into_micheline_optimized_legacy`.
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        gas: &mut Gas,
    ) -> Result<Micheline<'a>, OutOfGas> {
        // Walk the Apply spine, collecting (arg_ty, arg_val) pairs in outer
        // to inner order. Stop at the terminal Lambda. Iterative so a deep
        // APPLY chain does not blow the WASM call stack.
        let mut applies: Vec<(Type, TypedValue<'a>)> = Vec::new();
        let mut cur = self;
        let terminal: Lambda<'a> = loop {
            match cur {
                Closure::Apply {
                    arg_ty,
                    arg_val,
                    closure,
                } => {
                    applies.push((arg_ty, *arg_val));
                    cur = *closure;
                }
                Closure::Lambda(lambda) => break lambda,
            }
        };

        // No Applies: emit the bare lambda exactly as before.
        if applies.is_empty() {
            return Ok(match terminal {
                Lambda::Lambda { micheline_code, .. } => micheline_code,
                Lambda::LambdaRec { micheline_code, .. } => {
                    Micheline::prim1(arena, Prim::Lambda_rec, micheline_code, gas)?
                }
            });
        }

        // Innermost Apply: the layer directly wrapping the terminal. Shape
        // depends on the terminal variant: LambdaRec produces the
        // PUSH; PAIR; LAMBDA_REC; SWAP; EXEC sequence; Lambda produces
        // PUSH; PAIR; <code>.
        let (inner_arg_ty, inner_arg_val) = applies.pop().expect("non-empty");
        let mut acc: Micheline<'a> = match terminal {
            Lambda::LambdaRec {
                in_ty,
                out_ty,
                micheline_code,
                ..
            } => Micheline::seq_arr(
                arena,
                [
                    Micheline::prim2(
                        arena,
                        Prim::PUSH,
                        inner_arg_ty.into_micheline_optimized_legacy(arena, gas)?,
                        inner_arg_val.into_micheline_optimized_legacy(arena, gas)?,
                        gas,
                    )?,
                    Micheline::prim0(Prim::PAIR, gas)?,
                    Micheline::prim3(
                        arena,
                        Prim::LAMBDA_REC,
                        in_ty.into_micheline_optimized_legacy(arena, gas)?,
                        out_ty.into_micheline_optimized_legacy(arena, gas)?,
                        micheline_code,
                        gas,
                    )?,
                    Micheline::prim0(Prim::SWAP, gas)?,
                    Micheline::prim0(Prim::EXEC, gas)?,
                ],
                gas,
            )?,
            Lambda::Lambda { micheline_code, .. } => Micheline::seq_arr(
                arena,
                [
                    Micheline::prim2(
                        arena,
                        Prim::PUSH,
                        inner_arg_ty.into_micheline_optimized_legacy(arena, gas)?,
                        inner_arg_val.into_micheline_optimized_legacy(arena, gas)?,
                        gas,
                    )?,
                    Micheline::prim0(Prim::PAIR, gas)?,
                    micheline_code,
                ],
                gas,
            )?,
        };

        // Outer Apply layers: wrap acc in seq[PUSH(arg), PAIR, acc].
        while let Some((arg_ty, arg_val)) = applies.pop() {
            acc = Micheline::seq_arr(
                arena,
                [
                    Micheline::prim2(
                        arena,
                        Prim::PUSH,
                        arg_ty.into_micheline_optimized_legacy(arena, gas)?,
                        arg_val.into_micheline_optimized_legacy(arena, gas)?,
                        gas,
                    )?,
                    Micheline::prim0(Prim::PAIR, gas)?,
                    acc,
                ],
                gas,
            )?;
        }

        Ok(acc)
    }
}

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::{
        ast::{
            micheline::{
                test_helpers::{app, seq},
                IntoMicheline,
            },
            TypedValue,
        },
        context::Ctx,
        gas::Gas,
        irrefutable_match::irrefutable_match,
        parser::Parser,
        stack::Stack,
    };

    #[test]
    fn apply_micheline() {
        let parser = Parser::new();
        let arena = Arena::new();
        let code = parser.parse("{ LAMBDA (pair int nat unit) unit { DROP; UNIT }; PUSH int 1; APPLY; PUSH nat 2; APPLY }").unwrap();
        let code = code
            .typecheck_instruction(&mut Gas::default(), None, &[])
            .unwrap();
        let mut stack = Stack::new();
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(
            TypedValue::unwrap_rc(stack.pop().unwrap());
            TypedValue::Lambda
        );
        let arena = Arena::new();
        let mut gas = Gas::default();
        assert_eq!(
            closure
                .into_micheline_optimized_legacy(&arena, &mut gas)
                .unwrap(),
            // checked against octez-client
            // { PUSH nat 2 ; PAIR ; { PUSH int 1 ; PAIR ; { DROP ; UNIT } } }
            seq! {
              app!(PUSH[app!(nat), 2]);
              app!(PAIR);
              seq!{
                app!(PUSH[app!(int), 1]);
                app!(PAIR);
                seq! {
                  app!(DROP);
                  app!(UNIT)
                }
              }
            }
        )
    }

    #[test]
    fn apply_micheline_rec() {
        let parser = Parser::new();
        let arena = Arena::new();
        let code = parser.parse("{ LAMBDA_REC (pair int nat unit) unit { DROP 2; UNIT }; PUSH int 1; APPLY; PUSH nat 2; APPLY }").unwrap();
        let code = code
            .typecheck_instruction(&mut Gas::default(), None, &[])
            .unwrap();
        let mut stack = Stack::new();
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(
            TypedValue::unwrap_rc(stack.pop().unwrap());
            TypedValue::Lambda
        );
        let arena = Arena::new();
        let mut gas = Gas::default();
        assert_eq!(
            closure
                .into_micheline_optimized_legacy(&arena, &mut gas)
                .unwrap(),
            // checked against octez-client
            //   { PUSH nat 2 ;
            //     PAIR ;
            //     { PUSH int 1 ;
            //       PAIR ;
            //       LAMBDA_REC (pair int nat unit) unit { DROP 2 ; UNIT } ;
            //       SWAP ;
            //       EXEC } }
            seq! {
              app!(PUSH[app!(nat), 2]);
              app!(PAIR);
              seq!{
                app!(PUSH[app!(int), 1]);
                app!(PAIR);
                app!(LAMBDA_REC[app!(pair[app!(int), app!(nat), app!(unit)]), app!(unit), seq!{
                  app!(DROP[2]); app!(UNIT)
                }]);
                app!(SWAP);
                app!(EXEC)
              }
            }
        )
    }

    #[test]
    fn apply_micheline_rec_pair_linearization() {
        // PACK always encodes pair values as right-combs, and always encodes
        // pair types as a flat sequence. Test we're doing the same.
        let parser = Parser::new();
        let arena = Arena::new();
        let code = parser
            .parse(
                r#"
                  {
                    LAMBDA (pair (pair nat nat nat) unit) unit {CDR;};
                    PUSH
                      (pair nat (pair nat nat))    # The pair type is given as a tree
                      (Pair 1 1 1);                # The pair value is given as a flat sequence
                    APPLY;
                  }
                "#,
            )
            .unwrap();
        let code = code
            .typecheck_instruction(&mut Gas::default(), None, &[])
            .unwrap();
        let mut stack = Stack::new();
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(
            TypedValue::unwrap_rc(stack.pop().unwrap());
            TypedValue::Lambda
        );
        let arena = Arena::new();
        let mut gas = Gas::default();
        assert_eq!(
            closure
                .into_micheline_optimized_legacy(&arena, &mut gas)
                .unwrap(),
            // checked against octez-client's PACK behaviour.
            // the partially-applied lambda above packs into
            // 0x020000002507430965000000060362036203620000000007070001070700010001034202000000020317
            // which, converted to Micheline, yields:
            //   { PUSH (pair nat nat nat) # pair is now a flat sequence
            //          (Pair 1 (Pair 1 1)) ; # Pair is now an explicit comb
            //     PAIR ;
            //     { CDR }
            //   }
            seq! {
                app!(PUSH[
                    app!(pair[app!(nat), app!(nat), app!(nat)]),
                    app!(Pair[1, app!(Pair[1, 1])])
                ]);
                app!(PAIR);
                seq!{ app!(CDR) }
            }
        )
    }
}
