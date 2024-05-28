/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Representation for typed Michelson `lambda 'a 'b` values.

use std::rc::Rc;

use crate::lexer::Prim;

use super::{annotations::NO_ANNS, Instruction, IntoMicheline, Micheline, Type, TypedValue};

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
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a typed_arena::Arena<Micheline<'a>>,
    ) -> Micheline<'a> {
        match self {
            Closure::Lambda(Lambda::Lambda { micheline_code, .. }) => micheline_code,
            Closure::Lambda(Lambda::LambdaRec { micheline_code, .. }) => {
                Micheline::prim1(arena, Prim::Lambda_rec, micheline_code)
            }
            Closure::Apply {
                arg_ty,
                arg_val,
                closure,
            } => match *closure {
                Closure::Lambda(Lambda::LambdaRec {
                    in_ty,
                    out_ty,
                    micheline_code,
                    ..
                }) => Micheline::seq(
                    arena,
                    [
                        Micheline::prim2(
                            arena,
                            Prim::PUSH,
                            arg_ty.into_micheline_optimized_legacy(arena),
                            arg_val.into_micheline_optimized_legacy(arena),
                        ),
                        Micheline::prim0(Prim::PAIR),
                        Micheline::prim3(
                            arena,
                            Prim::LAMBDA_REC,
                            in_ty.into_micheline_optimized_legacy(arena),
                            out_ty.into_micheline_optimized_legacy(arena),
                            micheline_code,
                        ),
                        Micheline::App(Prim::SWAP, &[], NO_ANNS),
                        Micheline::App(Prim::EXEC, &[], NO_ANNS),
                    ],
                ),
                Closure::Apply { .. } | Closure::Lambda(Lambda::Lambda { .. }) => Micheline::seq(
                    arena,
                    [
                        Micheline::prim2(
                            arena,
                            Prim::PUSH,
                            arg_ty.into_micheline_optimized_legacy(arena),
                            arg_val.into_micheline_optimized_legacy(arena),
                        ),
                        Micheline::App(Prim::PAIR, &[], NO_ANNS),
                        closure.into_micheline_optimized_legacy(arena),
                    ],
                ),
            },
        }
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
        irrefutable_match::irrefutable_match,
        parser::Parser,
        stk,
    };

    #[test]
    fn apply_micheline() {
        let parser = Parser::new();
        let arena = Arena::new();
        let code = parser.parse("{ LAMBDA (pair int nat unit) unit { DROP; UNIT }; PUSH int 1; APPLY; PUSH nat 2; APPLY }").unwrap();
        let code = code
            .typecheck_instruction(&mut Ctx::default(), None, &[])
            .unwrap();
        let mut stack = stk![];
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(stack.pop().unwrap(); TypedValue::Lambda);
        let arena = Arena::new();
        assert_eq!(
            closure.into_micheline_optimized_legacy(&arena),
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
            .typecheck_instruction(&mut Ctx::default(), None, &[])
            .unwrap();
        let mut stack = stk![];
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(stack.pop().unwrap(); TypedValue::Lambda);
        let arena = Arena::new();
        assert_eq!(
            closure.into_micheline_optimized_legacy(&arena),
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
            .typecheck_instruction(&mut Ctx::default(), None, &[])
            .unwrap();
        let mut stack = stk![];
        code.interpret(&mut Ctx::default(), &arena, &mut stack)
            .unwrap();
        let closure = irrefutable_match!(stack.pop().unwrap(); TypedValue::Lambda);
        let arena = Arena::new();
        assert_eq!(
            closure.into_micheline_optimized_legacy(&arena),
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
