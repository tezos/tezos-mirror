// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Representation for typed Michelson `lambda 'a 'b` values.

use std::rc::Rc;

use crate::gas::{Gas, OutOfGas};

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
///
/// `Debug` is implemented manually as an iterative walk over the
/// `Apply` chain so a closure built by many nested `APPLY`s does not
/// blow the WASM call stack when formatted into an error message
/// (`InterpretError::FailedWith` embeds a `TypedValue` via `{1:?}`,
/// which in turn formats `TypedValue::Lambda(Closure)` via `Closure`'s
/// `Debug`; the kernel runs `err.to_string()` outside MIR's gas
/// accounting).
#[derive(Clone, Eq, PartialEq)]
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

/// A trivial empty lambda, used as a cheap placeholder when moving a `Closure`
/// out of a `&mut` field without cloning (cloning could recurse through a deep
/// `Apply` spine).
impl Default for Closure<'_> {
    fn default() -> Self {
        Closure::Lambda(Lambda::Lambda {
            micheline_code: Micheline::Seq(&[]),
            code: Vec::new().into(),
        })
    }
}

/// Debug seeds the shared `TypedValue`/`Closure` walker so the alternating
/// `Closure::Apply { arg_val: TypedValue, .. }` /
/// `TypedValue::Lambda(Closure)` cycle (reachable via repeated `APPLY` of
/// lambda values) is unwound on the heap, not the call stack.
impl<'a> std::fmt::Debug for Closure<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::ast::debug_fmt_walk(crate::ast::DebugFrame::VisitCl(self), f)
    }
}

impl<'a> IntoMicheline<'a> for Closure<'a> {
    /// Delegates to the iterative `TypedValue::Lambda(self)` unparser, whose
    /// worklist flattens both the `Closure::Apply` spine and the captured arg
    /// values (which may themselves be deep lambdas), so neither overflows the
    /// WASM call stack.
    fn into_micheline_optimized_legacy(
        self,
        arena: &'a typed_arena::Arena<Micheline<'a>>,
        gas: &mut Gas,
    ) -> Result<Micheline<'a>, OutOfGas> {
        TypedValue::Lambda(self).into_micheline_optimized_legacy(arena, gas)
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

    /// L2-1436: a lambda value with a deeply nested body must not overflow when
    /// formatted (reachable via `FAILWITH` -> `InterpretError::FailedWith`,
    /// which the kernel turns into a string outside gas). `micheline_code` goes
    /// through the iterative `Micheline` Debug; the typechecked `code` (whose
    /// derived `Instruction` Debug is still recursive) is elided as `code: ..`.
    #[test]
    fn deep_lambda_debug_does_not_overflow() {
        use crate::ast::Micheline;
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let arena: Arena<Micheline<'_>> = Arena::new();
                let mut m = Micheline::Seq(&[]);
                for _ in 0..DEPTH {
                    m = Micheline::Seq(std::slice::from_ref(arena.alloc(m)));
                }
                let lam = super::Lambda::Lambda {
                    micheline_code: m,
                    code: Vec::new().into(),
                };
                let tv = TypedValue::Lambda(super::Closure::Lambda(lam));
                let s = format!("{tv:?}");
                assert!(s.contains("code: .."));
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// L2-1436: the iterative `Closure::Debug` must not overflow on a deep
    /// `Closure::Apply` spine (an APPLY chain is ~7400-deep reachable within
    /// one operation's gas budget). Builds N nested `Apply` and formats on a
    /// 1 MiB worker thread; a regression to recursive walking would overflow.
    /// `Closure` has no iterative `Drop` yet (its `Box<Closure>` spine recurses
    /// on drop — tracked in L2-1446), so the deep value is `mem::forget`-ed to
    /// avoid the recursive destructor.
    #[test]
    fn deeply_nested_closure_apply_debug_format() {
        use crate::ast::{Micheline, Type};
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let mut c = super::Closure::Lambda(super::Lambda::Lambda {
                    micheline_code: Micheline::Seq(&[]),
                    code: Vec::new().into(),
                });
                for _ in 0..DEPTH {
                    c = super::Closure::Apply {
                        arg_ty: Type::Unit,
                        arg_val: Box::new(TypedValue::Unit),
                        closure: Box::new(c),
                    };
                }
                let tv = TypedValue::Lambda(c);
                let s = format!("{tv:?}");
                assert!(s.contains("Apply {") && s.len() > DEPTH);
                std::mem::forget(tv);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

    /// L2-1436 follow-up (review on !21988): when an `APPLY` captures a
    /// lambda value as `arg_val`, `Closure::Debug` recurses into
    /// `TypedValue::Debug`, which then recurses back into `Closure::Debug`
    /// for the inner `Lambda(...)` — N alternations burn ~2N real frames
    /// and overflow the 1 MiB stack independently of the `Apply` spine
    /// length. The pre-fix sibling test only exercised `arg_val: Unit`, so
    /// it missed the alternating path. Here every level nests another
    /// lambda inside the captured `arg_val`; a regression to per-impl
    /// recursion would overflow even at small DEPTH.
    #[test]
    fn deep_apply_arg_lambda_debug_does_not_overflow() {
        use crate::ast::{Micheline, Type};
        const DEPTH: usize = 100_000;
        std::thread::Builder::new()
            .stack_size(1024 * 1024)
            .spawn(|| {
                let leaf = || {
                    super::Closure::Lambda(super::Lambda::Lambda {
                        micheline_code: Micheline::Seq(&[]),
                        code: Vec::new().into(),
                    })
                };
                let mut c = leaf();
                for _ in 0..DEPTH {
                    c = super::Closure::Apply {
                        arg_ty: Type::new_lambda(Type::Unit, Type::Unit),
                        arg_val: Box::new(TypedValue::Lambda(leaf())),
                        closure: Box::new(c),
                    };
                }
                // Wrap the whole chain inside one final outer Lambda so the
                // formatting starts on the `TypedValue` side.
                let tv = TypedValue::Lambda(c);
                let s = format!("{tv:?}");
                // Every captured `arg_val` is itself rendered as `Lambda(...)`
                // — count occurrences must exceed DEPTH (one per Apply +
                // the outer / leaf wrappers).
                assert!(s.matches("Lambda(").count() > DEPTH);
                std::mem::forget(tv);
            })
            .unwrap()
            .join()
            .expect("worker thread completes");
    }

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
