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

impl<'a> std::fmt::Debug for Closure<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Walk the Apply spine to the terminal Lambda iteratively; emit the
        // outer Apply tokens on the way down, then the terminal Lambda's
        // Debug at the deepest point, then close the Apply parentheses.
        let mut depth: usize = 0;
        let mut cur = self;
        loop {
            match cur {
                Closure::Lambda(lam) => {
                    f.write_str("Lambda(")?;
                    write!(f, "{:?}", lam)?;
                    f.write_str(")")?;
                    break;
                }
                Closure::Apply {
                    arg_ty,
                    arg_val,
                    closure,
                } => {
                    // arg_ty uses Type's iterative Debug; arg_val uses
                    // TypedValue's iterative Debug. Both safe at any
                    // depth.
                    write!(
                        f,
                        "Apply {{ arg_ty: {:?}, arg_val: {:?}, closure: ",
                        arg_ty, arg_val
                    )?;
                    depth = depth.checked_add(1).ok_or(std::fmt::Error)?;
                    cur = closure;
                }
            }
        }
        for _ in 0..depth {
            f.write_str(" }")?;
        }
        Ok(())
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
