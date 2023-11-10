/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

pub mod ast;
pub mod context;
pub mod gas;
pub mod interpreter;
pub mod irrefutable_match;
pub mod lexer;
pub mod parser;
pub mod stack;
pub mod syntax;
pub mod typechecker;
pub mod tzt;

#[cfg(test)]
mod tests {
    use crate::ast::micheline::test_helpers::*;
    use crate::ast::*;
    use crate::context::Ctx;
    use crate::gas::Gas;
    use crate::interpreter;
    use crate::parser::test_helpers::{parse, parse_contract_script};
    use crate::stack::{stk, tc_stk};
    use crate::typechecker;

    fn report_gas<R, F: FnOnce(&mut Ctx) -> R>(ctx: &mut Ctx, f: F) -> R {
        let initial_milligas = ctx.gas.milligas();
        let r = f(ctx);
        let gas_diff = initial_milligas - ctx.gas.milligas();
        println!("Gas consumed: {}.{:0>3}", gas_diff / 1000, gas_diff % 1000);
        r
    }

    #[test]
    fn interpret_test_expect_success() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), None, &[app!(nat)])
            .unwrap();
        let mut istack = stk![TypedValue::Nat(10)];
        assert!(ast.interpret(&mut Ctx::default(), &mut istack).is_ok());
        assert!(istack.len() == 1 && istack[0] == TypedValue::Int(55));
    }

    #[test]
    fn interpret_mutez_push_add() {
        let ast = parse("{ PUSH mutez 100; PUSH mutez 500; ADD }").unwrap();
        let mut ctx = Ctx::default();
        let ast = ast.typecheck(&mut ctx, None, &[]).unwrap();
        let mut istack = stk![];
        assert!(ast.interpret(&mut ctx, &mut istack).is_ok());
        assert_eq!(istack, stk![TypedValue::Mutez(600)]);
    }

    #[test]
    fn interpret_test_gas_consumption() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), None, &[app!(nat)])
            .unwrap();
        let mut istack = stk![TypedValue::Nat(5)];
        let mut ctx = Ctx::default();
        report_gas(&mut ctx, |ctx| {
            assert!(ast.interpret(ctx, &mut istack).is_ok());
        });
        assert_eq!(Gas::default().milligas() - ctx.gas.milligas(), 1359);
    }

    #[test]
    fn interpret_test_gas_out_of_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), None, &[app!(nat)])
            .unwrap();
        let mut istack = stk![TypedValue::Nat(5)];
        let mut ctx = Ctx {
            gas: Gas::new(1),
            ..Ctx::default()
        };
        assert_eq!(
            ast.interpret(&mut ctx, &mut istack),
            Err(interpreter::InterpretError::OutOfGas(crate::gas::OutOfGas)),
        );
    }

    #[test]
    fn typecheck_test_expect_success() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        assert!(
            typechecker::typecheck_instruction(&ast, &mut Ctx::default(), None, &mut stack).is_ok()
        );
        assert_eq!(stack, tc_stk![Type::Int])
    }

    #[test]
    fn typecheck_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let mut ctx = Ctx::default();
        let start_milligas = ctx.gas.milligas();
        report_gas(&mut ctx, |ctx| {
            assert!(ast.typecheck(ctx, None, &[app!(nat)]).is_ok());
        });
        assert_eq!(start_milligas - ctx.gas.milligas(), 12680);
    }

    #[test]
    fn typecheck_out_of_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let mut ctx = Ctx {
            gas: Gas::new(1000),
            ..Ctx::default()
        };
        assert_eq!(
            ast.typecheck(&mut ctx, None, &[app!(nat)]),
            Err(typechecker::TcError::OutOfGas(crate::gas::OutOfGas))
        );
    }

    #[test]
    fn typecheck_test_expect_fail() {
        use typechecker::{NoMatchingOverloadReason, TcError};
        let ast = parse(FIBONACCI_ILLTYPED_SRC).unwrap();
        assert_eq!(
            ast.typecheck(&mut Ctx::default(), None, &[app!(nat)]),
            Err(TcError::NoMatchingOverload {
                instr: crate::lexer::Prim::DUP,
                stack: stk![Type::Int, Type::Int, Type::Int],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 4 })
            })
        );
    }

    #[test]
    fn parser_test_expect_success() {
        use crate::ast::micheline::test_helpers::*;

        let ast = parse(FIBONACCI_SRC).unwrap();
        // use built in pretty printer to validate the expected AST.
        assert_eq!(
            ast,
            seq! {
                app!(INT);
                app!(PUSH[app!(int), 0]);
                app!(DUP[2]);
                app!(GT);
                app!(IF[
                    seq!{
                        app!(DIP[seq!{app!(PUSH[app!(int), -1]); app!(ADD) }]);
                        app!(PUSH[app!(int), 1]);
                        app!(DUP[3]);
                        app!(GT);
                        app!(LOOP[seq!{
                            app!(SWAP);
                            app!(DUP[2]);
                            app!(ADD);
                            app!(DIP[2, seq!{
                                app!(PUSH[app!(int), -1]);
                                app!(ADD)
                            }]);
                            app!(DUP[3]);
                            app!(GT);
                        }]);
                        app!(DIP[seq!{app!(DROP[2])}]);
                    },
                    seq!{
                        app!(DIP[seq!{ app!(DROP) }])
                    },
                ]);
            }
        );
    }

    #[test]
    fn parser_test_expect_fail() {
        use crate::ast::micheline::test_helpers::app;
        assert_eq!(
            parse(FIBONACCI_MALFORMED_SRC).unwrap().typecheck(
                &mut Ctx::default(),
                None,
                &[app!(nat)]
            ),
            Err(typechecker::TcError::UnexpectedMicheline(format!(
                "{:?}",
                app!(DUP[4, app!(GT)])
            )))
        );
    }

    #[test]
    fn parser_test_dip_dup_drop_args() {
        use crate::ast::micheline::test_helpers::*;

        assert_eq!(parse("DROP 1023"), Ok(app!(DROP[1023])));
        assert_eq!(parse("DIP 1023 {}"), Ok(app!(DIP[1023, seq!{}])));
        assert_eq!(parse("DUP 1023"), Ok(app!(DUP[1023])));
    }

    #[test]
    fn vote_contract() {
        use crate::ast::micheline::test_helpers::*;
        let mut ctx = Ctx {
            amount: 5_000_000,
            ..Ctx::default()
        };
        let interp_res = parse_contract_script(VOTE_SRC)
            .unwrap()
            .typecheck_script(&mut ctx)
            .unwrap()
            .interpret(
                &mut ctx,
                "foo".into(),
                seq! {app!(Elt["bar", 0]); app!(Elt["baz", 0]); app!(Elt["foo", 0])},
            );
        use TypedValue as TV;
        match interp_res.unwrap() {
            (_, TV::Map(m)) => {
                assert_eq!(m.get(&TV::String("foo".to_owned())).unwrap(), &TV::Int(1))
            }
            _ => panic!("unexpected contract output"),
        }
    }

    const FIBONACCI_SRC: &str = "{ INT ; PUSH int 0 ; DUP 2 ; GT ;
           IF { DIP { PUSH int -1 ; ADD } ;
            PUSH int 1 ;
            DUP 3 ;
            GT ;
            LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
            DIP { DROP 2 } }
          { DIP { DROP } } }";

    const FIBONACCI_ILLTYPED_SRC: &str = "{ INT ; PUSH int 0 ; DUP 2 ; GT ;
           IF { DIP { PUSH int -1 ; ADD } ;
            PUSH int 1 ;
            DUP 4 ;
            GT ;
            LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
            DIP { DROP 2 } }
          { DIP { DROP } } }";

    const FIBONACCI_MALFORMED_SRC: &str = "{ INT ; PUSH int 0 ; DUP 2 ; GT ;
           IF { DIP { PUSH int -1 ; ADD } ;
            PUSH int 1 ;
            DUP 4
            GT ;
            LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
            DIP { DROP 2 } }
          { DIP { DROP } } }";

    const VOTE_SRC: &str = "{
          parameter (string %vote);
          storage (map string int);
          code {
              AMOUNT;
              PUSH mutez 5000000;
              COMPARE; GT;
              IF { { UNIT; FAILWITH } } {};
              DUP; DIP { CDR; DUP }; CAR; DUP;
              DIP {
                  GET; { IF_NONE { { UNIT ; FAILWITH } } {} };
                  PUSH int 1; ADD; SOME
              };
              UPDATE;
              NIL operation; PAIR
          }
      }";
}
