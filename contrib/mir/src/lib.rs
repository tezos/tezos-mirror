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
    use crate::ast::*;
    use crate::context::Ctx;
    use crate::gas::Gas;
    use crate::interpreter;
    use crate::parser;
    use crate::parser::parse_contract_script;
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
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), &mut tc_stk![Type::Nat])
            .unwrap();
        let mut istack = stk![TypedValue::Nat(10)];
        assert!(ast.interpret(&mut Ctx::default(), &mut istack).is_ok());
        assert!(istack.len() == 1 && istack[0] == TypedValue::Int(55));
    }

    #[test]
    fn interpret_mutez_push_add() {
        let ast = parser::parse("{ PUSH mutez 100; PUSH mutez 500; ADD }").unwrap();
        let mut ctx = Ctx::default();
        let ast = ast.typecheck(&mut ctx, &mut tc_stk![]).unwrap();
        let mut istack = stk![];
        assert!(ast.interpret(&mut ctx, &mut istack).is_ok());
        assert_eq!(istack, stk![TypedValue::Mutez(600)]);
    }

    #[test]
    fn interpret_test_gas_consumption() {
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), &mut tc_stk![Type::Nat])
            .unwrap();
        let mut istack = stk![TypedValue::Nat(5)];
        let mut ctx = Ctx::default();
        report_gas(&mut ctx, |ctx| {
            assert!(ast.interpret(ctx, &mut istack).is_ok());
        });
        assert_eq!(ctx.gas.milligas(), Gas::default().milligas() - 1359);
    }

    #[test]
    fn interpret_test_gas_out_of_gas() {
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck(&mut Ctx::default(), &mut tc_stk![Type::Nat])
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
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        assert!(ast.typecheck(&mut Ctx::default(), &mut stack).is_ok());
        assert_eq!(stack, tc_stk![Type::Int])
    }

    #[test]
    fn typecheck_gas() {
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        let mut ctx = Ctx::default();
        let start_milligas = ctx.gas.milligas();
        report_gas(&mut ctx, |ctx| {
            assert!(ast.typecheck(ctx, &mut stack).is_ok());
        });
        assert_eq!(start_milligas - ctx.gas.milligas(), 12680);
    }

    #[test]
    fn typecheck_out_of_gas() {
        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        let mut ctx = Ctx {
            gas: Gas::new(1000),
            ..Ctx::default()
        };
        assert_eq!(
            ast.typecheck(&mut ctx, &mut stack),
            Err(typechecker::TcError::OutOfGas(crate::gas::OutOfGas))
        );
    }

    #[test]
    fn typecheck_test_expect_fail() {
        use typechecker::{NoMatchingOverloadReason, TcError};
        let ast = parser::parse(FIBONACCI_ILLTYPED_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        assert_eq!(
            ast.typecheck(&mut Ctx::default(), &mut stack),
            Err(TcError::NoMatchingOverload {
                instr: crate::lexer::Prim::DUP,
                stack: stk![Type::Int, Type::Int, Type::Int],
                reason: Some(NoMatchingOverloadReason::StackTooShort { expected: 4 })
            })
        );
    }

    #[test]
    fn parser_test_expect_success() {
        use Instruction::*;
        use Value::*;

        let ast = parser::parse(FIBONACCI_SRC).unwrap();
        // use built in pretty printer to validate the expected AST.
        assert_eq!(
            ast,
            Instruction::Seq(vec![
                Int,
                Push((Type::Int, Number(0))),
                Dup(Some(2)),
                Gt,
                If(
                    vec![
                        Dip(None, vec![Push((Type::Int, Number(-1))), Add(())]),
                        Push((Type::Int, Number(1))),
                        Dup(Some(3)),
                        Gt,
                        Loop(vec![
                            Swap,
                            Dup(Some(2)),
                            Add(()),
                            Dip(Some(2), vec![Push((Type::Int, Number(-1))), Add(())]),
                            Dup(Some(3)),
                            Gt,
                        ]),
                        Dip(None, vec![Drop(Some(2))]),
                    ],
                    vec![Dip(None, vec![Drop(None)])],
                ),
            ])
        );
    }

    #[test]
    fn parser_test_expect_fail() {
        assert_eq!(
            &parser::parse(FIBONACCI_MALFORMED_SRC)
                .unwrap_err()
                .to_string(),
            "Unrecognized token `GT` found at 133:135\nExpected one of \";\" or \"}\""
        );
    }

    #[test]
    fn parser_test_dip_dup_drop_args() {
        use Instruction::{Dip, Drop, Dup};

        assert_eq!(parser::parse("DROP 1023"), Ok(Drop(Some(1023))));
        assert_eq!(parser::parse("DIP 1023 {}"), Ok(Dip(Some(1023), vec![])));
        assert_eq!(parser::parse("DUP 1023"), Ok(Dup(Some(1023))));

        // failures
        assert_eq!(
            parser::parse("{ DROP 1025 }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "expected a natural from 0 to 1023 inclusive, but got 1025"
        );
        assert_eq!(
            parser::parse("{ DIP 1024 {} }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "expected a natural from 0 to 1023 inclusive, but got 1024"
        );
        assert_eq!(
            parser::parse("{ DUP 65536 }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "expected a natural from 0 to 1023 inclusive, but got 65536"
        );
    }

    #[test]
    fn vote_contract() {
        let mut ctx = Ctx {
            amount: 5_000_000,
            ..Ctx::default()
        };
        let interp_res = parse_contract_script(VOTE_SRC)
            .unwrap()
            .typecheck(&mut ctx)
            .unwrap()
            .interpret(
                &mut ctx,
                "foo".into(),
                vec![Elt("bar", 0), Elt("baz", 0), Elt("foo", 0)].into(),
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
