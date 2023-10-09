/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

mod ast;
mod gas;
mod interpreter;
mod parser;
mod stack;
mod syntax;
mod typechecker;

fn main() {}

#[cfg(test)]
mod tests {
    use crate::ast::*;
    use crate::gas::Gas;
    use crate::interpreter;
    use crate::parser;
    use crate::stack::stk;
    use crate::typechecker;

    fn report_gas<R, F: FnOnce(&mut Gas) -> R>(gas: &mut Gas, f: F) -> R {
        let initial_milligas = gas.milligas();
        let r = f(gas);
        let gas_diff = initial_milligas - gas.milligas();
        println!("Gas consumed: {}.{:0>3}", gas_diff / 1000, gas_diff % 1000,);
        r
    }

    #[test]
    fn interpret_test_expect_success() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let ast = typechecker::typecheck(ast, &mut Gas::default(), &mut stk![Type::Nat]).unwrap();
        let mut istack = stk![Value::NumberValue(10)];
        let mut gas = Gas::default();
        assert!(interpreter::interpret(&ast, &mut gas, &mut istack).is_ok());
        assert!(istack.len() == 1 && istack[0] == Value::NumberValue(55));
    }

    #[test]
    fn interpret_mutez_push_add() {
        let ast = parser::parse("{ PUSH mutez 100; PUSH mutez 500; ADD }").unwrap();
        let mut gas = Gas::default();
        let ast = typechecker::typecheck(ast, &mut gas, &mut stk![]).unwrap();
        let mut istack = stk![];
        assert!(interpreter::interpret(&ast, &mut gas, &mut istack).is_ok());
        assert_eq!(istack, stk![Value::NumberValue(600)]);
    }

    #[test]
    fn interpret_test_gas_consumption() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let ast = typechecker::typecheck(ast, &mut Gas::default(), &mut stk![Type::Nat]).unwrap();
        let mut istack = stk![Value::NumberValue(5)];
        let mut gas = Gas::new(1359);
        report_gas(&mut gas, |gas| {
            assert!(interpreter::interpret(&ast, gas, &mut istack).is_ok());
        });
        assert_eq!(gas.milligas(), 0);
    }

    #[test]
    fn interpret_test_gas_out_of_gas() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let ast = typechecker::typecheck(ast, &mut Gas::default(), &mut stk![Type::Nat]).unwrap();
        let mut istack = stk![Value::NumberValue(5)];
        let mut gas = Gas::new(1);
        assert_eq!(
            interpreter::interpret(&ast, &mut gas, &mut istack),
            Err(interpreter::InterpretError::OutOfGas),
        );
    }

    #[test]
    fn typecheck_test_expect_success() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let mut stack = stk![Type::Nat];
        assert!(typechecker::typecheck(ast, &mut Gas::default(), &mut stack).is_ok());
        assert_eq!(stack, stk![Type::Int]);
    }

    #[test]
    fn typecheck_gas() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let mut stack = stk![Type::Nat];
        let mut gas = Gas::new(11460);
        report_gas(&mut gas, |gas| {
            assert!(typechecker::typecheck(ast, gas, &mut stack).is_ok());
        });
        assert_eq!(gas.milligas(), 0);
    }

    #[test]
    fn typecheck_out_of_gas() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        let mut stack = stk![Type::Nat];
        let mut gas = Gas::new(1000);
        assert_eq!(
            typechecker::typecheck(ast, &mut gas, &mut stack),
            Err(typechecker::TcError::OutOfGas)
        );
    }

    #[test]
    fn typecheck_test_expect_fail() {
        let ast = parser::parse(&FIBONACCI_ILLTYPED_SRC).unwrap();
        let mut stack = stk![Type::Nat];
        assert_eq!(
            typechecker::typecheck(ast, &mut Gas::default(), &mut stack),
            Err(typechecker::TcError::StackTooShort)
        );
    }

    #[test]
    fn parser_test_expect_success() {
        let ast = parser::parse(&FIBONACCI_SRC).unwrap();
        // use built in pretty printer to validate the expected AST.
        assert_eq!(format!("{:#?}", ast), AST_PRETTY_EXPECTATION);
    }

    #[test]
    fn parser_test_expect_fail() {
        assert_eq!(
            &parser::parse(&FIBONACCI_MALFORMED_SRC)
                .unwrap_err()
                .to_string(),
            "Unrecognized token `GT` found at 133:135\nExpected one of \";\" or \"}\""
        );
    }

    #[test]
    fn parser_test_dip_dup_drop_args() {
        use Instruction::{Dip, Drop, Dup};

        assert_eq!(parser::parse("{ DROP 1023 }"), Ok(vec![Drop(Some(1023))]));
        assert_eq!(
            parser::parse("{ DIP 1023 {} }"),
            Ok(vec![Dip(Some(1023), vec![])])
        );
        assert_eq!(parser::parse("{ DUP 1023 }"), Ok(vec![Dup(Some(1023))]));

        // failures
        assert_eq!(
            parser::parse("{ DROP 1025 }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "Expected a natural from 0 to 1023 inclusive"
        );
        assert_eq!(
            parser::parse("{ DIP 1024 {} }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "Expected a natural from 0 to 1023 inclusive"
        );
        assert_eq!(
            parser::parse("{ DUP 65536 }")
                .unwrap_err()
                .to_string()
                .as_str(),
            "Expected a natural from 0 to 1023 inclusive"
        );
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

    const AST_PRETTY_EXPECTATION: &str = "[
    Int,
    Push(
        Int,
        NumberValue(
            0,
        ),
    ),
    Dup(
        Some(
            2,
        ),
    ),
    Gt,
    If(
        [
            Dip(
                None,
                [
                    Push(
                        Int,
                        NumberValue(
                            -1,
                        ),
                    ),
                    Add(
                        (),
                    ),
                ],
            ),
            Push(
                Int,
                NumberValue(
                    1,
                ),
            ),
            Dup(
                Some(
                    3,
                ),
            ),
            Gt,
            Loop(
                [
                    Swap,
                    Dup(
                        Some(
                            2,
                        ),
                    ),
                    Add(
                        (),
                    ),
                    Dip(
                        Some(
                            2,
                        ),
                        [
                            Push(
                                Int,
                                NumberValue(
                                    -1,
                                ),
                            ),
                            Add(
                                (),
                            ),
                        ],
                    ),
                    Dup(
                        Some(
                            3,
                        ),
                    ),
                    Gt,
                ],
            ),
            Dip(
                None,
                [
                    Drop(
                        Some(
                            2,
                        ),
                    ),
                ],
            ),
        ],
        [
            Dip(
                None,
                [
                    Drop(
                        None,
                    ),
                ],
            ),
        ],
    ),
]";
}
