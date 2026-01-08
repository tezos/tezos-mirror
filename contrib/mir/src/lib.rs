// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT
#![warn(clippy::redundant_clone)]
#![warn(missing_docs)]
#![deny(clippy::disallowed_methods)]

//! # M.I.R. -- Michelson in Rust
//!
//! Rust implementation of the typechecker and interpreter for the Michelson
//! smart contract language.
//!
//! The library is currently incomplete. The following instructions are not
//! supported:
//!
//! - `EMPTY_MAP`
//! - `SAPLING_EMPTY_STATE`
//! - `SAPLING_VERIFY_UPDATE`
//! - `OPEN_CHEST`
//! - `VIEW`
//!
//! The following types are currently not supported:
//!
//! - `chest`
//! - `chest_key`
//! - `tx_rollup_l2_address`
//! - `sapling_state`
//! - `sapling_transaction`
//!
//! # Usage
//!
//! The general pipeline is as follows: parse → typecheck → interpret →
//! serialize result.
//!
//! There are essentially two parsers available, one, in [parser::Parser], which
//! can be used to parse Michelson source code from strings. Another one is
//! implemented as [ast::Micheline::decode_raw], which can be used to
//! deserialize Michelson from bytes.
//!
//! Whether parsed from string or bytes, the result of a parse is
//! [ast::Micheline]. Since Micheline can represent any part of a Michelson
//! script, several associated functions exist for typechecking:
//!
//! - [ast::Micheline::typecheck_value] can be used to typecheck a Michelson
//!   value, e.g. `1` or `Some "string"`.
//! - [ast::Micheline::typecheck_instruction] can be used to typecheck a Michelson
//!   instruction or a sequence of instructions.
//! - [ast::Micheline::typecheck_script] can be used to typecheck a full
//!   Michelson script, i.e. something that defines `parameter`, `storage` and
//!   `code` fields.
//!
//! Any of these functions requires a reference to the external context,
//! [context::Ctx]. Context keeps track of the used gas, and also carries
//! information about the world outside of the interpreter. You can construct a
//! context with reasonable defaults using [`context::Ctx::default()`]. After that, you
//! may want to adjust some things. Refer to [context::Ctx] documentation.
//!
//! Once `Micheline` is typechecked, it will result in either [ast::TypedValue],
//! [ast::Instruction], or [ast::ContractScript]. The latter two have
//! [ast::Instruction::interpret] and [ast::ContractScript::interpret]
//! associated functions that serve as main entry-points for the interpreter.
//!
//! The result of interpretation is either a [ast::TypedValue] or a stack of
//! them. [ast::IntoMicheline::into_micheline_optimized_legacy] can be used to
//! convert [ast::TypedValue] into [ast::Micheline], at which point,
//! [ast::Micheline::encode] can be employed to serialize the data.
//!
//! Some functions require access to a [typed_arena::Arena]. [parser::Parser]
//! already has one, so that one can be reused. If memory consumption is a
//! concern, and depending on the workload, it may be slightly more economical
//! to create a new `Arena` for different stages.
//!
//! Here's a simple example, running a Fibonacci contract:
//!
//! ```
//! use mir::ast::*;
//! use mir::context::{Ctx, TypecheckingCtx};
//! use mir::parser::Parser;
//! use typed_arena::Arena;
//! let script = r#"
//! parameter nat;
//! storage int;
//! code { CAR ; INT ; PUSH int 0 ; DUP 2 ; GT ;
//!        IF { DIP { PUSH int -1 ; ADD } ;
//!             PUSH int 1 ;
//!             DUP 3 ;
//!             GT ;
//!             LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
//!             DIP { DROP 2 } }
//!           { DIP { DROP } };
//!         NIL operation;
//!         PAIR }
//! "#;
//! let parser = Parser::new();
//! let contract_micheline = parser.parse_top_level(script).unwrap();
//! let mut ctx = Ctx::default();
//! // You can change various things about the context here, see [Ctx]
//! // documentation.
//! let contract_typechecked = contract_micheline.split_script().unwrap().typecheck_script(ctx.gas(), true, true).unwrap();
//! // We construct parameter and storage manually, but you'd probably
//! // parse or deserialize them from some sort of input/storage, so we use
//! // parser and decoder respectively.
//! // Note that you can opt to use a new parser and/or a new arena for
//! // parameter and storage. However, they _must_ outlive `ctx`.
//! let parameter = parser.parse("123").unwrap();
//! let storage = Micheline::decode_raw(&parser.arena, &[0x00, 0x00]).unwrap(); // integer 0
//! // Note: the arena passed in here _must_ outlive `ctx`. We reuse the one
//! // from `parser` for simplicity, you may also opt to create a new one to
//! // potentially save a bit of memory (depends on the workload).
//! let (operations_iter, new_storage) = contract_typechecked
//!     .interpret(&mut ctx, &parser.arena, parameter, &Entrypoint::default(), &storage)
//!     .unwrap();
//! let TypedValue::Int(new_storage_int) = &new_storage else { unreachable!() };
//! assert_eq!(new_storage_int, &22698374052006863956975682u128.into());
//! assert_eq!(operations_iter.collect::<Vec<_>>(), vec![]);
//! // Arena passed in here does not need to outlive `ctx`. Could reuse the one
//! // from `parser` again, but we create a new one to mix things up. If you're
//! // not concerned about memory consumption, it may be faster to reuse the
//! // same arena everywhere.
//! let packed_new_storage = new_storage
//!     .into_micheline_optimized_legacy(&Arena::new())
//!     .encode();
//! assert_eq!(
//!     packed_new_storage,
//!     vec![0x00, 0x82, 0x81, 0x8d, 0xe6, 0xdf, 0x96, 0x8c, 0xad, 0xa5, 0xc5, 0xb4, 0xac, 0x02]
//! );
//! ```
//!
//! You can find more examples in
//! <https://gitlab.com/tezos/tezos/-/tree/master/contrib/mir/examples>

pub mod ast;
#[cfg(feature = "bls")]
pub mod bls;
pub mod context;
pub mod gas;
pub mod interpreter;
mod irrefutable_match;
pub mod lexer;
pub mod parser;
pub mod serializer;
pub mod stack;
mod syntax;
pub mod typechecker;
pub mod tzt;

#[cfg(test)]
mod tests {
    use typed_arena::Arena;

    use crate::ast::annotations::FieldAnnotation;
    use crate::ast::*;
    use crate::context::{Ctx, TypecheckingCtx};
    use crate::gas::Gas;

    use crate::interpreter;
    use crate::parser::test_helpers::{parse, parse_contract_script};
    use crate::stack::{stk, tc_stk, FailingTypeStack, Stack, TypeStack};
    use crate::typechecker;
    use crate::typechecker::typecheck_instruction;
    use std::collections::HashMap;
    use std::rc::Rc;
    use tezos_crypto_rs::hash::ContractKt1Hash;

    fn report_gas<'a, R, F: FnOnce(&mut Ctx<'a>) -> R>(ctx: &mut Ctx<'a>, f: F) -> R {
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
            .typecheck_instruction(&mut Gas::default(), None, &[parse("nat").unwrap()])
            .unwrap();
        let mut istack = stk![TypedValue::nat(10)];
        let temp = Arena::new();
        assert!(ast
            .interpret(&mut Ctx::default(), &temp, &mut istack)
            .is_ok());
        assert!(istack.len() == 1 && istack[0] == TypedValue::int(55));
    }

    #[test]
    fn interpret_mutez_push_add() {
        let ast = parse("{ PUSH mutez 100; PUSH mutez 500; ADD }").unwrap();
        let temp = Arena::new();
        let mut ctx = Ctx::default();
        let ast = ast.typecheck_instruction(ctx.gas(), None, &[]).unwrap();
        let mut istack = stk![];
        assert!(ast.interpret(&mut ctx, &temp, &mut istack).is_ok());
        assert_eq!(istack, stk![TypedValue::Mutez(600)]);
    }

    #[test]
    fn interpret_test_gas_consumption() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck_instruction(&mut Gas::default(), None, &[parse("nat").unwrap()])
            .unwrap();
        let mut istack = stk![TypedValue::nat(5)];
        let temp = Arena::new();
        let mut ctx = Ctx::default();
        report_gas(&mut ctx, |ctx| {
            assert!(ast.interpret(ctx, &temp, &mut istack).is_ok());
        });
        assert_eq!(Gas::default().milligas() - ctx.gas.milligas(), 1287);
    }

    #[test]
    fn interpret_test_gas_out_of_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let ast = ast
            .typecheck_instruction(&mut Gas::default(), None, &[parse("nat").unwrap()])
            .unwrap();
        let mut istack = stk![TypedValue::nat(5)];
        let temp = Arena::new();
        let ctx = &mut Ctx::default();
        ctx.gas = Gas::new(1);
        assert_eq!(
            ast.interpret(ctx, &temp, &mut istack),
            Err(interpreter::InterpretError::OutOfGas(crate::gas::OutOfGas)),
        );
    }

    #[test]
    fn interpret_test_macro_if_some() {
        let ast = parse(MACRO_IF_SOME_SRC).unwrap();
        let ast = ast
            .typecheck_instruction(&mut Gas::default(), None, &[parse("option nat").unwrap()])
            .unwrap();
        let mut istack = stk![TypedValue::new_option(Some(TypedValue::nat(5)))];
        let temp = Arena::new();
        assert!(ast
            .interpret(&mut Ctx::default(), &temp, &mut istack)
            .is_ok());
        assert_eq!(istack, stk![TypedValue::nat(6)]);
    }

    #[test]
    fn parse_naked_fail_in_if() {
        assert!(parse("{IF FAIL FAIL}").is_ok());
    }

    #[test]
    fn typecheck_test_expect_success() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let mut stack = tc_stk![Type::Nat];
        assert!(
            typechecker::typecheck_instruction(&ast, &mut Gas::default(), None, &mut stack).is_ok()
        );
        assert_eq!(stack, tc_stk![Type::Int])
    }

    #[test]
    fn typecheck_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        let mut ctx = Ctx::default();
        let start_milligas = ctx.gas.milligas();
        report_gas(&mut ctx, |ctx| {
            assert!(ast
                .typecheck_instruction(ctx.gas(), None, &[parse("nat").unwrap()])
                .is_ok());
        });
        assert_eq!(start_milligas - ctx.gas.milligas(), 12680);
    }

    #[test]
    fn typecheck_out_of_gas() {
        let ast = parse(FIBONACCI_SRC).unwrap();
        assert_eq!(
            ast.typecheck_instruction(&mut Gas::new(1000), None, &[parse("nat").unwrap()]),
            Err(typechecker::TcError::OutOfGas(crate::gas::OutOfGas))
        );
    }

    #[test]
    fn typecheck_test_expect_fail() {
        use typechecker::{NoMatchingOverloadReason, TcError};
        let ast = parse(FIBONACCI_ILLTYPED_SRC).unwrap();
        assert_eq!(
            ast.typecheck_instruction(&mut Gas::default(), None, &[parse("nat").unwrap()]),
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
        assert_eq!(
            parse(FIBONACCI_MALFORMED_SRC)
                .unwrap()
                .typecheck_instruction(&mut Gas::default(), None, &[parse("nat").unwrap()]),
            Err(typechecker::TcError::UnexpectedMicheline(format!(
                "{:?}",
                parse("DUP 4 GT").unwrap()
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
        let arena = typed_arena::Arena::new();
        let ctx = &mut Ctx::default();
        ctx.amount = 5_000_000;
        use crate::lexer::Prim;
        use Micheline as M;
        let interp_res = parse_contract_script(VOTE_SRC)
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(ctx.gas(), true, true)
            .unwrap()
            .interpret(
                ctx,
                &arena,
                "foo".into(),
                &Entrypoint::default(),
                &M::seq(
                    &arena,
                    [
                        M::prim2(&arena, Prim::Elt, "bar".into(), 0.into()),
                        M::prim2(&arena, Prim::Elt, "baz".into(), 0.into()),
                        M::prim2(&arena, Prim::Elt, "foo".into(), 0.into()),
                    ],
                ),
            );
        use TypedValue as TV;
        match interp_res.unwrap() {
            (_, TV::Map(m)) => {
                assert_eq!(m.get(&TV::String("foo".to_owned())).unwrap(), &TV::int(1))
            }
            _ => panic!("unexpected contract output"),
        };
    }

    #[track_caller]
    fn run_e2e_test<'a>(
        arena: &'a Arena<Micheline<'a>>,
        instr: &'a str,
        input_type_stack: TypeStack,
        output_type_stack: TypeStack,
        mut input_stack: Stack<TypedValue<'a>>,
        output_stack: Stack<TypedValue<'a>>,
        mut ctx: Ctx<'a>,
    ) {
        let ast = parse(instr).unwrap();
        let mut input_failing_type_stack = FailingTypeStack::Ok(input_type_stack);
        let ast =
            typecheck_instruction(&ast, ctx.gas(), None, &mut input_failing_type_stack).unwrap();
        assert_eq!(
            input_failing_type_stack,
            FailingTypeStack::Ok(output_type_stack)
        );
        assert!(ast.interpret(&mut ctx, arena, &mut input_stack).is_ok());
        assert_eq!(input_stack, output_stack);
    }

    #[test]
    fn ticket_instr() {
        let ctx = Ctx::default();
        run_e2e_test(
            &Arena::new(),
            "TICKET",
            stk![Type::Nat, Type::Int],
            stk![Type::new_option(Type::new_ticket(Type::Int))],
            stk![TypedValue::nat(10), TypedValue::int(20)],
            stk![TypedValue::new_option(Some(TypedValue::new_ticket(
                Ticket {
                    amount: 10u32.into(),
                    content_type: Type::Int,
                    content: TypedValue::int(20),
                    ticketer: ctx.self_address
                }
            )))],
            Ctx::default(),
        );
    }

    #[test]
    fn read_ticket() {
        let ticketer_address_hash =
            AddressHash::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        let ticketer_address = Address {
            hash: ticketer_address_hash.clone(),
            entrypoint: Entrypoint::default(),
        };
        let ticket = Ticket {
            ticketer: ticketer_address_hash,
            amount: 100u32.into(),
            content_type: Type::Int,
            content: TypedValue::int(20),
        };
        run_e2e_test(
            &Arena::new(),
            "READ_TICKET",
            stk![Type::new_ticket(Type::Int)],
            stk![
                Type::new_ticket(Type::Int),
                Type::new_pair(Type::Address, Type::new_pair(Type::Int, Type::Nat)),
            ],
            stk![TypedValue::new_ticket(ticket.clone())],
            stk![
                TypedValue::new_ticket(ticket),
                TypedValue::new_pair(
                    TypedValue::Address(ticketer_address),
                    TypedValue::new_pair(TypedValue::int(20), TypedValue::nat(100))
                ),
            ],
            Ctx::default(),
        );
    }

    #[test]
    fn split_ticket() {
        let ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address,
            amount: 100u32.into(),
            content_type: Type::Int,
            content: TypedValue::int(20),
        };
        run_e2e_test(
            &Arena::new(),
            "SPLIT_TICKET",
            stk![
                Type::new_pair(Type::Nat, Type::Nat),
                Type::new_ticket(Type::Int)
            ],
            stk![Type::new_option(Type::new_pair(
                Type::new_ticket(Type::Int),
                Type::new_ticket(Type::Int)
            )),],
            stk![
                TypedValue::new_pair(TypedValue::nat(20), TypedValue::nat(80)),
                TypedValue::new_ticket(ticket.clone())
            ],
            stk![TypedValue::new_option(Some(TypedValue::new_pair(
                TypedValue::new_ticket(Ticket {
                    amount: 20u32.into(),
                    ..ticket.clone()
                }),
                TypedValue::new_ticket(Ticket {
                    amount: 80u32.into(),
                    ..ticket
                })
            ))),],
            Ctx::default(),
        );
    }

    #[test]
    fn join_tickets() {
        let ctx = Ctx::default();
        let ticket = Ticket {
            ticketer: ctx.self_address,
            amount: 100u32.into(),
            content_type: Type::Int,
            content: TypedValue::int(20),
        };
        run_e2e_test(
            &Arena::new(),
            "JOIN_TICKETS",
            stk![Type::new_pair(
                Type::new_ticket(Type::Int),
                Type::new_ticket(Type::Int)
            )],
            stk![Type::new_option(Type::new_ticket(Type::Int)),],
            stk![TypedValue::new_pair(
                TypedValue::new_ticket(Ticket {
                    amount: 20u32.into(),
                    ..ticket.clone()
                }),
                TypedValue::new_ticket(Ticket {
                    amount: 80u32.into(),
                    ..ticket.clone()
                })
            )],
            stk![TypedValue::new_option(Some(TypedValue::new_ticket(ticket))),],
            Ctx::default(),
        );
    }

    #[test]
    fn balance() {
        run_e2e_test(
            &Arena::new(),
            "BALANCE",
            stk![],
            stk![Type::Mutez],
            stk![],
            stk![TypedValue::Mutez(45),],
            {
                let mut c = Ctx::default();
                c.balance = 45;
                c
            },
        )
    }

    #[test]
    fn contract() {
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        // When contract for the address does not exist.
        run_e2e_test(
            &Arena::new(),
            "CONTRACT int",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Int))],
            stk![TypedValue::Address(addr)],
            stk![TypedValue::new_option(None),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts(HashMap::new());
                c
            },
        );

        let addr = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        // When contract is implicit
        run_e2e_test(
            &Arena::new(),
            "CONTRACT unit",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Unit))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(Some(TypedValue::Contract(addr))),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts(HashMap::new());
                c
            },
        );

        let addr = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        // When contract is implicit and contract type is Ticket
        run_e2e_test(
            &Arena::new(),
            "CONTRACT (ticket unit)",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::new_ticket(
                Type::Unit
            )))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(Some(TypedValue::Contract(addr))),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts(HashMap::new());
                c
            },
        );

        let addr = Address::try_from("tz3McZuemh7PCYG2P57n5mN8ecz56jCfSBR6").unwrap();
        // When contract is implicit and contract type is some other type
        run_e2e_test(
            &Arena::new(),
            "CONTRACT int",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Int))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(None),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts(HashMap::new());
                c
            },
        );

        // When contract for the address does exist and is of expected type.
        run_e2e_test(
            &Arena::new(),
            "CONTRACT unit",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Unit))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(Some(TypedValue::Contract(
                addr.clone()
            ))),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts({
                    let mut x = HashMap::new();
                    x.insert(
                        addr.hash,
                        HashMap::from([(Entrypoint::default(), Type::Unit)]),
                    );
                    x
                });
                c
            },
        );

        // When the address has an entrypoint.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%foo").unwrap();
        run_e2e_test(
            &Arena::new(),
            "CONTRACT unit",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Unit))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(Some(TypedValue::Contract(
                addr.clone()
            ))),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts({
                    let mut x = HashMap::new();
                    x.insert(
                        addr.hash,
                        HashMap::from([(Entrypoint::try_from("foo").unwrap(), Type::Unit)]),
                    );
                    x
                });
                c
            },
        );

        // When the instruction has an entrypoint.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_e2e_test(
            &Arena::new(),
            "CONTRACT %foo unit",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Unit))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(Some(TypedValue::Contract(
                Address {
                    entrypoint: Entrypoint::try_from("foo").unwrap(),
                    ..addr.clone()
                }
            ))),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts({
                    let mut x = HashMap::new();
                    x.insert(
                        addr.hash,
                        HashMap::from([(Entrypoint::try_from("foo").unwrap(), Type::Unit)]),
                    );
                    x
                });
                c
            },
        );

        // When the instruction has an entrypoint and address has an entrypoint.
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye%bar").unwrap();
        run_e2e_test(
            &Arena::new(),
            "CONTRACT %foo unit",
            stk![Type::Address],
            stk![Type::new_option(Type::new_contract(Type::Unit))],
            stk![TypedValue::Address(addr.clone())],
            stk![TypedValue::new_option(None),],
            {
                let mut c = Ctx::default();
                c.set_known_contracts({
                    let mut x = HashMap::new();
                    x.insert(
                        addr.hash,
                        HashMap::from([
                            (Entrypoint::try_from("bar").unwrap(), Type::Unit),
                            (Entrypoint::try_from("foo").unwrap(), Type::Unit),
                        ]),
                    );
                    x
                });
                c
            },
        );
    }

    #[test]
    fn level() {
        run_e2e_test(
            &Arena::new(),
            "LEVEL",
            stk![],
            stk![Type::Nat],
            stk![],
            stk![TypedValue::nat(45),],
            {
                let mut c = Ctx::default();
                c.level = 45u32.into();
                c
            },
        );
    }

    #[test]
    fn min_block_time() {
        run_e2e_test(
            &Arena::new(),
            "MIN_BLOCK_TIME",
            stk![],
            stk![Type::Nat],
            stk![],
            stk![TypedValue::nat(45),],
            {
                let mut c = Ctx::default();
                c.min_block_time = 45u32.into();
                c
            },
        );
    }

    #[test]
    fn self_address() {
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_e2e_test(
            &Arena::new(),
            "SELF_ADDRESS",
            stk![],
            stk![Type::Address],
            stk![],
            stk![TypedValue::Address(addr.clone())],
            {
                let mut c = Ctx::default();
                c.self_address = addr.hash;
                c
            },
        );
    }

    #[test]
    fn sender() {
        let addr = Address::try_from("KT1BRd2ka5q2cPRdXALtXD1QZ38CPam2j1ye").unwrap();
        run_e2e_test(
            &Arena::new(),
            "SENDER",
            stk![],
            stk![Type::Address],
            stk![],
            stk![TypedValue::Address(addr.clone())],
            {
                let mut c = Ctx::default();
                c.sender = addr.hash;
                c
            },
        );
    }

    #[test]
    fn source() {
        let addr = PublicKeyHash::try_from("tz1TSbthBCECxmnABv73icw7yyyvUWFLAoSP").unwrap();
        run_e2e_test(
            &Arena::new(),
            "SOURCE",
            stk![],
            stk![Type::Address],
            stk![],
            stk![TypedValue::Address(Address {
                hash: addr.clone().into(),
                entrypoint: Entrypoint::default()
            })],
            {
                let mut c = Ctx::default();
                c.source = addr;
                c
            },
        );
    }

    #[test]
    fn timestamp_type_and_value() {
        run_e2e_test(
            &Arena::new(),
            "PUSH timestamp 1571659294",
            stk![],
            stk![Type::Timestamp],
            stk![],
            stk![TypedValue::timestamp(1571659294)],
            Ctx::default(),
        );
        run_e2e_test(
            &Arena::new(),
            "PUSH timestamp \"2019-10-21T12:01:34Z\"",
            stk![],
            stk![Type::Timestamp],
            stk![],
            stk![TypedValue::timestamp(1571659294)],
            Ctx::default(),
        );
    }

    #[test]
    fn now() {
        run_e2e_test(
            &Arena::new(),
            "NOW",
            stk![],
            stk![Type::Timestamp],
            stk![],
            stk![TypedValue::timestamp(4500),],
            {
                let mut c = Ctx::default();
                c.now = 4500i32.into();
                c
            },
        );
    }

    #[test]
    fn implicit_account() {
        let key_hash = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        run_e2e_test(
            &Arena::new(),
            "IMPLICIT_ACCOUNT",
            stk![Type::KeyHash],
            stk![Type::new_contract(Type::Unit)],
            stk![TypedValue::KeyHash(key_hash)],
            stk![TypedValue::Contract(
                Address::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap()
            )],
            Ctx::default(),
        );
    }

    #[test]
    fn voting_power() {
        let key_hash_1 = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = PublicKeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        let key_hash_3 = PublicKeyHash::try_from("tz3hpojUX9dYL5KLusv42SCBiggB77a2QLGx").unwrap();
        run_e2e_test(
            &Arena::new(),
            "VOTING_POWER",
            stk![Type::KeyHash],
            stk![Type::Nat],
            stk![TypedValue::KeyHash(key_hash_2.clone())],
            stk![TypedValue::nat(50)],
            {
                let mut c = Ctx::default();
                c.set_voting_powers([
                    (key_hash_1.clone(), 30u32.into()),
                    (key_hash_2.clone(), 50u32.into()),
                ]);
                c
            },
        );

        run_e2e_test(
            &Arena::new(),
            "VOTING_POWER",
            stk![Type::KeyHash],
            stk![Type::Nat],
            stk![TypedValue::KeyHash(key_hash_3)],
            stk![TypedValue::nat(0)],
            {
                let mut c = Ctx::default();
                c.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
                c
            },
        );
    }

    #[test]
    fn total_voting_power() {
        let key_hash_1 = PublicKeyHash::try_from("tz3d9na7gPpt5jxdjGBFzoGQigcStHB8w1uq").unwrap();
        let key_hash_2 = PublicKeyHash::try_from("tz4T8ydHwYeoLHmLNcECYVq3WkMaeVhZ81h7").unwrap();
        run_e2e_test(
            &Arena::new(),
            "TOTAL_VOTING_POWER",
            stk![],
            stk![Type::Nat],
            stk![],
            stk![TypedValue::nat(80)],
            {
                let mut c = Ctx::default();
                c.set_voting_powers([(key_hash_1, 30u32.into()), (key_hash_2, 50u32.into())]);
                c
            },
        );
    }

    #[test]
    fn emit() {
        run_e2e_test(
            &Arena::new(),
            "EMIT %mytag nat",
            stk![Type::Nat],
            stk![Type::Operation],
            stk![TypedValue::nat(10)],
            stk![TypedValue::new_operation(
                Operation::Emit(Emit {
                    tag: Some(FieldAnnotation::from_str_unchecked("mytag")),
                    value: TypedValue::nat(10),
                    arg_ty: Or::Right(parse("nat").unwrap())
                }),
                101
            )],
            {
                let mut ctx = Ctx::default();
                ctx.set_operation_counter(100);
                ctx
            },
        );

        run_e2e_test(
            &Arena::new(),
            "EMIT nat",
            stk![Type::Nat],
            stk![Type::Operation],
            stk![TypedValue::nat(10)],
            stk![TypedValue::new_operation(
                Operation::Emit(Emit {
                    tag: None,
                    value: TypedValue::nat(10),
                    arg_ty: Or::Right(parse("nat").unwrap())
                }),
                101
            )],
            {
                let mut ctx = Ctx::default();
                ctx.set_operation_counter(100);
                ctx
            },
        );

        run_e2e_test(
            &Arena::new(),
            "EMIT",
            stk![Type::Nat],
            stk![Type::Operation],
            stk![TypedValue::nat(10)],
            stk![TypedValue::new_operation(
                Operation::Emit(Emit {
                    tag: None,
                    value: TypedValue::nat(10),
                    arg_ty: Or::Left(Type::Nat)
                }),
                101
            )],
            {
                let mut ctx = Ctx::default();
                ctx.set_operation_counter(100);
                ctx
            },
        );
    }

    #[test]
    fn dig() {
        run_e2e_test(
            &Arena::new(),
            "DIG 3",
            stk![Type::Unit, Type::Nat, Type::Int, Type::String],
            stk![Type::Nat, Type::Int, Type::String, Type::Unit],
            stk![
                TypedValue::new_pair(TypedValue::Unit, TypedValue::Unit),
                TypedValue::Unit,
                TypedValue::nat(10),
                TypedValue::int(20),
            ],
            stk![
                TypedValue::Unit,
                TypedValue::nat(10),
                TypedValue::int(20),
                TypedValue::new_pair(TypedValue::Unit, TypedValue::Unit),
            ],
            Ctx::default(),
        );
    }

    #[test]
    fn dug() {
        run_e2e_test(
            &Arena::new(),
            "DUG 2",
            stk![
                Type::Unit,
                Type::Nat,
                Type::Int,
                Type::Bool,
                Type::Bool,
                Type::String
            ],
            stk![
                Type::Unit,
                Type::Nat,
                Type::Int,
                Type::String,
                Type::Bool,
                Type::Bool
            ],
            stk![
                TypedValue::new_pair(TypedValue::Unit, TypedValue::Unit),
                TypedValue::Unit,
                TypedValue::nat(10),
                TypedValue::int(20),
            ],
            stk![
                TypedValue::new_pair(TypedValue::Unit, TypedValue::Unit),
                TypedValue::int(20),
                TypedValue::Unit,
                TypedValue::nat(10),
            ],
            Ctx::default(),
        );
    }

    #[test]
    fn create_contract() {
        use tezos_crypto_rs::hash::OperationHash;
        let cs_mich =
            parse("{ parameter unit; storage unit; code { DROP; UNIT; NIL operation; PAIR; }}")
                .unwrap();
        let cs = cs_mich
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap();
        let expected_addr = "KT1D5WSrhAnvHDrcNg8AtDoQCFaeikYjim6K";
        let expected_op = TypedValue::new_operation(
            Operation::CreateContract(CreateContract {
                delegate: None,
                amount: 100,
                storage: TypedValue::Unit,
                code: Rc::new(cs),
                micheline_code: &cs_mich,
                address: ContractKt1Hash::try_from(expected_addr).unwrap(),
            }),
            101,
        );
        run_e2e_test(
            &Arena::new(),
            r#"CREATE_CONTRACT {
                parameter unit;
                storage unit;
                code { DROP; UNIT; NIL operation; PAIR; }
            }"#,
            stk![Type::Unit, Type::Mutez, Type::new_option(Type::KeyHash)],
            stk![Type::Address, Type::Operation],
            stk![
                TypedValue::Unit,
                TypedValue::Mutez(100),
                TypedValue::new_option(None)
            ],
            stk![
                TypedValue::Address(Address::try_from(expected_addr).unwrap()),
                expected_op
            ],
            {
                let mut ctx = Ctx::default();
                ctx.set_operation_counter(100);
                ctx.set_origination_counter(0);
                ctx.operation_group_hash = OperationHash::from_base58_check(
                    "onvsLP3JFZia2mzZKWaFuFkWg2L5p3BDUhzh5Kr6CiDDN3rtQ1D",
                )
                .unwrap()
                .as_ref()
                .try_into()
                .unwrap();
                ctx
            },
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

    const MACRO_IF_SOME_SRC: &str = "{IF_SOME { PUSH nat 1 ; ADD } { PUSH nat 5; }}";
}

#[cfg(test)]
mod multisig_tests {
    use crate::ast::*;
    use crate::context::{Ctx, TypecheckingCtx};
    use crate::gas::Gas;
    use crate::interpreter::{ContractInterpretError, InterpretError};
    use crate::lexer::Prim;
    use crate::parser::test_helpers::parse_contract_script;
    use num_bigint::BigUint;
    use typed_arena::Arena;
    use Type as T;
    use TypedValue as TV;

    // The comments below detail the steps used to
    // prepare the signature for calling the multisig contract.

    /*
        # Create a private/public key pair.
        $ octez-client import secret key bob 'unencrypted:edsk3SQWDxieaYEVsQbogKwVnArgwbWHQkQYaW1JcNmRmyWWLFXPTt'
        $ octez-client show address bob
        Public Key: edpku6Ffo8HgLgeBcArjtWeZ29hLEXP7ewsq5aAj8jr7giUVAAVnUM
    */
    static PUBLIC_KEY: &str = "edpku6Ffo8HgLgeBcArjtWeZ29hLEXP7ewsq5aAj8jr7giUVAAVnUM";

    /*
        $ PARAM_TYPE='
            pair
                (pair chain_id address)
                nat
                (or (pair mutez address) (or (option key_hash) (pair nat (list key))))'
        $ SELF_ADDRESS='KT1BFATQpdP5xJGErJyk2vfL46dvFanWz87H'
        $ CHAIN_ID='0xf3d48554'
        $ ANTI_REPLAY_COUNTER='111'
    */
    fn make_ctx<'a>() -> Ctx<'a> {
        let mut ctx = Ctx::default();
        ctx.self_address = "KT1BFATQpdP5xJGErJyk2vfL46dvFanWz87H".try_into().unwrap();
        ctx.chain_id =
            tezos_crypto_rs::hash::ChainId::try_from(hex::decode("f3d48554").unwrap()).unwrap();
        ctx
    }

    fn anti_replay_counter() -> BigUint {
        BigUint::from(111u32)
    }

    fn arena() -> &'static typed_arena::Arena<Micheline<'static>> {
        // this is generally terrible and will leak memory in some
        // (multi-threaded) workloads, but it's fine for these tests
        thread_local! {
            static BX: &'static typed_arena::Arena<Micheline<'static>> =
                Box::leak(Box::new(typed_arena::Arena::new()));
        }
        BX.with(|a| *a)
    }

    fn pair(
        x: impl Into<Micheline<'static>>,
        y: impl Into<Micheline<'static>>,
    ) -> Micheline<'static> {
        Micheline::prim2(arena(), Prim::Pair, x.into(), y.into())
    }
    fn right(x: impl Into<Micheline<'static>>) -> Micheline<'static> {
        Micheline::prim1(arena(), Prim::Right, x.into())
    }
    fn left(x: impl Into<Micheline<'static>>) -> Micheline<'static> {
        Micheline::prim1(arena(), Prim::Left, x.into())
    }
    fn some(x: impl Into<Micheline<'static>>) -> Micheline<'static> {
        Micheline::prim1(arena(), Prim::Some, x.into())
    }
    fn seq<const N: usize>(xs: [Micheline<'static>; N]) -> Micheline<'static> {
        Micheline::seq(arena(), xs)
    }

    #[test]
    fn multisig_transfer() {
        let temp = Arena::new();
        let mut ctx = make_ctx();
        let threshold = BigUint::from(1u32);

        /*
            # Pack the parameter we will be sending to the multisig contract.
            $ BYTES=$(octez-client --mode mockup hash data "
                Pair
                    (Pair $CHAIN_ID \"$SELF_ADDRESS\")
                    $ANTI_REPLAY_COUNTER
                    (Left (Pair 123 \"tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j\"))
                " of type $PARAM_TYPE | sed -n 's/^Raw packed data: //p')

            # Sign the packed parameter.
            $ octez-client --mode mockup sign bytes $BYTES for bob
            Signature: edsigu1GCyS754UrkFLng9P5vG5T51Hs8TcgZoV7fPfj5qeXYzC1JKuUYzyowpfGghEEqUyPxpUdU7WRFrdxad5pnspQg9hwk6v
        */
        let transfer_amount = 123;
        let transfer_destination = "tz1WrbkDrzKVqcGXkjw4Qk4fXkjXpAJuNP1j";
        let signature = "edsigu1GCyS754UrkFLng9P5vG5T51Hs8TcgZoV7fPfj5qeXYzC1JKuUYzyowpfGghEEqUyPxpUdU7WRFrdxad5pnspQg9hwk6v";

        let interp_res = parse_contract_script(MULTISIG_SRC)
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(&mut Gas::default(), true, true)
            .unwrap()
            .interpret(
                &mut ctx,
                &temp,
                pair(
                    // :payload
                    pair(
                        anti_replay_counter(),
                        left(
                            // :transfer
                            pair(transfer_amount as i128, transfer_destination),
                        ),
                    ),
                    // %sigs
                    seq([some(signature)]),
                ),
                &Entrypoint::default(),
                // make_initial_storage(),
                &pair(
                    anti_replay_counter(),
                    pair(threshold.clone(), seq([PUBLIC_KEY.into()])),
                ),
            );

        assert_eq!(
            collect_ops(interp_res),
            Ok((
                vec![OperationInfo {
                    operation: Operation::TransferTokens(TransferTokens {
                        param: TV::Unit,
                        destination_address: transfer_destination.try_into().unwrap(),
                        amount: transfer_amount,
                    }),
                    counter: 1
                }],
                TV::new_pair(
                    TV::Nat(anti_replay_counter() + BigUint::from(1u32)),
                    TV::new_pair(
                        TV::Nat(threshold),
                        TV::List(MichelsonList::from(vec![TV::Key(
                            PUBLIC_KEY.try_into().unwrap()
                        )]))
                    )
                )
            ))
        );
    }

    #[test]
    fn multisig_set_delegate() {
        let temp = Arena::new();
        let mut ctx = make_ctx();
        let threshold = BigUint::from(1u32);

        /*
            # Pack the parameter we will be sending to the multisig contract.
            $ BYTES=$(octez-client --mode mockup hash data "
                Pair
                    (Pair $CHAIN_ID \"$SELF_ADDRESS\")
                    $ANTI_REPLAY_COUNTER
                    (Right (Left (Some \"tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy\")))
                " of type $PARAM_TYPE | sed -n 's/^Raw packed data: //p')

            # Sign the packed parameter.
            $ octez-client --mode mockup sign bytes $BYTES for bob
            Signature: edsigtXyZmxgR3MDhDRdtAtopHNNE8rPsPRHgPXurkMacmRLvbLyBCTjtBFNFYHEcLTjx94jdvUf81Wd7uybJNGn5phJYaPAJST
        */
        let new_delegate = "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy";
        let signature = "edsigtXyZmxgR3MDhDRdtAtopHNNE8rPsPRHgPXurkMacmRLvbLyBCTjtBFNFYHEcLTjx94jdvUf81Wd7uybJNGn5phJYaPAJST";

        let interp_res = parse_contract_script(MULTISIG_SRC)
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(ctx.gas(), true, true)
            .unwrap()
            .interpret(
                &mut ctx,
                &temp,
                pair(
                    // :payload
                    pair(
                        anti_replay_counter(),
                        right(left(
                            // %delegate
                            some(new_delegate),
                        )),
                    ),
                    // %sigs
                    seq([some(signature)]),
                ),
                &Entrypoint::default(),
                &pair(
                    anti_replay_counter(),
                    pair(threshold.clone(), seq([PUBLIC_KEY.into()])),
                ),
            );

        assert_eq!(
            collect_ops(interp_res),
            Ok((
                vec![OperationInfo {
                    operation: Operation::SetDelegate(SetDelegate(Some(
                        new_delegate.try_into().unwrap()
                    ))),
                    counter: 1
                }],
                TV::new_pair(
                    TV::Nat(anti_replay_counter() + BigUint::from(1u32)),
                    TV::new_pair(
                        TV::Nat(threshold),
                        TV::List(MichelsonList::from(vec![TV::Key(
                            PUBLIC_KEY.try_into().unwrap()
                        )]))
                    )
                )
            ))
        );
    }

    #[test]
    fn invalid_signature() {
        let temp = Arena::new();
        let mut ctx = make_ctx();
        let threshold = 1;
        let new_delegate = "tz1V8fDHpHzN8RrZqiYCHaJM9EocsYZch5Cy";
        let invalid_signature = "edsigtt6SusfFFqwKqJNDuZMbhP6Q8f6zu3c3q7W6vPbjYKpv84H3hfXhRyRvAXHzNYSwBNNqjmf5taXKd2ZW3Rbix78bhWjxg5";

        let interp_res = parse_contract_script(MULTISIG_SRC)
            .unwrap()
            .split_script()
            .unwrap()
            .typecheck_script(ctx.gas(), true, true)
            .unwrap()
            .interpret(
                &mut ctx,
                &temp,
                pair(
                    // :payload
                    pair(
                        anti_replay_counter(),
                        right(left(
                            // %delegate
                            some(new_delegate),
                        )),
                    ),
                    // %sigs
                    seq([some(invalid_signature)]),
                ),
                &Entrypoint::default(),
                &pair(
                    anti_replay_counter(),
                    pair(threshold, seq([PUBLIC_KEY.into()])),
                ),
            );

        assert_eq!(
            collect_ops(interp_res),
            Err(ContractInterpretError::InterpretError(
                InterpretError::FailedWith(T::Unit, TV::Unit)
            ))
        );
    }

    // The interpretation result contains an iterator of operations,
    // which does not implement `Eq` and therefore cannot be used with `assert_eq!`.
    // This function collects the iterator into a vector so we can use `assert_eq!`.
    fn collect_ops<'a>(
        result: Result<
            (impl Iterator<Item = OperationInfo<'a>>, TypedValue<'a>),
            ContractInterpretError<'a>,
        >,
    ) -> Result<(Vec<OperationInfo<'a>>, TypedValue<'a>), ContractInterpretError<'a>> {
        result.map(|(ops, val)| (ops.collect(), val))
    }

    // From: https://github.com/murbard/smart-contracts/blob/eb2b7d81aedcfeaea219da8b66cdd86652bf42f7/multisig/michelson/multisig.tz
    const MULTISIG_SRC: &str = "
        parameter (pair
                    (pair :payload
                        (nat %counter) # counter, used to prevent replay attacks
                        (or :action    # payload to sign, represents the requested action
                        (pair :transfer    # transfer tokens
                            (mutez %amount) # amount to transfer
                            (contract %dest unit)) # destination to transfer to
                        (or
                            (option %delegate key_hash) # change the delegate to this address
                            (pair %change_keys          # change the keys controlling the multisig
                                (nat %threshold)         # new threshold
                                (list %keys key)))))     # new list of keys
                    (list %sigs (option signature)));    # signatures

        storage (pair (nat %stored_counter) (pair (nat %threshold) (list %keys key))) ;

        code
        {
            UNPAIR ; SWAP ; DUP ; DIP { SWAP } ;
            DIP
            {
                UNPAIR ;
                # pair the payload with the current contract address, to ensure signatures
                # can't be replayed accross different contracts if a key is reused.
                DUP ; SELF ; ADDRESS ; CHAIN_ID ; PAIR ; PAIR ;
                PACK ; # form the binary payload that we expect to be signed
                DIP { UNPAIR @counter ; DIP { SWAP } } ; SWAP
            } ;

            # Check that the counters match
            UNPAIR @stored_counter; DIP { SWAP };
            ASSERT_CMPEQ ;

            # Compute the number of valid signatures
            DIP { SWAP } ; UNPAIR @threshold @keys;
            DIP
            {
                # Running count of valid signatures
                PUSH @valid nat 0; SWAP ;
                ITER
                {
                    DIP { SWAP } ; SWAP ;
                    IF_CONS
                    {
                        IF_SOME
                        { SWAP ;
                            DIP
                            {
                                SWAP ; DIIP { DUUP } ;
                                # Checks signatures, fails if invalid
                                CHECK_SIGNATURE ; ASSERT ;
                                PUSH nat 1 ; ADD @valid } }
                        { SWAP ; DROP }
                    }
                    {
                        # There were fewer signatures in the list
                        # than keys. Not all signatures must be present, but
                        # they should be marked as absent using the option type.
                        FAIL
                    } ;
                    SWAP
                }
            } ;
            # Assert that the threshold is less than or equal to the
            # number of valid signatures.
            ASSERT_CMPLE ;
            DROP ; DROP ;

            # Increment counter and place in storage
            DIP { UNPAIR ; PUSH nat 1 ; ADD @new_counter ; PAIR} ;

            # We have now handled the signature verification part,
            # produce the operation requested by the signers.
            NIL operation ; SWAP ;
            IF_LEFT
            { # Transfer tokens
                UNPAIR ; UNIT ; TRANSFER_TOKENS ; CONS }
            { IF_LEFT {
                        # Change delegate
                        SET_DELEGATE ; CONS }
                        {
                        # Change set of signatures
                        DIP { SWAP ; CAR } ; SWAP ; PAIR ; SWAP }} ;
            PAIR }
        ";
}
