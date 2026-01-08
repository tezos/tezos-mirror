// SPDX-FileCopyrightText: [2024] Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use mir::ast::*;
use mir::context::{Ctx, TypecheckingCtx};
use mir::parser::Parser;

/// A simple contract that peformas arithmetic operations on an integer storage.
static SCRIPT: &str = r#" parameter (or (or (or (or (int %decrement) (int %increment)) (unit %reset)) (int %set)) (unit %double)) ;
        storage int;
        code {
            DUP ;
            CDR ;
            SWAP ;
            CAR ;
            IF_LEFT {
                IF_LEFT {
                    IF_LEFT
                    {
                        IF_LEFT
                            { SWAP ; SUB }
                            { ADD }
                    }
                    {
                        DROP 2; PUSH int 0
                    }
                }
                { SWAP ; DROP }
            }
            { DROP ; DUP ; ADD};
            NIL operation ;
            PAIR
        }"#;

/// We pass storage as a parameter, generally it would be stored somewhere.
fn run_contract<'a>(
    parameter: Micheline<'a>,
    storage: Micheline<'a>,
    entrypoint: &str,
    contract_typechecked: &ContractScript<'a>,
    ctx: &mut Ctx<'a>,
    parser: &'a Parser<'a>,
) {
    let entrypoint =
        Entrypoint::try_from(entrypoint).expect("Entrypoint should be valid, check the string");
    let (_, new_storage) = contract_typechecked
        .interpret(ctx, &parser.arena, parameter, &entrypoint, &storage)
        .unwrap();
    let TypedValue::Int(storage_int) = &new_storage else {
        unreachable!()
    };
    println!("{storage_int}");
}

fn main() {
    let parser = Parser::new();
    let contract_micheline = parser.parse_top_level(SCRIPT).unwrap();
    let mut ctx = Ctx::default();
    let contract_typechecked = contract_micheline
        .split_script()
        .unwrap()
        .typecheck_script(ctx.gas(), true, true)
        .unwrap();
    run_contract(
        30.into(),
        20.into(),
        "increment",
        &contract_typechecked,
        &mut ctx,
        &parser,
    ); // prints "50"
    run_contract(
        100.into(),
        80.into(),
        "decrement",
        &contract_typechecked,
        &mut ctx,
        &parser,
    ); // prints "-20"
    run_contract(
        7.into(),
        123.into(),
        "set",
        &contract_typechecked,
        &mut ctx,
        &parser,
    ); // prints "7"
    run_contract(
        ().into(),
        9.into(),
        "double",
        &contract_typechecked,
        &mut ctx,
        &parser,
    ); // prints "18"
    run_contract(
        ().into(),
        27.into(),
        "reset",
        &contract_typechecked,
        &mut ctx,
        &parser,
    ); // prints "0"
}
