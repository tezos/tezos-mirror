/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

//! Emulate persistent storage using `thread_local!`.

use mir::ast::*;
use mir::context::Ctx;
use mir::parser::Parser;
use std::cell::RefCell;

use typed_arena::Arena;

/// A simple counter contract
static SCRIPT: &str = r#"
    parameter unit;
    storage nat;
    code { CDR; PUSH nat 1; ADD; NIL operation; PAIR }
    "#;

fn run_contract(parameter: Micheline) {
    thread_local! {
        // you could define a global static, too, but you would use RwLock
        // instead of RefCell then.
        static STORAGE: RefCell<Vec<u8>> = RefCell::new(vec![0, 0]);
    }
    let parser = Parser::new();
    let contract_micheline = parser.parse_top_level(SCRIPT).unwrap();
    let mut ctx = Ctx::default();
    let contract_typechecked = contract_micheline.typecheck_script(&mut ctx).unwrap();
    STORAGE.with(|storage| {
        storage.replace_with(|storage| {
            let storage = Micheline::decode_raw(&parser.arena, storage).unwrap();
            let (_, new_storage) = contract_typechecked
                .interpret(&mut ctx, &parser.arena, parameter, storage)
                .unwrap();
            let TypedValue::Nat(storage_nat) = &new_storage else { unreachable!() };
            println!("{storage_nat}");
            new_storage
                .into_micheline_optimized_legacy(&Arena::new())
                .encode()
        });
    });
}

fn main() {
    run_contract(().into()); // prints "1"
    run_contract(().into()); // prints "2"
    run_contract(().into()); // prints "3"
    run_contract(().into()); // prints "4"
}
