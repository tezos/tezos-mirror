// SPDX-FileCopyrightText: [2023] Serokell <hi@serokell.io>
//
// SPDX-License-Identifier: MIT

//! Only parse the script once, save a few cycles on subsequent runs.

use mir::ast::*;
use mir::context::{Ctx, TypecheckingCtx};
use mir::parser::Parser;
use std::sync::OnceLock;

/// A simple contract that sums parameter and storage
static SCRIPT: &str = r#"
    parameter nat;
    storage nat;
    code { UNPAIR; ADD; NIL operation; PAIR }
    "#;

static CONTRACT: OnceLock<Micheline<'static>> = OnceLock::new();

/// Lazily parse the contract into [Micheline]. This unavoidably leaks a
/// [Parser] on initialization, but since it's supposed to be static, that
/// [Parser] would have to live for the duration of the program anyway, and it
/// only happens once, so memory-wise this shouldn't make a difference.
fn contract() -> &'static Micheline<'static> {
    CONTRACT.get_or_init(|| {
        let parser = Box::leak(Box::new(Parser::new()));
        parser.parse_top_level(SCRIPT).unwrap()
    })
}

/// We pass storage as a parameter, generally it would be stored somewhere.
fn run_contract(parameter: Micheline, storage: Micheline) {
    let parser = Parser::new();
    // The contract is only lazily parsed once.
    let contract_micheline = contract();
    let mut ctx = Ctx::default();
    let contract_typechecked = contract_micheline
        .split_script()
        .unwrap()
        .typecheck_script(ctx.gas(), true, true)
        .unwrap();

    let (_, new_storage) = contract_typechecked
        .interpret(
            &mut ctx,
            &parser.arena,
            parameter,
            &Entrypoint::default(),
            &storage,
        )
        .unwrap();
    let TypedValue::Nat(storage_nat) = &new_storage else {
        unreachable!()
    };
    println!("{storage_nat}");
}

fn time(f: impl Fn()) {
    let time = std::time::Instant::now();
    f();
    dbg!(time.elapsed());
}

fn main() {
    time(|| run_contract(1.into(), 2.into())); // prints "3", takes notably longer then subsequent runs.
    time(|| run_contract(2.into(), 3.into())); // prints "5"
    time(|| run_contract(3.into(), 4.into())); // prints "7"
    time(|| run_contract(4.into(), 5.into())); // prints "9"
}
