### M.I.R - Michelson In Rust

This repo hosts the Rust implementation of the typechecker and interpreter for
Michelson smart contract language.

#### Building

You need `cargo` to build this project. You can use the following
command to build the project.

`cargo build`

#### Testing

You can run the included tests by the following command.

`cargo test`

Some tests print gas consumption information (in addition to testing it), but `cargo test` omits output from successful tests by default. To see it, run

`cargo test -- --show-output`
