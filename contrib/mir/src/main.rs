/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

mod ast;
mod parser;
mod syntax;

fn main() {}

#[cfg(test)]
mod tests {
    use crate::parser;

    #[test]
    fn parser_test_expect_success() {
        let src = "{ INT ; PUSH int 0 ; DUP 2 ; GT ;
       IF { DIP { PUSH int -1 ; ADD } ;
            PUSH int 1 ;
            DUP 3 ;
            GT ;
            LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
            DIP { DROP 2 } }
          { DIP { DROP } } }";

        // use built in pretty printer to validate the expected AST.
        assert_eq!(format!("{:#?}", parser::parse(&src).unwrap()), EXPECTATION);
    }

    #[test]
    fn parser_test_expect_fail() {
        let src = "{ INT ; PUSH int 0 ; DUP 2 ; GT ;
       IF { DIP { PUSH int -1 ; ADD } ;
            PUSH int 1 ;
            DUP 3
            GT ;
            LOOP { SWAP ; DUP 2 ; ADD ; DIP 2 { PUSH int -1 ; ADD } ; DUP 3 ; GT } ;
            DIP { DROP 2 } }
          { DIP { DROP } } }";

        assert_eq!(&parser::parse(&src).unwrap_err().to_string(), "Unrecognized token `GT` found at 129:131\nExpected one of \";\" or \"}\"");
    }
    const EXPECTATION: &str = "[
    Int,
    Push(
        Int,
        NumberValue(
            0,
        ),
    ),
    DupN(
        2,
    ),
    Gt,
    If(
        [
            Dip(
                [
                    Push(
                        Int,
                        NumberValue(
                            -1,
                        ),
                    ),
                    Add,
                ],
            ),
            Push(
                Int,
                NumberValue(
                    1,
                ),
            ),
            DupN(
                3,
            ),
            Gt,
            Loop(
                [
                    Swap,
                    DupN(
                        2,
                    ),
                    Add,
                    DipN(
                        2,
                        [
                            Push(
                                Int,
                                NumberValue(
                                    -1,
                                ),
                            ),
                            Add,
                        ],
                    ),
                    DupN(
                        3,
                    ),
                    Gt,
                ],
            ),
            Dip(
                [
                    DropN(
                        2,
                    ),
                ],
            ),
        ],
        [
            Dip(
                [
                    Drop,
                ],
            ),
        ],
    ),
]";
}
