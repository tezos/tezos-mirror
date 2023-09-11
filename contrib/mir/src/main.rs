/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

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

        assert!(parser::parse(&src));
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

        assert!(!parser::parse(&src));
    }
}
