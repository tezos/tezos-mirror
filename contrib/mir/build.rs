/******************************************************************************/
/*                                                                            */
/* SPDX-License-Identifier: MIT                                               */
/* Copyright (c) [2023] Serokell <hi@serokell.io>                             */
/*                                                                            */
/******************************************************************************/

fn main() {
    lalrpop::Configuration::new().generate_in_source_tree().process().unwrap()
}
