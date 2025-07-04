(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let tezlink_protocol = Protocol.S023

let concat_hello () =
  Evm_node.
    {
      address = "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d";
      path =
        Michelson_script.(
          find ["opcodes"; "concat_hello"] tezlink_protocol |> path);
      initial_storage = "{ \"initial\" }";
    }

let faucet_contract () =
  Evm_node.
    {
      address = "KT1QuofAgnsWffHzLA7D78rxytJruGHDe7XG";
      path =
        Michelson_script.(
          find ["mini_scenarios"; "faucet"] tezlink_protocol |> path);
      initial_storage = "Unit";
    }
