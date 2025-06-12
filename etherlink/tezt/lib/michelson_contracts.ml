(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let tezlink_protocol = Protocol.R022

let bootstraps () =
  [|
    Evm_node.
      {
        address = "KT1Q36KWPSba7dHsH5E4ZsQHehrChc51e19d";
        path =
          Michelson_script.(
            find ["opcodes"; "concat_hello"] tezlink_protocol |> path);
        initial_storage = "{ \"initial\" }";
      };
  |]
