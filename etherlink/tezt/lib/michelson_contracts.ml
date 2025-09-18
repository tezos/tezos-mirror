(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let tezlink_protocol = Protocol.S023

(* How to add a new contract to this list to serve as bootstrap contracts in
   Tezlink integration tests.

   To add a contract to this list, we need to pick a fresh KT1 address. It's
   also a good idea to manually check that originating the new contract on Tezos
   succeeds. Note that a fresh KT1 address is part of the result of this manual
   origination test. The most convenient way to run this manual test and generate
   the address is to use the mockup mode of octez-client:


   1) Create a convenient alias for the client:
      alias mockup-client='/path/to/octez-client --mode mockup --base-dir /tmp/mockup'
      mockup-client create mockup

   2) Originate the contract; the address will be printed on success:
      mockup-client originate contract <contract_name> \
        transferring 0 from bootstrap1 \
        running <path/to/contract> \
        --init '<initial_storage>' \
        --burn-cap 1 \
        --force

   3) The address can be found on the bottom of the receipt and it can be later consulted by
      mockup-client list known contracts
*)

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

let big_map_option () =
  Evm_node.
    {
      address = "KT1WxdAwoKy6WKLe32UfDoRpB9uxfxPWjRAo";
      path =
        Michelson_script.(find ["big_maps"; "option"] tezlink_protocol |> path);
      initial_storage = "None";
    }

let big_map_counter () =
  Evm_node.
    {
      address = "KT1SRkwG4X65KD7sJhFFoc1fPyuFGECMZsYs";
      path =
        Michelson_script.(find ["big_maps"; "counter"] tezlink_protocol |> path);
      initial_storage = "(Pair 0 { Elt 0 Unit })";
    }
