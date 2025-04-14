(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

let () =
  Producer.register
    ~title:"general_contracts"
    ~registered:Primitives.registered_general_contracts
    ~tx_per_call:10 ;
  Producer.register
    ~title:"problematic_opcodes"
    ~registered:Primitives.registered_problematic_opcodes
    ~tx_per_call:10 ;
  Producer.register
    ~title:"gas_sinks"
    ~registered:Primitives.registered_gas_sinks
    ~tx_per_call:1 ;
  Producer.register
    ~title:"precompiled"
    ~registered:Primitives.registered_precompiled
    ~tx_per_call:1 ;
  Test.run ()
