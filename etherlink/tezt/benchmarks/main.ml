(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2025 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

let () =
  Etherlink_benchmark_lib.Benchmark_utils.parse_cli () ;
  Evm_node_capacity.register () ;
  Snailtracer.register () ;
  Uniswap.register ()

let () = Test.run ()
