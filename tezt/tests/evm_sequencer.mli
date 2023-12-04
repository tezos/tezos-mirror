(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Etherlink: sequencer
   Requirement:  make -f kernels.mk build
                 npm install eth-cli
   Invocation:   dune exec tezt/tests/main.exe -- --file evm_sequencer.ml
*)

val register : protocols:Protocol.t list -> unit
