(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component: All
   Invocation: make test-tezt
   Subject: This file is the entrypoint of all Tezt tests. It dispatches to
            other files.
 *)

(* This module runs the tests implemented in all other modules of this directory.
   Each module defines tests which are thematically related,
   as functions to be called here. *)

let register protocol =
  Basic.register protocol ;
  Bootstrap.register protocol ;
  Synchronisation_heuristic.register protocol ;
  Mockup.register protocol ;
  Normalize.register protocol ;
  Proxy.register protocol ;
  Double_bake.register protocol

let register_mockup () =
  (* Support for Mockup in Carthage was removed in https://gitlab.com/tezos/tezos/-/merge_requests/2502 *)
  let supported_mockup_protocols =
    Protocol.all_protocols |> List.filter (fun p -> p > Protocol.Carthage)
  in
  List.iter Mockup.register supported_mockup_protocols

let register_proxy () =
  (* Tests don't pass for Carthage, but this is not a big deal as Carthage support will drop in the near future. *)
  let supported_proxy_protocols =
    Protocol.all_protocols |> List.filter (fun p -> p > Protocol.Carthage)
  in
  List.iter Proxy.register supported_proxy_protocols

let () =
  register Alpha ;
  register_mockup () ;
  Mockup.register_protocol_independent () ;
  register_proxy () ;
  P2p.register Alpha ;
  Bootstrap.register_protocol_independent () ;
  Cli_tezos.register_protocol_independent () ;
  Encoding.register () ;
  RPC_test.register () ;
  Baking.register Alpha ;
  (* Test.run () should be the last statement, don't register afterwards! *)
  Test.run ()
