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

let () =
  (* Tests that are relatively protocol-agnostic.
     We can run them on all protocols, or only one if the CI would be too slow. *)
  Basic.register Alpha ;
  Bootstrap.register Alpha ;
  Synchronisation_heuristic.register Alpha ;
  Normalize.register Alpha ;
  Double_bake.register Alpha ;
  Mockup.register Protocol.current_mainnet ;
  Mockup.register Alpha ;
  Mockup.register_constant_migration ~migrate_from:Edo ~migrate_to:Alpha ;
  Proxy.register Protocol.current_mainnet ;
  Proxy.register Alpha ;
  P2p.register Alpha ;
  (* TODO: the "Baking" test does not have a documentation.
     I don't know if it is about baking accounts (and thus it is not a protocol-agnostic
     test since it requires Alpha) or about baking (which would make it possible to run
     on previous protocols, if not for a problem that was introduced in
     Client.bake_for which causes the default key to be a baking account key). *)
  Baking.register Alpha ;
  (* Tests that are protocol-independent.
     They do not take a protocol as a parameter and thus need to be registered only once. *)
  Mockup.register_protocol_independent () ;
  Bootstrap.register_protocol_independent () ;
  Cli_tezos.register_protocol_independent () ;
  (* Tests that are heavily protocol-dependent.
     Those modules define different tests for different protocols in their [register]. *)
  Encoding.register () ;
  RPC_test.register () ;
  (* Test.run () should be the last statement, don't register afterwards! *)
  Test.run ()
