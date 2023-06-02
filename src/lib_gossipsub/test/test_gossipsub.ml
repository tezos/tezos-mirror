(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_gossipsub
open Gossipsub_intf
module Milliseconds = Test_gossipsub_shared.Milliseconds

let parameters = {peer_filter = (fun _peer _action -> true)}

(* This is to use a seed with Tezt. *)
let rng =
  let seed =
    match
      Tezt_core.Cli.get
        ~default:None
        (fun x ->
          try int_of_string x |> Option.some |> Option.some
          with _ -> Option.none)
        "seed"
    with
    | None ->
        Random.self_init () ;
        Random.bits ()
    | Some seed -> seed
  in
  Random.State.make [|seed|]

let () =
  let open Default_limits in
  let default_limits = default_limits () in
  Test_unit.register rng default_limits parameters ;
  Test_integration_worker.register rng default_limits parameters ;
  Test_pbt.register rng default_limits parameters ;
  Test_message_cache.register () ;
  Tezt.Test.run ()
