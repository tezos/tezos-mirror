(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client / Chain ids
   Invocation:   dune exec tezt/tests/main.exe -- --file client_without_node.ml
   Subject:      Tests the [compute chain id] commands of octez-client
*)

let test_chain_id () =
  Test.register
    ~__FILE__
    ~title:"Chain id"
    ~tags:["chain"; "id"]
    ~uses_node:false
  @@ fun () ->
  Log.info "Chain id block hash" ;
  let* client = Client.init () in
  let block_hash = "BKyFui5WPY1n3e9aKF3qd2kGBKBtHu3rtm5miYFnUagJC1BdHTF" in
  let* chain_id = Client.compute_chain_id_from_block_hash client block_hash in
  Check.(
    (chain_id = "NetXuwrXPL4VeX5")
      string
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  Log.info "Chain id seed" ;
  let seed = "choucroute" in
  let* chain_id = Client.compute_chain_id_from_seed client seed in
  Check.(
    (chain_id = "NetXLGmPi3c5DXf")
      string
      ~__LOC__
      ~error_msg:"Expected %R, got %L") ;
  unit

let register_protocol_independent () = test_chain_id ()
