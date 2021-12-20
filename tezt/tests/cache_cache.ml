(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Shell / Cache
   Invocation:   dune exec tezt/tests/main.exe -- --file cache_cache.ml
   Subject:      Check that annotations in contracts do not break cache consistency.
*)

let contract =
  {|
parameter (or (unit %renew) (unit %keep));
storage (big_map (nat :a) string);
code {
       UNPAIR;
       IF_LEFT
         {
           DROP 2;
           EMPTY_BIG_MAP nat string;
         }
         {
           DROP;
         };
       NIL operation;
       PAIR
     }
|}

let register =
  Protocol.register_test
    ~__FILE__
    ~title:"cache cache"
    ~tags:["cache"; "node"; "baker"]
  @@ fun protocol ->
  let* (node, client) = Client.init_with_protocol `Client ~protocol () in
  let data_dir = Node.data_dir node in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* contract_hash =
    Client.originate_contract
      ~init:"{}"
      ~alias:"supercontrat"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:contract
      ~burn_cap:Tez.one
      client
  in
  let* () = wait_injection in
  (* We use [context_path] to ensure the baker will not use the
     preapply RPC. Indeed, this test was introduced because of a bug
     that appends when the baker does not use the preapply RPC. *)
  let* () = Client.bake_for ~context_path:(data_dir // "context") client in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* (`OpHash _todo) =
    Operation.inject_contract_call
      ~amount:0
      ~source:Constant.bootstrap1
      ~dest:contract_hash
      ~entrypoint:"renew"
      ~arg:(`Michelson "Unit")
      client
  in
  let* () = wait_injection in
  let* () = Client.bake_for ~context_path:(data_dir // "context") client in
  let wait_injection = Node.wait_for_request ~request:`Inject node in
  let* (`OpHash _op_hash) =
    Operation.inject_contract_call
      ~amount:0
      ~source:Constant.bootstrap1
      ~dest:contract_hash
      ~entrypoint:"keep"
      ~arg:(`Michelson "Unit")
      client
  in
  let* () = wait_injection in
  let* () = Client.bake_for ~context_path:(data_dir // "context") client in
  let* _ = Node.wait_for_level node 4 in
  unit

let register ~protocols = register ~protocols
