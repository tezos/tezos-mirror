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
   Component:    Client
   Invocation:   dune exec tezt/tests/main.exe -- --file stresstest_command.ml
   Subject:      Test the [stresstest] client command
*)

(** Tests the [stresstest] client command with explicit values for
    optional arguments [transfers] and [tps]. *)
let test_stresstest_explicit =
  Protocol.register_test ~__FILE__ ~title:"stresstest explicit" ~tags:["client"]
  @@ fun protocol ->
  let* node =
    Node.init
      [Synchronisation_threshold 0; Connections 0; Disable_operations_precheck]
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
     Stresstest command uses counters to inject a lot of operations with limited
     number of bootstrap accounts. With precheck these operations are mostly
     rejected because we don't apply the effect of operations in the
     prevalidation context in mempool mode anymore. So, only the operation with
     the correct counter is considered as Applied (without incrementing the
     counter in the context). Once the issue is fixed, the
     [Disable_operations_precheck] flag above can be removed. *)
  let* client = Client.init ~endpoint:Client.(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  Client.stresstest ~transfers:30 ~tps:25 client

(** Waits for [n] injection request events. *)
let wait_for_n_injections n node =
  let seen = ref 0 in
  let filter json =
    match JSON.(json |-> "view" |-> "request" |> as_string_opt) with
    | Some s when s = "inject" ->
        incr seen ;
        if !seen >= n then Some () else None
    | Some _ | None -> None
  in
  Node.wait_for node "request_completed_notice.v0" filter

(** Tests the [stresstest] client command without providing optional
    arguments [transfers] and [tps]. This means the command won't
    stop (it only stops after [transfers] operations when the argument
    is given), so instead we use {!wait_for_n_injections} to finish
    the test. *)
let test_stresstest_implicit =
  Protocol.register_test ~__FILE__ ~title:"stresstest implicit" ~tags:["client"]
  @@ fun protocol ->
  let* node =
    Node.init
      [Synchronisation_threshold 0; Connections 0; Disable_operations_precheck]
  in
  (* FIXME: https://gitlab.com/tezos/tezos/-/issues/2085
     Stresstest command uses counters to inject a lot of operations with limited
     number of bootstrap accounts. With precheck these operations are mostly
     rejected because we don't apply the effect of operations in the
     prevalidation context in mempool mode anymore. So, only the operation with
     the correct counter is considered as Applied (without incrementing the
     counter in the context). Once the issue is fixed, the
     [Disable_operations_precheck] flag above can be removed. *)
  let* client = Client.init ~endpoint:Client.(Node node) () in
  let* () = Client.activate_protocol ~protocol client in
  let* _ = Node.wait_for_level node 1 in
  let waiter = wait_for_n_injections 50 node in
  let _ = Client.stresstest client in
  waiter

(** Tests the various possible formats to provide sources to the
    [stresstest] command: alias, public key hash, or explicit key. *)
let test_stresstest_sources_format =
  Protocol.register_test
    ~__FILE__
    ~title:"stresstest sources format"
    ~tags:["client"]
  @@ fun protocol ->
  let transfers = 30 in
  let* (node, client) =
    Client.init_with_protocol
      ~nodes_args:
        [
          Synchronisation_threshold 0; Connections 0; Disable_operations_precheck;
        ]
      `Client
      ~protocol
      ()
  in
  let waiter = wait_for_n_injections transfers node in
  let* () =
    Constant.(
      Client.stresstest
        ~source_aliases:[bootstrap1.alias; bootstrap2.alias]
        ~source_pkhs:[bootstrap3.public_key_hash]
        ~source_accounts:[bootstrap1; bootstrap4]
        ~transfers
        client)
  in
  let* () = waiter in
  let* mempool_ops = RPC.get_mempool_pending_operations client in
  let actual_sources =
    let open JSON in
    mempool_ops |-> "applied" |> as_list
    |> List.fold_left
         (fun sources op ->
           String_set.add
             (op |-> "contents" |=> 0 |-> "source" |> as_string)
             sources)
         String_set.empty
  in
  let expected_sources =
    List.map
      (fun bootstrap -> bootstrap.Account.public_key_hash)
      Constant.[bootstrap1; bootstrap2; bootstrap3; bootstrap4]
    |> String_set.of_list
  in
  (if not (String_set.equal actual_sources expected_sources) then
   let pp_sources fmt sources =
     Format.(pp_print_seq pp_print_string fmt (String_set.to_seq sources))
   in
   let msg =
     Format.asprintf
       "Set of sources is %a, expected %a."
       pp_sources
       actual_sources
       pp_sources
       expected_sources
   in
   Test.fail "%s" msg) ;
  unit

let register ~protocols =
  test_stresstest_explicit ~protocols ;
  test_stresstest_implicit ~protocols ;
  test_stresstest_sources_format ~protocols
