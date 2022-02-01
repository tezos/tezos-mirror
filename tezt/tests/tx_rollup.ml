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
   Component:    Mempool
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup.ml
   Subject:      .
*)

(*                               utils                                       *)

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

type state = {fees_per_byte : int}

type inbox = {cumulated_size : int; contents : string list}

let get_state ?hooks tx_rollup client =
  (* The state is currently empty, but the RPC can fail if [tx_rollup]
     does not exist. *)
  let* json = RPC.Tx_rollup.get_state ?hooks ~tx_rollup client in
  let fees_per_byte = JSON.(json |-> "fees_per_byte" |> as_int) in
  return {fees_per_byte}

let get_inbox ?hooks tx_rollup client =
  let* json = RPC.Tx_rollup.get_inbox ?hooks ~tx_rollup client in
  let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
  let contents = JSON.(json |-> "contents" |> as_list |> List.map as_string) in
  return {cumulated_size; contents}

let parameter_file protocol =
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, None))
    [(["tx_rollup_enable"], Some "true")]

(*                               test                                        *)

let test_submit_batch ~protocols =
  let open Tezt_tezos in
  Protocol.register_regression_test
    ~__FILE__
    ~output_file:"tx_rollup_simple_use_case"
    ~title:"Simple use case"
    ~tags:["tx_rollup"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* tx_rollup =
    Client.originate_tx_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~storage_limit:60_000
      ~src:Constant.bootstrap1.public_key_hash
      client
  in

  Regression.capture tx_rollup ;

  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in

  (* We check the rollup exists by trying to fetch its state. Since it
     is a regression test, we can detect changes to this default
     state. *)
  let* _state = get_state ~hooks tx_rollup client in

  (* Submit a batch *)
  let batch = "tezos" in

  let* () =
    Client.submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~tx_rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in

  let* _ = Node.wait_for_level node 3 in

  (* Check the inbox has been created *)
  let* inbox = get_inbox ~hooks tx_rollup client in

  assert (String.length batch = inbox.cumulated_size) ;

  unit

let test_invalid_rollup_address ~protocols =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit to an invalid rollup address should fail"
    ~tags:["tx_rollup"; "cli"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (_node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let invalid_address = "this is an invalid tx rollup address" in
  let* () =
    Client.spawn_submit_tx_rollup_batch
      ~hooks
      ~content:""
      ~tx_rollup:invalid_address
      ~src:Constant.bootstrap1.public_key_hash
      client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex
              ("Parameter '" ^ invalid_address
             ^ "' is an invalid tx rollup address"))
  in

  unit

let test_submit_from_originated_source ~protocols =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit from an originated contract should fail"
    ~tags:["tx_rollup"; "cli"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  (* We begin by originating a contract *)
  let* originated_contract =
    Client.originate_contract
      ~alias:"originated_contract_simple"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/str_id.tz"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
  in

  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in

  (* We originate a tx_rollup using an implicit account *)
  let* tx_rollup =
    Client.originate_tx_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~storage_limit:60_000
      ~src:Constant.bootstrap1.public_key_hash
      client
  in

  let* () = Client.bake_for client in

  let batch = "tezos" in

  (* Finally, we submit a batch to the tx_rollup from an originated contract *)
  let* () =
    Client.spawn_submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~tx_rollup
      ~src:originated_contract
      client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex "Only implicit accounts can submit transaction rollup batches")
  in

  unit

let register ~protocols =
  test_submit_batch ~protocols ;
  test_invalid_rollup_address ~protocols ;
  test_submit_from_originated_source ~protocols
