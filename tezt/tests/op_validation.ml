(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Validation components
   Invocation:   dune exec tezt/tests/main.exe -- --file "op_validation.ml"
   Subject:      Checks the validation of operations
*)

let check_run_operation_illformed_batch ~supports check_answer =
  Protocol.register_test
    ~__FILE__
    ~supports
    ~title:"Run_operation ill-formed batch"
    ~tags:["rpc"; "run_operation"; "batch"]
  @@ fun protocol ->
  Log.info "Initialize a node and a client." ;
  let* node, client =
    Client.init_with_protocol
      ~nodes_args:[Synchronisation_threshold 0]
      ~protocol
      `Client
      ()
  in

  Log.info
    "Do a transfer from %s and bake to increment its counter."
    Constant.bootstrap2.alias ;
  let* _ =
    Client.transfer
      ~amount:Tez.one
      ~giver:Constant.bootstrap2.alias
      ~receiver:Constant.bootstrap3.alias
      client
  in
  let* _ = Client.bake_for_and_wait ~protocol ~node client in

  Log.info "Create a first operation." ;
  let source1 = Constant.bootstrap1 in
  let dest = Constant.bootstrap3 in
  let op1 = Operation.Manager.(make ~source:source1 @@ transfer ~dest ()) in
  let* op1_json = Operation.Manager.json client op1 in

  Log.info
    "Create a second operation with a different source and an incremented \
     counter." ;
  let source2 = Constant.bootstrap2 in
  let* counter = Operation.get_next_counter ~source:source2 client in
  let op2 =
    Operation.Manager.(make ~source:source2 ~counter @@ transfer ~dest ())
  in
  let* op2_json = Operation.Manager.json client op2 in

  Log.info "Craft a batch in JSON that contains both operations." ;
  let* branch = Operation.get_injection_branch client in
  let signature = Tezos_crypto.Signature.zero in
  let* chain_id = RPC.Client.call client @@ RPC.get_chain_chain_id () in
  let batch =
    Format.asprintf
      {|{ "operation":
            {"branch": "%s",
             "contents": [%s,%s],
             "signature": "%a" },
            "chain_id": %s }|}
      branch
      (Ezjsonm.value_to_string op1_json)
      (Ezjsonm.value_to_string op2_json)
      Tezos_crypto.Signature.pp
      signature
      (JSON.encode_u (`String chain_id))
  in

  Log.info "Call the [run_operation] RPC with this JSON batch." ;
  let*? p =
    RPC.Client.spawn client
    @@ RPC.post_run_operation (Ezjsonm.from_string batch)
  in
  check_answer p

(** This test checks that the [run_operation] RPC used to allow
    batches of manager operations containing different sources in
    protocol versions before 14, but rejects them from 14 on. *)
let check_run_operation_illformed_batch ~protocols =
  check_run_operation_illformed_batch
    ~supports:(Protocol.Until_protocol 13)
    (Process.check ~expect_failure:false)
    protocols ;
  check_run_operation_illformed_batch
    ~supports:(Protocol.From_protocol 14)
    (Process.check ~expect_failure:true)
    protocols

let register ~protocols = check_run_operation_illformed_batch ~protocols
