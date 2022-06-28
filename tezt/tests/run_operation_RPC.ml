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
   Component:    Protocol's plugin
   Invocation:   dune exec tezt/tests/main.exe -- --file "run_operation_RPC.ml"
   Subject:      Test the [run_operation] RPC:
                 [POST /chains/<chain>/blocks/<block>/helpers/scripts/run_operation].
                 These tests focus on the semantics of the RPC, ie. whether
                 the operation is successfully run, rather than on the exact
                 form of the output, which is why they are in their own file
                 instead of [RPC_test.ml].
*)

(** Tags shared by all tests in this file. *)
let run_operation_tags = ["rpc"; "run_operation"]

(** This test checks that the [run_operation] RPC used to allow
    batches of manager operations containing different sources in
    protocol versions before Kathmandu (014), but rejects them from
    Kathmandu on. *)
let test_batch_inconsistent_sources protocols =
  let register_inconsistent_sources ~supports ~title
      call_run_operation_and_check_response =
    Protocol.register_test
      ~__FILE__
      ~supports
      ~title
      ~tags:(run_operation_tags @ ["manager"; "batch"; "inconsistent_sources"])
      (fun protocol ->
        Log.info "Initialize a node and a client." ;
        let* node, client =
          Client.init_with_protocol
            ~nodes_args:[Synchronisation_threshold 0]
            ~protocol
            `Client
            ()
        in
        let source1 = Constant.bootstrap1
        and source2 = Constant.bootstrap2
        and dest = Constant.bootstrap3 in
        Log.info
          "Increment [%s]'s counter so that the batch we craft below has \
           consistent counters. To do this, we inject a transaction from this \
           account and bake a block."
          source2.alias ;
        let* () =
          Client.transfer
            ~amount:Tez.one
            ~giver:source2.alias
            ~receiver:dest.alias
            client
        in
        let* () = Client.bake_for_and_wait ~protocol ~node client in
        Log.info
          "Craft a batch containing an operation from [%s] and an operation \
           from [%s]."
          source1.alias
          source2.alias ;
        let manager_op1 =
          Operation.Manager.(make ~source:source1 (transfer ~dest ()))
        in
        let manager_op2 =
          Operation.Manager.(make ~source:source2 (transfer ~dest ()))
        in
        let* batch =
          Operation.Manager.operation [manager_op1; manager_op2] client
        in
        let* batch_json = Operation.make_run_operation_input batch client in
        Log.info
          "Crafted batch: %s"
          (Ezjsonm.value_to_string ~minify:false batch_json) ;
        call_run_operation_and_check_response node batch_json)
  in
  register_inconsistent_sources
    ~supports:Protocol.(Until_protocol (number Jakarta))
    ~title:"Run_operation inconsistent sources ok"
    (fun node batch_json ->
      Log.info
        "Call the [run_operation] RPC on this batch and check that it succeeds." ;
      let* _run_operation_output =
        RPC.(
          call node (post_chain_block_helpers_scripts_run_operation batch_json))
      in
      unit)
    protocols ;
  register_inconsistent_sources
    ~supports:(Protocol.From_protocol 014)
    ~title:"Run_operation inconsistent sources ko"
    (fun node batch_json ->
      Log.info
        "Call the [run_operation] RPC on this batch and check that it fails \
         with code [500 Internal Server Error] and protocol error \
         [inconsistent_sources]." ;
      let* response =
        RPC.call_json
          node
          (RPC.post_chain_block_helpers_scripts_run_operation batch_json)
      in
      Log.info
        "RPC response:\n  code: %d\n  body: %s"
        response.code
        (JSON.encode response.body) ;
      Check.(
        (response.code = 500)
          int
          ~error_msg:
            "The RPC call was expected to fail with code 500 Internal Server \
             Error, but it returned code %L.") ;
      let id = JSON.(response.body |=> 0 |-> "id" |> as_string) in
      let proto_error =
        try List.(hd (rev (String.split_on_char '.' id)))
        with exn ->
          Test.fail
            "Failed to parse the following RPC response body: %s. The \
             following exception was raised: %s"
            (JSON.encode response.body)
            (Printexc.to_string exn)
      in
      Check.(
        (proto_error = "inconsistent_sources")
          string
          ~error_msg:"Expected the [%R] protocol error, but got [%L].") ;
      unit)
    protocols

let register ~protocols = test_batch_inconsistent_sources protocols
