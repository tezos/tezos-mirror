(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
   Component:    Transactional rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup.ml
   Subject:      .
*)

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

module Rollup = Rollup.Tx_rollup

let parameter_file protocol =
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, None))
    [(["tx_rollup_enable"], Some "true")]

(* This module only registers regressions tests. Those regressions
   tests should be used to ensure there is no regressions with the
   various RPCs exported by the tx_rollups. *)
module Regressions = struct
  type t = {node : Node.t; client : Client.t; rollup : string}

  let init_with_tx_rollup ~protocol =
    let* parameter_file = parameter_file protocol in
    let* (node, client) =
      Client.init_with_protocol ~parameter_file `Client ~protocol ()
    in
    (* We originate a dumb rollup to be able to generate a paths for
       tx_rollups related RPCs. *)
    let* rollup =
      Client.originate_tx_rollup ~src:Constant.bootstrap1.public_key_hash client
    in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node 2 in
    return {node; client; rollup}

  let rpc_state ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_state"
      ~title:"RPC (tx_rollups, regression) - state"
      ~tags:["tx_rollup"; "rpc"]
      ~protocols
    @@ fun protocol ->
    let* {node = _; client; rollup} = init_with_tx_rollup ~protocol in
    let* _state = Rollup.get_state ~hooks ~rollup client in
    return ()

  let submit_batch ~batch {rollup; client; node} =
    let* () =
      Client.submit_tx_rollup_batch
        ~hooks
        ~content:batch
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_inbox ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_inbox"
      ~title:"RPC (tx_rollups, regression) - inbox"
      ~tags:["tx_rollup"; "rpc"; "inbox"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node = _} as state) =
      init_with_tx_rollup ~protocol
    in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let* _state = Rollup.get_inbox ~hooks ~rollup client in
    unit

  let submit_commitment ~level ~roots ~predecessor {rollup; client; node} =
    let* () =
      Client.submit_tx_rollup_commitment
        ~hooks
        ~level
        ~roots
        ~predecessor
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_commitment ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_commitment"
      ~title:"RPC (tx_rollups, regression) - commitment"
      ~tags:["tx_rollup"; "rpc"; "commitment"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node} as state) = init_with_tx_rollup ~protocol in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let batch_level = Node.get_level node in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       we introduce two bakes to ensure the block is finalised. This
       should be removed once we do not rely on Tenderbake anymore. *)
    let* () = Client.bake_for client in
    let* () = Client.bake_for client in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       At the same time we remove the dependency to Tenderbake for
       commitment, we will ensure the root is indeed the root of the
       previous inbox. I don't know yet how we will be able to do that
       yes, something is missing. *)
    let* () =
      submit_commitment
        ~level:batch_level
        ~roots:["root"]
        ~predecessor:None
        state
    in
    let offset = Node.get_level node - batch_level in
    let* _state =
      Rollup.get_commitments ~hooks ~block:"head" ~offset ~rollup client
    in
    unit

  let register ~protocols =
    rpc_state ~protocols ;
    rpc_inbox ~protocols ;
    rpc_commitment ~protocols
end

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

type state = {burn_per_byte : int}

type inbox = {cumulated_size : int; contents : string list}

let test_submit_batch ~protocols =
  Protocol.register_test
    ~__FILE__
    ~title:"Simple use case"
    ~tags:["tx_rollup"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* rollup =
    Client.originate_tx_rollup ~src:Constant.bootstrap1.public_key_hash client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  (* We check the rollup exists by trying to fetch its state. Since it
     is a regression test, we can detect changes to this default
     state. *)
  let* _state = Rollup.get_state ~hooks ~rollup client in

  (* Submit a batch *)
  let batch = "tezos" in

  let* () =
    Client.submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in

  let* _ = Node.wait_for_level node 3 in

  (* Check the inbox has been created *)
  let* inbox = Rollup.get_inbox ~hooks ~rollup client in
  Check.(
    (( = )
       (String.length batch)
       inbox.cumulated_size
       ~error_msg:
         "Cumulated size of inboxes should be equal of the length of the \
          submited message")
      int) ;

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
      ~rollup:invalid_address
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
  let* rollup =
    Client.originate_tx_rollup ~src:Constant.bootstrap1.public_key_hash client
  in

  let* () = Client.bake_for client in

  let batch = "tezos" in

  (* Finally, we submit a batch to the tx_rollup from an originated contract *)
  let* () =
    Client.spawn_submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:originated_contract
      client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex "Only implicit accounts can submit transaction rollup batches")
  in

  unit

let register ~protocols =
  Regressions.register ~protocols ;
  test_submit_batch ~protocols ;
  test_invalid_rollup_address ~protocols ;
  test_submit_from_originated_source ~protocols
