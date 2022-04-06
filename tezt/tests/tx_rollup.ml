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
module Parameters = Rollup.Parameters

type t = {node : Node.t; client : Client.t; rollup : string}

let assert_some res = match res with Some r -> r | None -> assert false

let init_with_tx_rollup ?additional_bootstrap_account_count
    ?(parameters = Parameters.default) ~protocol () =
  let* parameter_file = Parameters.parameter_file ~parameters protocol in
  let* (node, client) =
    Client.init_with_protocol
      ?additional_bootstrap_account_count
      ~parameter_file
      `Client
      ~protocol
      ()
  in
  (* We originate a dumb rollup to be able to generate a paths for
     tx_rollups related RPCs. *)
  let*! rollup =
    Client.Tx_rollup.originate
      ~hooks
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  return {node; client; rollup}

let submit_batch ~batch:(`Batch content) ?(batches = []) {rollup; client; node}
    =
  let*! () =
    Client.Tx_rollup.submit_batch
      ~hooks
      ~content
      ~rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () =
    Lwt_list.iter_s
      (fun (`Batch content, src) ->
        let*! () =
          Client.Tx_rollup.submit_batch ~hooks ~content ~rollup ~src client
        in
        return ())
      batches
  in
  let current_level = Node.get_level node in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node (current_level + 1) in
  return ()

let submit_commitment ?(src = Constant.bootstrap1.public_key_hash) ~level ~roots
    ~inbox_content ~predecessor {rollup; client; node} =
  let* inbox_merkle_root =
    match inbox_content with
    | `Root inbox_merkle_root -> inbox_merkle_root
    | `Content messages ->
        let* inbox = Rollup.compute_inbox_from_messages messages client in
        return inbox.merkle_root
  in
  let*! () =
    Client.Tx_rollup.submit_commitment
      ~hooks
      ~level
      ~roots
      ~inbox_merkle_root
      ~predecessor
      ~rollup
      ~src
      client
  in
  let current_level = Node.get_level node in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node (current_level + 1) in
  return ()

let submit_return_bond ?(src = Constant.bootstrap1.public_key_hash)
    {rollup; client; node} =
  let*! () = Client.Tx_rollup.submit_return_bond ~hooks ~rollup ~src client in
  let current_level = Node.get_level node in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node (current_level + 1) in
  unit

let submit_finalize_commitment ?(src = Constant.bootstrap1.public_key_hash)
    {rollup; client; node = _} =
  Client.Tx_rollup.submit_finalize_commitment ~hooks ~rollup ~src client

let submit_remove_commitment ?(src = Constant.bootstrap1.public_key_hash)
    {rollup; client; node = _} =
  Client.Tx_rollup.submit_remove_commitment ~hooks ~rollup ~src client

let submit_rejection ?(src = Constant.bootstrap1.public_key_hash) ~level
    ~message ~position ~proof {rollup; client; node = _} ~context_hash
    ~withdraw_list_hash =
  Client.Tx_rollup.submit_rejection
    ~hooks
    ~level
    ~message
    ~position
    ~proof
    ~rollup
    ~src
    ~context_hash
    ~withdraw_list_hash
    client

(* This module only registers regressions tests. Those regressions
   tests should be used to ensure there is no regressions with the
   various RPCs exported by the tx_rollups. *)
module Regressions = struct
  module RPC = struct
    let rpc_state =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_state")
        ~title:"RPC (tx_rollup, regression) - state"
        ~tags:["tx_rollup"; "rpc"]
      @@ fun protocol ->
      let* {node = _; client; rollup} = init_with_tx_rollup ~protocol () in
      let*! _state = Rollup.get_state ~hooks ~rollup client in
      return ()

    let rpc_inbox =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_inbox")
        ~title:"RPC (tx_rollups, regression) - inbox"
        ~tags:["tx_rollup"; "rpc"; "inbox"]
      @@ fun protocol ->
      let* ({rollup; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:0 client in
      Check.(inbox <> None)
        (Check.option Rollup.Check.inbox)
        ~error_msg:"Expected some inbox" ;
      unit

    let rpc_inbox_message_hash =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_inbox_message_hash")
        ~title:"RPC (tx_rollups, regression) - inbox message hash"
        ~tags:["tx_rollup"; "rpc"; "inbox"; "message"]
      @@ fun protocol ->
      let* (_node, client) = Client.init_with_protocol `Client ~protocol () in
      let message = Rollup.make_batch "blob" in
      let*! _hash = Rollup.message_hash ~hooks ~message client in
      unit

    let rpc_inbox_merkle_tree_hash =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_inbox_merkle_tree_hash")
        ~title:"RPC (tx_rollups, regression) - inbox merkle tree hash"
        ~tags:["tx_rollup"; "rpc"; "inbox"; "merkle_tree_hash"]
      @@ fun protocol ->
      let* (_node, client) = Client.init_with_protocol `Client ~protocol () in
      let messages = List.map Rollup.make_batch ["blob"; "gloubiboulga"] in
      let* message_hashes =
        Lwt_list.map_p
          (fun message ->
            let*! message_hash = Rollup.message_hash ~hooks ~message client in
            return message_hash)
          messages
      in
      let*! _hash =
        Rollup.inbox_merkle_tree_hash ~hooks ~message_hashes client
      in
      unit

    let rpc_inbox_merkle_tree_path =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_inbox_merkle_tree_path")
        ~title:"RPC (tx_rollups, regression) - inbox merkle tree path"
        ~tags:["tx_rollup"; "rpc"; "inbox"; "merkle_tree_path"]
      @@ fun protocol ->
      let* (_node, client) = Client.init_with_protocol `Client ~protocol () in
      let messages =
        List.map
          Rollup.make_batch
          ["Kouroukoukou"; "roukoukou"; "stach"; "stach"]
      in
      let* message_hashes =
        Lwt_list.map_p
          (fun message ->
            let*! message_hash = Rollup.message_hash ~hooks ~message client in
            return message_hash)
          messages
      in
      let*! _ =
        Rollup.inbox_merkle_tree_path ~hooks ~message_hashes ~position:3 client
      in
      let*! _ =
        Rollup.inbox_merkle_tree_path ~hooks ~message_hashes ~position:0 client
      in
      let*? process =
        Rollup.inbox_merkle_tree_path ~hooks ~message_hashes ~position:4 client
      in
      Process.check_error ~msg:(rex "Merkle_list_invalid_positio") process

    let rpc_commitment =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_commitment")
        ~title:"RPC (tx_rollups, regression) - commitment"
        ~tags:["tx_rollup"; "rpc"; "commitment"]
      @@ fun protocol ->
      let* ({rollup; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let* () = Client.bake_for client in
      let inbox_content = `Content [batch] in
      let* () =
        submit_commitment
          ~level:0
          ~roots:[Constant.tx_rollup_initial_message_result]
          ~inbox_content
          ~predecessor:None
          state
      in
      let*! commitment =
        Rollup.get_commitment ~hooks ~block:"head" ~level:0 ~rollup client
      in
      let hash =
        Option.map
          (fun (c : Rollup.submitted_commitment) -> c.commitment_hash)
          commitment
      in
      let*! state = Rollup.get_state ~hooks ~rollup client in
      Check.(state.Rollup.commitment_newest_hash = hash)
        (Check.option Check.string)
        ~error_msg:"Commitment hash mismatch: %L vs %R" ;
      unit

    let rpc_pending_bonded_commitment =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_rpc_pending_bonded_commitments")
        ~title:"RPC (tx_rollups, regression) - pending bonded commitments"
        ~tags:["tx_rollup"; "rpc"; "commitment"; "bond"]
      @@ fun protocol ->
      let* ({rollup; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let* () = Client.bake_for client in
      let inbox_content = `Content [batch] in
      let* () =
        submit_commitment
          ~level:0
          ~roots:[Constant.tx_rollup_initial_message_result]
          ~inbox_content
          ~predecessor:None
          state
      in
      let*! _commitment =
        Rollup.get_pending_bonded_commitments
          ~hooks
          ~block:"head"
          ~rollup
          ~pkh:Constant.bootstrap1.public_key_hash
          client
      in
      (* Use a key which has no commitment. *)
      let*! _commitment =
        Rollup.get_pending_bonded_commitments
          ~hooks
          ~block:"head"
          ~rollup
          ~pkh:Constant.bootstrap2.public_key_hash
          client
      in
      unit

    let batch_encoding =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_batch_encoding")
        ~title:"RPC (tx_rollups, regression) - batch encoding"
        ~tags:["tx_rollup"; "batch"; "encoding"]
      @@ fun protocol ->
      let* ({client; rollup = _; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* Batch with all possible characters. *)
      let batch = Rollup.make_batch (String.init 256 Char.chr) in
      let* () = submit_batch ~batch state in
      let* block = RPC.get_block ~block:"head" client in
      let op = JSON.(block |-> "operations" |=> 3 |=> 0 |-> "contents" |=> 0) in
      Check.(
        ((JSON.(op |-> "kind" |> as_string) = "tx_rollup_submit_batch")
           ~error_msg:"Unexpected operation. Got: %L. Expected: %R.")
          string) ;
      let batch_content = JSON.(op |-> "content") in
      let batch_content_str = JSON.encode batch_content in
      Regression.capture batch_content_str ;
      if not (JSON.is_string batch_content) then
        Test.fail
          ~__LOC__
          "Batch content in JSON should be a string: %s."
          batch_content_str ;
      unit

    let rpc_inbox_future =
      Protocol.register_test
        ~__FILE__
        ~title:"RPC (tx_rollups, regression) - inbox from the future"
        ~tags:["tx_rollup"; "rpc"; "inbox"]
      @@ fun protocol ->
      let* ({rollup; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:1 client in
      Check.(inbox = None)
        (Check.option Rollup.Check.inbox)
        ~error_msg:"Expected no inbox" ;
      unit
  end

  module Limits = struct
    (* The constant comes from the default parameters of the protocol. *)
    let batch_limit = 5_000

    let inbox_limit = 100_000

    let submit_empty_batch =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_limit_empty_batch")
        ~title:"Submit empty batch"
        ~tags:["tx_rollup"; "batch"; "client"]
      @@ fun protocol ->
      let* state = init_with_tx_rollup ~protocol () in
      let batch = Rollup.make_batch "" in
      let* () = submit_batch ~batch state in
      unit

    let submit_maximum_size_batch =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_limit_maximum_size_batch")
        ~title:"Submit maximum size batch"
        ~tags:["tx_rollup"; "batch"; "client"]
      @@ fun protocol ->
      let* state = init_with_tx_rollup ~protocol () in
      let batch = Rollup.make_batch (String.make batch_limit 'b') in
      let* () = submit_batch ~batch state in
      let (`Batch content) =
        Rollup.make_batch (String.make (batch_limit + 1) 'c')
      in
      let*? process =
        Client.Tx_rollup.submit_batch
          ~hooks
          ~content
          ~rollup:state.rollup
          ~src:Constant.bootstrap1.public_key_hash
          state.client
      in
      Process.check_error
        ~msg:
          (rex
             "A message submitted to a transaction rollup inbox exceeds limit")
        process

    let inbox_maximum_size =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_limit_maximum_size_inbox")
        ~title:"Submit maximum size inbox"
        ~tags:["tx_rollup"; "inbox"; "client"]
      @@ fun protocol ->
      (* The test assumes inbox_limit % batch_limit = 0 *)
      let max_batch_number_per_inbox = inbox_limit / batch_limit in
      let additional_bootstrap_account_count = max_batch_number_per_inbox - 5 in
      let* {client; rollup; node} =
        init_with_tx_rollup ~additional_bootstrap_account_count ~protocol ()
      in
      let (`Batch content) = Rollup.make_batch (String.make batch_limit 'a') in
      let* () =
        fold max_batch_number_per_inbox () (fun i () ->
            let src = Account.Bootstrap.alias (i + 1) in
            let*! () =
              Client.Tx_rollup.submit_batch ~hooks ~content ~rollup ~src client
            in
            unit)
      in
      let current_level = Node.get_level node in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node (current_level + 1) in
      let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:0 client in
      let inbox = assert_some inbox in
      Check.(inbox.cumulated_size = inbox_limit)
        Check.int
        ~error_msg:"Unexpected inbox size. Expected %R. Got %L" ;
      unit
  end

  module Fail = struct
    let client_submit_batch_invalid_rollup_address =
      let open Tezt_tezos in
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ ->
          "tx_rollup_client_submit_batch_invalid_rollup_address")
        ~title:"Submit a batch to an invalid rollup address should fail"
        ~tags:["tx_rollup"; "client"; "fail"; "batch"]
      @@ fun protocol ->
      let* parameter_file = Parameters.parameter_file protocol in
      let* (_node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let invalid_address = "this is an invalid tx rollup address" in
      let*? process =
        Client.Tx_rollup.submit_batch
          ~hooks
          ~content:(Hex.of_string "")
          ~rollup:invalid_address
          ~src:Constant.bootstrap1.public_key_hash
          client
      in

      let* () =
        Process.check_error
          ~exit_code:1
          ~msg:
            (rex
               ("Parameter '" ^ invalid_address
              ^ "' is an invalid tx rollup address"))
          process
      in
      unit

    let client_submit_finalize_commitment_no_batch =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_finalize_commitment_no_batch")
        ~title:"Submit a finalize commitment operation without batch"
        ~tags:["tx_rollup"; "client"; "fail"; "finalize"]
      @@ fun protocol ->
      let* ({rollup = _; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      let* () = Client.bake_for client in
      let*? process = submit_finalize_commitment state in
      Process.check_error
        ~exit_code:1
        ~msg:(rex "proto.alpha.tx_rollup_no_commitment_to_finalize")
        process

    let client_submit_finalize_commitment_no_commitment =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_finalize_commitment_no_commitment")
        ~title:"Submit a finalize commitment operation without commitment"
        ~tags:["tx_rollup"; "client"; "fail"; "finalize"]
      @@ fun protocol ->
      let* ({rollup = _; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let* () = Client.bake_for client in
      let*? process = submit_finalize_commitment state in
      Process.check_error
        ~exit_code:1
        ~msg:(rex "proto.alpha.tx_rollup_no_commitment_to_finalize")
        process

    let client_submit_finalize_commitment_future =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_finalize_commitment_future")
        ~title:
          "Submit a finalize commitment operation for a commitment in the \
           future"
        ~tags:["tx_rollup"; "client"; "fail"; "finalize"]
      @@ fun protocol ->
      let* ({rollup = _; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let* () = Client.bake_for client in
      let*? process = submit_finalize_commitment state in
      Process.check_error
        ~exit_code:1
        ~msg:(rex "proto.alpha.tx_rollup_no_commitment_to_finalize")
        process

    let client_submit_finalize_too_recent_commitment =
      Protocol.register_regression_test
        ~__FILE__
        ~output_file:(fun _ -> "tx_rollup_finalize_too_recent_commitment")
        ~title:"Try to finalize a too recent commitment"
        ~tags:["tx_rollup"; "client"; "fail"; "finalize"]
      @@ fun protocol ->
      let* ({rollup = _; client; node = _} as state) =
        init_with_tx_rollup ~protocol ()
      in
      (* The content of the batch does not matter for the regression test. *)
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in
      let* () = Client.bake_for client in
      let inbox_content = `Content [batch] in
      let* () =
        submit_commitment
          ~level:0
          ~roots:[Constant.tx_rollup_initial_message_result]
          ~inbox_content
          ~predecessor:None
          state
      in
      let* () = Client.bake_for client in
      let*? process = submit_finalize_commitment state in
      Process.check_error
        ~exit_code:1
        ~msg:(rex "proto.alpha.tx_rollup_no_commitment_to_finalize")
        process
  end

  let register protocols =
    RPC.rpc_state protocols ;
    RPC.rpc_inbox protocols ;
    RPC.rpc_inbox_message_hash protocols ;
    RPC.rpc_inbox_merkle_tree_hash protocols ;
    RPC.rpc_inbox_merkle_tree_path protocols ;
    RPC.rpc_commitment protocols ;
    RPC.rpc_pending_bonded_commitment protocols ;
    RPC.batch_encoding protocols ;
    RPC.rpc_inbox_future protocols ;
    Limits.submit_empty_batch protocols ;
    Limits.submit_maximum_size_batch protocols ;
    Limits.inbox_maximum_size protocols ;
    Fail.client_submit_batch_invalid_rollup_address protocols ;
    Fail.client_submit_finalize_commitment_no_batch protocols ;
    Fail.client_submit_finalize_commitment_no_commitment protocols ;
    Fail.client_submit_finalize_commitment_future protocols ;
    Fail.client_submit_finalize_too_recent_commitment protocols
end

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

let submit_three_batches_and_check_size ~rollup ~tezos_level ~tx_level node
    client batches =
  let messages =
    List.map (fun (batch_message, _, _) -> batch_message) batches
  in
  let* () =
    Lwt_list.iter_s
      (fun (`Batch content, src, _) ->
        let*! () =
          Client.Tx_rollup.submit_batch ~hooks ~content ~rollup ~src client
        in
        unit)
      batches
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node tezos_level in
  (* Check the inbox has been created, with the expected cumulated size. *)
  let* expected_inbox =
    Rollup.compute_inbox_from_messages ~hooks messages client
  in
  let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:tx_level client in
  Check.(
    ((inbox = Some expected_inbox)
       ~error_msg:"Unexpected inbox. Got: %L. Expected: %R.")
      (Check.option Rollup.Check.inbox)) ;
  return ()

let test_submit_batches_in_several_blocks =
  Protocol.register_test
    ~__FILE__
    ~title:"Submit batches in several blocks"
    ~tags:["tx_rollup"]
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let*! rollup =
    Client.Tx_rollup.originate
      ~hooks
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  (* We check the rollup exists by trying to fetch its state. Since it
     is a regression test, we can detect changes to this default
     state. *)
  let*! state = Rollup.get_state ~hooks ~rollup client in
  let expected_state =
    Rollup.
      {
        finalized_commitments = Empty 0;
        unfinalized_commitments = Empty 0;
        uncommitted_inboxes = Empty 0;
        tezos_head_level = None;
        commitment_newest_hash = None;
        burn_per_byte = 0;
        inbox_ema = 0;
      }
  in
  Check.(state = expected_state)
    Rollup.Check.state
    ~error_msg:"Unexpected state. Got: %L. Expected: %R." ;
  let (`Batch content) = Rollup.make_batch "tezos" in
  let*! () =
    Client.Tx_rollup.submit_batch
      ~hooks
      ~content
      ~rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let batch1 = Rollup.make_batch "tezos" in
  let batch2 = Rollup.make_batch "tx_rollup" in
  let batch3 = Rollup.make_batch "layer-2" in
  let*! (`Hash batch1_hash) = Rollup.message_hash ~message:batch1 client in
  let*! (`Hash batch2_hash) = Rollup.message_hash ~message:batch2 client in
  let*! (`Hash batch3_hash) = Rollup.message_hash ~message:batch3 client in
  let submission =
    [
      (batch2, Constant.bootstrap2.public_key_hash, batch2_hash);
      (batch3, Constant.bootstrap3.public_key_hash, batch3_hash);
      (batch1, Constant.bootstrap1.public_key_hash, batch1_hash);
    ]
  in
  (* Let’s try once and see if everything goes as expected *)
  let* () =
    submit_three_batches_and_check_size
      ~rollup
      node
      client
      submission
      ~tezos_level:3
      ~tx_level:0
  in
  (* Let’s try to see if we can submit three more batches in the next level *)
  let* () =
    submit_three_batches_and_check_size
      ~rollup
      node
      client
      submission
      ~tezos_level:4
      ~tx_level:1
  in
  let* () =
    submit_three_batches_and_check_size
      ~rollup
      node
      client
      submission
      ~tezos_level:5
      ~tx_level:2
  in
  let*! _state = Rollup.get_state ~hooks ~rollup client in
  unit

let test_submit_from_originated_source =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit from an originated contract should fail"
    ~tags:["tx_rollup"; "client"]
  @@ fun protocol ->
  let* parameter_file = Parameters.parameter_file protocol in
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
  let*! rollup =
    Client.Tx_rollup.originate ~src:Constant.bootstrap1.public_key_hash client
  in
  let* () = Client.bake_for client in
  let (`Batch content) = Rollup.Tx_rollup.make_batch "tezos" in
  (* Finally, we submit a batch to the tx_rollup from an originated contract *)
  let*? process =
    Client.Tx_rollup.submit_batch
      ~hooks
      ~content
      ~rollup
      ~src:originated_contract
      client
  in
  let* () =
    Process.check_error
      ~exit_code:1
      ~msg:(rex "Only implicit accounts can submit transaction rollup batches")
      process
  in
  unit

let test_rollup_with_two_commitments =
  Protocol.register_test
    ~__FILE__
    ~title:"Submit 2 batches, commit, finalize and remove the commitments"
    ~tags:["tx_rollup"; "commitment"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:[Constant.tx_rollup_initial_message_result]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let*! commitment = Rollup.get_commitment ~hooks ~rollup ~level:0 client in
  let first_commitment_level = (assert_some commitment).commitment.level in
  Check.(first_commitment_level = 0)
    Check.int
    ~error_msg:"First commitment level must be 0" ;
  (* There is only one commitment, so trying to get level 1 will fail *)
  let*! commitment = Rollup.get_commitment ~hooks ~rollup ~level:1 client in
  Check.(commitment = None)
    (Check.option Rollup.Check.commitment)
    ~error_msg:"Expected no commitment" ;
  let*! () = submit_finalize_commitment state in
  (* A second submission just to ensure it can be included into a
     block even if it fails. *)
  let*! () =
    submit_finalize_commitment ~src:Constant.bootstrap2.public_key_hash state
  in
  let* _ = Client.bake_for client in
  let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:0 client in
  Check.(inbox = None)
    (Check.option Rollup.Check.inbox)
    ~error_msg:"Expected no inbox" ;
  let* json = RPC.get_operations client in
  let manager_operations = JSON.(json |=> 3 |> as_list) in
  Check.(List.length manager_operations = 2)
    Check.int
    ~error_msg:"Two operations manager expected in the last block" ;
  let first_op = List.nth manager_operations 0 in
  let second_op = List.nth manager_operations 1 in
  let get_status op =
    JSON.(
      op |-> "contents" |=> 0 |-> "metadata" |-> "operation_result" |-> "status"
      |> as_string)
  in
  let first_op_status = get_status first_op in
  let second_op_status = get_status second_op in
  Check.(first_op_status = "applied")
    Check.string
    ~error_msg:"The first operation status expected is %R. Got %L" ;
  Check.(second_op_status = "failed")
    Check.string
    ~error_msg:"The second operation status expected is %R. Got %L" ;
  (* We try to finalize a new commitment but it fails. *)
  let*? process = submit_finalize_commitment state in
  let* () =
    Process.check_error
      ~exit_code:1
      ~msg:(rex "proto.alpha.tx_rollup_no_commitment_to_finalize")
      process
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let*! commitment = Rollup.get_commitment ~hooks ~rollup ~level:0 client in
  let* () = Client.bake_for client in
  let*! () =
    submit_remove_commitment ~src:Constant.bootstrap2.public_key_hash state
  in
  let* () = Client.bake_for client in
  let predecessor =
    Option.map
      (fun (c : Rollup.submitted_commitment) -> c.commitment_hash)
      commitment
  in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:1
      ~roots:[Constant.tx_rollup_initial_message_result]
      ~inbox_content
      ~predecessor
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let*! () =
    submit_finalize_commitment ~src:Constant.bootstrap2.public_key_hash state
  in
  let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:1 client in
  Check.(inbox <> None)
    (Check.option Rollup.Check.inbox)
    ~error_msg:"Expected some inbox" ;
  let* () = Client.bake_for client in
  let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:0 client in
  Check.(inbox = None)
    (Check.option Rollup.Check.inbox)
    ~error_msg:"Expected no inbox" ;
  let*! _commitment = Rollup.get_commitment ~hooks ~rollup ~level:1 client in
  let*! commitment = Rollup.get_commitment ~hooks ~rollup ~level:0 client in
  Check.(commitment = None)
    (Check.option Rollup.Check.commitment)
    ~error_msg:"Expected no commitment" ;
  let* () = Client.bake_for client in
  let*! () =
    submit_remove_commitment ~src:Constant.bootstrap2.public_key_hash state
  in
  let* () = Client.bake_for client in
  let*! _commitment = Rollup.get_commitment ~hooks ~rollup ~level:0 client in
  let*! commitment = Rollup.get_commitment ~hooks ~rollup ~level:1 client in
  Check.(commitment = None)
    (Check.option Rollup.Check.commitment)
    ~error_msg:"Expected no commitment" ;
  let* () = submit_return_bond ~src:Constant.bootstrap1.public_key_hash state in
  let* json = RPC.raw_bytes ~path:["tx_rollup"] client in
  let json_object = JSON.as_object json in
  (* Only the state for the rollup should be allocated. *)
  Check.(List.length json_object = 1)
    Check.int
    ~error_msg:"Expected the rollup storage containing one field. Got: %L" ;
  unit

let test_rollup_last_commitment_is_rejected =
  Protocol.register_test
    ~__FILE__
    ~title:"reject last commitment"
    ~tags:["tx_rollup"; "rejection"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:["txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2"]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let*! message_hash =
    Rollup.message_hash ~message:(Rollup.make_batch "blob") client
  in
  let*! path =
    Rollup.inbox_merkle_tree_path
      ~message_hashes:[message_hash]
      ~position:0
      client
  in
  (* This is the encoding of [batch]. *)
  let message =
    Format.sprintf
      "{ \"batch\": \"%s\"}"
      (let (`Hex s) = Hex.of_string "blob" in
       s)
  in
  let message_result_hash =
    "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2"
  in
  let*! rejected_message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:
        [`Hash "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2"]
      ~position:0
      client
  in
  let agreed_message_result_path = "[]" in
  let*! () =
    submit_rejection
      ~level:0
      ~message
      ~position:0
      ~path:(path |> JSON.encode)
      ~message_result_hash
      ~rejected_message_result_path:(rejected_message_result_path |> JSON.encode)
      ~agreed_message_result_path
      ~proof:Constant.tx_rollup_proof_initial_state
      ~context_hash:Constant.tx_rollup_empty_l2_context
      ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list
      state
  in
  let* () = Client.bake_for client in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let* _ = RPC.get_block client in
  unit

let test_rollup_reject_position_one =
  Protocol.register_test
    ~__FILE__
    ~title:"reject commitment using position one"
    ~tags:["tx_rollup"; "rejection"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () =
    submit_batch
      ~batch
      ~batches:[(batch, Constant.bootstrap2.public_key_hash)]
      state
  in
  let* _ = RPC.get_block client in

  let inbox_content = `Content [batch; batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:
        [
          Constant.tx_rollup_initial_message_result;
          "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2";
        ]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let*! message_hash =
    Rollup.message_hash ~message:(Rollup.make_batch "blob") client
  in
  let*! path =
    Rollup.inbox_merkle_tree_path
      ~message_hashes:[message_hash; message_hash]
      ~position:1
      client
  in
  (* This is the encoding of [batch]. *)
  let message =
    Format.sprintf
      "{ \"batch\": \"%s\"}"
      (let (`Hex s) = Hex.of_string "blob" in
       s)
  in
  let*! agreed_message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:
        [
          `Hash Constant.tx_rollup_initial_message_result;
          `Hash "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2";
        ]
      ~position:0
      client
  in
  let*! rejected_message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:
        [
          `Hash Constant.tx_rollup_initial_message_result;
          `Hash "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2";
        ]
      ~position:1
      client
  in
  let*! () =
    submit_rejection
      ~level:0
      ~message
      ~position:1
      ~path:(path |> JSON.encode)
      ~message_result_hash:
        "txmr2DouKqJu5o8KEVGe6gLoiw1J3krjsxhf6C2a1kDNTTr8BdKpf2"
      ~rejected_message_result_path:(rejected_message_result_path |> JSON.encode)
      ~agreed_message_result_path:(agreed_message_result_path |> JSON.encode)
      ~proof:Constant.tx_rollup_proof_initial_state
      ~context_hash:Constant.tx_rollup_empty_l2_context
      ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list
      state
  in
  let* () = Client.bake_for client in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let* _ = RPC.get_block client in
  unit

let test_rollup_wrong_rejection =
  Protocol.register_test
    ~__FILE__
    ~title:"wrong rejection"
    ~tags:["tx_rollup"; "rejection"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:[Constant.tx_rollup_initial_message_result]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  (* This is the encoding of [batch]. *)
  let message = batch in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let*! message_hash =
    Rollup.message_hash ~message:(Rollup.make_batch "blob") client
  in
  let*! path =
    Rollup.inbox_merkle_tree_path
      ~message_hashes:[message_hash]
      ~position:0
      client
  in
  let message_path = List.map (fun x -> JSON.as_string x) (JSON.as_list path) in
  let message_result_hash = Constant.tx_rollup_initial_message_result in
  let*! message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:[`Hash Constant.tx_rollup_initial_message_result]
      ~position:0
      client
  in
  (* The proof is invalid, as the submitted batch is stupid, the after
     hash should be the same as before. *)
  let* (`OpHash _op) =
    Operation.inject_rejection
      ~source:Constant.bootstrap1
      ~tx_rollup:state.rollup
      ~proof:
        {|{ "version": 3,
  "before": { "node": "CoVu7Pqp1Gh3z33mink5T5Q2kAQKtnn3GHxVhyehdKZpQMBxFBGF" } ,
  "after": { "node": "CoUeJrcPBj3T3iJL3PY4jZHnmZa5rRZ87VQPdSBNBcwZRMWJGh9j" } ,
  "state": [] }|}
      ~level:0
      ~message
      ~message_position:0
      ~message_path
      ~message_result_hash
      ~message_result_path:(JSON.unannotate message_result_path)
      ~previous_message_result_path:(`A [])
      ~previous_message_context_hash:Constant.tx_rollup_empty_l2_context
      ~previous_message_withdraw_list_hash:
        Constant.tx_rollup_empty_withdraw_list
      state.client
  in
  let* () = Client.bake_for client in
  let*! state = Rollup.get_state ~rollup client in
  let* json = RPC.get_block client in
  let operation_result =
    JSON.(
      json |-> "operations" |=> 3 |=> 0 |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  let status = JSON.(operation_result |-> "status" |> as_string) in
  Check.(status = "failed")
    Check.string
    ~error_msg:"Expected status: %R. Got %L" ;

  let error_id =
    JSON.(operation_result |-> "errors" |=> 0 |-> "id" |> as_string)
  in
  Check.(error_id = "proto.alpha.tx_rollup_proof_failed_to_reject")
    Check.string
    ~error_msg:"Expected error id: %R. Got %L" ;
  match state.unfinalized_commitments with
  | Interval (0, _) -> unit
  | _ -> Test.fail "Wrong rollup state: Expected commitment head at level 0"

let test_rollup_wrong_path_for_rejection =
  Protocol.register_test
    ~__FILE__
    ~title:"wrong message path for rejection"
    ~tags:["tx_rollup"; "rejection"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:[Constant.tx_rollup_initial_message_result]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let message_result_hash = Constant.tx_rollup_initial_message_result in
  let*! message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:[`Hash Constant.tx_rollup_initial_message_result]
      ~position:0
      client
  in
  let*! _ = RPC.Tx_rollup.get_state ~rollup client in
  let* (`OpHash _op) =
    Operation.inject_rejection
      ~source:Constant.bootstrap1
      ~tx_rollup:state.rollup
      ~proof:Constant.tx_rollup_proof_initial_state
      ~level:0
      ~message:batch
      ~message_position:0
      ~message_path:[]
      ~message_result_hash
      ~message_result_path:(message_result_path |> JSON.unannotate)
      ~previous_message_result_path:(`A [])
      ~previous_message_context_hash:Constant.tx_rollup_empty_l2_context
      ~previous_message_withdraw_list_hash:
        Constant.tx_rollup_empty_withdraw_list
      client
  in
  let* () = Client.bake_for client in
  let* json = RPC.get_block client in
  let operation_result =
    JSON.(
      json |-> "operations" |=> 3 |=> 0 |-> "contents" |=> 0 |-> "metadata"
      |-> "operation_result")
  in
  let status = JSON.(operation_result |-> "status" |> as_string) in
  Check.(status = "failed")
    Check.string
    ~error_msg:"Expected status: %R. Got %L" ;
  let error_id =
    JSON.(operation_result |-> "errors" |=> 0 |-> "id" |> as_string)
  in
  Check.(error_id = "proto.alpha.tx_rollup_wrong_message_path")
    Check.string
    ~error_msg:"Expected error id: %R. Got %L" ;
  unit

let test_rollup_wrong_rejection_long_path =
  Protocol.register_test
    ~__FILE__
    ~title:"wrong rejection with long path"
    ~tags:["tx_rollup"; "rejection"; "batch"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 1; withdraw_period = 1} in
  let* ({rollup = _; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let batch = Rollup.make_batch "blob" in
  let* () = submit_batch ~batch state in
  let* () = Client.bake_for client in
  let inbox_content = `Content [batch] in
  let* () =
    submit_commitment
      ~level:0
      ~roots:[Constant.tx_rollup_initial_message_result]
      ~inbox_content
      ~predecessor:None
      state
  in
  let* () =
    repeat parameters.finality_period (fun () -> Client.bake_for client)
  in
  let message = batch in
  let*! message_hash = Rollup.message_hash ~message client in
  let message_hashes = List.init 1025 (fun _ -> message_hash) in
  let position = 5 in
  let*! bad_path =
    Rollup.inbox_merkle_tree_path ~message_hashes ~position client
  in
  let message_hashes = List.init 1024 (fun _ -> message_hash) in
  let*! good_path =
    Rollup.inbox_merkle_tree_path ~message_hashes ~position client
  in
  let bad_path = JSON.encode bad_path in
  let good_path = JSON.encode good_path in
  let (`Batch (`Hex content)) = batch in
  let message = Ezjsonm.value_to_string @@ `O [("batch", `String content)] in
  let message_result_hash = Constant.tx_rollup_initial_message_result in
  let*! rejected_message_result_path =
    Rollup.commitment_merkle_tree_path
      ~message_result_hashes:[`Hash Constant.tx_rollup_initial_message_result]
      ~position:0
      client
  in
  let agreed_message_result_path = "[]" in
  let*? process =
    Client.Tx_rollup.submit_rejection
      ~src:Constant.bootstrap1.alias
      ~rollup:state.rollup
      ~proof:Constant.tx_rollup_proof_initial_state
      ~level:0
      ~message
      ~position
      ~path:bad_path
      ~message_result_hash
      ~rejected_message_result_path:(rejected_message_result_path |> JSON.encode)
      ~agreed_message_result_path
      ~context_hash:Constant.tx_rollup_empty_l2_context
      ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list
      state.client
  in
  let* () =
    Process.check_error
      ~msg:(rex "proto.alpha.tx_rollup_wrong_message_path_depth")
      process
  in
  (* We check here the path is valid but the operation is rejected for
     a different reason. *)
  let*? process =
    Client.Tx_rollup.submit_rejection
      ~src:Constant.bootstrap1.alias
      ~rollup:state.rollup
      ~proof:Constant.tx_rollup_proof_initial_state
      ~level:0
      ~message
      ~position:0
      ~path:good_path
      ~message_result_hash
      ~rejected_message_result_path:(rejected_message_result_path |> JSON.encode)
      ~agreed_message_result_path
      ~context_hash:Constant.tx_rollup_empty_l2_context
      ~withdraw_list_hash:Constant.tx_rollup_empty_withdraw_list
      state.client
  in
  Process.check_error
    ~msg:(rex "proto.alpha.tx_rollup_wrong_message_path")
    process

let check_bond_is ~src client ~expected =
  let*! bond = RPC.Contracts.get_frozen_bonds ~contract_id:src client in
  let given = JSON.as_int bond in
  Check.(given = expected)
    Check.int
    ~error_msg:"Unexpected frozen bond for tx rollup. Expected %R. Got %L" ;
  unit

let attempt_return_bond ~(expected : [`Ok | `Ko]) ~src state client =
  let* expected_bond_after_op =
    Lwt.catch
      (fun () ->
        let* () = submit_return_bond ~src state in
        if expected = `Ko then
          Test.fail "Return bond expected to fail but succeeded" ;
        return 0)
      (fun _exn ->
        if expected = `Ok then
          Test.fail "Return bond expected to succeed but failed" ;
        let* constants = RPC.get_constants client in
        return JSON.(constants |-> "tx_rollup_commitment_bond" |> as_int))
  in
  check_bond_is ~src state.client ~expected:expected_bond_after_op

(** This function tests some simple situations where bond for commiting in a
    rollup could be returned or not. *)
let test_rollup_bond_return =
  Protocol.register_test
    ~__FILE__
    ~title:"bond return"
    ~tags:["tx_rollup"; "bond"; "return"]
  @@ fun protocol ->
  let parameters = Parameters.{finality_period = 4; withdraw_period = 4} in
  let* ({rollup; client; node = _} as state) =
    init_with_tx_rollup ~parameters ~protocol ()
  in
  let src = Constant.bootstrap2.public_key_hash in
  let* constants = RPC.get_constants client in
  let commit_bond =
    JSON.(constants |-> "tx_rollup_commitment_bond" |> as_int)
  in
  (* No bond deposited at the beginning *)
  let* () = check_bond_is ~src client ~expected:0 in

  (* Auxiliary function that sends a batch to a rollup, then the corresponding commitment,
     and then finalizes the commitment if [finalize] is true. *)
  let batch_commit_finalize =
    let current_calls_counter = ref 0 in
    let step msg = Log.info "call %d) - %s" !current_calls_counter msg in
    fun ?(finalize = true) ?(remove = true) ~rollup_level () ->
      incr current_calls_counter ;

      step "1. Submit batch" ;
      let batch = Rollup.make_batch "blob" in
      let* () = submit_batch ~batch state in

      step "2. Submit commitment" ;
      let*! s = Rollup.get_state ~rollup client in
      let* () =
        submit_commitment
          ~src
          ~level:rollup_level
          ~roots:[Constant.tx_rollup_initial_message_result]
          ~predecessor:s.Rollup.commitment_newest_hash
          ~inbox_content:(`Content [batch])
          state
      in

      step "3. Repeat bake before finalizing commitment" ;
      let* () =
        repeat parameters.finality_period (fun () -> Client.bake_for client)
      in

      step "4. Attempt return bond, which should fail" ;
      let* () = attempt_return_bond state client ~src ~expected:`Ko in
      if not finalize then unit
      else
        let () = step "5. Submit finalize_commitment and bake" in
        let*! () = submit_finalize_commitment state in
        if not remove then unit
        else (
          step "6. Repeat bake before finalizing commitment" ;
          let* () =
            (* +1 because [submit_finalize_commitment] does not bake *)
            repeat (parameters.withdraw_period + 1) (fun () ->
                Client.bake_for client)
          in
          let () = step "7. Submit remove_commitment and bake" in
          let*! () = submit_remove_commitment state in
          let* () = check_bond_is ~src client ~expected:commit_bond in
          Client.bake_for client)
  in
  (* 1st scenario: batch; commit; finalize; return bond (OK) *)
  Log.info "1st scenario: batch; commit; finalize; return bond (OK)" ;
  let* () = batch_commit_finalize ~rollup_level:0 () in
  let* () = attempt_return_bond state client ~src ~expected:`Ok in

  (* 2nd scenario: batch; commit; finalize; batch; commit; finalize;
     return bond (OK) *)
  Log.info "2nd scenario: (batch; commit; finalize)x2, return bond (OK)" ;
  let* () = batch_commit_finalize ~rollup_level:1 () in
  let* () = batch_commit_finalize ~rollup_level:2 () in
  let* () = attempt_return_bond state client ~src ~expected:`Ok in

  (* 3rd scenario: batch; commit; finalize; batch; commit; return bond (KO) *)
  Log.info
    "3rd scenario: batch; commit; finalize; batch; commit; return bond (KO)" ;
  let* () = batch_commit_finalize ~rollup_level:3 () in
  let* () = batch_commit_finalize ~rollup_level:4 ~finalize:false () in
  let* () = attempt_return_bond state client ~src ~expected:`Ko in
  unit

let register ~protocols =
  Regressions.register protocols ;
  test_submit_batches_in_several_blocks protocols ;
  test_submit_from_originated_source protocols ;
  test_rollup_with_two_commitments protocols ;
  test_rollup_last_commitment_is_rejected protocols ;
  test_rollup_reject_position_one protocols ;
  test_rollup_wrong_rejection protocols ;
  test_rollup_wrong_path_for_rejection protocols ;
  test_rollup_wrong_rejection_long_path protocols ;
  test_rollup_bond_return protocols
