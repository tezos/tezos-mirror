(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Plugin.Block_validation
    Invocation:   dune exec src/proto_025_PsUshuai/lib_plugin/test/main.exe \
                  -- --file test_relabelled_inbox_proof.ml
    Subject:      A refutation Proof move whose inbox proof shows rewritten
                  indices is found in its batch, using the index proof the same
                  batch publishes.
*)

open Alpha_context
module Payloads = Sc_rollup.Inbox_merkelized_payload_hashes
module Inbox_index_proof = Tezos_smart_rollup_025_PsUshuai.Inbox_index_proof

let register_test =
  Helpers.register_test
    ~__FILE__
    ~file_title:"relabelled_inbox_proof"
    ~file_tags:["sc_rollup"; "refutation"; "inbox"]
    ~additional_tags:[]

let level = Raw_level.(succ root)

(* Counter 3 is the second external message of the level: 0 and 1 are the
   Start_of_level and Info_per_level messages the protocol adds. *)
let message_counter = Z.of_int 3

let messages = ["m0"; "m1"; "m2"; "m3"]

let ok_or_fail ~__LOC__ = function
  | Ok x -> Lwt.return x
  | Error trace -> Test.fail ~__LOC__ "%a" pp_print_trace trace

(* The inclusion proof, the payloads proof and the index proof of the head
   cell of [level], for a request at [message_counter]. *)
let honest_parts () =
  let open Lwt_result_wrap_syntax in
  let*? node_inbox, proto_inbox =
    Sc_rollup_helpers.construct_node_and_protocol_inbox
      ~inbox_creation_level:Raw_level.root
      [Sc_rollup_helpers.wrap_messages level messages]
  in
  let snapshot = Sc_rollup.Inbox.take_snapshot proto_inbox in
  let* (inclusion, payloads_proof), _input =
    Sc_rollup_helpers.Node_inbox.produce_and_expose_proof
      node_inbox
      snapshot
      (level, message_counter)
  in
  let* head =
    match
      (payloads_proof.Sc_rollup.Inbox.Internal_for_tests.proof
        :> Payloads.t list)
    with
    | head :: _ -> return head
    | [] -> failwith "the payloads proof holds no cell"
  in
  let*! history =
    Sc_rollup_helpers.get_payloads_history
      node_inbox.Sc_rollup_helpers.Node_inbox.payloads_histories
      (Payloads.hash head)
  in
  let* index_proof =
    match Inbox_index_proof.produce history ~head with
    | Some index_proof -> return index_proof
    | None -> failwith "could not produce the index proof of the head cell"
  in
  return (inclusion, payloads_proof, Inbox_index_proof.to_messages index_proof)

(* The same payloads proof, with the index its head cell declares moved one
   step up. The cell still hashes the same. *)
let relabel_head payloads_proof =
  let encoding = Payloads.encoding in
  let relabel position cell =
    if Compare.Int.(position <> 0) then cell
    else
      match Data_encoding.Json.construct encoding cell with
      | `O fields ->
          Data_encoding.Json.destruct
            encoding
            (`O
               (List.map
                  (fun (name, value) ->
                    if String.equal name "index" then
                      let index =
                        Data_encoding.Json.destruct Data_encoding.n value
                      in
                      ( name,
                        Data_encoding.Json.construct
                          Data_encoding.n
                          (Z.succ index) )
                    else (name, value))
                  fields))
      | _ -> Stdlib.failwith "a skip list cell is encoded as an object"
  in
  {
    payloads_proof with
    Sc_rollup.Inbox.Internal_for_tests.proof =
      Payloads.Internal_for_tests.make_proof
        (List.mapi
           relabel
           (payloads_proof.Sc_rollup.Inbox.Internal_for_tests.proof
             :> Payloads.t list));
  }

let manager operation =
  Manager_operation
    {
      source = Signature.Public_key_hash.zero;
      fee = Tez.zero;
      counter = Manager_counter.Internal_for_tests.of_int 1;
      operation;
      gas_limit = Gas.Arith.zero;
      storage_limit = Z.zero;
    }

(* [find_relabelled_inbox_proof] reads the inbox proof only, so the PVM step is
   left empty. *)
let refute inclusion payloads_proof =
  let proof =
    Sc_rollup.Inbox.Internal_for_tests.make_proof inclusion payloads_proof
  in
  let pvm_step =
    Data_encoding.Json.destruct Sc_rollup.Proof.serialized_encoding (`String "")
  in
  manager
    (Sc_rollup_refute
       {
         rollup = Sc_rollup.Address.zero;
         opponent = Signature.Public_key_hash.zero;
         refutation =
           Sc_rollup.Game.Move
             {
               choice = Sc_rollup.Tick.initial;
               step =
                 Proof
                   {
                     pvm_step;
                     input_proof =
                       Some
                         (Inbox_proof
                            {
                              level;
                              message_counter;
                              proof = Sc_rollup.Inbox.to_serialized_proof proof;
                            });
                   };
             };
       })

let add_messages messages = manager (Sc_rollup_add_messages {messages})

let pack contents : packed_operation =
  Operation.pack
    {
      shell = {branch = Block_hash.zero};
      protocol_data = {contents; signature = None};
    }

let error_name = function
  | Environment.Ecoproto_error
      (Plugin.Block_validation.Sc_rollup_inbox_proof_missing_index_proof _) ->
      "missing index proof"
  | Environment.Ecoproto_error
      (Plugin.Block_validation.Sc_rollup_inbox_proof_relabelled_indices _) ->
      "relabelled indices"
  | Environment.Ecoproto_error
      (Plugin.Block_validation.Sc_rollup_inbox_proof_unreadable_cells _) ->
      "unreadable cells"
  | _ -> "other error"

let check ~__LOC__ expected operation =
  let found =
    match Plugin.Block_validation.find_relabelled_inbox_proof operation with
    | None -> "accepted"
    | Some trace -> (
        match Environment.wrap_tztrace trace with
        | [error] -> error_name error
        | _ -> "several errors")
  in
  Check.((found = expected) string)
    ~error_msg:(__LOC__ ^ ": expected %R, got %L")

let () =
  register_test ~title:"an honest move and its index proof are accepted"
  @@ fun () ->
  let* inclusion, payloads_proof, index_proof =
    Lwt.bind (honest_parts ()) (ok_or_fail ~__LOC__)
  in
  let move = refute inclusion payloads_proof in
  check
    ~__LOC__
    "accepted"
    (pack (Cons (add_messages index_proof, Single move))) ;
  (* The index proof may also come after the move it backs. *)
  check
    ~__LOC__
    "accepted"
    (pack (Cons (move, Single (add_messages index_proof)))) ;
  unit

let () =
  register_test ~title:"a move without the index proof of its level is found"
  @@ fun () ->
  let* inclusion, payloads_proof, _index_proof =
    Lwt.bind (honest_parts ()) (ok_or_fail ~__LOC__)
  in
  check
    ~__LOC__
    "missing index proof"
    (pack (Single (refute inclusion payloads_proof))) ;
  unit

let () =
  register_test
    ~title:"a move with a relabelled head cell is found despite its index proof"
  @@ fun () ->
  let* inclusion, payloads_proof, index_proof =
    Lwt.bind (honest_parts ()) (ok_or_fail ~__LOC__)
  in
  check
    ~__LOC__
    "relabelled indices"
    (pack
       (Cons
          ( add_messages index_proof,
            Single (refute inclusion (relabel_head payloads_proof)) ))) ;
  unit
