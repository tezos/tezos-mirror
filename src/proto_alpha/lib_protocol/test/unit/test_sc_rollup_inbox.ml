(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:  Protocol (smart contract rollup inbox)
    Invocation: dune exec src/proto_alpha/lib_protocol/test/unit/main.exe \
                -- test "^\[Unit\] sc rollup inbox$"
    Subject:    These unit tests check the off-line inbox implementation for
                smart contract rollups
*)

open Protocol
open Sc_rollup_inbox_repr

exception Sc_rollup_inbox_test_error of string

let err x = Exn (Sc_rollup_inbox_test_error x)

let rollup = Sc_rollup_repr.Address.hash_string [""]

let level =
  Raw_level_repr.of_int32 0l |> function Ok x -> x | _ -> assert false

let create_context () =
  Context.init1 () >>=? fun (block, _contract) -> return block.context

let populate_inboxes ctxt level history inbox inboxes level_tree
    list_of_payloads =
  let open Lwt_syntax in
  let rec aux level history inbox inboxes level_tree = function
    | [] -> return (ok (level_tree, history, inbox, inboxes))
    | [] :: ps ->
        let level = Raw_level_repr.succ level in
        aux level history inbox inboxes level_tree ps
    | payloads :: ps ->
        Lwt.return
          (List.map_e
             (fun payload ->
               Sc_rollup_inbox_message_repr.(serialize @@ External payload))
             payloads)
        >|= Environment.wrap_tzresult
        >>=? fun payloads ->
        add_messages ctxt history inbox level payloads level_tree
        >|= Environment.wrap_tzresult
        >>=? fun (level_tree, history, inbox') ->
        let level = Raw_level_repr.succ level in
        aux level history inbox' (inbox :: inboxes) (Some level_tree) ps
  in
  aux level history inbox inboxes level_tree list_of_payloads

let test_empty () =
  create_context () >>=? fun ctxt ->
  empty ctxt rollup level >>= fun inbox ->
  fail_unless
    Compare.Int64.(equal (number_of_messages_during_commitment_period inbox) 0L)
    (err "An empty inbox should have no available message.")

let setup_inbox_with_messages list_of_payloads f =
  let open Lwt_syntax in
  create_context () >>=? fun ctxt ->
  let* inbox = empty ctxt rollup level in
  let history = history_at_genesis ~capacity:10000L in
  populate_inboxes ctxt level history inbox [] None list_of_payloads
  >>=? fun (level_tree, history, inbox, inboxes) ->
  match level_tree with
  | None -> fail (err "setup_inbox_with_messages called with no messages")
  | Some tree -> f ctxt tree history inbox inboxes

let test_add_messages payloads =
  let nb_payloads = List.length payloads in
  setup_inbox_with_messages [payloads]
  @@ fun _ctxt _messages _history inbox _inboxes ->
  fail_unless
    Compare.Int64.(
      equal
        (number_of_messages_during_commitment_period inbox)
        (Int64.of_int nb_payloads))
    (err "Invalid number of messages during commitment period.")

(* An external message is prefixed with a tag whose length is one byte, and
   whose value is 1. *)
let encode_external_message message =
  let prefix = "\001" in
  Bytes.of_string (prefix ^ message)

let check_payload messages external_message =
  Environment.Context.Tree.find messages ["payload"] >>= function
  | None -> fail (err "No payload in messages")
  | Some payload ->
      let expected_payload = encode_external_message external_message in
      fail_unless
        (expected_payload = payload)
        (err
           (Printf.sprintf
              "Expected payload %s, got %s"
              (Bytes.to_string expected_payload)
              (Bytes.to_string payload)))

let test_get_message_payload payloads =
  setup_inbox_with_messages [payloads]
  @@ fun _ctxt messages _history _inbox _inboxes ->
  List.iteri_es
    (fun i message ->
      let expected_payload = encode_external_message message in
      get_message_payload messages (Z.of_int i) >>= function
      | Some payload ->
          let payload = Sc_rollup_inbox_message_repr.unsafe_to_string payload in
          fail_unless
            (String.equal payload (Bytes.to_string expected_payload))
            (err (Printf.sprintf "Expected %s, got %s" message payload))
      | None ->
          fail
            (err (Printf.sprintf "No message payload number %d in messages" i)))
    payloads

let test_inclusion_proof_production (list_of_payloads, n) =
  setup_inbox_with_messages list_of_payloads
  @@ fun _ctxt _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  produce_inclusion_proof
    history
    (old_levels_messages old_inbox)
    (old_levels_messages inbox)
  |> function
  | None ->
      fail
      @@ err
           "It should be possible to produce an inclusion proof between two \
            versions of the same inbox."
  | Some proof ->
      fail_unless
        (verify_inclusion_proof
           proof
           (old_levels_messages old_inbox)
           (old_levels_messages inbox))
        (err "The produced inclusion proof is invalid.")

let test_inclusion_proof_verification (list_of_payloads, n) =
  setup_inbox_with_messages list_of_payloads
  @@ fun _ctxt _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  produce_inclusion_proof
    history
    (old_levels_messages old_inbox)
    (old_levels_messages inbox)
  |> function
  | None ->
      fail
      @@ err
           "It should be possible to produce an inclusion proof between two \
            versions of the same inbox."
  | Some proof ->
      let old_inbox' = Stdlib.List.nth inboxes (Random.int (1 + n)) in
      fail_unless
        (equal old_inbox old_inbox'
        || not
             (verify_inclusion_proof
                proof
                (old_levels_messages old_inbox')
                (old_levels_messages inbox)))
        (err
           "Verification should rule out a valid proof which is not about the \
            given inboxes.")

module Tree = struct
  open Tezos_context_memory.Context

  type nonrec t = t

  type nonrec tree = tree

  module Tree = struct
    include Tezos_context_memory.Context.Tree

    type nonrec t = t

    type nonrec tree = tree

    type key = string list

    type value = bytes
  end

  let commit_tree context key tree =
    let open Lwt_syntax in
    let* ctxt = Tezos_context_memory.Context.add_tree context key tree in
    let* _ = commit ~time:Time.Protocol.epoch ~message:"" ctxt in
    return ()

  let lookup_tree context hash =
    let open Lwt_syntax in
    let* _, tree =
      produce_tree_proof
        (index context)
        (`Node (Hash.to_context_hash hash))
        (fun x -> Lwt.return (x, x))
    in
    return (Some tree)

  type proof = Proof.tree Proof.t

  let verify_proof proof f =
    Lwt.map Result.to_option (verify_tree_proof proof f)

  let produce_proof context tree f =
    let open Lwt_syntax in
    let* proof =
      produce_tree_proof (index context) (`Node (Tree.hash tree)) f
    in
    return (Some proof)

  let kinded_hash_to_inbox_hash = function
    | `Value hash | `Node hash -> Hash.of_context_hash hash

  let proof_before proof = kinded_hash_to_inbox_hash proof.Proof.before

  let proof_encoding =
    Tezos_context_merkle_proof_encoding.Merkle_proof_encoding.V1.Tree32
    .tree_proof_encoding
end

(** This is a second instance of the inbox module. It uses the {!Tree}
    module above for its Irmin interface, which gives it a full context
    and the ability to generate tree proofs.

    It is intended to resemble (at least well enough for these tests)
    the rollup node's inbox instance. *)
module Node = Make_hashing_scheme (Tree)

(** In the tests below we use the {!Node} inbox above to generate proofs,
    but we need to test that they can be interpreted and validated by
    the protocol instance of the inbox code. We rely on the two
    instances having the same encoding, and use this function to
    convert. *)
let node_proof_to_protocol_proof p =
  let open Data_encoding.Binary in
  let enc = serialized_proof_encoding in
  let bytes = Node.to_serialized_proof p |> to_bytes_exn enc in
  of_bytes_exn enc bytes |> of_serialized_proof
  |> WithExceptions.Option.get ~loc:__LOC__

(** This is basically identical to {!setup_inbox_with_messages}, except
    that it uses the {!Node} instance instead of the protocol instance. *)
let setup_node_inbox_with_messages list_of_payloads f =
  let open Node in
  let open Lwt_syntax in
  let* index = Tezos_context_memory.Context.init "foo" in
  let ctxt = Tezos_context_memory.Context.empty index in
  let* inbox = empty ctxt rollup level in
  let history = history_at_genesis ~capacity:10000L in
  let rec aux level history inbox inboxes level_tree = function
    | [] -> return (ok (level_tree, history, inbox, inboxes))
    | payloads :: ps -> (
        Lwt.return
          (List.map_e
             (fun payload ->
               Sc_rollup_inbox_message_repr.(serialize @@ External payload))
             payloads)
        >|= Environment.wrap_tzresult
        >>=? fun payloads ->
        match payloads with
        | [] ->
            let level = Raw_level_repr.succ level in
            aux level history inbox inboxes level_tree ps
        | _ ->
            add_messages ctxt history inbox level payloads level_tree
            >|= Environment.wrap_tzresult
            >>=? fun (level_tree, history, inbox') ->
            let level = Raw_level_repr.succ level in
            aux level history inbox' (inbox :: inboxes) (Some level_tree) ps)
  in
  aux level history inbox [] None list_of_payloads
  >>=? fun (level_tree, history, inbox, inboxes) ->
  match level_tree with
  | None -> fail (err "setup_inbox_with_messages called with no messages")
  | Some tree -> f ctxt tree history inbox inboxes

let look_in_tree key tree =
  let open Lwt_syntax in
  let* x = Tree.Tree.find tree [key] in
  match x with
  | Some x -> return (tree, x)
  | None -> return (tree, Bytes.of_string "nope")

let key_of_level level =
  let level_bytes =
    Data_encoding.Binary.to_bytes_exn Raw_level_repr.encoding level
  in
  Bytes.to_string level_bytes

let level_of_int n = Raw_level_repr.of_int32_exn (Int32.of_int n)

let level_to_int l = Int32.to_int (Raw_level_repr.to_int32 l)

let payload_string msg =
  Sc_rollup_inbox_message_repr.unsafe_of_string
    (Bytes.to_string (encode_external_message msg))

let next_input ps l n =
  let ( let* ) = Option.bind in
  let* level = List.nth ps (level_to_int l) in
  match List.nth level (Z.to_int n) with
  | Some msg ->
      let payload = payload_string msg in
      Some Sc_rollup_PVM_sem.{inbox_level = l; message_counter = n; payload}
  | None ->
      let rec aux l =
        let* payloads = List.nth ps l in
        match List.hd payloads with
        | Some msg ->
            let payload = payload_string msg in
            Some
              Sc_rollup_PVM_sem.
                {
                  inbox_level = level_of_int l;
                  message_counter = Z.zero;
                  payload;
                }
        | None -> aux (l + 1)
      in
      aux (level_to_int l + 1)

let test_inbox_proof_production (list_of_payloads, l, n) =
  (* We begin with a Node inbox so we can produce a proof. *)
  let exp_input = next_input list_of_payloads l n in
  setup_node_inbox_with_messages list_of_payloads
  @@ fun ctxt current_level_tree history inbox _inboxes ->
  let open Lwt_syntax in
  let* history, history_proof =
    Node.form_history_proof ctxt history inbox (Some current_level_tree)
  in
  let* result = Node.produce_proof ctxt history history_proof (l, n) in
  match result with
  | Ok (proof, input) -> (
      (* We now switch to a protocol inbox built from the same messages
         for verification. *)
      setup_inbox_with_messages list_of_payloads
      @@ fun _ctxt _current_level_tree _history inbox _inboxes ->
      let snapshot = take_snapshot inbox in
      let proof = node_proof_to_protocol_proof proof in
      let* verification = verify_proof (l, n) snapshot proof in
      match verification with
      | Ok v_input ->
          fail_unless
            (v_input = input && v_input = exp_input)
            (err "Proof verified but did not match")
      | Error _ -> fail (err "Proof verification failed"))
  | Error _ -> fail (err "Proof production failed")

let test_inbox_proof_verification (list_of_payloads, l, n) =
  (* We begin with a Node inbox so we can produce a proof. *)
  setup_node_inbox_with_messages list_of_payloads
  @@ fun ctxt current_level_tree history inbox _inboxes ->
  let open Lwt_syntax in
  let* history, history_proof =
    Node.form_history_proof ctxt history inbox (Some current_level_tree)
  in
  let* result = Node.produce_proof ctxt history history_proof (l, n) in
  match result with
  | Ok (proof, _input) -> (
      (* We now switch to a protocol inbox built from the same messages
         for verification. *)
      setup_inbox_with_messages list_of_payloads
      @@ fun _ctxt _current_level_tree _history _inbox inboxes ->
      (* Use the incorrect inbox *)
      match List.hd inboxes with
      | Some inbox -> (
          let snapshot = take_snapshot inbox in
          let proof = node_proof_to_protocol_proof proof in
          let* verification = verify_proof (l, n) snapshot proof in
          match verification with
          | Ok _ -> fail (err "Proof should not be valid")
          | Error _ -> return (ok ()))
      | None -> fail (err "inboxes was empty"))
  | Error _ -> fail (err "Proof production failed")

let test_empty_inbox_proof (level, n) =
  let open Lwt_syntax in
  let* index = Tezos_context_memory.Context.init "foo" in
  let ctxt = Tezos_context_memory.Context.empty index in
  let* inbox = Node.empty ctxt rollup level in
  let history = Node.history_at_genesis ~capacity:10000L in
  let* history, history_proof =
    Node.form_history_proof ctxt history inbox None
  in
  let* result =
    Node.produce_proof ctxt history history_proof (Raw_level_repr.root, n)
  in
  match result with
  | Ok (proof, input) -> (
      (* We now switch to a protocol inbox for verification. *)
      create_context ()
      >>=? fun ctxt ->
      let* inbox = empty ctxt rollup level in
      let snapshot = take_snapshot inbox in
      let proof = node_proof_to_protocol_proof proof in
      let* verification =
        verify_proof (Raw_level_repr.root, n) snapshot proof
      in
      match verification with
      | Ok v_input ->
          fail_unless
            (v_input = input && v_input = None)
            (err "Proof verified but did not match")
      | Error _ -> fail (err "Proof verification failed"))
  | Error _ -> fail (err "Proof production failed")

let tests =
  let msg_size = QCheck2.Gen.(0 -- 100) in
  let bounded_string = QCheck2.Gen.string_size msg_size in
  [
    Tztest.tztest "Empty inbox" `Quick test_empty;
    Tztest.tztest_qcheck2
      ~name:"Added messages are available."
      QCheck2.Gen.(list_size (1 -- 50) bounded_string)
      test_add_messages;
    Tztest.tztest_qcheck2
      ~name:"Get message payload."
      QCheck2.Gen.(list_size (1 -- 50) bounded_string)
      test_get_message_payload;
  ]
  @
  let gen_inclusion_proof_inputs =
    QCheck2.Gen.(
      let small = 2 -- 10 in
      let* a = list_size small bounded_string in
      let* b = list_size small bounded_string in
      let* l = list_size small (list_size small bounded_string) in
      let l = a :: b :: l in
      let* n = 0 -- (List.length l - 2) in
      return (l, n))
  in
  let gen_proof_inputs =
    QCheck2.Gen.(
      let small = 0 -- 5 in
      let* level = 0 -- 8 in
      let* before = list_size (return level) (list_size small bounded_string) in
      let* at = list_size (2 -- 6) bounded_string in
      let* after = list_size small (list_size small bounded_string) in
      let payloads = List.append before (at :: after) in
      let* n = 0 -- (List.length at + 3) in
      return (payloads, level_of_int level, Z.of_int n))
  in
  [
    Tztest.tztest_qcheck2
      ~name:"Produce inclusion proof between two related inboxes."
      gen_inclusion_proof_inputs
      test_inclusion_proof_production;
    Tztest.tztest_qcheck2
      ~name:"Verify inclusion proofs."
      gen_inclusion_proof_inputs
      test_inclusion_proof_verification;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:"Produce inbox proofs"
      gen_proof_inputs
      test_inbox_proof_production;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:"Verify inbox proofs"
      gen_proof_inputs
      test_inbox_proof_verification;
    Tztest.tztest_qcheck2
      ~name:"An empty inbox is still able to produce proofs that return None"
      QCheck2.Gen.(
        let* n = 0 -- 2000 in
        let* m = 0 -- 1000 in
        return (level_of_int n, Z.of_int m))
      test_empty_inbox_proof;
  ]
