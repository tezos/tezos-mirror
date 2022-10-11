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

let first_level = Raw_level_repr.(succ root)

let inbox_message_testable =
  Alcotest.testable
    Sc_rollup_PVM_sig.pp_inbox_message
    Sc_rollup_PVM_sig.inbox_message_equal

let create_context () =
  Context.init1 () >>=? fun (block, _contract) -> return block.context

let make_payload message =
  WithExceptions.Result.get_ok ~loc:__LOC__
  @@ Sc_rollup_inbox_message_repr.(serialize @@ External message)

let payloads_from_messages =
  List.map (fun Sc_rollup_helpers.{input_repr = input; _} ->
      match input with
      | Inbox_message {payload; _} -> payload
      | Reveal _ -> assert false)

let populate_inboxes ctxt level history inbox inboxes level_tree
    list_of_payloads =
  let open Lwt_syntax in
  let rec aux level history inbox inboxes level_tree = function
    | [] -> return (ok (level_tree, history, inbox, inboxes))
    | [] :: ps ->
        let level = Raw_level_repr.succ level in
        aux level history inbox inboxes level_tree ps
    | payloads :: ps ->
        add_messages ctxt history inbox level payloads level_tree
        >|= Environment.wrap_tzresult
        >>=? fun (level_tree, history, inbox') ->
        let level = Raw_level_repr.succ level in
        aux level history inbox' (inbox :: inboxes) (Some level_tree) ps
  in
  aux level history inbox inboxes level_tree list_of_payloads

let test_empty () =
  create_context () >>=? fun ctxt ->
  empty ctxt (Raw_level_repr.of_int32_exn 42l) >>= fun inbox ->
  fail_unless
    Compare.Int64.(equal (number_of_messages_during_commitment_period inbox) 0L)
    (err "An empty inbox should have no available message.")

let setup_inbox_with_messages list_of_payloads f =
  let open Lwt_syntax in
  create_context () >>=? fun ctxt ->
  let* inbox = empty ctxt first_level in
  let history = History.empty ~capacity:10000L in
  populate_inboxes ctxt first_level history inbox [] None list_of_payloads
  >>=? fun (level_tree, history, inbox, inboxes) ->
  match level_tree with
  | None -> fail (err "setup_inbox_with_messages called with no messages")
  | Some tree -> f ctxt tree history inbox inboxes

let test_add_messages messages =
  let payloads = List.map make_payload messages in
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

let test_get_message_payload messages =
  let payloads = List.map make_payload messages in
  setup_inbox_with_messages [payloads]
  @@ fun _ctxt level_tree _history _inbox _inboxes ->
  List.iteri_es
    (fun i message ->
      let expected_payload = encode_external_message message in
      get_message_payload level_tree (Z.of_int i) >>= function
      | Some payload ->
          let payload = Sc_rollup_inbox_message_repr.unsafe_to_string payload in
          fail_unless
            (String.equal payload (Bytes.to_string expected_payload))
            (err (Printf.sprintf "Expected %s, got %s" message payload))
      | None ->
          fail
            (err (Printf.sprintf "No message payload number %d in messages" i)))
    messages

let test_inclusion_proof_production (list_of_messages, n) =
  let open Lwt_result_syntax in
  let list_of_payloads = List.map (List.map make_payload) list_of_messages in
  setup_inbox_with_messages list_of_payloads
  @@ fun _ctxt _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  let*? res =
    Internal_for_tests.produce_inclusion_proof
      history
      (old_levels_messages old_inbox)
      (old_levels_messages inbox)
    |> Environment.wrap_tzresult
  in
  match res with
  | None ->
      fail
        [
          err
            "It should be possible to produce an inclusion proof between two \
             versions of the same inbox.";
        ]
  | Some proof ->
      fail_unless
        (verify_inclusion_proof
           proof
           (old_levels_messages old_inbox)
           (old_levels_messages inbox))
        (err "The produced inclusion proof is invalid.")

let test_inclusion_proof_verification (list_of_messages, n) =
  let open Lwt_result_syntax in
  let list_of_payloads = List.map (List.map make_payload) list_of_messages in
  setup_inbox_with_messages list_of_payloads
  @@ fun _ctxt _messages history _inbox inboxes ->
  let inbox = Stdlib.List.hd inboxes in
  let old_inbox = Stdlib.List.nth inboxes n in
  let*? res =
    Internal_for_tests.produce_inclusion_proof
      history
      (old_levels_messages old_inbox)
      (old_levels_messages inbox)
    |> Environment.wrap_tzresult
  in
  match res with
  | None ->
      fail
        [
          err
            "It should be possible to produce an inclusion proof between two \
             versions of the same inbox.";
        ]
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
    let* (_ : value_key) = commit ~time:Time.Protocol.epoch ~message:"" ctxt in
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
  let* inbox = empty ctxt first_level in
  let history = History.empty ~capacity:10000L in
  let rec aux level history inbox inboxes level_tree = function
    | [] -> return (ok (level_tree, history, inbox, inboxes))
    | payloads :: ps ->
        add_messages ctxt history inbox level payloads level_tree
        >|= Environment.wrap_tzresult
        >>=? fun (level_tree, history, inbox') ->
        let level = Raw_level_repr.succ level in
        aux level history inbox' (inbox :: inboxes) (Some level_tree) ps
  in
  aux first_level history inbox [] None list_of_payloads
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

let inbox_message_of_input input =
  match input with Sc_rollup_PVM_sig.Inbox_message x -> Some x | _ -> None

let next_inbox_message levels_and_messages l n =
  let equal = Raw_level_repr.( = ) in
  let messages =
    WithExceptions.Option.get ~loc:__LOC__
    @@ List.assoc ~equal l levels_and_messages
  in
  match List.nth messages (Z.to_int n) with
  | Some Sc_rollup_helpers.{input_repr = input; _} ->
      inbox_message_of_input input
  | None -> (
      (* If no input at (l, n), the next input is (l+1, 0). *)
      match List.assoc ~equal (Raw_level_repr.succ l) levels_and_messages with
      | None -> None
      | Some messages ->
          let Sc_rollup_helpers.{input_repr = input; _} =
            Stdlib.List.hd messages
          in
          inbox_message_of_input input)

let test_inbox_proof_production (levels_and_messages, l, n) =
  (* We begin with a Node inbox so we can produce a proof. *)
  let exp_input = next_inbox_message levels_and_messages l n in
  let list_of_payloads =
    List.map
      (fun (_, messages) -> payloads_from_messages messages)
      levels_and_messages
  in
  setup_node_inbox_with_messages list_of_payloads
  @@ fun ctxt current_level_tree history inbox _inboxes ->
  let open Lwt_result_syntax in
  let* history, history_proof =
    Node.form_history_proof ctxt history inbox (Some current_level_tree)
    >|= Environment.wrap_tzresult
  in
  let*! result = Node.produce_proof ctxt history history_proof (l, n) in
  match result with
  | Ok (proof, input) -> (
      (* We now switch to a protocol inbox built from the same messages
         for verification. *)
      (* The snapshot takes the snapshot at the end of the last level,
         we need to set the level ahead to match the inbox. *)
      setup_inbox_with_messages (list_of_payloads @ [[make_payload "foo"]])
      @@ fun _ctxt _ _history inbox _inboxes ->
      let snapshot = take_snapshot inbox in
      let proof = node_proof_to_protocol_proof proof in
      let*! verification = verify_proof (l, n) snapshot proof in
      match verification with
      | Ok v_input ->
          Alcotest.(check (option inbox_message_testable))
            "input = v_input"
            input
            v_input ;
          Alcotest.(check (option inbox_message_testable))
            "exp_input = v_input"
            exp_input
            v_input ;
          return_unit
      | Error _ -> fail [err "Proof verification failed"])
  | Error _ -> fail [err "Proof production failed"]

let test_inbox_proof_verification (levels_and_messages, l, n) =
  (* We begin with a Node inbox so we can produce a proof. *)
  let list_of_payloads =
    List.map
      (fun (_, messages) -> payloads_from_messages messages)
      levels_and_messages
  in
  setup_node_inbox_with_messages list_of_payloads
  @@ fun ctxt current_level_tree history inbox _inboxes ->
  let open Lwt_result_syntax in
  let* history, history_proof =
    Node.form_history_proof ctxt history inbox (Some current_level_tree)
    >|= Environment.wrap_tzresult
  in
  let*! result = Node.produce_proof ctxt history history_proof (l, n) in
  match result with
  | Ok (proof, _input) -> (
      (* We now switch to a protocol inbox built from the same messages
         for verification. *)
      setup_inbox_with_messages (list_of_payloads @ [[make_payload "foo"]])
      @@ fun _ctxt _ _history _inbox inboxes ->
      (* Use the incorrect inbox *)
      match List.hd inboxes with
      | Some inbox -> (
          let snapshot = take_snapshot inbox in
          let proof = node_proof_to_protocol_proof proof in
          let*! verification = verify_proof (l, n) snapshot proof in
          match verification with
          | Ok _ -> fail [err "Proof should not be valid"]
          | Error _ -> return (ok ()))
      | None -> fail [err "inboxes was empty"])
  | Error _ -> fail [err "Proof production failed"]

(** This helper function initializes inboxes and histories with different
    capacities and populates them. *)
let init_inboxes_histories_with_different_capacities
    (nb_levels, default_capacity, small_capacity, next_index) =
  let open Lwt_result_syntax in
  let* () =
    fail_when
      Int64.(of_int nb_levels <= small_capacity)
      (err
         (Format.sprintf
            "Bad inputs: nb_levels = %d should be greater than small_capacity \
             = %Ld"
            nb_levels
            small_capacity))
  in
  let* () =
    fail_when
      Int64.(of_int nb_levels >= default_capacity)
      (err
         (Format.sprintf
            "Bad inputs: nb_levels = %d should be smaller than \
             default_capacity = %Ld"
            nb_levels
            default_capacity))
  in
  let*? payloads =
    List.init ~when_negative_length:[] nb_levels (fun i -> [string_of_int i])
  in
  let mk_history ?(next_index = 0L) ~capacity () =
    let open Lwt_syntax in
    create_context () >>=? fun ctxt ->
    let* inbox = empty ctxt first_level in
    let history =
      Sc_rollup_inbox_repr.History.Internal_for_tests.empty
        ~capacity
        ~next_index
    in
    let payloads = List.map (List.map make_payload) payloads in
    populate_inboxes ctxt first_level history inbox [] None payloads
  in
  (* Here, we have `~capacity:0L`. So no history is kept *)
  mk_history ~capacity:0L () >>=? fun no_history ->
  (* Here, we set a [default_capacity] supposed to be greater than [nb_levels],
     and keep the default [next_index]. This history will serve as a witeness *)
  mk_history ~capacity:default_capacity () >>=? fun big_history ->
  (* Here, we choose a small capacity supposed to be smaller than [nb_levels] to
     cover cases where the history is full and older elements should be removed.
     We also set a non-default [next_index] value to cover cases where the
     incremented index may overflow or is negative. *)
  mk_history ~next_index ~capacity:small_capacity () >>=? fun small_history ->
  return (no_history, small_history, big_history)

(** In this test, we mainly check that the number of entries in histories
    doesn't exceed their respective capacities. *)
let test_history_length
    ((_nb_levels, default_capacity, small_capacity, _next_index) as params) =
  let open Lwt_result_syntax in
  let module I = Sc_rollup_inbox_repr in
  let err expected given ~exact =
    err
    @@ Format.sprintf
         "We expect a history of %Ld capacity (%s), but we got %d elements"
         expected
         (if exact then "exactly" else "at most")
         given
  in
  let no_capacity = 0L in
  let* no_history, small_history, big_history =
    init_inboxes_histories_with_different_capacities params
  in
  let _level_tree0, history0, _inbox0, _inboxes0 = no_history in
  let _level_tree1, history1, _inbox1, _inboxes1 = small_history in
  let _level_tree2, history2, _inbox2, _inboxes2 = big_history in
  let hh0 = I.History.Internal_for_tests.keys history0 in
  let hh1 = I.History.Internal_for_tests.keys history1 in
  let hh2 = I.History.Internal_for_tests.keys history2 in
  (* The first history is supposed to have exactly 0 elements *)
  let* () =
    let len = List.length hh0 in
    fail_unless
      Int64.(equal no_capacity (of_int @@ len))
      (err no_capacity len ~exact:true)
  in
  (* The second history is supposed to have exactly [small_capacity], because
     we are supposed to add _nb_level > small_capacity entries. *)
  let* () =
    let len = List.length hh1 in
    fail_unless
      Int64.(small_capacity = of_int len)
      (err small_capacity len ~exact:false)
  in
  (* The third history's capacity, named [default_capacity], is supposed to be
     greater than _nb_level. So, we don't expect this history to be full. *)
  let* () =
    let len = List.length hh2 in
    fail_unless
      Int64.(default_capacity > of_int len)
      (err default_capacity len ~exact:true)
  in
  return ()

(** In this test, we check that for two inboxes of the same content, the entries
    of the history with the lower capacity, taken in the insertion order, is a
    prefix of the entries of the history with the higher capacity. *)
let test_history_prefix params =
  let open Lwt_result_syntax in
  let module I = Sc_rollup_inbox_repr in
  let* no_history, small_history, big_history =
    init_inboxes_histories_with_different_capacities params
  in
  let _level_tree0, history0, _inbox0, _inboxes0 = no_history in
  let _level_tree1, history1, _inbox1, _inboxes1 = small_history in
  let _level_tree2, history2, _inbox2, _inboxes2 = big_history in
  let hh0 = I.History.Internal_for_tests.keys history0 in
  let hh1 = I.History.Internal_for_tests.keys history1 in
  let hh2 = I.History.Internal_for_tests.keys history2 in
  let check_is_suffix sub super =
    let rec aux super to_remove =
      let* () =
        fail_unless
          (to_remove >= 0)
          (err "A bigger list cannot be a suffix of a smaller one.")
      in
      if to_remove = 0 then
        fail_unless
          (List.for_all2 ~when_different_lengths:false I.Hash.equal sub super
          = Ok true)
          (err "The smaller list is not a prefix the bigger one.")
      else
        match List.tl super with
        | None -> assert false
        | Some super -> aux super (to_remove - 1)
    in
    aux super (List.length super - List.length sub)
  in
  (* The empty history's hashes list is supposed to be a suffix of a history
     with bigger capacity. *)
  let* () = check_is_suffix hh0 hh1 in
  (* The history's hashes list of the smaller capacity should be a prefix of
     the history's hashes list of a bigger capacity. *)
  check_is_suffix hh1 hh2

(** In this test, we make some checks on production and verification of
    inclusion proofs depending on histories' capacity. *)
let test_inclusion_proofs_depending_on_history_capacity
    ((_nb_levels, _default_capacity, _small_capacity, _next_index) as params) =
  let open Lwt_result_syntax in
  let module I = Sc_rollup_inbox_repr in
  let* no_history, small_history, big_history =
    init_inboxes_histories_with_different_capacities params
  in
  let _level_tree0, history0, inbox0, _inboxes0 = no_history in
  let _level_tree1, history1, inbox1, _inboxes1 = small_history in
  let _level_tree2, history2, inbox2, _inboxes2 = big_history in
  let hp0 = I.old_levels_messages inbox0 in
  let hp1 = I.old_levels_messages inbox1 in
  let (hp2 as hp) = I.old_levels_messages inbox2 in
  let* () =
    fail_unless
      (I.equal_history_proof hp0 hp1 && I.equal_history_proof hp1 hp2)
      (err
         "History proof of equal inboxes shouldn't depend on the capacity of \
          history.")
  in
  let proof s v =
    let open Result_syntax in
    let* v = v |> Environment.wrap_tzresult in
    Option.to_result ~none:[err (s ^ ": Expecting some inclusion proof.")] v
  in
  (* Producing inclusion proofs using history1 and history2 should succeeed.
     But, we should not be able to produce any proof with history0 as bound
     is 0. *)
  let*? ip0 =
    I.Internal_for_tests.produce_inclusion_proof history0 hp hp
    |> Environment.wrap_tzresult
  in
  let*? ip1 =
    proof "history1"
    @@ I.Internal_for_tests.produce_inclusion_proof history1 hp hp
  in
  let*? ip2 =
    proof "history2"
    @@ I.Internal_for_tests.produce_inclusion_proof history2 hp hp
  in
  let* () =
    fail_unless
      (Option.is_none ip0)
      (err
         "Should not be able to get inbox inclusion proofs without a history \
          (i.e., a history with no capacity). ")
  in
  fail_unless
    (I.verify_inclusion_proof ip1 hp hp && I.verify_inclusion_proof ip2 hp hp)
    (err "Inclusion proofs are expected to be valid.")

(** In this test, we make sure that the snapshot of an inbox is taken
    at the beginning of a block level. *)
let test_inbox_snapshot_taking payloads =
  let open Lwt_result_syntax in
  let payloads = List.map make_payload payloads in
  create_context () >>=? fun ctxt ->
  let*! inbox = empty ctxt first_level in
  let inbox_level = inbox_level inbox in
  let expected_snapshot = take_snapshot inbox in
  (* Now, if we add messages to the inbox at [current_level], the inbox's
     snapshot for this level should not changed. *)
  let* _ =
    add_messages_no_history ctxt inbox inbox_level payloads None
    >|= Environment.wrap_tzresult
  in
  let new_snapshot = take_snapshot inbox in
  fail_unless
    (equal_history_proof expected_snapshot new_snapshot)
    (err
       "Adding messages in an inbox for a level should not modify the snapshot \
        when the current level is equal to the level where the messages are \
        added.")

(** This test checks that inboxes of the same levels that are supposed to contain
    the same messages are equal. It also check the level trees obtained from
    the last calls to add_messages are equal. *)
let test_for_successive_add_messages_with_different_histories_capacities
    ((_nb_levels, _default_capacity, _small_capacity, _next_index) as params) =
  let open Lwt_result_syntax in
  let module I = Sc_rollup_inbox_repr in
  let* no_history, small_history, big_history =
    init_inboxes_histories_with_different_capacities params
  in
  let level_tree0, _history0, _inbox0, inboxes0 = no_history in
  let level_tree1, _history1, _inbox1, inboxes1 = small_history in
  let level_tree2, _history2, _inbox2, inboxes2 = big_history in
  (* The latest inbox's value shouldn't depend on the value of [bound]. *)
  let eq_inboxes_list = List.for_all2 ~when_different_lengths:false I.equal in
  let* () =
    fail_unless
      (eq_inboxes_list inboxes0 inboxes1 = Ok true
      && eq_inboxes_list inboxes1 inboxes2 = Ok true)
      (err "Inboxes at the same level with the same content should be equal.")
  in
  fail_unless
    (Option.equal I.Internal_for_tests.eq_tree level_tree0 level_tree1
    && Option.equal I.Internal_for_tests.eq_tree level_tree1 level_tree2)
    (err "Trees of (supposedly) equal inboxes should be equal.")

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
      let* levels = 2 -- 15 in
      let* levels_and_messages =
        Sc_rollup_helpers.gen_message_reprs_for_levels_repr
          ~start_level:1
          ~max_level:levels
          bounded_string
      in
      let* l = 1 -- (levels - 1) in
      let l = level_of_int l in
      let messages_at_l = Stdlib.List.assoc l levels_and_messages in
      let* n = 0 -- List.length messages_at_l in
      return (levels_and_messages, l, Z.of_int n))
  in
  let gen_history_params =
    QCheck2.Gen.(
      (* We fix the number of levels/ inboxes. *)
      let* nb_levels = pure 30 in
      (* The default capacity is intentionally very big compared to [nb_levels]. *)
      let* default_capacity =
        frequencyl [(1, Int64.of_int (1000 * nb_levels)); (1, Int64.max_int)]
      in
      (* The small capacity is intended to be smaller than nb_levels
         (but greater than zero). *)
      let* small_capacity = 3 -- (nb_levels / 2) in
      let* next_index_delta = -5000 -- 5000 in
      let big_next_index = Int64.(add max_int (of_int next_index_delta)) in
      (* for the [next_index] counter of the history, we test both default values
         (i.e., 0L) and values close to [max_int]. *)
      let* next_index = frequencyl [(1, 0L); (1, big_next_index)] in
      return
        (nb_levels, default_capacity, Int64.of_int small_capacity, next_index))
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
      ~count:10
      ~name:"Checking inboxes history length"
      gen_history_params
      test_history_length;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:"Checking inboxes history content and order"
      gen_history_params
      test_history_prefix;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:"Checking inclusion proofs validity depending on history capacity"
      gen_history_params
      test_inclusion_proofs_depending_on_history_capacity;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:
        "Checking results of add_messages when histories have different \
         capacities"
      gen_history_params
      test_for_successive_add_messages_with_different_histories_capacities;
    Tztest.tztest_qcheck2
      ~count:10
      ~name:
        "Take snapshot is not impacted by messages added during the current \
         level"
      (let open QCheck2.Gen in
      let* payloads = list_size (1 -- 10) bounded_string in
      return payloads)
      test_inbox_snapshot_taking;
  ]
