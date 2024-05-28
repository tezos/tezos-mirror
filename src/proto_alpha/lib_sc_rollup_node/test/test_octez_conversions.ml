(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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
    Component:  Smart rollup node library, type conversions
    Invocation: dune exec src/proto_alpha/lib_sc_rollup_node/test/main.exe \
                -- -f test_octez_conversions.ml
    Subject:    Ensure conversions between octez smart rollup structures and
                protocol ones are bijective.
*)

open Qcheck2_helpers
open Octez_smart_rollup

let gen_hash ~size of_bytes =
  let open QCheck2.Gen in
  let gen =
    let* x = bytes_size (pure size) in
    return @@ of_bytes x
  in
  (* This is not beautiful, but there is currently no other way to
     remove the shrinker. *)
  make_primitive
    ~gen:(fun rand -> generate1 ~rand gen)
    ~shrink:(fun _ -> Seq.empty)

let gen_address = gen_hash ~size:Address.size Address.of_bytes_exn

let gen_state_hash = gen_hash ~size:State_hash.size State_hash.of_bytes_exn

let gen_inbox_hash = gen_hash ~size:Inbox.Hash.size Inbox.Hash.of_bytes_exn

let gen_commitment_hash =
  gen_hash ~size:Commitment.Hash.size Commitment.Hash.of_bytes_exn

let gen_payload_hash =
  gen_hash
    ~size:Merkelized_payload_hashes_hash.size
    Merkelized_payload_hashes_hash.of_bytes_exn

let gen_kind = QCheck2.Gen.oneofl Kind.[Example_arith; Wasm_2_0_0]

let gen_level = int32_range_gen 0l Int32.max_int

let uint64 = int64_range_gen 0L Int64.max_int

let gen_tick =
  let open QCheck2.Gen in
  let+ i = uint64 in
  Z.of_int64 i

let gen_commitment =
  let open QCheck2.Gen in
  let* compressed_state = gen_state_hash in
  let* inbox_level = gen_level in
  let* predecessor = gen_commitment_hash in
  let+ number_of_ticks = uint64 in
  Octez_smart_rollup.Commitment.
    {compressed_state; inbox_level; predecessor; number_of_ticks}

let gen_dissection_chunk =
  let open QCheck2.Gen in
  let* state_hash = option gen_state_hash in
  let+ tick = gen_tick in
  Octez_smart_rollup.Game.{state_hash; tick}

let gen_dissection = QCheck2.Gen.small_list gen_dissection_chunk

let gen_proof =
  let open QCheck2.Gen in
  let+ hex = oneofl Serialized_proofs.proofs in
  Hex.to_string (`Hex hex) |> WithExceptions.Option.get ~loc:__LOC__

let gen_step =
  let open QCheck2.Gen in
  let* what = option ~ratio:0.1 (pure ()) in
  match what with
  | None ->
      (* shrink there *)
      let+ dissection = gen_dissection in
      Octez_smart_rollup.Game.Dissection dissection
  | Some () ->
      let+ proof = gen_proof in
      Octez_smart_rollup.Game.Proof proof

let random_seed ~rng_state =
  Bytes.init Tezos_crypto.Hacl.Ed25519.sk_size (fun _i ->
      Char.chr (Random.State.int rng_state 256))

let random_algo ~rng_state : Signature.algo =
  match Random.State.int rng_state 3 with
  | 0 -> Ed25519
  | 1 -> Secp256k1
  | 2 -> P256
  | 3 -> Bls
  | _ -> assert false

let gen_algo = QCheck2.Gen.oneofl [Signature.Ed25519; Secp256k1; P256; Bls]

let gen_pkh =
  let open QCheck2.Gen in
  let+ algo = gen_algo in
  let pkh, _pk, _sk = Signature.generate_key ~algo () in
  pkh

let gen_stakers =
  let open QCheck2.Gen in
  let* p1 = gen_pkh in
  let+ p2 = gen_pkh in
  Octez_smart_rollup.Game.make_index p1 p2

let gen_refutation =
  let open QCheck2.Gen in
  let* b = bool in
  match b with
  | true ->
      let* player_commitment_hash = gen_commitment_hash in
      let+ opponent_commitment_hash = gen_commitment_hash in
      Octez_smart_rollup.Game.Start
        {player_commitment_hash; opponent_commitment_hash}
  | false ->
      let* choice = gen_tick in
      let+ step = gen_step in
      Octez_smart_rollup.Game.Move {choice; step}

let gen_inbox =
  let open Protocol in
  let open Alpha_context in
  let open QCheck2.Gen in
  let gen_msg = small_string ~gen:printable in
  let* hd = gen_msg in
  let* tail = small_list gen_msg in
  let payloads = hd :: tail in
  let* level = gen_level in
  let level = Raw_level.of_int32_exn level in
  let witness_and_inbox =
    let open Result_syntax in
    let inbox =
      Sc_rollup.Inbox.genesis
        ~predecessor_timestamp:Time.Protocol.epoch
        ~predecessor:Block_hash.zero
        level
    in
    Environment.wrap_tzresult
    @@
    let witness = Sc_rollup.Inbox.init_witness_no_history in
    let witness =
      Sc_rollup.Inbox.add_info_per_level_no_history
        ~predecessor_timestamp:Time.Protocol.epoch
        ~predecessor:Block_hash.zero
        witness
    in
    let* input_messages =
      List.map_e
        (fun msg -> Sc_rollup.Inbox_message.(serialize (External msg)))
        payloads
    in
    let* witness =
      Sc_rollup.Inbox.add_messages_no_history input_messages witness
    in
    return (Sc_rollup.Inbox.finalize_inbox_level_no_history inbox witness)
  in
  return
  @@ (witness_and_inbox |> function
      | Ok v -> Sc_rollup_proto_types.Inbox.to_octez v
      | Error e ->
          Stdlib.failwith (Format.asprintf "%a" Error_monad.pp_print_trace e))

let number_of_slots = 256

let gen_slot_index =
  let open QCheck2.Gen in
  let max = number_of_slots - 1 in
  graft_corners (int_bound max) [0; 1; 2; max] ()

let gen_page_index =
  let open QCheck2.Gen in
  let max = 0xfff / 2 in
  graft_corners (int_bound max) [0; 1; 2; max] ()

let gen_slot_header_commitment =
  let open QCheck2.Gen in
  make_primitive
    ~gen:(fun state ->
      Tezos_crypto_dal.Cryptobox.Internal_for_tests.dummy_commitment ~state ())
    ~shrink:(fun _ -> Seq.empty)

let gen_slot_header =
  let open QCheck2.Gen in
  let* published_level = gen_level in
  let* index = gen_slot_index in
  let+ commitment = gen_slot_header_commitment in
  Octez_smart_rollup.Dal.Slot_header.{id = {published_level; index}; commitment}

let compare_slot_header_id (s1 : Octez_smart_rollup.Dal.Slot_header.id)
    (s2 : Octez_smart_rollup.Dal.Slot_header.id) =
  let c = Int32.compare s1.published_level s2.published_level in
  if c <> 0 then c else Int.compare s1.index s2.index

let gen_slot_headers =
  let open QCheck2.Gen in
  let size = int_bound 50 in
  let+ l = list_size size gen_slot_header in
  List.sort
    (fun (h1 : Octez_smart_rollup.Dal.Slot_header.t)
         (h2 : Octez_smart_rollup.Dal.Slot_header.t) ->
      compare_slot_header_id h1.id h2.id)
    l
  |> fun l ->
  match l with
  | [] -> []
  | (h : Octez_smart_rollup.Dal.Slot_header.t) :: _ ->
      let min_level = h.id.published_level in
      (* smallest level *)
      List.mapi
        (fun i (h : Octez_smart_rollup.Dal.Slot_header.t) ->
          (* patch the published level to comply with the invariants *)
          let published_level = Int32.(add min_level (of_int i)) in
          let h = {h with id = {h.id with published_level}} in
          (published_level, [h]))
        l

let gen_slot_history =
  let open Protocol.Alpha_context in
  let open QCheck2.Gen in
  let+ l = gen_slot_headers in
  let l =
    List.map
      (fun (lvl, h) ->
        ( Raw_level.of_int32_exn lvl,
          List.map
            (Sc_rollup_proto_types.Dal.Slot_header.of_octez ~number_of_slots)
            h ))
      l
  in
  List.fold_left_e
    (fun hist (published_level, attested_slots) ->
      Dal.Slots_history.add_confirmed_slot_headers_no_cache
        ~number_of_slots
        hist
        published_level
        attested_slots)
    Dal.Slots_history.genesis
    l
  |> function
  | Error e ->
      Stdlib.failwith (Format.asprintf "%a" Environment.Error_monad.pp_trace e)
  | Ok v -> Sc_rollup_proto_types.Dal.Slot_history.to_octez v

let gen_slot_history_cache =
  let open Protocol.Alpha_context in
  let open QCheck2.Gen in
  let+ l = gen_slot_headers in
  let cache = Dal.Slots_history.History_cache.empty ~capacity:Int64.max_int in
  let l =
    List.map
      (fun (lvl, h) ->
        ( Raw_level.of_int32_exn lvl,
          List.map
            (Sc_rollup_proto_types.Dal.Slot_header.of_octez ~number_of_slots)
            h ))
      l
  in
  List.fold_left_e
    (fun (hist, cache) (published_level, attested_slots) ->
      Dal.Slots_history.add_confirmed_slot_headers
        ~number_of_slots
        hist
        cache
        published_level
        attested_slots)
    (Dal.Slots_history.genesis, cache)
    l
  |> function
  | Error e ->
      Stdlib.failwith (Format.asprintf "%a" Environment.Error_monad.pp_trace e)
  | Ok (_, c) -> Sc_rollup_proto_types.Dal.Slot_history_cache.to_octez c

let test_roundtrip ~count name gen to_octez from_octez octez_encoding
    proto_encoding =
  let test octez1 =
    try
      let proto1 = from_octez octez1 in
      let octez2 = to_octez proto1 in
      let proto2 = from_octez octez2 in
      let check version enc v1 v2 =
        let b1 = Data_encoding.Binary.to_bytes_exn enc v1 in
        let b2 = Data_encoding.Binary.to_bytes_exn enc v2 in
        if not (Bytes.equal b1 b2) then
          QCheck2.Test.fail_reportf
            "%s-%s not identical after roundtrip conversion"
            name
            version
      in
      check "protocol" proto_encoding proto1 proto2 ;
      check "octez" octez_encoding octez1 octez2 ;
      true
    with exn ->
      QCheck2.Test.fail_reportf
        "%s roundtrip conversion error: %s"
        name
        (Printexc.to_string exn)
  in
  let print v =
    Data_encoding.Json.construct octez_encoding v
    |> Data_encoding.Json.to_string ~minify:false
  in
  QCheck2.Test.make
    ~count
    ~print
    ~name:(Format.asprintf "roundtrip %s" name)
    gen
    test

let test_address =
  test_roundtrip
    ~count:1000
    "address"
    gen_address
    Sc_rollup_proto_types.Address.to_octez
    Sc_rollup_proto_types.Address.of_octez
    Octez_smart_rollup.Address.encoding
    Protocol.Alpha_context.Sc_rollup.Address.encoding

let test_state_hash =
  test_roundtrip
    ~count:1000
    "state_hash"
    gen_state_hash
    Sc_rollup_proto_types.State_hash.to_octez
    Sc_rollup_proto_types.State_hash.of_octez
    Octez_smart_rollup.State_hash.encoding
    Protocol.Alpha_context.Sc_rollup.State_hash.encoding

let test_payload_hash =
  test_roundtrip
    ~count:1000
    "payload_hash"
    gen_payload_hash
    Sc_rollup_proto_types.Merkelized_payload_hashes_hash.to_octez
    Sc_rollup_proto_types.Merkelized_payload_hashes_hash.of_octez
    Octez_smart_rollup.Merkelized_payload_hashes_hash.encoding
    Protocol.Alpha_context.Sc_rollup.Inbox_merkelized_payload_hashes.Hash
    .encoding

let test_commitment_hash =
  test_roundtrip
    ~count:1000
    "commitment_hash"
    gen_commitment_hash
    Sc_rollup_proto_types.Commitment_hash.to_octez
    Sc_rollup_proto_types.Commitment_hash.of_octez
    Octez_smart_rollup.Commitment.Hash.encoding
    Protocol.Alpha_context.Sc_rollup.Commitment.Hash.encoding

let test_commitment =
  test_roundtrip
    ~count:1000
    "commitment"
    gen_commitment
    Sc_rollup_proto_types.Commitment.to_octez
    Sc_rollup_proto_types.Commitment.of_octez
    Octez_smart_rollup.Commitment.encoding
    Protocol.Alpha_context.Sc_rollup.Commitment.encoding

let test_stakers =
  test_roundtrip
    ~count:1000
    "stakers"
    gen_stakers
    Sc_rollup_proto_types.Game.index_to_octez
    Sc_rollup_proto_types.Game.index_of_octez
    Octez_smart_rollup.Game.index_encoding
    Protocol.Alpha_context.Sc_rollup.Game.Index.encoding

let test_refutation =
  test_roundtrip
    ~count:1000
    "refutation"
    gen_refutation
    Sc_rollup_proto_types.Game.refutation_to_octez
    Sc_rollup_proto_types.Game.refutation_of_octez
    Octez_smart_rollup.Game.refutation_encoding
    Protocol.Alpha_context.Sc_rollup.Game.refutation_encoding

let test_inbox =
  test_roundtrip
    ~count:1000
    "inbox"
    gen_inbox
    Sc_rollup_proto_types.Inbox.to_octez
    Sc_rollup_proto_types.Inbox.of_octez
    Octez_smart_rollup.Inbox.encoding
    Protocol.Alpha_context.Sc_rollup.Inbox.encoding

let test_slot_index =
  test_roundtrip
    ~count:100
    "dal_slot_index"
    gen_slot_index
    Sc_rollup_proto_types.Dal.Slot_index.to_octez
    (Sc_rollup_proto_types.Dal.Slot_index.of_octez ~number_of_slots)
    Octez_smart_rollup.Dal.Slot_index.encoding
    Protocol.Alpha_context.Dal.Slot_index.encoding

let test_page_index =
  test_roundtrip
    ~count:100
    "dal_page_index"
    gen_page_index
    Sc_rollup_proto_types.Dal.Page_index.to_octez
    Sc_rollup_proto_types.Dal.Page_index.of_octez
    Octez_smart_rollup.Dal.Page_index.encoding
    Protocol.Alpha_context.Dal.Page.Index.encoding

let test_slot_header =
  test_roundtrip
    ~count:1000
    "dal_slot_header"
    gen_slot_header
    Sc_rollup_proto_types.Dal.Slot_header.to_octez
    (Sc_rollup_proto_types.Dal.Slot_header.of_octez ~number_of_slots)
    Octez_smart_rollup.Dal.Slot_header.encoding
    Protocol.Alpha_context.Dal.Slot.Header.encoding

let test_slot_history =
  test_roundtrip
    ~count:300
    "dal_slot_history"
    gen_slot_history
    Sc_rollup_proto_types.Dal.Slot_history.to_octez
    Sc_rollup_proto_types.Dal.Slot_history.of_octez
    Octez_smart_rollup.Dal.Slot_history.encoding
    Protocol.Alpha_context.Dal.Slots_history.encoding

let test_slot_history_cache =
  test_roundtrip
    ~count:300
    "dal_slot_history_cache"
    gen_slot_history_cache
    Sc_rollup_proto_types.Dal.Slot_history_cache.to_octez
    Sc_rollup_proto_types.Dal.Slot_history_cache.of_octez
    Octez_smart_rollup.Dal.Slot_history_cache.encoding
    Protocol.Alpha_context.Dal.Slots_history.History_cache.encoding

let tests =
  [
    test_address;
    test_state_hash;
    test_payload_hash;
    test_commitment_hash;
    test_commitment;
    test_stakers;
    test_refutation;
    test_inbox;
    test_slot_index;
    test_page_index;
    test_slot_header;
    test_slot_history;
    test_slot_history_cache;
  ]

let () =
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": Smart rollup types octez conversions")
    [("roundtrip", qcheck_wrap tests)]
