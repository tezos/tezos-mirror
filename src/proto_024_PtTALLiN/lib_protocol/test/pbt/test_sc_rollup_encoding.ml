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
    Component:    Protocol Library
    Invocation:   dune exec src/proto_024_PtTALLiN/lib_protocol/test/pbt/main.exe \
                  -- --file test_sc_rollup_encoding.ml
    Subject:      SC rollup encoding
*)

open Protocol
open QCheck2
open Qcheck2_helpers

(** {2 Generators} *)

let gen_state_hash =
  let open Gen in
  let* bytes = bytes_fixed_gen Sc_rollup_repr.State_hash.size in
  return (Sc_rollup_repr.State_hash.of_bytes_exn bytes)

let gen_inbox_level =
  let open Gen in
  let* level = map Int32.abs int32 in
  (* There is no inbox for level [0l]. *)
  let level = if level = 0l then 1l else level in
  return (Raw_level_repr.of_int32_exn level)

let gen_start_level =
  let open Gen in
  let* level = map Int32.abs int32 in
  let start_level = Raw_level_repr.of_int32_exn level in
  return start_level

let gen_commitment_hash =
  let open Gen in
  let* bytes = bytes_fixed_gen Sc_rollup_commitment_repr.Hash.size in
  return (Sc_rollup_commitment_repr.Hash.of_bytes_exn bytes)

let gen_number_of_ticks =
  let open Gen in
  let open Sc_rollup_repr.Number_of_ticks in
  let* v = int64_range_gen min_value max_value in
  return (WithExceptions.Option.get ~loc:__LOC__ (of_value v))

let gen_commitment =
  let open Gen in
  let* compressed_state = gen_state_hash
  and* inbox_level = gen_inbox_level
  and* predecessor = gen_commitment_hash
  and* number_of_ticks = gen_number_of_ticks in
  return
    Sc_rollup_commitment_repr.
      {compressed_state; inbox_level; predecessor; number_of_ticks}

let gen_versioned_commitment =
  let open Gen in
  let* commitment = gen_commitment in
  return (Sc_rollup_commitment_repr.to_versioned commitment)

let gen_player = Gen.oneofl Sc_rollup_game_repr.[Alice; Bob]

let gen_inbox level =
  let open Gen in
  let gen_msg = small_string ~gen:printable in
  let* hd = gen_msg in
  let* tail = small_list gen_msg in
  let payloads = hd :: tail in
  let witness_and_inbox =
    let open Result_wrap_syntax in
    let inbox = Sc_rollup_helpers.dumb_init_repr level in
    wrap
    @@
    let witness = Sc_rollup_inbox_repr.init_witness_no_history in
    let witness =
      Sc_rollup_inbox_repr.add_info_per_level_no_history
        ~predecessor_timestamp:Time.Protocol.epoch
        ~predecessor:Block_hash.zero
        witness
    in
    let* input_messages =
      List.map_e
        (fun msg -> Sc_rollup_inbox_message_repr.(serialize (External msg)))
        payloads
    in
    let* witness =
      Sc_rollup_inbox_repr.add_messages_no_history input_messages witness
    in
    return (Sc_rollup_inbox_repr.finalize_inbox_level_no_history inbox witness)
  in
  return
  @@ ( witness_and_inbox |> function
       | Ok v -> v
       | Error e ->
           Stdlib.failwith (Format.asprintf "%a" Error_monad.pp_print_trace e)
     )

module Index = Dal_slot_index_repr

let pack_slots_headers_by_level list =
  let module ML = Map.Make (Raw_level_repr) in
  let module SSH = Set.Make (struct
    type t =
      Dal_slot_repr.Header.t
      * Contract_repr.t
      * Dal_attestation_repr.Accountability.attestation_status

    let compare (a, _, _) (b, _, _) =
      let open Dal_slot_repr.Header in
      Dal_slot_index_repr.compare a.id.index b.id.index
  end) in
  let map =
    List.fold_left
      (fun map
           (( Dal_slot_repr.Header.{id = {published_level; _}; _},
              _publisher,
              _status ) as sh)
         ->
        let l =
          ML.find published_level map |> Option.value ~default:SSH.empty
        in
        ML.add published_level (SSH.add sh l) map)
      ML.empty
      list
  in
  match ML.max_binding_opt map with
  | None -> [] (* map is empty *)
  | Some (max_level, _) ->
      let rec loop counter map =
        if Raw_level_repr.(counter >= max_level) then map
        else
          let counter = Raw_level_repr.succ counter in
          let map =
            if ML.mem counter map then map else ML.add counter SSH.empty map
          in
          loop counter map
      in
      loop Raw_level_repr.root map
      |> ML.bindings
      |> List.map (fun (k, ssh) -> (k, SSH.elements ssh))

let gen_pkh =
  let pkh, _, _ = Signature.generate_key ~algo:Ed25519 () in
  Gen.return pkh

let gen_dal_slots_history () =
  let open Gen in
  let open Dal_slot_repr in
  let constants : Alpha_context.Constants.Parametric.t =
    Tezos_protocol_024_PtTALLiN_parameters.Default_parameters.constants_test
  in
  let number_of_slots = constants.dal.number_of_slots in
  (* Generate a list of (level * confirmed slot ID * public key hash *
     attestation flag). *)
  let* list = small_list (quad small_nat small_nat gen_pkh bool) in
  let list =
    List.rev_map
      (fun (level, slot_index, publisher, is_proto_attested) ->
        let attestation_status =
          Dal_attestation_repr.Accountability.
            {
              attested_shards = (if is_proto_attested then 1 else 0);
              total_shards = 1;
              is_proto_attested;
            }
        in
        let published_level =
          Raw_level_repr.(
            (* use succ to avoid having a published_level = 0, as it's the
               genesis cell's level in the skip list. *)
            succ @@ try of_int32_exn (Int32.of_int level) with _ -> root)
        in
        let index =
          Index.of_int_opt ~number_of_slots slot_index
          |> Option.value ~default:Index.zero
        in
        ( Header.{id = {published_level; index}; commitment = Commitment.zero},
          Contract_repr.Implicit publisher,
          attestation_status ))
      list
  in
  let rec loop history = function
    | [] -> return history
    | (published_level, slot_headers) :: llist -> (
        let slot_headers =
          (* Sort the list in the right ordering before adding slots to slots_history. *)
          List.sort_uniq
            (fun ({Header.id = a; _}, _publisher, _status)
                 ({id = b; _}, _publisher, _status)
               ->
              let c =
                Raw_level_repr.compare a.published_level b.published_level
              in
              if c <> 0 then c else Index.compare a.index b.index)
            slot_headers
        in
        History.(
          update_skip_list_no_cache
            ~number_of_slots
            history
            ~published_level
            slot_headers)
        |> function
        | Ok history -> loop history llist
        | Error e ->
            return
            @@ Stdlib.failwith
                 (Format.asprintf "%a" Error_monad.pp_print_trace
                 @@ Environment.wrap_tztrace e))
  in
  pack_slots_headers_by_level list |> loop History.genesis

let gen_inbox_history_proof inbox_level =
  let open Gen in
  let* inbox = gen_inbox inbox_level in
  return (Sc_rollup_inbox_repr.take_snapshot inbox)

let gen_tick =
  let open Gen in
  let* t = small_nat in
  match Sc_rollup_tick_repr.of_int t with
  | None -> assert false
  | Some r -> return r

let gen_dissection_chunk =
  let open Gen in
  let* state_hash = opt gen_state_hash in
  let+ tick = gen_tick in
  Sc_rollup_dissection_chunk_repr.{state_hash; tick}

let gen_dissection =
  let open Gen in
  small_list gen_dissection_chunk

let gen_game_state =
  let open Sc_rollup_game_repr in
  let open Gen in
  let gen_dissecting =
    let* dissection = gen_dissection in
    let+ default_number_of_sections = int_range 4 100 in
    Dissecting {dissection; default_number_of_sections}
  in
  let gen_final_move =
    let* agreed_start_chunk = gen_dissection_chunk in
    let+ refuted_stop_chunk = gen_dissection_chunk in
    Final_move {agreed_start_chunk; refuted_stop_chunk}
  in
  oneof [gen_dissecting; gen_final_move]

let gen_game =
  let open Gen in
  let* turn = gen_player in
  let* inbox_level = gen_inbox_level in
  let* start_level = gen_start_level in
  let* inbox_snapshot = gen_inbox_history_proof inbox_level in
  let* dal_snapshot = gen_dal_slots_history () in
  let* game_state = gen_game_state in
  return
    Sc_rollup_game_repr.
      {turn; dal_snapshot; inbox_snapshot; start_level; inbox_level; game_state}

let gen_conflict =
  let open Gen in
  let other = Sc_rollup_repr.Staker.zero in
  let* their_commitment = gen_commitment in
  let* our_commitment = gen_commitment in
  let* parent_commitment = gen_commitment_hash in
  return
    Sc_rollup_refutation_storage.
      {other; their_commitment; our_commitment; parent_commitment}

let gen_rollup =
  let open QCheck2.Gen in
  let* bytes = bytes_fixed_gen Sc_rollup_repr.Address.size in
  return (Sc_rollup_repr.Address.hash_bytes [bytes])

let gen_inbox_message =
  let open Gen in
  let open Sc_rollup_inbox_message_repr in
  let gen_external =
    let+ s = small_string ~gen:printable in
    External s
  in
  let gen_sol = return (Internal Start_of_level) in
  let gen_eol = return (Internal End_of_level) in
  let gen_deposit =
    (* We won't test the encoding of these values. It's out of scope. *)
    let payload = Script_repr.unit in
    let sender = Contract_hash.zero in
    let source = Signature.Public_key_hash.zero in
    (* But the encoding of the rollup's address is our problem. *)
    let+ destination = gen_rollup in
    Internal (Transfer {payload; sender; source; destination})
  in
  oneof [gen_external; gen_sol; gen_eol; gen_deposit]

(** {2 Tests} *)

let test_commitment =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_commitment.t"
    ~gen:gen_commitment
    ~eq:( = )
    Sc_rollup_commitment_repr.encoding

let test_versioned_commitment =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_commitment.versioned"
    ~gen:gen_versioned_commitment
    ~eq:( = )
    Sc_rollup_commitment_repr.versioned_encoding

let test_game =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_game.t"
    ~gen:gen_game
    ~eq:Sc_rollup_game_repr.equal
    Sc_rollup_game_repr.encoding

let test_conflict =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_refutation_storage.conflict"
    ~gen:gen_conflict
    ~eq:( = )
    Sc_rollup_refutation_storage.conflict_encoding

let test_inbox_message =
  test_roundtrip
    ~count:1_000
    ~title:"Sc_rollup_inbox_message_repr.t"
    ~gen:gen_inbox_message
    ~eq:( = )
    Sc_rollup_inbox_message_repr.encoding

let tests =
  [
    test_commitment;
    test_versioned_commitment;
    test_game;
    test_conflict;
    test_inbox_message;
  ]

let () =
  Alcotest.run
    ~__FILE__
    (Protocol.name ^ ": SC rollup encoding")
    [(": roundtrip", qcheck_wrap tests)]
