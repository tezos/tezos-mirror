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

open Protocol
open Alpha_context

module Address = struct
  type t = Sc_rollup.Address.t

  let of_octez address =
    Tezos_crypto.Hashed.Smart_rollup_address.to_bytes address
    |> Protocol.Alpha_context.Sc_rollup.Address.of_bytes_exn

  let to_octez address =
    Protocol.Alpha_context.Sc_rollup.Address.to_bytes address
    |> Tezos_crypto.Hashed.Smart_rollup_address.of_bytes_exn
end

module State_hash = struct
  type t = Sc_rollup.State_hash.t

  let of_octez state_hash =
    Tezos_crypto.Hashed.Smart_rollup_state_hash.to_bytes state_hash
    |> Protocol.Alpha_context.Sc_rollup.State_hash.of_bytes_exn

  let to_octez state_hash =
    Protocol.Alpha_context.Sc_rollup.State_hash.to_bytes state_hash
    |> Tezos_crypto.Hashed.Smart_rollup_state_hash.of_bytes_exn
end

module Merkelized_payload_hashes_hash = struct
  type t = Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t

  let of_octez state_hash =
    Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash.to_bytes
      state_hash
    |> Protocol.Alpha_context.Sc_rollup.Inbox_merkelized_payload_hashes.Hash
       .of_bytes_exn

  let to_octez state_hash =
    Protocol.Alpha_context.Sc_rollup.Inbox_merkelized_payload_hashes.Hash
    .to_bytes
      state_hash
    |> Tezos_crypto.Hashed.Smart_rollup_merkelized_payload_hashes_hash
       .of_bytes_exn
end

module Commitment_hash = struct
  type t = Sc_rollup.Commitment.Hash.t

  let of_octez commitment_hash =
    Tezos_crypto.Hashed.Smart_rollup_commitment_hash.to_bytes commitment_hash
    |> Protocol.Alpha_context.Sc_rollup.Commitment.Hash.of_bytes_exn

  let to_octez commitment_hash =
    Protocol.Alpha_context.Sc_rollup.Commitment.Hash.to_bytes commitment_hash
    |> Tezos_crypto.Hashed.Smart_rollup_commitment_hash.of_bytes_exn
end

module Commitment = struct
  type t = Sc_rollup.Commitment.t

  let of_octez
      Octez_smart_rollup.Commitment.
        {compressed_state; inbox_level; predecessor; number_of_ticks} : t =
    {
      compressed_state = State_hash.of_octez compressed_state;
      inbox_level =
        Raw_level.of_int32 inbox_level
        |> WithExceptions.Result.to_exn_f ~error:(fun _trace ->
               Stdlib.failwith "Commitment.of_octez: invalid inbox_level");
      predecessor = Commitment_hash.of_octez predecessor;
      number_of_ticks =
        Sc_rollup.Number_of_ticks.of_value number_of_ticks
        |> WithExceptions.Option.to_exn_f ~none:(fun () ->
               Stdlib.failwith "Commitment.of_octez: invalid number_of_ticks");
    }

  let to_octez
      Sc_rollup.Commitment.
        {compressed_state; inbox_level; predecessor; number_of_ticks} :
      Octez_smart_rollup.Commitment.t =
    {
      compressed_state = State_hash.to_octez compressed_state;
      inbox_level = Raw_level.to_int32 inbox_level;
      predecessor = Commitment_hash.to_octez predecessor;
      number_of_ticks = Sc_rollup.Number_of_ticks.to_value number_of_ticks;
    }
end

module Inbox_hash = struct
  type t = Sc_rollup.Inbox.Hash.t

  let of_octez inbox_hash =
    Tezos_crypto.Hashed.Smart_rollup_inbox_hash.to_bytes inbox_hash
    |> Protocol.Alpha_context.Sc_rollup.Inbox.Hash.of_bytes_exn

  let to_octez inbox_hash =
    Protocol.Alpha_context.Sc_rollup.Inbox.Hash.to_bytes inbox_hash
    |> Tezos_crypto.Hashed.Smart_rollup_inbox_hash.of_bytes_exn
end

module Inbox = struct
  type t = Sc_rollup.Inbox.t

  type history_proof = Sc_rollup.Inbox.history_proof

  let to_repr inbox =
    inbox
    |> Data_encoding.Binary.to_string_exn Sc_rollup.Inbox.encoding
    |> Data_encoding.Binary.of_string_exn Sc_rollup_inbox_repr.encoding

  let of_repr inbox =
    inbox
    |> Data_encoding.Binary.to_string_exn Sc_rollup_inbox_repr.encoding
    |> Data_encoding.Binary.of_string_exn Sc_rollup.Inbox.encoding

  let of_octez (inbox : Octez_smart_rollup.Inbox.t) : t =
    inbox |> Octez_smart_rollup.Inbox.to_versioned
    |> Data_encoding.Binary.to_string_exn
         Octez_smart_rollup.Inbox.versioned_encoding
    |> Data_encoding.Binary.of_string_exn
         Sc_rollup_inbox_repr.versioned_encoding
    |> Sc_rollup_inbox_repr.of_versioned |> of_repr

  let to_octez (inbox : t) : Octez_smart_rollup.Inbox.t =
    inbox |> to_repr |> Sc_rollup_inbox_repr.to_versioned
    |> Data_encoding.Binary.to_string_exn
         Sc_rollup_inbox_repr.versioned_encoding
    |> Data_encoding.Binary.of_string_exn
         Octez_smart_rollup.Inbox.versioned_encoding
    |> Octez_smart_rollup.Inbox.of_versioned

  (* Workaround because history_proof encoding not in Alpha_context *)
  let proto_history_proof_encoding :
      Sc_rollup.Inbox.history_proof Data_encoding.t =
    let level_proof_encoding =
      let open Data_encoding in
      conv
        (fun {Sc_rollup.Inbox.hash; level} -> (hash, level))
        (fun (hash, level) -> {hash; level})
        (obj2
           (req "hash" Sc_rollup.Inbox_merkelized_payload_hashes.Hash.encoding)
           (req "level" Raw_level.encoding))
    in
    Sc_rollup.Inbox.Skip_list.encoding
      Sc_rollup.Inbox.Hash.encoding
      level_proof_encoding

  let history_proof_of_octez (hist : Octez_smart_rollup.Inbox.history_proof) :
      history_proof =
    hist
    |> Data_encoding.Binary.to_string_exn
         Octez_smart_rollup.Inbox.history_proof_encoding
    |> Data_encoding.Binary.of_string_exn proto_history_proof_encoding

  let history_proof_to_octez (hist : history_proof) :
      Octez_smart_rollup.Inbox.history_proof =
    hist
    |> Data_encoding.Binary.to_string_exn proto_history_proof_encoding
    |> Data_encoding.Binary.of_string_exn
         Octez_smart_rollup.Inbox.history_proof_encoding
end

module Dal = struct
  module Slot_index = struct
    type t = Dal.Slot_index.t

    let of_octez (i : Octez_smart_rollup.Dal.Slot_index.t) : t =
      match Dal.Slot_index.of_int_opt i with
      | None -> Format.ksprintf invalid_arg "Dal.Slot_index.of_octez: %d" i
      | Some i -> i

    let to_octez : t -> Octez_smart_rollup.Dal.Slot_index.t =
      Dal.Slot_index.to_int
  end

  module Page_index = struct
    type t = Dal.Page.Index.t

    let of_octez : Octez_smart_rollup.Dal.Page_index.t -> t = Fun.id

    let to_octez : t -> Octez_smart_rollup.Dal.Page_index.t = Fun.id
  end

  module Slot_header = struct
    type t = Dal.Slot.Header.t

    let of_octez
        Octez_smart_rollup.Dal.Slot_header.
          {id = {published_level; index}; commitment} : t =
      Dal.Slot.Header.
        {
          id =
            {
              published_level = Raw_level.of_int32_exn published_level;
              index = Slot_index.of_octez index;
            };
          commitment;
        }

    let to_octez Dal.Slot.Header.{id = {published_level; index}; commitment} :
        Octez_smart_rollup.Dal.Slot_header.t =
      Octez_smart_rollup.Dal.Slot_header.
        {
          id =
            {
              published_level = Raw_level.to_int32 published_level;
              index = Slot_index.to_octez index;
            };
          commitment;
        }
  end

  module Slot_history = struct
    type t = Dal.Slots_history.t

    let of_octez (h : Octez_smart_rollup.Dal.Slot_history.t) : t =
      h
      |> Data_encoding.Binary.to_bytes_exn
           Octez_smart_rollup.Dal.Slot_history.encoding
      |> Data_encoding.Binary.of_bytes_exn Dal.Slots_history.encoding

    let to_octez (h : t) : Octez_smart_rollup.Dal.Slot_history.t =
      h
      |> Data_encoding.Binary.to_bytes_exn Dal.Slots_history.encoding
      |> Data_encoding.Binary.of_bytes_exn
           Octez_smart_rollup.Dal.Slot_history.encoding
  end

  module Slot_history_cache = struct
    type t = Dal.Slots_history.History_cache.t

    let of_octez (h : Octez_smart_rollup.Dal.Slot_history_cache.t) : t =
      h
      |> Data_encoding.Binary.to_bytes_exn
           Octez_smart_rollup.Dal.Slot_history_cache.encoding
      |> Data_encoding.Binary.of_bytes_exn
           Dal.Slots_history.History_cache.encoding

    let to_octez (h : t) : Octez_smart_rollup.Dal.Slot_history_cache.t =
      h
      |> Data_encoding.Binary.to_bytes_exn
           Dal.Slots_history.History_cache.encoding
      |> Data_encoding.Binary.of_bytes_exn
           Octez_smart_rollup.Dal.Slot_history_cache.encoding
  end
end

module Game = struct
  type dissection_chunk = Sc_rollup.Game.dissection_chunk

  type step = Sc_rollup.Game.step

  type refutation = Sc_rollup.Game.refutation

  type index = Sc_rollup.Game.Index.t

  type player = Sc_rollup.Game.player

  type game_state = Sc_rollup.Game.game_state

  type t = Sc_rollup.Game.t

  type conflict = Sc_rollup.Refutation_storage.conflict

  let dissection_chunk_of_octez Octez_smart_rollup.Game.{state_hash; tick} :
      dissection_chunk =
    {
      state_hash = Option.map State_hash.of_octez state_hash;
      tick = Sc_rollup.Tick.of_z tick;
    }

  let dissection_chunk_to_octez Sc_rollup.Dissection_chunk.{state_hash; tick} :
      Octez_smart_rollup.Game.dissection_chunk =
    {
      state_hash = Option.map State_hash.to_octez state_hash;
      tick = Sc_rollup.Tick.to_z tick;
    }

  let step_of_octez (step : Octez_smart_rollup.Game.step) : step =
    match step with
    | Dissection chunks ->
        Dissection (List.map dissection_chunk_of_octez chunks)
    | Proof serialized_proof ->
        let proof =
          Data_encoding.Binary.of_string
            Sc_rollup.Proof.encoding
            serialized_proof
          |> WithExceptions.Result.to_exn_f ~error:(fun err ->
                 Format.kasprintf
                   Stdlib.failwith
                   "Game.step_of_octez: cannot deserialize proof\n"
                   Data_encoding.Binary.pp_read_error
                   err)
        in
        Proof proof

  let step_to_octez (step : step) : Octez_smart_rollup.Game.step =
    match step with
    | Dissection chunks ->
        Dissection (List.map dissection_chunk_to_octez chunks)
    | Proof serialized_proof ->
        let proof =
          Data_encoding.Binary.to_string_exn
            Sc_rollup.Proof.encoding
            serialized_proof
        in
        Proof proof

  let refutation_of_octez (refutation : Octez_smart_rollup.Game.refutation) :
      refutation =
    match refutation with
    | Start {player_commitment_hash; opponent_commitment_hash} ->
        Start
          {
            player_commitment_hash =
              Commitment_hash.of_octez player_commitment_hash;
            opponent_commitment_hash =
              Commitment_hash.of_octez opponent_commitment_hash;
          }
    | Move {choice; step} ->
        Move {choice = Sc_rollup.Tick.of_z choice; step = step_of_octez step}

  let refutation_to_octez (refutation : refutation) :
      Octez_smart_rollup.Game.refutation =
    match refutation with
    | Start {player_commitment_hash; opponent_commitment_hash} ->
        Start
          {
            player_commitment_hash =
              Commitment_hash.to_octez player_commitment_hash;
            opponent_commitment_hash =
              Commitment_hash.to_octez opponent_commitment_hash;
          }
    | Move {choice; step} ->
        Move {choice = Sc_rollup.Tick.to_z choice; step = step_to_octez step}

  let index_of_octez Octez_smart_rollup.Game.{alice; bob} =
    Sc_rollup.Game.Index.make alice bob

  let index_to_octez Sc_rollup.Game.Index.{alice; bob} =
    Octez_smart_rollup.Game.make_index alice bob

  let player_of_octez : Octez_smart_rollup.Game.player -> player = function
    | Alice -> Alice
    | Bob -> Bob

  let player_to_octez : player -> Octez_smart_rollup.Game.player = function
    | Alice -> Alice
    | Bob -> Bob

  let game_state_of_octez : Octez_smart_rollup.Game.game_state -> game_state =
    function
    | Dissecting {dissection; default_number_of_sections} ->
        Dissecting
          {
            dissection = List.map dissection_chunk_of_octez dissection;
            default_number_of_sections;
          }
    | Final_move {agreed_start_chunk; refuted_stop_chunk} ->
        Final_move
          {
            agreed_start_chunk = dissection_chunk_of_octez agreed_start_chunk;
            refuted_stop_chunk = dissection_chunk_of_octez refuted_stop_chunk;
          }

  let game_state_to_octez : game_state -> Octez_smart_rollup.Game.game_state =
    function
    | Dissecting {dissection; default_number_of_sections} ->
        Dissecting
          {
            dissection = List.map dissection_chunk_to_octez dissection;
            default_number_of_sections;
          }
    | Final_move {agreed_start_chunk; refuted_stop_chunk} ->
        Final_move
          {
            agreed_start_chunk = dissection_chunk_to_octez agreed_start_chunk;
            refuted_stop_chunk = dissection_chunk_to_octez refuted_stop_chunk;
          }

  let of_octez
      Octez_smart_rollup.Game.
        {
          turn;
          inbox_snapshot;
          dal_snapshot;
          start_level;
          inbox_level;
          game_state;
        } : t =
    {
      turn = player_of_octez turn;
      inbox_snapshot = Inbox.history_proof_of_octez inbox_snapshot;
      dal_snapshot = Dal.Slot_history.of_octez dal_snapshot;
      start_level = Raw_level.of_int32_exn start_level;
      inbox_level = Raw_level.of_int32_exn inbox_level;
      game_state = game_state_of_octez game_state;
    }

  let to_octez
      Sc_rollup.Game.
        {
          turn;
          inbox_snapshot;
          dal_snapshot;
          start_level;
          inbox_level;
          game_state;
        } : Octez_smart_rollup.Game.t =
    {
      turn = player_to_octez turn;
      inbox_snapshot = Inbox.history_proof_to_octez inbox_snapshot;
      dal_snapshot = Dal.Slot_history.to_octez dal_snapshot;
      start_level = Raw_level.to_int32 start_level;
      inbox_level = Raw_level.to_int32 inbox_level;
      game_state = game_state_to_octez game_state;
    }

  let conflict_of_octez
      Octez_smart_rollup.Game.
        {other; their_commitment; our_commitment; parent_commitment} : conflict
      =
    {
      other;
      their_commitment = Commitment.of_octez their_commitment;
      our_commitment = Commitment.of_octez our_commitment;
      parent_commitment = Commitment_hash.of_octez parent_commitment;
    }

  let conflict_to_octez
      Sc_rollup.Refutation_storage.
        {other; their_commitment; our_commitment; parent_commitment} :
      Octez_smart_rollup.Game.conflict =
    {
      other;
      their_commitment = Commitment.to_octez their_commitment;
      our_commitment = Commitment.to_octez our_commitment;
      parent_commitment = Commitment_hash.to_octez parent_commitment;
    }
end

module Kind = struct
  type t = Sc_rollup.Kind.t

  let of_octez : Octez_smart_rollup.Kind.t -> t = function
    | Example_arith -> Example_arith
    | Wasm_2_0_0 -> Wasm_2_0_0
    | Riscv -> invalid_arg "Riscv rollup is inactive in this protocol"

  let to_octez : t -> Octez_smart_rollup.Kind.t = function
    | Example_arith -> Example_arith
    | Wasm_2_0_0 -> Wasm_2_0_0
end
