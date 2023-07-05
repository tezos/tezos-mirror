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

  (* Same as octez version through environment *)

  let of_octez = Fun.id

  let to_octez = Fun.id
end

module State_hash = struct
  type t = Sc_rollup.State_hash.t

  (* Same as octez version through environment *)

  let of_octez = Fun.id

  let to_octez = Fun.id
end

module Merkelized_payload_hashes_hash = struct
  type t = Sc_rollup.Inbox_merkelized_payload_hashes.Hash.t

  (* Same as octez version through environment *)

  let of_octez = Fun.id

  let to_octez = Fun.id
end

module Commitment_hash = struct
  type t = Sc_rollup.Commitment.Hash.t

  (* Same as octez version through environment *)

  let of_octez = Fun.id

  let to_octez = Fun.id
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

  (* Same as octez version through environment *)

  let of_octez = Fun.id

  let to_octez = Fun.id
end

module Inbox = struct
  type t = Sc_rollup.Inbox.t

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
end

module Game = struct
  type dissection_chunk = Sc_rollup.Game.dissection_chunk

  type step = Sc_rollup.Game.step

  type refutation = Sc_rollup.Game.refutation

  type index = Sc_rollup.Game.Index.t

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
end

module Kind = struct
  type t = Sc_rollup.Kind.t

  let of_octez : Octez_smart_rollup.Kind.t -> t = function
    | Example_arith -> Example_arith
    | Wasm_2_0_0 -> Wasm_2_0_0

  let to_octez : t -> Octez_smart_rollup.Kind.t = function
    | Example_arith -> Example_arith
    | Wasm_2_0_0 -> Wasm_2_0_0
end

module Dal = struct
  module Slot_index = struct
    type t = Dal.Slot_index.t

    let of_octez ~number_of_slots (i : Octez_smart_rollup.Dal.Slot_index.t) : t
        =
      match Dal.Slot_index.of_int_opt ~number_of_slots i with
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

    let of_octez ~number_of_slots
        Octez_smart_rollup.Dal.Slot_header.
          {id = {published_level; index}; commitment} : t =
      Dal.Slot.Header.
        {
          id =
            {
              published_level = Raw_level.of_int32_exn published_level;
              index = Slot_index.of_octez ~number_of_slots index;
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

module Constants = struct
  type reveal_activation_level =
    Constants.Parametric.sc_rollup_reveal_activation_level

  let reveal_activation_level_of_octez
      Octez_smart_rollup.Rollup_constants.{blake2B; metadata; dal_page} :
      reveal_activation_level =
    {
      raw_data = {blake2B = Raw_level.of_int32_exn blake2B};
      metadata = Raw_level.of_int32_exn metadata;
      dal_page = Raw_level.of_int32_exn dal_page;
    }

  let reveal_activation_level_to_octez
      Constants.Parametric.{raw_data = {blake2B}; metadata; dal_page} :
      Octez_smart_rollup.Rollup_constants.reveal_activation_level =
    {
      blake2B = Raw_level.to_int32 blake2B;
      metadata = Raw_level.to_int32 metadata;
      dal_page = Raw_level.to_int32 dal_page;
    }
end
