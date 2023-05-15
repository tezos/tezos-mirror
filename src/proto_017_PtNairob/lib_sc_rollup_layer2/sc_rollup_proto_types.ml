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
