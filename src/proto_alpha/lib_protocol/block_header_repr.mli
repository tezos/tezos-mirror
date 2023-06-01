(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Representation of block headers. *)

type contents = {
  payload_hash : Block_payload_hash.t;
  payload_round : Round_repr.t;
  seed_nonce_hash : Nonce_hash.t option;
  proof_of_work_nonce : bytes;
  toggle_votes : Toggle_votes_repr.toggle_votes;
}

type protocol_data = {contents : contents; signature : Signature.t}

type t = {shell : Block_header.shell_header; protocol_data : protocol_data}

type block_header = t

type raw = Block_header.t

type shell_header = Block_header.shell_header

val raw : block_header -> raw

val encoding : block_header Data_encoding.encoding

val raw_encoding : raw Data_encoding.t

val contents_encoding : contents Data_encoding.t

val unsigned_encoding : (Block_header.shell_header * contents) Data_encoding.t

val protocol_data_encoding : protocol_data Data_encoding.encoding

val shell_header_encoding : shell_header Data_encoding.encoding

type block_watermark = Block_header of Chain_id.t

val to_watermark : block_watermark -> Signature.watermark

val of_watermark : Signature.watermark -> block_watermark option

(** The maximum size of block headers in bytes *)
val max_header_length : int

val hash : block_header -> Block_hash.t

val hash_raw : raw -> Block_hash.t

type error += (* Permanent *) Invalid_stamp

(** Checks if the header that would be built from the given components
   is valid for the given difficulty. The signature is not passed as
   it is does not impact the proof-of-work stamp. The stamp is checked
   on the hash of a block header whose signature has been
   zeroed-out. *)
module Proof_of_work : sig
  val check_hash : Block_hash.t -> int64 -> bool

  val check_header_proof_of_work_stamp :
    shell_header -> contents -> int64 -> bool

  val check_proof_of_work_stamp :
    proof_of_work_threshold:int64 -> block_header -> unit tzresult
end

(** [check_timestamp ctxt timestamp round predecessor_timestamp
   predecessor_round] verifies that the block's timestamp and round
   are coherent with the predecessor block's timestamp and
   round. Fails with an error if that is not the case. *)
val check_timestamp :
  Round_repr.Durations.t ->
  timestamp:Time.t ->
  round:Round_repr.t ->
  predecessor_timestamp:Time.t ->
  predecessor_round:Round_repr.t ->
  unit tzresult

val check_signature : t -> Chain_id.t -> Signature.Public_key.t -> unit tzresult

val begin_validate_block_header :
  block_header:t ->
  chain_id:Chain_id.t ->
  predecessor_timestamp:Time.t ->
  predecessor_round:Round_repr.t ->
  fitness:Fitness_repr.t ->
  timestamp:Time.t ->
  delegate_pk:Signature.public_key ->
  round_durations:Round_repr.Durations.t ->
  proof_of_work_threshold:int64 ->
  expected_commitment:bool ->
  unit tzresult
