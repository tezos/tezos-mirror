(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2021 Oxhead Alpha <info@oxheadalpha.com>                    *)
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

(** This module introduces various functions to manipulate the storage related
    to commitments for transaction rollups. *)

(** [add_commitment context tx_rollup contract commitment] adds a
    commitment to a rollup.

    This functions returns the errors

    {ul {li [Level_already_has_commitment] iff there is already a
            commitment at this level.}
        {li [Missing_commitment_predecessor] iff the predecessor does
            not match the already-stored predecessor commitment.}
        {li [Wrong_commitment_predecessor_level] iff there is no
            predecessor level, but a predecessor commitment is
            provided (or no predecessor commitment is provided but
            there is a precessor level)}
        {li [Wrong_batch_count] iff the number of batches does not
            equal the length of the inbox.}} *)
val add_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Signature.Public_key_hash.t ->
  Tx_rollup_commitment_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [get_commitment context tx_rollup level] returns the commitment
    for a level, if any exists.  If the rollup does not exist,
    the error [Tx_rollup_does_not_exist] is returned. *)
val get_commitment :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Raw_level_repr.t ->
  (Raw_context.t * Tx_rollup_commitment_repr.Submitted_commitment.t option)
  tzresult
  Lwt.t
