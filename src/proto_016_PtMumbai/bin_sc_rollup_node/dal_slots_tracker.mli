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
open Protocol
open Alpha_context

(** The rollup node keeps the list of dal slots for each block it needs to
    process.  This is to determine whether the inbox for a given block will need
    to be retrieved from the block operations, or from the data availability
    layer after lag levels have passed and the slot for the block has been
    declared available.

    The state of slots per block is persistent.  *)

(** [is_slot_confirmed node_ctxt head slot_index] checks whether the slot
    with index [slot_index] has been confirmed in [head]. *)
val is_slot_confirmed :
  _ Node_context.t -> Layer1.head -> Dal.Slot_index.t -> bool tzresult Lwt.t

(** [process_head node_ctxt head] performs the following operations:
    {ul
      {li it reads the endorsements for headers published attestation_lag
      levels preceding [head] from the block metadata, determines which
      ones the rollup node will download, and stores the results in
      [Store.Dal_confirmed_slots].}
    }  *)
val process_head : Node_context.rw -> Layer1.head -> unit tzresult Lwt.t

(** [slots_history_of_hash node_ctxt block_hash] returns the DAL confirmed slots
   history at the end of the given [block_hash] validation. *)
val slots_history_of_hash :
  _ Node_context.t ->
  Layer1.head ->
  Protocol.Alpha_context.Dal.Slots_history.t tzresult Lwt.t

(** [slots_history_cache_of_hash node_ctxt block_hash] returns the DAL confirmed
   slots history cache at the end of the given [block_hash] validation. *)
val slots_history_cache_of_hash :
  _ Node_context.t ->
  Layer1.head ->
  Protocol.Alpha_context.Dal.Slots_history.History_cache.t tzresult Lwt.t
