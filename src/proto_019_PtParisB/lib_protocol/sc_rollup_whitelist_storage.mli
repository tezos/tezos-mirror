(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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

open Sc_rollup_whitelist_repr

(** [is_private context rollup] returns true if and only if the [rollup]
    is private, along with the new context accounting for the gas consumption
    of the function call. *)
val is_private :
  Raw_context.t -> Sc_rollup_repr.t -> (Raw_context.t * bool) tzresult Lwt.t

(** [init  context rollup ~whitelist] returns the new context resulting from
    the addition of the elements of [whitelist] to the whitelist in the given
    [rollup]'s storage, along with the used storage space. *)
val init :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  whitelist:t ->
  origination_level:Raw_level_repr.t ->
  (Raw_context.t * Z.t) tzresult Lwt.t

(** [check_access_to_private_rollup context rollup staker_pkh] returns an error
    if [staker_pkh] is not in the whitelist of [rollup] if the [rollup] is marked
    as private. Returns the gas consumed by performing the call otherwise.
    Assumes the private rollup feature is activated. *)
val check_access_to_private_rollup :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Signature.public_key_hash ->
  Raw_context.t tzresult Lwt.t

(** [find_whitelist_uncarbonated context rollup] returns the whitelist from the storage. *)
val find_whitelist_uncarbonated :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Signature.public_key_hash list option tzresult Lwt.t

(** [replace context rollup ~whitelist] replaces the whitelist of
    [rollup] in the storage by [whitelist]. Returns the resulting
    context along with the used storage space. *)
val replace :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  whitelist:t ->
  (Raw_context.t * Z.t) tzresult Lwt.t

(** [make_public context rollup] removes the whitelist of [rollup] from
    the storage thus making the rollup public. Returns the resulting
    context along with the freed storage space. *)
val make_public :
  Raw_context.t -> Sc_rollup_repr.t -> (Raw_context.t * Z.t) tzresult Lwt.t

(** [adjust_storage_space ctxt ~new_storage_size] updates the used
    storage space for the whitelist according to
    [new_storage_size]. The additional positive amount of unpaid
    storage is returned. If no unpaid storage is consumed, this amount
    is 0.

    Note that when storage space for the whitelist is released we may later
    use that space for free. For this reason, the amount returned may be less
    than the given (positive) [storage_diff]. *)
val adjust_storage_space :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  new_storage_size:Z.t ->
  (Raw_context.t * Z.t) tzresult Lwt.t

(** [get_last_whitelist_update ctxt rollup] returns the pair (outbox level,
    message index) of the latest message of update to the whitelist. Returns
    None if no whitelist update has been applied. The returned context accounts
    for the gas consumption of the storage's update. *)
val get_last_whitelist_update :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Raw_context.t * last_whitelist_update) tzresult Lwt.t

(** [set_last_whitelist_update ctxt rollup (outbox_level, message_index)] set
    the outbox level and message index of the latest message of update to the
    whitelist. Returns the new context, and the difference from the old (maybe 0)
    to the new size of the underlying storage. *)
val set_last_whitelist_update :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  last_whitelist_update ->
  (Raw_context.t * Z.t) tzresult Lwt.t
