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
  whitelist:Sc_rollup_whitelist_repr.t ->
  (Raw_context.t * int) tzresult Lwt.t

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
