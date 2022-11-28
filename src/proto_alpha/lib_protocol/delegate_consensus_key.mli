(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 G.B. Fefe, <gb.fefe@protonmail.com>                    *)
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

(** Management of a delegate's consensus key, the one used to sign
    blocks and consensus operations.  It is responsible for maintaining
    the tables {!Storage.Consensus_keys},
    {!Storage.Contract.Consensus_key}, and
    {!Storage.Contract.Pending_consensus_keys}. *)

type error +=
  | Invalid_consensus_key_update_noop of Cycle_repr.t
  | Invalid_consensus_key_update_active
  | Invalid_consensus_key_update_tz4 of Bls.Public_key.t

(** The public key of a consensus key and the associated delegate. *)
type pk = Raw_context.consensus_pk = {
  delegate : Signature.Public_key_hash.t;
  consensus_pk : Signature.Public_key.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

(** The public key hash of a consensus key and the associated delegate. *)
type t = {
  delegate : Signature.Public_key_hash.t;
  consensus_pkh : Signature.Public_key_hash.t;
}

val zero : t

val pp : Format.formatter -> t -> unit

val pkh : pk -> t

(** [check_not_tz4 pk] checks that [pk] is not a BLS address. *)
val check_not_tz4 : Signature.public_key -> unit tzresult

(** Initialize the consensus key when registering a delegate. *)
val init :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t ->
  Raw_context.t tzresult Lwt.t

(** Returns the active consensus key for the current cycle. *)
val active_pubkey :
  Raw_context.t -> Signature.Public_key_hash.t -> pk tzresult Lwt.t

(** Returns the active consensus key for the current cycle. *)
val active_key :
  Raw_context.t -> Signature.Public_key_hash.t -> t tzresult Lwt.t

(** Returns the active consensus key for the given cycle. *)
val active_pubkey_for_cycle :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Cycle_repr.t ->
  pk tzresult Lwt.t

(** Returns the list of pending consensus-key updates in upcoming cycles. *)
val pending_updates :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  (Cycle_repr.t * Signature.Public_key_hash.t) list tzresult Lwt.t

(** Register a consensus-key update. *)
val register_update :
  Raw_context.t ->
  Signature.Public_key_hash.t ->
  Signature.Public_key.t ->
  Raw_context.t tzresult Lwt.t

(** Activate consensus keys at the beginning of cycle [new_cycle].
    This function iterates on all registered delegates. *)
val activate :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t
