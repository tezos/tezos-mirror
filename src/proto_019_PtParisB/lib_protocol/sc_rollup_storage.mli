(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** [originate ?whitelist context ~kind ~parameters_ty ~genesis_commitment] produces an
   address [a] for a smart contract rollup using the origination nonce found in
   [context]. This function also initializes the storage with a new
   entry indexed by [a] to remember the [kind] of the rollup at
   address [a].

   Also returns the number of allocated bytes.  *)
val originate :
  ?whitelist:Sc_rollup_whitelist_repr.t ->
  Raw_context.t ->
  kind:Sc_rollups.Kind.t ->
  parameters_ty:Script_repr.lazy_expr ->
  genesis_commitment:Sc_rollup_commitment_repr.t ->
  (Sc_rollup_repr.Address.t
  * Z.t
  * Sc_rollup_commitment_repr.Hash.t
  * Raw_context.t)
  tzresult
  Lwt.t

(** [raw_originate ?whitelist context ~kind ~parameters_ty ~genesis_commitment ~address] is
    exactly {!originate} but provides the rollup's address ([address]) instead
    of randomly generating it.

    This should not be used by [apply.ml], this is needed for bootstrap
    smart rollups only.
*)
val raw_originate :
  ?whitelist:Sc_rollup_whitelist_repr.t ->
  Raw_context.t ->
  kind:Sc_rollups.Kind.t ->
  parameters_ty:Script_repr.lazy_expr ->
  genesis_commitment:Sc_rollup_commitment_repr.t ->
  address:Sc_rollup_repr.Address.t ->
  (Z.t * Sc_rollup_commitment_repr.Hash.t * Raw_context.t) tzresult Lwt.t

(** [kind context address] returns the kind of the given rollup [address] iff
    [address] is an existing rollup. Fails with an [Sc_rollup_does_not_exist]
    error in case the rollup does not exist. *)
val kind :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Raw_context.t * Sc_rollups.Kind.t) tzresult Lwt.t

val list_unaccounted : Raw_context.t -> Sc_rollup_repr.t list tzresult Lwt.t

(** [genesis_info ctxt sc_rollup] returns the level at which a [sc_rollup] was
   originated, and its genesis commitment hash. *)
val genesis_info :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Raw_context.t * Sc_rollup_commitment_repr.genesis_info) tzresult Lwt.t

(** [get_metadata ctxt rollup] retrieves the origination level of the [rollup]
    using {!Sc_rollup_commitment_repr.genesis_info} and creates a
    {!Sc_rollup_metadata_repr.t}.
    Fails with [Sc_rollup_does_not_exist {rollup}] if the genesis info is
    missing. *)
val get_metadata :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Raw_context.t * Sc_rollup_metadata_repr.t) tzresult Lwt.t

(** [parameters_type ctxt rollup] returns the registered type of a rollup.
    Returns [None] in case there is no registered type for the rollup. *)
val parameters_type :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  (Script_repr.lazy_expr option * Raw_context.t) tzresult Lwt.t

(** [must_exist ctxt rollup] checks whether the given [rollup] exists
    in [ctxt]. If [rollup] exists, a new context is returned with gas
    consumed for the lookup cost. If it does not exist, an error is
    returned. *)
val must_exist :
  Raw_context.t -> Sc_rollup_repr.t -> Raw_context.t tzresult Lwt.t
