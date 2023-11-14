(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [is_forbidden ctxt delegate] returns [true] if the given [delegate]
    is forbidden to bake or attest. This means that its current frozen deposit
    is equal to zero. Returns [false] otherwise. *)
val is_forbidden : Raw_context.t -> Signature.Public_key_hash.t -> bool

(** [forbid ctxt delegate] adds [delegate] to the set of forbidden
    delegates and stores the updated set, which prevents this delegate from
    baking or attesting. *)
val forbid : Raw_context.t -> Signature.Public_key_hash.t -> Raw_context.t Lwt.t

(** [load ctxt] reads from the storage the saved set of
    forbidden delegates and sets the raw context's in-memory cached value. *)
val load : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** [reset ctxt delegates] overwrites the forbidden
    delegates set with an empty set in both storage and in-memory. *)
val reset : Raw_context.t -> Raw_context.t Lwt.t
