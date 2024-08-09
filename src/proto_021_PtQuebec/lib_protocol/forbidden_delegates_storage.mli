(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module maintains the storage related to forbidden delegates.
    It is responsible for maintaining the
    {!Storage.Tenderbake.Forbidden_delegates} table.
*)

(** [is_forbidden ctxt delegate] returns [true] if the given [delegate]
    is forbidden to bake or attest. *)
val is_forbidden : Raw_context.t -> Signature.Public_key_hash.t -> bool

(** [forbid ctxt delegate] adds [delegate] to the set of forbidden
    delegates. *)
val forbid : Raw_context.t -> Signature.public_key_hash -> Raw_context.t Lwt.t

(** [load ctxt] reads from the storage the saved set of
    forbidden delegates and sets the raw context's in-memory cached value. *)
val load : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Unforbids all delegates who

    - have no pending denunciations (for which slashing has yet to be
    applied), and

    - have enough current frozen deposits to insure their previously
    computed baking rights for [new_cycle].

    This function should be called at the end of each cycle, after
    having applied any slashings that were scheduled for the same
    cycle end. *)
val update_at_cycle_end_after_slashing :
  Raw_context.t -> new_cycle:Cycle_repr.t -> Raw_context.t tzresult Lwt.t

val init_for_genesis : Raw_context.t -> Raw_context.t tzresult Lwt.t
