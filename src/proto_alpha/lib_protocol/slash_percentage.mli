(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [get ctxt ~kind ~level denounced] returns the percentage that needs to be
    applied for the given misbehaviour.

    [denounced] is the list of delegates that have been denounced together for
    the given [kind], for the given [level] and for the same round. The amount
    slashed increases quadratically as the number of attesting slots of
    denounced delegates increases. The maximum slashing value
    [max_slashing_per_block] is reached when that number of slots reaches
    [max_slashing_threshold] . *)
val get :
  Raw_context.t ->
  kind:Misbehaviour_repr.kind ->
  level:Level_repr.t ->
  Signature.public_key_hash list ->
  (Raw_context.t * Percentage.t) tzresult Lwt.t
