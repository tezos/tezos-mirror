(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
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

val init :
  Raw_context.t -> Voting_period_repr.t -> Raw_context.t tzresult Lwt.t

(** Sets the initial period to [{voting_period = root; kind = Proposal;
    start_position}]. *)
val init_first_period :
  Raw_context.t -> start_position:Int32.t -> Raw_context.t tzresult Lwt.t

(** Increment the index by one and set the kind to Proposal. *)
val reset : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Increment the index by one and set the kind to its successor. *)
val succ : Raw_context.t -> Raw_context.t tzresult Lwt.t

val get_current : Raw_context.t -> Voting_period_repr.t tzresult Lwt.t

val get_current_kind : Raw_context.t -> Voting_period_repr.kind tzresult Lwt.t

(** Returns true if the context level is the last of current voting period.  *)
val is_last_block : Raw_context.t -> bool tzresult Lwt.t

(* Given the issue explained in voting_period_storage.ml this function behaves
   currectly during the validation of a block but returns inconsistent info if
   called after the finalization of the block.
   For this reason when used by the RPC `votes/current_period_kind` gives an
   unintuitive result: after the validation of the last block of a voting period
   (e.g. proposal), it returns the kind of the next period (e.g. exploration).
   To fix this, at least part of the current vote finalization should be moved
   at the beginning of the block validation.
   For retro-compatibility, we keep this function but we provide two new fixed
   functions to reply correctly to RPCs [get_rpc_fixed_current_info] and
   [get_rpc_fixed_succ_info]. *)
val get_current_info : Raw_context.t -> Voting_period_repr.info tzresult Lwt.t

(* In order to avoid the problem of `get_current_info` explained above, this
   function provides the corrent behavior for the new RPC `votes/current_period`.
*)
val get_rpc_fixed_current_info :
  Raw_context.t -> Voting_period_repr.info tzresult Lwt.t

(* In order to avoid the problem of `get_current_info` explained above, this
   function provides the corrent behavior for the new RPC `votes/successor_period`.
*)
val get_rpc_fixed_succ_info :
  Raw_context.t -> Voting_period_repr.info tzresult Lwt.t
