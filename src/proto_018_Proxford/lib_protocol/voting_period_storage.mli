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

(** Initializes the current context with voting period information. *)
val init : Raw_context.t -> Voting_period_repr.t -> Raw_context.t tzresult Lwt.t

(** Sets the initial period to [{voting_period = root; kind = Proposal;
    start_position}]. *)
val init_first_period :
  Raw_context.t -> start_position:Int32.t -> Raw_context.t tzresult Lwt.t

(** Increment the index by one and set the kind to Proposal. *)
val reset : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Increment the index by one and set the kind to its successor. *)
val succ : Raw_context.t -> Raw_context.t tzresult Lwt.t

(** Returns information about the current voting period. *)
val get_current : Raw_context.t -> Voting_period_repr.t tzresult Lwt.t

(** Returns the current voting period kind. *)
val get_current_kind : Raw_context.t -> Voting_period_repr.kind tzresult Lwt.t

(** Returns true if the context level is the last of current voting period.  *)
val is_last_block : Raw_context.t -> bool tzresult Lwt.t

(** [blocks_before_activation ctxt] returns [Some b] if the current
    voting period is the Adoption and [b] blocks must be waited before activation
    of the next protocol amendment. Returns [None] if the current period is not
    Adoption (then more than [Constants_storage.blocks_per_voting_period] must
    be waited before activation). *)
val blocks_before_activation : Raw_context.t -> int32 option tzresult Lwt.t

(** Returns the voting period information for the current level. *)
val get_rpc_current_info :
  Raw_context.t -> Voting_period_repr.info tzresult Lwt.t

(** Returns the voting period information for the next level. *)
val get_rpc_succ_info : Raw_context.t -> Voting_period_repr.info tzresult Lwt.t

module Testnet_dictator : sig
  (** Overwrites the kind of the current voting period WITHOUT
      incrementing the index.

      Must ONLY be called by the testnet dictator on a testnet.

      @return [Error Storage_error] if the current voting period is
      not set or its deserialization fails. *)
  val overwrite_current_kind :
    Raw_context.t ->
    Chain_id.t ->
    Voting_period_repr.kind ->
    Raw_context.t tzresult Lwt.t
end
