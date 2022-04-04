(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** Functions to manipulate transaction rollup’s inboxes.

    Except explicit mention of the contrary, all the functions of this
    module are carbonated. *)

(** [append_message ctxt tx_rollup state message] tries to append
    [message] to the inbox of [tx_rollup] at the current level, creating
    it in the process if need be. This function returns the size of the
    appended message (in bytes), in order for the appropriate burn to be
    taken from the message author, the new state, as well as the storage
    size diff. It is the caller's responsibility to store the returned state.

    {b Note:} [tx_rollup] needs to be a valid transaction address. It
    is the responsibility of the caller to assert it.

    Returns the error

    {ul {li [Inbox_size_would_exceed_limit] if appending [message] to
            the inbox would make it exceed the maximum size specified
            by the [tx_rollup_hard_size_limit_per_inbox] protocol
            parameter.}
        {li [Message_size_exceeds_limit] if the size of [message] is
            greater than the [tx_rollup_hard_size_limit_per_message]
            protocol parameter.}} *)
val append_message :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_message_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t * Z.t) tzresult Lwt.t

(** [get ctxt level tx_rollup] returns the inbox of [tx_rollup] at
    level [level].

    Returns the errors

    {ul {li [Inbox_does_not_exist] iff [tx_rollup] does not have an
    inbox at level [level]. }} *)
val get :
  Raw_context.t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_inbox_repr.t) tzresult Lwt.t

(** [find ctxt level tx_rollup] returns the inbox of [tx_rollup] at
    level [level].

    Returns [None] when the similar function [get] returns an
    error. *)
val find :
  Raw_context.t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_inbox_repr.t option) tzresult Lwt.t

(** [remove ctxt level tx_rollup] removes from the context the inbox
    of [level].

    It is expected that this function is only called for inboxes that
    has been “adopted” by a commitment. As a consequence, the storage
    accounting is not performed by this function.

    This function will returns the error [Inbox_does_not_exist] if
    there is no inbox for [level] in the storage. It is the
    reponsibility of the caller to ensure the [tx_rollup] actually
    exists. *)
val remove :
  Raw_context.t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_repr.t ->
  Raw_context.t tzresult Lwt.t

(** [check_message_hash ctxt level tx_rollup position message path]
    checks that [message] is part of the [tx_rollup] inbox for [level]
    by checking the merkelised proof given by [path].

    If the proof failed, returns [Wrong_message_path]. *)
val check_message_hash :
  Raw_context.t ->
  Tx_rollup_level_repr.t ->
  Tx_rollup_repr.t ->
  position:int ->
  Tx_rollup_message_repr.t ->
  Tx_rollup_inbox_repr.Merkle.path ->
  Raw_context.t tzresult Lwt.t
