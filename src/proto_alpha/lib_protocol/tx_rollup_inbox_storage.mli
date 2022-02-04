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

type error +=
  | Tx_rollup_inbox_does_not_exist of Tx_rollup_repr.t * Raw_level_repr.t
  | Tx_rollup_inbox_size_would_exceed_limit of Tx_rollup_repr.t
  | Tx_rollup_message_size_exceeds_limit

(** [append_message ctxt tx_rollup state message] tries to append
    [message] to the inbox of [tx_rollup] at the current level, creating
    it in theprocess if need be. This function returns the size of the
    appended message (in bytes), in order for the appropriate fees to be
    taken from the message author, as well as the new state.  It
    is the caller's responsibility to store the returned state.

    {b Note:} [tx_rollup] needs to be a valid transaction address. It
    is the responsibility of the caller to assert it.

    Returns the error

    {ul {li [Tx_rollup_hard_size_limit_reached] if appending [message]
            to the inbox would make it exceed the maximum size
            specified by the [tx_rollup_hard_size_limit_per_inbox]
            protocol parameter.}
        {li [Tx_rollup_message_size_exceeds_limit] if the size of
            [message] is greater than the
            [tx_rollup_hard_size_limit_per_message] protocol
            parameter.}} *)
val append_message :
  Raw_context.t ->
  Tx_rollup_repr.t ->
  Tx_rollup_state_repr.t ->
  Tx_rollup_message_repr.t ->
  (Raw_context.t * Tx_rollup_state_repr.t * int) tzresult Lwt.t

(** [messages ctxt ~level tx_rollup] returns the list of messages
    hashes stored in the inbox of [tx_rollup] at level [level].

    If the [level] label is omitted, then it is inferred from [ctxt]
    (namely, from the current level of the chain).

    Returns the errors

    {ul {li [Tx_rollup_does_not_exist] iff [tx_rollup] does not exist}
        {li [Tx_rollup_inbox_does_not_exist] iff [tx_rollup] exists,
            but does not have an inbox at level [level]. }} *)
val messages :
  Raw_context.t ->
  level:[`Current | `Level of Raw_level_repr.t] ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_message_repr.hash list) tzresult Lwt.t

(** [size ctxt ~level tx_rollup] returns the number of bytes allocated
    by the messages of the inbox of [tx_rollup] at level [level].

    If the [level] label is omitted, then it is inferred from [ctxt]
    (namely, from the current level of the chain).

    Returns the errors

    {ul {li [Tx_rollup_does_not_exist] iff [tx_rollup] does not exist}
        {li [Tx_rollup_inbox_does_not_exist] iff [tx_rollup] exists,
            but does not have an inbox at level [level]. }} *)
val size :
  Raw_context.t ->
  level:[`Current | `Level of Raw_level_repr.t] ->
  Tx_rollup_repr.t ->
  (Raw_context.t * int) tzresult Lwt.t

(** [get ctxt ~level tx_rollup] returns the inbox of [tx_rollup] at
    level [level].

    If the [level] label is omitted, then it is inferred from [ctxt]
    (namely, from the current level of the chain).

    Returns the errors

    {ul {li [Tx_rollup_does_not_exist] iff [tx_rollup] does not exist}
        {li [Tx_rollup_inbox_does_not_exist] iff [tx_rollup] exists,
            but does not have an inbox at level [level]. }} *)
val get :
  Raw_context.t ->
  level:[`Current | `Level of Raw_level_repr.t] ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_inbox_repr.t) tzresult Lwt.t

(** [find ctxt ~level tx_rollup] returns the inbox of [tx_rollup] at
    level [level], or [None] if said inbox does not exists.

    If the [level] label is omitted, then it is inferred from [ctxt]
    (namely, from the current level of the chain).

    Returns the [Tx_rollup_does_not_exist] error iff [tx_rollup] does
    not exist. *)
val find :
  Raw_context.t ->
  level:[`Current | `Level of Raw_level_repr.t] ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Tx_rollup_inbox_repr.t option) tzresult Lwt.t

(** [get_adjacent_levels ctxt level tx_rollup] returns the first level
    before [level] that has a non-empty inbox, if any, and the first
    level after [level] that has a non-empty inbox, if any. It is
    assumed that [level] itself has a non-empty inbox, and if it does
    not, or if [tx_rollup] does not exist, the result is an error. *)
val get_adjacent_levels :
  Raw_context.t ->
  Raw_level_repr.t ->
  Tx_rollup_repr.t ->
  (Raw_context.t * Raw_level_repr.t option * Raw_level_repr.t option) tzresult
  Lwt.t
