(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** A module for managing state concerning a rollup's outbox. *)

(** [record_applied_message ctxt rollup level ~message_index] marks the message
    in the outbox of rollup [rollup] at level [level] and position
    [message_index] as processed. Returns the size diff resulting from adding an
    entry. The size diff may be 0 if an entry already exists, or negative if an
    index is replaced with a new level.

    An attempt to apply an old level that has already been replaced fails with
    an [Sc_rollup_outbox_level_expired] error.

    In case a message has already been applied for the given level and message
    index, the function fails with an [Sc_rollup_outbox_message_already_applied]
    error. *)
val record_applied_message :
  Raw_context.t ->
  Sc_rollup_repr.t ->
  Raw_level_repr.t ->
  message_index:int ->
  (Z.t * Raw_context.t) tzresult Lwt.t
