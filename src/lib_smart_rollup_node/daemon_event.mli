(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** This module defines functions that emit the events used by the smart
    rollup node daemon (see {!Daemon}). *)

(** [head_processing hash level] emits the event that the
    block of the given [hash] and at the given [level] is being processed. *)
val head_processing : Block_hash.t -> int32 -> unit Lwt.t

(** [new_head_processed hash level process_time] emits the event that the daemon
    has finished processing the head of the given [hash] and at the given
    [level] in [process_time] seconds. *)
val new_head_processed : Block_hash.t -> int32 -> Ptime.Span.t -> unit Lwt.t

(** [processing_heads_iteration heads] emits the event that the [heads] are
    going to be processed. *)
val processing_heads_iteration : Layer1.head list -> unit Lwt.t

(** [new_heads_processed heads] emits the event that the [heads] were
    processed. *)
val new_heads_processed : Layer1.head list -> unit Lwt.t

(** [included_operation op result] emits an event that an operation
    for the rollup was included in a block. *)
val included_operation :
  ?errors:tztrace ->
  [`Applied | `Backtracked | `Failed | `Skipped] ->
  L1_operation.t ->
  unit Lwt.t

(** [migration ~catching_up (old_protocol, old_protocol_level) (new_protocol,
    new_protocol_level)] emits and event for when the rollup node detects and
    handles a protocol migration. *)
val migration :
  catching_up:bool ->
  Protocol_hash.t * int ->
  Protocol_hash.t * int ->
  unit Lwt.t

(** Emit a fatal error for the daemon. *)
val error : tztrace -> unit Lwt.t

(** Emit an event for when the node enters the degraded mode to only play
    refutations. *)
val degraded_mode : unit -> unit Lwt.t

(** Emit an event when the node exits after recovering the operator's
    stakes. *)
val exit_bailout_mode : unit -> unit Lwt.t
