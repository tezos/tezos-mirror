(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** [head_processing hash level ~finalized] emits the event that the
    block of the given [hash] and at the given [level] is being processed, and
    whether it is [finalized]. *)
val head_processing : Block_hash.t -> int32 -> finalized:bool -> unit Lwt.t

(** [new_head_processed hash level] emits the event that the daemon has finished
    processing the head of the given [hash] and at the given [level]. *)
val new_head_processed : Block_hash.t -> int32 -> unit Lwt.t

(** [processing_heads_iteration heads] emits the event that the [heads] are
    going to be processed. *)
val processing_heads_iteration : Layer1.head list -> unit Lwt.t

(** [new_heads_processed heads] emits the event that the [heads] were
    processed. *)
val new_heads_processed : Layer1.head list -> unit Lwt.t

(** [included_operation ~finalized op result] emits an event that an operation
    for the rollup was included in a block (or finalized). *)
val included_operation :
  finalized:bool ->
  'kind Protocol.Alpha_context.manager_operation ->
  'kind Protocol.Apply_results.manager_operation_result ->
  unit Lwt.t
