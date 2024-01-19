(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
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

(** [starting_context_gc hash] emits an event which indicates that a GC run
    was launched for [hash]. *)
val starting_context_gc : Context_hash.t -> unit Lwt.t

(** [context_gc_already_launched ()] emits an event which indicates that a GC
    launch was attempted but resulted in no action because a GC run is already
    in progress. *)
val context_gc_already_launched : unit -> unit Lwt.t

(** [ending_context_gc total_duration finalise_duration] emits an event which
    indicates that a GC run has ended, providing its total duration and its
    finalisation duration. *)
val ending_context_gc : Ptime.span * Ptime.span -> unit Lwt.t

(** [context_gc_failure err] emits an event which indicates a GC failure. *)
val context_gc_failure : string -> unit Lwt.t

(** [context_gc_launch_failure err] emits an event which indicates a GC launch
    error. *)
val context_gc_launch_failure : string -> unit Lwt.t
