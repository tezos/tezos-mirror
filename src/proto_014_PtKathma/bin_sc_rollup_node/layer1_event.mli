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

(** This module defines functions that emit the events used by the layer 1 chain
    (see {!Layer}). *)

val starting : unit -> unit Lwt.t

val stopping : unit -> unit Lwt.t

(** [rollback hash level] emits the event that the layer 1 head is rolling back
    to the block of the given [hash] and at the given [level]. *)
val rollback : Block_hash.t -> int32 -> unit Lwt.t

(** [setting_new_head hash level] emits the event that the layer 1 head is set
    to the block of the given [hash] and at the given [level]. *)
val setting_new_head : Block_hash.t -> int32 -> unit Lwt.t

(** [new_head_processed hash level] emits the event that the layer 1 head of the
    given [hash] and at the given [level] is finished processing. *)
val new_head_processed : Block_hash.t -> int32 -> unit Lwt.t

(** [reacting_to_reorganization rollbacked_block new_blocks] emits the event
    that the rollup node is rolling back to the block of hash [rollbacked_block]
    and will be processing the [new_blocks] due to a layer 1 reorganization. *)
val reacting_to_reorganization : Block_hash.t -> Block_hash.t list -> unit Lwt.t
