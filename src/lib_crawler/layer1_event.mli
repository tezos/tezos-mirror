(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) Nomadic Labs <contact@nomadic-labs.com>                     *)
(* Copyright (c) Functori, <contact@functori.com>                            *)
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
    (see {!Layer_1}). *)

val starting : name:string -> unit Lwt.t

val stopping : name:string -> unit Lwt.t

(** Emits the event that the connection to the Tezos node has been lost. *)
val connection_lost : name:string -> unit Lwt.t

(** Emits the event that the connection to the Tezos node has timeouted. *)
val connection_timeout : name:string -> timeout:float -> unit Lwt.t

(** Emits the event that the connection to the Tezos node has errored. *)
val connection_error : name:string -> tztrace -> unit Lwt.t

(** [cannot_connect ~count error] emits the event that the rollup node cannot
    connect to the Tezos node because of [error] for the [count]'s time. *)
val cannot_connect : name:string -> count:int -> tztrace -> unit Lwt.t

(** [wait_reconnect delay] emits the event that the rollup will wait [delay]
    seconds before attempting to reconnect to the Tezos node . *)
val wait_reconnect : name:string -> float -> unit Lwt.t

(** [switched_new_head hash level] emits the event that the layer 1 has notified
    a new head with [hash] at some given [level]. *)
val switched_new_head : name:string -> Block_hash.t -> int32 -> unit Lwt.t

val connected : name:string -> unit Lwt.t

val stopping_old_connection : name:string -> unit Lwt.t

val reconnect_connecting : name:string -> unit Lwt.t

val reconnect_notified : name:string -> unit Lwt.t

val reconnect_disconnected : name:string -> unit Lwt.t

val reconnect_connected : name:string -> unit Lwt.t
