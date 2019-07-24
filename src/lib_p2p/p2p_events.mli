(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

type t = {
  too_few_connections : unit Lwt_condition.t;
  too_many_connections : unit Lwt_condition.t;
  new_peer : unit Lwt_condition.t;
  new_point : unit Lwt_condition.t;
  new_connection : unit Lwt_condition.t;
}

val create : unit -> t

(** [wait_too_few_connections t] is determined when the number of
    connections drops below the desired level. *)
val wait_too_few_connections : t -> unit Lwt.t

(** [wait_too_many_connections t] is determined when the number of
    connections exceeds the desired level. *)
val wait_too_many_connections : t -> unit Lwt.t

(** [wait_new_peer t] is determined when a new peer
    (i.e. authentication successful) gets added to the pool. *)
val wait_new_peer : t -> unit Lwt.t

(** [wait_new_point t] is determined when a new point gets registered
    to the pool. *)
val wait_new_point : t -> unit Lwt.t

(** [wait_new_connection t] is determined when a new connection is
    successfully established in the pool. *)
val wait_new_connection : t -> unit Lwt.t
