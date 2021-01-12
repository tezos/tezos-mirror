(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** This module provides functions used for tests. *)

(** Is a timeout used with [wait_pred] function. *)
type 'a timeout_t = {
  time : float;  (** Duration of the timeout. *)
  msg : 'a -> string;  (** Create the error message. *)
}

(** [wait_pred] wait until [pred arg] is true. If [pred] is not satisfy after
   [timeout.time] seconds a [Timeout (timeout.msg arg)] error is raised. *)
val wait_pred :
  ?timeout:'a timeout_t ->
  pred:('a -> bool) ->
  arg:'a ->
  unit ->
  unit tzresult Lwt.t

(** Same as [wait_pred]. *)
val wait_pred_s :
  ?timeout:'a timeout_t ->
  pred:('a -> bool Lwt.t) ->
  arg:'a ->
  unit ->
  unit tzresult Lwt.t

(** Based on [wait_pred]. [wait_conns ~pool n] waits until at least [n]
   connections are actives in [~pool]. *)
val wait_conns :
  ?timeout:float ->
  pool:('a, 'b, 'c) Tezos_p2p.P2p_pool.t ->
  int ->
  unit tzresult Lwt.t

(** [connect_all connect_handler points] establishes the connections to
    [points] using [connect_handler] and returns them. If one connection need
    more than [?timeout] seconds to be established, the function fails with
    [Timeout] error. *)
val connect_all :
  ?timeout:Time.System.Span.t ->
  ('a, 'b, 'c) P2p_connect_handler.t ->
  P2p_point.Id.t list ->
  ('a, 'b, 'c) P2p_conn.t list tzresult Lwt.t

(** [close_active_conns pool@] closes all actives connections of the pool. This
    function waits until the connections are effectively closed. *)
val close_active_conns : ('a, 'b, 'c) Tezos_p2p.P2p_pool.t -> unit Lwt.t
