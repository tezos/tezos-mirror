(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** This module defines constructor for the private and default
    [P2p_answerer.t]. Both pass messages `[Message msg]` to the upper-layer.
    The private answerer ignore all other messages. *)

(** The [config] type contains "global" values and configuration option used
    by the default answerer, known when setting up the connection handler
    in [P2p] *)
type ('msg, 'peer, 'conn) config = {
  swap_linger : Time.System.Span.t option;
      (** Peer swapping does not occur more than once during a timespan of
      [swap_linger]. If None, the swap mechanism is disabled. *)
  pool : ('msg, 'peer, 'conn) P2p_pool.t;
  log : P2p_connection.P2p_event.t -> unit;
  connect : P2p_point.Id.t -> ('msg, 'peer, 'conn) P2p_conn.t tzresult Lwt.t;
  mutable latest_accepted_swap : Time.System.t;
  mutable latest_successful_swap : Time.System.t;
}

val create_default : ('msg, 'peer, 'conn) config -> 'msg P2p_answerer.t

val create_private : unit -> 'msg P2p_answerer.t
