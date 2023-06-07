(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Prometheus

(** All metrics related to the p2p protocol. *)
module Messages : sig
  (** bootstrap messages counter *)
  val bootstrap_received : Counter.t

  (** bootstrap messages counter *)
  val bootstrap_sent : Counter.t

  (** advertise messages counter *)
  val advertise_received : Counter.t

  (** advertise messages counter *)
  val advertise_sent : Counter.t

  (**  user messages sent counter *)
  val user_message_sent : Counter.t

  (**  user messages sent by broadcasting counter *)
  val broadcast_message_sent : Counter.t

  (**  user messages received counter *)
  val user_message_received : Counter.t

  (**  user messages received with errors counter *)
  val user_message_received_error : Counter.t

  (** swap requests messages counter *)
  val swap_request_received : Counter.t

  (** swap requests messages counter *)
  val swap_request_sent : Counter.t

  (** swap acks messages counter *)
  val swap_ack_received : Counter.t

  (** swap acks messages counter *)
  val swap_ack_sent : Counter.t
end

(** Metrics related to the status of swap messages *)
module Swap : sig
  (** ignored swaps counter *)
  val ignored : Counter.t

  (** successful swaps counter *)
  val success : Counter.t

  (** failed swaps counter *)
  val fail : Counter.t
end

(** [collect pool] registers metrics collections of [pool] *)
val collect : ('msg, 'peer, 'conn) P2p_pool.t -> unit
