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

(** This module contains the parameters for the worker (see {!Worker}) used by
    the batcher. *)

type order_request = {drop_no_order : bool; order_below : Z.t}

module Request : sig
  (** Type of requests accepted by the batcher worker. *)
  type ('a, 'b) t =
    | Register : {
        order : Z.t option;
        messages : string list;
        drop_duplicate : bool;
      }
        -> (L2_message.id list, error trace) t
        (** Request to register new L2 messages in the queue. if
        [drop_duplicate] is [true], then the elements of
        [messages] already processed by the batcher, with
        [drop_duplicate = true], are dropped. *)
    | Produce_batches : (unit, error trace) t
        (** Request to produce new messages batches and
         submit them to the injector. *)
    | Clear_queues : (unit, error trace) t
        (** Request to clear all queues, the heap and the batched
            messages.  *)
    | Remove_messages : order_request -> (unit, error trace) t
        (** Request to remove messages in the heap and batched with
        order inferior to [order_below]. If [drop_no_order] is
        set, remove also all messages that have no order
        specified.  *)

  type view = View : _ t -> view

  include
    Worker_intf.REQUEST
      with type ('a, 'request_error) t := ('a, 'request_error) t
       and type view := view
end
