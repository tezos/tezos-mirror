(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

module Request : sig
  type 'a t =
    | Flush :
        Block_hash.t
        * Chain_validator_worker_state.Event.update
        * Block_hash.Set.t
        * Operation_hash.Set.t
        -> unit t
        (** The chain changed, the mempool is being notified of the new state. *)
    | Notify : P2p_peer.Id.t * Mempool.t -> unit t
        (** The given peer sent this mempool. *)
    | Leftover : unit t
        (** Operations not yet processed should be processed. *)
    | Inject : {op : Operation.t; force : bool} -> unit t
        (** Operation has been locally injected (sent) to the node. *)
    | Arrived : Operation_hash.t * Operation.t -> unit t
        (** Operation was fetched by the node. *)
    | Advertise : unit t
        (** Current mempool should be advertised to all known peers. *)
    | Ban : Operation_hash.t -> unit t
        (** User requested the node to ban operation with this hash. *)

  type view = View : _ t -> view

  val view : 'a t -> view

  val encoding : view Data_encoding.t

  val pp : Format.formatter -> view -> unit
end

(** Indicates how an operation hash has been encountered:
    + [Injected]         : The corresponding operation has been directly
                           injected into the node.
    + [Notified peer_id] : The hash is present in a mempool advertised by
                           [peer_id].
    + [Arrived]          : The node was fetching and has just received the
                           corresponding operation.

    [Other] serves as default value for an argument, but in practice it is
    not used atm (June 2021).

    This module is used in {!Event.Banned_operation_encountered}. *)
module Operation_encountered : sig
  type situation =
    | Injected
    | Arrived
    | Notified of P2p_peer_id.t option
    | Other

  type t = situation * Operation_hash.t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/1266

   This module should be removed once backlogs will be removed from
   the RPC /worker/prevalidators. *)
module Event : sig
  type t = unit

  type view = t

  val view : t -> view

  val level : t -> Internal_event.level

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end
