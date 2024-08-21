(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base

module Request : sig
  (** In general, the publisher receives a full blueprint chunked, and handles
      the transformation of the data to parsable message for the kernel. In case
      of a catchup, the messages have already been produced for the inbox and
      extracted from the store to be republished as is. *)
  type payload =
    | Blueprint of Sequencer_blueprint.t list
    | Inbox of Blueprint_types.payload
        (** Type of requests accepted by the publisher worker. *)

  type ('a, 'b) t =
    | Publish : {level : Z.t; payload : payload} -> (unit, error trace) t
        (** Request to publish a blueprint. *)
    | New_rollup_node_block : int32 -> (unit, error trace) t

  type view = View : _ t -> view

  include
    Worker_intf.REQUEST
      with type ('a, 'request_error) t := ('a, 'request_error) t
       and type view := view
end
