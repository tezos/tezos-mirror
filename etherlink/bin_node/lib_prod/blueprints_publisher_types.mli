(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_base

module Request : sig
  (** Type of requests accepted by the publisher worker. *)
  type ('a, 'b) t =
    | Publish : {
        level : Z.t;
        payload : [`External of string] list;
      }
        -> (unit, error trace) t  (** Request to publish a blueprint. *)

  type view = View : _ t -> view

  include
    Worker_intf.REQUEST
      with type ('a, 'request_error) t := ('a, 'request_error) t
       and type view := view
end
