(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open Protocol.Alpha_context

type 'rpc service =
  ('meth, 'prefix, 'params, 'query, 'input, 'output) Tezos_rpc.Service.service
  constraint
    'rpc =
    < meth : 'meth
    ; prefix : 'prefix
    ; params : 'params
    ; query : 'query
    ; input : 'input
    ; output : 'output >

module Commitments_history : sig
  (** Service for returning the skip list cell of the given hash. *)
  val hash_content :
    < meth : [`GET]
    ; input : unit
    ; output : Dal.Slots_history.t
    ; prefix : unit
    ; params : unit * Dal.Slots_history.Pointer_hash.t
    ; query : unit >
    service
end
