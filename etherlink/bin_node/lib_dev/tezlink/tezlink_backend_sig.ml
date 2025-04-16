(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_types

module type S = sig
  val current_level :
    [> `Main] ->
    [> `Head of 'a] ->
    offset:int32 ->
    Tezos_types.level tzresult Lwt.t

  val constants :
    [> `Main] -> [> `Head of 'a] -> Tezlink_constants.t tzresult Lwt.t

  val balance :
    [> `Main] -> [> `Head of 'a] -> Contract.t -> Tez.t tzresult Lwt.t
end
