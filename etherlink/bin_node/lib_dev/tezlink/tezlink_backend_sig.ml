(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module type S = sig
  val current_level :
    [> `Main] ->
    [> `Head of 'a] ->
    offset:int32 ->
    Tezos_types.level tzresult Lwt.t

  val constants :
    [> `Main] -> [> `Head of 'a] -> Tezlink_constants.t tzresult Lwt.t

  val balance :
    [> `Main] ->
    [> `Head of 'a] ->
    Tezos_types.Contract.t ->
    Tezos_types.Tez.t tzresult Lwt.t

  val manager_key :
    [> `Main] ->
    [> `Head of 'a] ->
    Tezos_types.Contract.t ->
    Signature.V1.Public_key.t option tzresult Lwt.t

  val counter :
    [> `Main] -> [> `Head of 'a] -> Tezos_types.Contract.t -> Z.t tzresult Lwt.t

  val header :
    [> `Main] -> [> `Head of 'a] -> L2_types.Tezos_block.t tzresult Lwt.t
end
