(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

(** [init ~data_dir] returns a handler to the EVM node store located under
    [data_dir]. If no store is located in [data_dir], an empty store is
    created. *)
val init : data_dir:string -> t tzresult Lwt.t

module Executable_blueprints : sig
  val store :
    t ->
    Ethereum_types.quantity ->
    Blueprint_types.payload ->
    unit tzresult Lwt.t

  val find :
    t ->
    Ethereum_types.quantity ->
    Blueprint_types.payload option tzresult Lwt.t
end

module Publishable_blueprints : sig
  val store :
    t ->
    Ethereum_types.quantity ->
    Blueprint_types.payload ->
    unit tzresult Lwt.t

  val find :
    t ->
    Ethereum_types.quantity ->
    Blueprint_types.payload option tzresult Lwt.t
end

module Context_hashes : sig
  val store :
    t -> Ethereum_types.quantity -> Context_hash.t -> unit tzresult Lwt.t

  val find :
    t -> Ethereum_types.quantity -> Context_hash.t option tzresult Lwt.t
end
