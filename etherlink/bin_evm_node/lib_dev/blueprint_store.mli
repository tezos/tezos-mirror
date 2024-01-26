(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type t

val make : data_dir:string -> t

(** [store s blueprint number] saves the [blueprint] of height [number] in
    the store [s]. *)
val store :
  t -> Blueprint_types.t -> Ethereum_types.quantity -> unit tzresult Lwt.t

(** [find s number] tries to fetch the blueprint of height [number] in the
    store [s], and returns [None] if it does not exist. *)
val find : t -> Ethereum_types.quantity -> Blueprint_types.t option Lwt.t
