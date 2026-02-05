(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Top-level module shadowing the other modules of the library. *)

module type S = sig
  val name : string

  val up : (unit, unit, [`Zero]) Sqlite.Request.t list
end

type migration = (module S)

val tezlink_migrations : int -> migration list
