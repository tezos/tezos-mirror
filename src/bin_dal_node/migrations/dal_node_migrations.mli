(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Top-level module shadowing the other modules of the library. *)

module type S = sig
  val name : string

  val up : (unit, unit, [`Zero]) Caqti_request.t list
end

type migration = (module S)

val migrations : int -> migration list
