(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(* Top-level module shadowing the other modules of the library. *)

module type S = sig
  val name : string

  val apply : (unit, unit, [`Zero]) Caqti_request.t list
end

type migration = (module S)

(** [migrations v] returns the list of migrations to apply to obtain version [v]
    from an empty database. *)
val migrations : int -> migration list
