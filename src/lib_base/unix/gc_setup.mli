(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** [get_ocamlrunparam_param param] checks if [param] is set in the
    [OCAMLRUNPARAM] environment variable and returns its value or none if not
*)
val get_ocamlrunparam_param : string -> string option

(** [set_gc_space_overhead ~space_overhead process_name] sets the
    [Gc.space_overhead] for [process_name] to [space_overhead] (defaults to 40)
*)
val set_gc_space_overhead : ?space_overhead:int -> string -> unit
