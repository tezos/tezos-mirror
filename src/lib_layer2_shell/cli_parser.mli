(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type 'a t = string list -> ('a * string list, string trace) result

val constant : string -> unit t

val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

val return : 'a -> 'a t

val switch : char -> bool t

val long : string -> string option t

val default_long : default:string -> string -> string t

val short : char -> string option t

val required_short : char -> string t

val pos_arg : string -> string t

val seal : 'a t -> 'a t

val validation_error :
  ('a, Format.formatter, unit, ('b, string) result) format4 -> 'a

val validate : ('a -> (unit, string) result) -> 'a t -> 'a t

val union : 'a t trace -> 'a t

val select : (string * 'a t) trace -> 'a t

val fail : ('a, Format.formatter, unit, 'c t) format4 -> 'a
