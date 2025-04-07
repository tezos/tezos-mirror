(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Represents a duration such as '1h30m' *)

(** Time units available are the following:
      ms - miliseconds
      s  - seconds
      m  - minutes
      h  - hours
      d  - days
      w  - weeks
      y  - years. *)
type t

(** [of_string str] parses a duration. *)
val of_string : string -> t

(** [to_string str] returns the representation of a duration. *)
val to_string : t -> string

(** [compare left right] compares two durations with respect to the
    number of miliseconds they represent. *)
val compare : t -> t -> int
