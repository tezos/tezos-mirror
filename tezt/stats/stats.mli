(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Make statistics on sets of tests. *)

module Duration : sig
  type t

  val seconds : t -> float

  val minutes : t -> float

  val hours : t -> float
end

type t = {
  count : int;
  total_duration : Duration.t;
  average_duration : Duration.t;
}

val make : Record.t -> t
