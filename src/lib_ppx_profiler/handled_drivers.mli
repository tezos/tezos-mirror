(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** A [Driver_kind] represents an enabled driver by `TEZOS_PPX_PROFILER` *)
module Driver_kind : sig
  type t = OpenTelemetry | Prometheus | Text | Json

  val of_string : string -> t
end

(** [Handled_drivers.t] gathers all the enabled drivers *)
type t

val empty : t

val is_empty : t -> bool

val of_list : Driver_kind.t list -> t

val of_string : string -> t

val mem : Driver_kind.t -> t -> bool

val exists : (Driver_kind.t -> bool) -> t -> bool
