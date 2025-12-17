(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Repo : sig
  type t
end

module State : sig
  type t = Octez_riscv_api.state

  val equal : t -> t -> bool
end

module Mutable_state : sig
  type t = Octez_riscv_api.mut_state

  val equal : t -> t -> bool
end

module Id : sig
  type t

  val unsafe_of_raw_string : string -> t

  val to_raw_string : t -> string

  val equal : t -> t -> bool
end

val load : cache_size:int -> readonly:bool -> string -> Repo.t Lwt.t

val close : Repo.t -> unit Lwt.t

val checkout : Repo.t -> Id.t -> Mutable_state.t option Lwt.t

val empty : unit -> Mutable_state.t

val commit : ?message:string -> Repo.t -> Mutable_state.t -> Id.t Lwt.t

val is_gc_finished : Repo.t -> bool

val cancel_gc : Repo.t -> bool

val split : Repo.t -> unit

val gc : Repo.t -> ?callback:(unit -> unit Lwt.t) -> Id.t -> unit Lwt.t

val wait_gc_completion : Repo.t -> unit Lwt.t

val export_snapshot : Repo.t -> Id.t -> string -> unit tzresult Lwt.t

val pvm_state_key : string list

val find : State.t -> string list -> State.t option Lwt.t

val lookup : State.t -> string list -> bytes option Lwt.t

val set : State.t -> string list -> State.t -> State.t Lwt.t

val add : State.t -> string list -> bytes -> State.t Lwt.t
