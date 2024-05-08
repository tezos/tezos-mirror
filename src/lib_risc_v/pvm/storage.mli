(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

type repo

type tree

module Id : sig
  type t

  val unsafe_of_raw_string : string -> t

  val to_raw_string : t -> string
end

val load : cache_size:int -> readonly:bool -> string -> repo Lwt.t

val close : repo -> unit Lwt.t

val checkout : repo -> Id.t -> tree option Lwt.t

val empty : unit -> tree

val commit : ?message:string -> repo -> tree -> Id.t Lwt.t

val is_gc_finished : repo -> bool

val split : repo -> unit

val gc : repo -> ?callback:(unit -> unit Lwt.t) -> Id.t -> unit Lwt.t

val wait_gc_completion : repo -> unit Lwt.t

val export_snapshot : repo -> Id.t -> string -> unit tzresult Lwt.t

val find : tree -> string list -> tree option Lwt.t

val lookup : tree -> string list -> bytes option Lwt.t

val set : tree -> string list -> tree -> tree Lwt.t

val add : tree -> string list -> bytes -> tree Lwt.t
