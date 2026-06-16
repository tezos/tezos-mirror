(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** A pretty-printer capable of rendering raw bytes into a human readable
    format. *)
type t

(** Makes a new pretty-printer available under a given [name]. *)
val register_pp : name:string -> (Format.formatter -> bytes -> unit) -> unit

(** [of_string name] returns a pretty-printer which was registered under
    a given [name] using {!register_pp}. *)
val of_string : string -> t option

val pp : t -> Format.formatter -> bytes -> unit

(** Lists the pretty-printers names registered using {!register_pp}. *)
val supported_pp : unit -> string list
