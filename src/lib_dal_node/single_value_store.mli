(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Helper module to load/store a single value from/to disk. *)

module type VALUE = sig
  type t

  val name : string

  val encoding : t Data_encoding.t
end

module type S = sig
  type value

  type ro

  type rw

  type _ t

  (** Initializes a KVS store with read-write permissions at the given location
      to remember some value. *)
  val init : root_dir:string -> rw t tzresult Lwt.t

  (** Initializes a KVS store with read-only permissions at the given location
      to read some value. *)
  val init_readonly : root_dir:string -> ro t tzresult Lwt.t

  (** [load t] loads the value from store [t]. If the value is not found, the
      result is [None]. *)
  val load : _ t -> value option tzresult Lwt.t

  (** [save t value] saves the [value] into the store [t], overriding
    any previous value. *)
  val save : rw t -> value -> unit tzresult Lwt.t
end

module Make : functor (Value : VALUE) -> S with type value = Value.t
