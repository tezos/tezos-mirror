(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Abstract identifier of an implicit account.

    An abstract representation that decouples accounts from
    their keys. Formerly {!Signature.Public_key_hash.t}, but representation was
    insufficient for stateful accounts. FIXME: {!Forbidden} breaks this abstraction,
    but the end goal is to remove this module. Any further usage is prohibited. *)

open Error_monad

type t

include Compare.S with type t := t

val to_b58check : t -> string

val of_b58check : string -> t tzresult

val of_b58check_opt : string -> t option

val of_b58data : Base58.data -> t option

val pp : Format.formatter -> t -> unit

val pp_short : Format.formatter -> t -> unit

(** {2 Serializers} *)

val encoding : t Data_encoding.t

(** A value [zero] for type {!t}. *)
val zero : t

val is_tz5 : t -> bool

type error += Invalid_notation of string (* `Permanent *)

module Map : sig
  include Map.S with type key := t

  val encoding : 'a Data_encoding.t -> 'a t Data_encoding.t
end

module Set : sig
  include Set.S with type elt := t

  val encoding : t Data_encoding.t
end

(* Usage of the functions in the [Forbidden] module SHOULD be avoided.
   If it seems unavoidable, their use MUST be indicated with a
   comment starting with "FIXME-PA", followed by a very brief justification.
   Test files are exempt from this. *)
module Forbidden : sig
  val to_pkh : t -> Signature.Public_key_hash.t

  val of_pkh : Signature.Public_key_hash.t -> t
end

val rpc_arg : t Tezos_rpc.Arg.t

module Path : sig
  val path_length : int

  val of_path : string list -> t option

  val to_path : t -> string list -> string list
end
