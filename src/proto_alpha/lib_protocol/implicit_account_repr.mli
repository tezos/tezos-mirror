(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Protocol-side representation of an implicit account identifier.

    A thin wrapper that re-exports {!Account_hash} and adds the storage
    {!Index}. Kept abstract so the protocol manipulates account ids without
    depending on their underlying encoding. *)

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

type error += Account_hash_invalid_notation of string (* `Permanent *)

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
   comment starting with "FIXME-PA" *)
(* FIXME-PA: remove all occurences of this module *)
module Forbidden : sig
  val to_pkh : t -> Signature.Public_key_hash.t

  val of_pkh : Signature.Public_key_hash.t -> t
end

val rpc_arg : t RPC_arg.arg

module Index : Storage_description.INDEX with type t = t
