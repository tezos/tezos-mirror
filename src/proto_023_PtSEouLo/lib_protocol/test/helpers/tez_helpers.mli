(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** This module wraps the errors from the protocol and provides some helpful
    functions manipulating Tez. *)
type t = Protocol.Alpha_context.Tez.t

val zero : t

val one_mutez : t

val one_cent : t

val fifty_cents : t

val one : t

(* Same as max_mutez *)
val max_tez : t

val equal : t -> t -> bool

val ( -? ) : t -> t -> t tzresult

val sub_opt : t -> t -> t option

val ( +? ) : t -> t -> t tzresult

val ( *? ) : t -> int64 -> t tzresult

val ( /? ) : t -> int64 -> t tzresult

(* These operators can raise exceptions *)
val ( -! ) : t -> t -> t

val ( +! ) : t -> t -> t

val ( *! ) : t -> int64 -> t

val ( /! ) : t -> int64 -> t

val to_mutez : t -> int64

(* Is actually of_mutez_exn *)
val of_mutez : int64 -> t

val to_repr : t -> Protocol.Tez_repr.t

val of_repr : Protocol.Tez_repr.t -> t

val min : t -> t -> t

module Compare : Compare.S with type t := t

val pp : Format.formatter -> t -> unit

val to_string : t -> string

(** See {!Protocol.Tez_repr.mul_percentage}. *)
val mul_percentage : rounding:[`Down | `Up] -> t -> Protocol.Percentage.t -> t

(* Helper functions, not exported from the protocol *)
val ratio : t -> t -> Q.t

val mul_q : t -> Q.t -> Q.t

val of_int : int -> t

val of_q : round:[`Down | `Up] -> Q.t -> t

val of_z : Z.t -> t

val to_z : t -> Z.t

(** Functions to manipulate Tez in a high level way *)
module Ez_tez : sig
  (** Aliases for tez values *)
  type tez_quantity =
    | Half
    | All
    | All_but_one
    | Nothing
    | Max_tez
    | Amount of t

  val tez_quantity_pp : Format.formatter -> tez_quantity -> unit

  (** [quantity_to_tez max qty] returns a tez value corresponding to the given
      [qty]. If [qty] is [All], then returns [max]. If [qty] is [All_but_one],
      returns [max - one_mutez]. *)
  val quantity_to_tez : t -> tez_quantity -> t
end
