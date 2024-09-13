(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** A value representing percentages, between 0% and 100%, inclusive.
    Precision of the representation is 0.01% *)
type t

(* TODO #6918: Remove after P *)
val encoding_legacy_in_o : t Data_encoding.t

val encoding : t Data_encoding.t

(** Rounds down to the nearest 0.01% *)
val of_ratio_bounded : Ratio_repr.t -> t

val of_q_bounded : round:[`Down | `Up] -> Q.t -> t

val to_q : t -> Q.t

(** [neg p] is [100% - p]. *)
val neg : t -> t

val add_bounded : t -> t -> t

val sub_bounded : t -> t -> t

val mul : round:[`Down | `Up] -> t -> t -> t

val mul_q_bounded : round:[`Down | `Up] -> t -> Q.t -> t

(** Constants *)

(** 0% *)
val p0 : t

(** 5% *)
val p5 : t

(** 50% *)
val p50 : t

(** 51% *)
val p51 : t

(** 100% *)
val p100 : t

module Compare : sig
  val ( >= ) : t -> t -> bool
end

val convert_from_o_to_p : t -> t
