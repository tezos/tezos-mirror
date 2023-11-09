(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** An integer value between 0 and 100, inclusive. *)
type t = private int

val encoding : t Data_encoding.t

val of_ratio_bounded : Ratio_repr.t -> t

(** [neg p] is [100 - p]. *)
val neg : t -> t

val add_bounded : t -> t -> t

val sub_bounded : t -> t -> t

(** Constants *)

(** 0 *)
val p0 : t

(** 7 *)
val p7 : t

(** 50 *)
val p50 : t
