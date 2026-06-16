(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(*                                                                           *)
(*****************************************************************************)

(** DAL related parameters for the PVMs.  *)

(** DAL related parameters that would be useful to the kernel. These parameters are a
    subset of the DAL parametric constants defined in {!Constants_parametric_repr}.
    We use [int64] so they should be "large enough" to accommodate encoding changes
    of types in {!Constants_parametric_repr} *)
type t = {
  number_of_slots : int64;
  attestation_lag : int64;
  slot_size : int64;
  page_size : int64;
}

(** Pretty-printer for the parameters. *)
val pp : Format.formatter -> t -> unit

(** Equality of the parameters. *)
val equal : t -> t -> bool

(** Encoding of the parameters. *)
val encoding : t Data_encoding.t
