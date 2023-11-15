(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

(** Ethereum Bloom filters.
    See Ethereum yellow paper, section 4.3.1 for reference. *)

(** Type of Bloom filters, represented as a 256-byte string.
    Note that values of this type are mutable. *)
type t = bytes

(** [make ()] returns a new empty filter. *)
val make : unit -> t

(** [is_empty filter] returns whether the [filter] is empty. *)
val is_empty : t -> bool

(** [contains_input ~input filter] returns whether the
    [filter] contains [input]. *)
val contains_input : input:hex -> t -> bool

(** [contains_bloom filter1 filter2] returns whether
    [filter1] contains [filter2]. *)
val contains_bloom : t -> t -> bool

(** [accrue ~input filter] extends [filter] by adding
    [input]. *)
val accrue : input:hex -> t -> unit

(** [accrue_bloom filter1 filter2] extends [filter1] by adding
    all of the elements from [filter2]. *)
val accrue_bloom : t -> t -> unit
