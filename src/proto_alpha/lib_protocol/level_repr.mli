(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** This module defines the protocol representation of a level. Besides the "raw
    level", which is the shell's notion of the level, this representation also
    contains additional information, like the cycle the level belongs to. *)

type t = private {
  level : Raw_level_repr.t;
      (** The level of the block relative to genesis. This
                              is also the Shell's notion of level. *)
  level_position : int32;
      (** The level of the block relative to the block that starts the
     alpha family of protocols.  *)
  cycle : Cycle_repr.t;
      (** The current cycle's number. Note that cycles are a protocol-specific
     notion. As a result, the cycle number starts at 0 with the first block of
     the first version of protocol alpha. *)
  cycle_position : int32;
      (** The current level of the block relative to the first block of the current
     cycle. *)
  expected_commitment : bool;
}

type level = t

include Compare.S with type t := level

val encoding : level Data_encoding.t

val pp : Format.formatter -> level -> unit

val pp_full : Format.formatter -> level -> unit

val diff : level -> level -> int32

(** A cycle era is a chunk of cycles having the same number of levels
   per cycle and the same number of blocks per commitment. *)
type cycle_era = {
  first_level : Raw_level_repr.t;  (** The first level of a cycle era. *)
  first_cycle : Cycle_repr.t;  (** The first cycle of a cycle era. *)
  blocks_per_cycle : int32;
      (** The value of the blocks_per_cycle constant used during the cycle
       era starting with first_level. *)
  blocks_per_commitment : int32;
      (** The value of the blocks_per_commitment constant used during the
       cycle era starting with first_level. *)
}

(** Stores the cycles eras of the Alpha family of protocols *)
type cycle_eras

val cycle_eras_encoding : cycle_eras Data_encoding.t

(** Preconditions on the input list of cycle eras:
   - the list is not empty
   - the first levels and the first cycles are decreasing, meaning that the
     first era in the list is the current era, and the last era in the list
     is the oldest era
   Invariants:
   - the first era therefore contains the same constants as in Constants
   - the first level of an era is the first level of a cycle
*)
val create_cycle_eras : cycle_era list -> cycle_eras tzresult

(** Add a new cycle era *)
val add_cycle_era : cycle_era -> cycle_eras -> cycle_eras tzresult

(** Returns the current era *)
val current_era : cycle_eras -> cycle_era

(** Returns the first level of the oldest era *)
val root_level : cycle_eras -> level

(** Returns the cycle corresponding to a raw level *)
val cycle_from_raw : cycle_eras:cycle_eras -> Raw_level_repr.t -> Cycle_repr.t

(** Returns the annotated level corresponding to a raw level *)
val level_from_raw : cycle_eras:cycle_eras -> Raw_level_repr.t -> level

(** Returns the annotated level corresponding to a raw level and an
   offset. A positive offset corresponds to a higher level.
   Fails with [Negative_level_and_offset_sum] if the sum of the raw_level and the offset is negative.
   Fails with [Level_not_in_alpha] if the sum of the raw_level and the offset 
   is a level before the first level in the Alpha family of protocols. *)
val level_from_raw_with_offset :
  cycle_eras:cycle_eras -> offset:int32 -> Raw_level_repr.t -> level tzresult

(** Returns the first level of the given cycle. *)
val first_level_in_cycle_from_eras :
  cycle_eras:cycle_eras -> Cycle_repr.t -> level

(** Returns true if the given level is the last of a cycle. *)
val last_of_cycle : cycle_eras:cycle_eras -> level -> bool

module Internal_for_tests : sig
  val add_level : t -> int -> t

  val add_cycles : blocks_per_cycle:int -> t -> int -> t
end

(**/**)
