(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Slot header representation for the data-availability layer.

    {1 Overview}

    For the data-availability layer, the L1 provides a list of slots
   at every level. A slot is a blob of data that can be interpreted by
   the users of the data-availability layer (such as SCORU).

    The purpose of the data-availability layer is to increase the
   bandwidth of the layer 1 thanks to the distribution of "slots". A
   slot is never posted directly onto the layer 1 blocks but on the
   data-availability layer. The producer of a slot sill has to post a
   slot header onto the layer 1. A slot header is an abstract datatype
   certifying that the corresponding slot has some maximum size
   (provided by the layer 1). In other words, the whole data contained
   into the slot cannot exceed some fixed size. This is to avoid
   attacks where a slot header would be posted onto the layer 1 block,
   declared available by the protocol, but actually the slot size
   would be too large to be refuted a posteriori.

   The slot header can also be used to prove that a blob of data is a
   portion of the initial slot. *)

module Header : sig
  type t

  val encoding : t Data_encoding.t
end

(** An `Index.t` is a possible value for a slot index. We assume this value
    to be a positive 8-bit integer. Note that this is a hard constraint,
    which is independent of protocol constants. If a choice is ever made to
    increase the size of available slots in the protocol, we also need
    to change this module to accommodate for higher values.
*)
module Index : sig
  type t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val zero : t

  val max_value : t

  (** [of_int n] constructs a`Slot_index.t`
      May fail with:
     {ul
       {li [Dal_invalid_slot_header n] if [n] is either negative or greater than [max_slot_value].}
     }
    *)
  val of_int : int -> t option

  val to_int : t -> int

  val compare : t -> t -> int
end

type header = Header.t

type t = private {level : Raw_level_repr.t; index : Index.t; header : header}

type slot = t

(** [make ~level ~index ~header] builds a slot. *)

val make : level:Raw_level_repr.t -> index:Index.t -> header:header -> t

(** The encoding ensures the slot is always a non-negative number. *)
val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** Only one slot header is accepted per slot index. If two slots
   headers are included into a block, the second one will fail.

   Consequently, we rely on the order of operations which is done
   thanks to the fee market.

  This is encapsulated in the following module.  *)
module Slot_market : sig
  (** Represent the fee market for a list of slots. *)
  type t

  (** [init ~length] encodes a list of [length] slots without
     candidates. *)
  val init : length:int -> t

  (** [length t] returns the [length] provided at initialisation time
     (see {!val:init}). *)
  val length : t -> int

  (** [register t index fees] updates the candidate associated to
     index [index]. Returns [Some (_, true)] if the candidate is
     registered. Returns [Some (_, false)] otherwise. Returns [None]
     if the [index] is not in the interval [0;length] where [length]
     is the value provided to the [init] function. *)
  val register : t -> slot -> (t * bool) option

  (** [candidates t] returns a list of slot candidates. *)
  val candidates : t -> slot list
end
