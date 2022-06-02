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

type header = Header.t

(** A non-negative number which encodes the position of a slot from
   the list of slots provided by the L1. We expect this index to be
   below [256] (see {!val:encoding}). *)
type index = int

(** DAL/FIXME https://gitlab.com/tezos/tezos/-/issues/3145

Consider using the `Bounded` module. *)
type t = private {level : Raw_level_repr.t; index : index; header : header}

type slot = t

(** [make ~level ~index ~header] builds a slot.

    @raise Invalid_arg if [index] is a non-positive number *)
val make : level:Raw_level_repr.t -> index:index -> header:header -> t

(** The encoding ensures the slot index is always a non-negative
   number below strictly below 256. *)
val encoding : t Data_encoding.t

val pp : Format.formatter -> t -> unit

(** Only one slot header is accepted per slot index. If two slots
   headers are included into a block, we use the fee market to know
   which slot header will be chosen.

  This is encapsulated in the following module.
*)
module Slot_market : sig
  (** Represent the fee market for a list of slots. *)
  type t

  (** [init ~length] encodes a list of [length] slots without candidates. *)
  val init : length:int -> t

  (** [current_fees t index] returns [Some fees] if the best candidate
     recorded for slot at index [index] was posted with fees
     [fees]. [None] is returned iff no candidate were recorded or if
     the index is negative. It is the responsability of the caller to
     ensure [index] is below some reasonable upper bound. *)
  val current_fees : t -> index -> Tez_repr.t option

  (** [update t index fees] updates the candidate associated to index
     [index]. Returns [Some (_, true)] if the candidate was better
     than the current one. Returns [Some (_, false)] otherwise. It is
     a no-op if the [index] is not in the interval [0;length] where is
     the value provided to the [init] function. *)
  val update : t -> slot -> Tez_repr.t -> t * bool

  (** [candidates t] returns a list of slot candidates. *)
  val candidates : t -> slot list
end
