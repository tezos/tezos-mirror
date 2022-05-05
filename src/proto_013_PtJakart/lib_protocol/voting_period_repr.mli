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

(** The voting period kinds are ordered as follows:
    Proposal -> Exploration -> Cooldown -> Promotion -> Adoption.
    This order is the one used be the function [succ] below.
 *)
type kind =
  | Proposal  (** protocols can be proposed *)
  | Exploration  (** a proposal can be voted *)
  | Cooldown  (** a delay before the second vote of the Promotion period. *)
  | Promotion  (** activation can be voted *)
  | Adoption  (** a delay before activation *)

val kind_encoding : kind Data_encoding.t

(** A voting period can be of several kinds and is uniquely identified by
   the counter 'index'. The 'start_position' represents the relative
   position of the first level of the period with respect to the
   first level of the Alpha family of protocols. *)
type voting_period = {index : Int32.t; kind : kind; start_position : Int32.t}

type t = voting_period

(** Information about a block with respect to the voting period it
   belongs to: the voting period, the position within the voting
   period and the number of remaining blocks till the end of the
   period. The following invariant is satisfied:
     `position + remaining + 1 = blocks_per_voting_period` *)
type info = {voting_period : t; position : Int32.t; remaining : Int32.t}

val root : start_position:Int32.t -> t

include Compare.S with type t := voting_period

val encoding : t Data_encoding.t

val info_encoding : info Data_encoding.t

val pp : Format.formatter -> t -> unit

val pp_info : Format.formatter -> info -> unit

val pp_kind : Format.formatter -> kind -> unit

(** [raw_reset period ~start_position] increment the index by one and set the
    kind to Proposal which is the period kind that start the voting
    process. [start_position] is the level at wich this voting_period started.
*)
val raw_reset : t -> start_position:Int32.t -> t

(** [raw_succ period ~start_position] increment the index by one and set the
    kind to its successor. [start_position] is the level at which this
    voting_period started. *)
val raw_succ : t -> start_position:Int32.t -> t

val position_since : Level_repr.t -> t -> Int32.t

val remaining_blocks :
  Level_repr.t -> t -> blocks_per_voting_period:Int32.t -> Int32.t
