(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** [Distributed_db] protocol version. *)

module Name : sig
  type t = private string

  val of_string : string -> t

  val pp : Format.formatter -> t -> unit

  val encoding : t Data_encoding.t

  val equal : t -> t -> bool

  val zero : t
end

(** An abstract version number for the high-level [Distributed_db] messages. *)
type t = private int

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : Format.formatter -> t -> unit

val encoding : t Data_encoding.t

val zero : t

(** This version adds the following messages:

    - [Get_checkpoint/Checkpoint] for fetching the checkpoint of a
   peer.

    - [Get_protocol_branch/Protocol_branch] for fetching the branch of
   a peer given a protocol level.

    - [Get_predecessor_header/Predecessor_header] for fetching a block
   header given an offset from a block hash.

    Those messages can help to implement a faster bootstrap. *)
val one : t

(** This version adds no messages. However, the semantics of the
   message [Current_head] is changed. In particular, a peer can send a
   message [Current_head] which was only [prechecked] but the block
   was not validated yet. This means that a [peer] which sends a
   [Current_head] message with a block for which the [precheck]
   function will return [true] but is invalid should not be kicked
   anymore. *)
val two : t
