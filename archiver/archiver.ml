(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

module type S = sig
  type t

  val name : string

  val launch : t -> string -> unit Lwt.t

  val stop : unit -> unit

  (* [add_received ?unaccurate level ops] adds information about the list of
     received consensus operations [ops], all at level [level]. [unaccurate] is
     true iff the [level] is the same as the current head's level. [ops] is an
     association list of tuples [(delegate, ops)], where [ops] is a list of
     operations all produced by [delegate]. *)
  val add_mempool :
    ?unaccurate:bool -> level:Int32.t -> Consensus_ops.delegate_ops -> unit

  (* [add_applied_block level block_info consensus_ops] adds information about
     a newly applied block, like its level, hash, round, its
     timestamp, its reception time, and the included consensus
     operations. *)
  val add_applied_block :
    level:Int32.t ->
    Data.Block.t * (Consensus_ops.block_op list * Consensus_ops.block_op list) ->
    unit

  (* [add_validated_block level block_info consensus_ops] adds information about
     a newly validated block, like its level, hash, round, its
     timestamp, its reception time, and the included consensus
     operations. *)
  val add_validated_block :
    level:Int32.t ->
    Data.Block.t * (Consensus_ops.block_op list * Consensus_ops.block_op list) ->
    unit

  val add_rights : level:Int32.t -> Consensus_ops.rights -> unit
end
