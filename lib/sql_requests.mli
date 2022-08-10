(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

val bool_to_int : bool -> int

val create_tables : string list

val db_schema : string

val maybe_insert_source : string -> string

val maybe_insert_delegates_from_rights : Consensus_ops.rights -> string

val maybe_insert_delegates_from_received : Consensus_ops.delegate_ops -> string

val maybe_insert_endorsing_rights :
  level:Int32.t -> Consensus_ops.rights -> string

val maybe_insert_operations_from_block :
  level:Int32.t -> Consensus_ops.block_op list -> string

val maybe_insert_operations_from_received :
  level:Int32.t -> Consensus_ops.delegate_ops -> string

val maybe_insert_block :
  Block_hash.t ->
  level:Int32.t ->
  round:Int32.t ->
  Time.Protocol.t ->
  Signature.public_key_hash ->
  string

val insert_received_operations :
  source:string ->
  level:Int32.t ->
  (Signature.public_key_hash * Consensus_ops.received_operation list) list ->
  string

val insert_included_operations :
  Block_hash.t -> level:Int32.t -> Consensus_ops.block_op trace -> string

val insert_received_block : source:string -> Block_hash.t -> Ptime.t -> string
