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

module Type : sig
  val time_protocol : Tezos_base.Time.Protocol.t Caqti_type.t

  val block_hash : Tezos_crypto.Block_hash.t Caqti_type.t

  val operation_hash : Tezos_crypto.Operation_hash.t Caqti_type.t

  val public_key_hash : Tezos_crypto.Signature.public_key_hash Caqti_type.t

  val errors : Tezos_error_monad.TzCore.error list option Caqti_type.t
end

val bool_to_int : bool -> int

val create_tables : string list

val db_schema : string

val maybe_insert_source : (string, unit, [`Zero]) Caqti_request.t

val maybe_insert_delegates_from_rights :
  Teztale_lib.Consensus_ops.rights -> string

val maybe_insert_delegates_from_received :
  Teztale_lib.Consensus_ops.delegate_ops -> string

val maybe_insert_endorsing_rights :
  level:Int32.t -> Teztale_lib.Consensus_ops.rights -> string

val maybe_insert_operations_from_block :
  level:Int32.t -> Teztale_lib.Consensus_ops.block_op list -> string

val maybe_insert_operations_from_received :
  level:Int32.t -> Teztale_lib.Consensus_ops.delegate_ops -> string

val maybe_insert_block :
  ( (int32 (* level *) * Tezos_base.Time.Protocol.t * Tezos_crypto.Block_hash.t)
    * (Tezos_crypto.Signature.public_key_hash * int32 (* round *)),
    unit,
    [`Zero] )
  Caqti_request.t

val insert_received_operations :
  source:string ->
  level:Int32.t ->
  (Tezos_crypto.Signature.public_key_hash
  * Teztale_lib.Consensus_ops.received_operation list)
  list ->
  string

val insert_included_operations :
  Tezos_crypto.Block_hash.t ->
  level:Int32.t ->
  Teztale_lib.Consensus_ops.block_op list ->
  string

val insert_received_block :
  (Ptime.t * Tezos_crypto.Block_hash.t * string, unit, [`Zero]) Caqti_request.t
