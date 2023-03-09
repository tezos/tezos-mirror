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

module type PROTOCOL_SERVICES = sig
  val hash : Protocol_hash.t

  type wrap_full

  val wrap_full : Tezos_client_base.Client_context.full -> wrap_full

  val endorsing_rights :
    wrap_full ->
    reference_level:Int32.t ->
    Int32.t ->
    Consensus_ops.rights tzresult Lwt.t

  (* [couple_ops_to_rights ops rights] returns [(participating,
     missing)], where [participating] is a list associating delegates
     with their operations in [ops], and [missing] is the list of
     delegates which do not have associated operations in [ops].

     TODO: it might be clearer to use a map instead of an association
     list for [participating]. *)
  val couple_ops_to_rights :
    (int * 'a) list ->
    Consensus_ops.rights ->
    (Tezos_crypto.Signature.public_key_hash * 'a) list
    * Tezos_crypto.Signature.public_key_hash list

  type block_id

  module BlockIdMap : Map.S with type key = block_id

  val consensus_operation_stream :
    wrap_full ->
    (((Operation_hash.t
      * ((block_id * Int32.t * Consensus_ops.operation_kind * Int32.t option)
        * int))
     * error trace option)
     Lwt_stream.t
    * Tezos_rpc.Context.stopper)
    tzresult
    Lwt.t

  val baker :
    wrap_full ->
    Block_hash.t ->
    Tezos_crypto.Signature.public_key_hash tzresult Lwt.t

  val block_round : Block_header.t -> int tzresult

  val consensus_ops_info_of_block :
    wrap_full -> Block_hash.t -> Consensus_ops.block_op list tzresult Lwt.t

  val get_block_info :
    wrap_full ->
    Int32.t ->
    (Tezos_crypto.Signature.public_key_hash
    * Time.Protocol.t
    * int
    * Block_hash.t)
    tzresult
    Lwt.t
end
