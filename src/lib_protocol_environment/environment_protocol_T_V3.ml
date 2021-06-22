(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
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

(* Documentation for this interface can be found in
   module type [PROTOCOL] of [sigs/v3/updater.mli]. *)

module type T = sig
  type context

  type quota

  type validation_result

  type rpc_context

  type 'a tzresult

  val max_block_length : int

  val max_operation_data_length : int

  val validation_passes : quota list

  type block_header_data

  val block_header_data_encoding : block_header_data Data_encoding.t

  type block_header = {
    shell : Block_header.shell_header;
    protocol_data : block_header_data;
  }

  type block_header_metadata

  val block_header_metadata_encoding : block_header_metadata Data_encoding.t

  type operation_data

  type operation_receipt

  type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
  }

  val operation_data_encoding : operation_data Data_encoding.t

  val operation_receipt_encoding : operation_receipt Data_encoding.t

  val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t

  val acceptable_passes : operation -> int list

  val relative_position_within_block : operation -> operation -> int

  type validation_state

  val current_context : validation_state -> context tzresult Lwt.t

  val begin_partial_application :
    chain_id:Chain_id.t ->
    ancestor_context:context ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  val begin_application :
    chain_id:Chain_id.t ->
    predecessor_context:context ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_fitness:Fitness.t ->
    block_header ->
    validation_state tzresult Lwt.t

  val begin_construction :
    chain_id:Chain_id.t ->
    predecessor_context:context ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Block_hash.t ->
    timestamp:Time.Protocol.t ->
    ?protocol_data:block_header_data ->
    unit ->
    validation_state tzresult Lwt.t

  val apply_operation :
    validation_state ->
    operation ->
    (validation_state * operation_receipt) tzresult Lwt.t

  val finalize_block :
    validation_state ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  val rpc_services : rpc_context RPC_directory.t

  val init :
    context -> Block_header.shell_header -> validation_result tzresult Lwt.t
end
