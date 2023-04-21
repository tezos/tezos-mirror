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
   module type [PROTOCOL] of [sigs/v10/updater.mli]. *)

module type T = sig
  type context

  type quota

  type validation_result

  type rpc_context

  type tztrace

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

  val acceptable_pass : operation -> int option

  val compare_operations :
    Tezos_crypto.Hashed.Operation_hash.t * operation ->
    Tezos_crypto.Hashed.Operation_hash.t * operation ->
    int

  type validation_state

  type application_state

  type mode =
    | Application of block_header
    | Partial_validation of block_header
    | Construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
        block_header_data : block_header_data;
      }
    | Partial_construction of {
        predecessor_hash : Tezos_crypto.Hashed.Block_hash.t;
        timestamp : Time.Protocol.t;
      }

  val begin_validation :
    context ->
    Tezos_crypto.Hashed.Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    validation_state tzresult Lwt.t

  val validate_operation :
    ?check_signature:bool ->
    validation_state ->
    Tezos_crypto.Hashed.Operation_hash.t ->
    operation ->
    validation_state tzresult Lwt.t

  val finalize_validation : validation_state -> unit tzresult Lwt.t

  val begin_application :
    context ->
    Tezos_crypto.Hashed.Chain_id.t ->
    mode ->
    predecessor:Block_header.shell_header ->
    application_state tzresult Lwt.t

  val apply_operation :
    application_state ->
    Tezos_crypto.Hashed.Operation_hash.t ->
    operation ->
    (application_state * operation_receipt) tzresult Lwt.t

  val finalize_application :
    application_state ->
    Block_header.shell_header option ->
    (validation_result * block_header_metadata) tzresult Lwt.t

  val rpc_services : rpc_context Tezos_rpc.Directory.t

  val init :
    Tezos_crypto.Hashed.Chain_id.t ->
    context ->
    Block_header.shell_header ->
    validation_result tzresult Lwt.t

  type cache_value

  type cache_key

  val value_of_key :
    chain_id:Tezos_crypto.Hashed.Chain_id.t ->
    predecessor_context:context ->
    predecessor_timestamp:Time.Protocol.t ->
    predecessor_level:Int32.t ->
    predecessor_fitness:Fitness.t ->
    predecessor:Tezos_crypto.Hashed.Block_hash.t ->
    timestamp:Time.Protocol.t ->
    (cache_key -> cache_value tzresult Lwt.t) tzresult Lwt.t

  module Mempool : sig
    type t

    type validation_info

    type conflict_handler =
      existing_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      new_operation:Tezos_crypto.Hashed.Operation_hash.t * operation ->
      [`Keep | `Replace]

    type operation_conflict =
      | Operation_conflict of {
          existing : Tezos_crypto.Hashed.Operation_hash.t;
          new_operation : Tezos_crypto.Hashed.Operation_hash.t;
        }

    type add_result =
      | Added
      | Replaced of {removed : Tezos_crypto.Hashed.Operation_hash.t}
      | Unchanged

    type add_error =
      | Validation_error of tztrace
      | Add_conflict of operation_conflict

    type merge_error =
      | Incompatible_mempool
      | Merge_conflict of operation_conflict

    val init :
      context ->
      Tezos_crypto.Hashed.Chain_id.t ->
      head_hash:Tezos_crypto.Hashed.Block_hash.t ->
      head:Block_header.shell_header ->
      (validation_info * t) tzresult Lwt.t

    val encoding : t Data_encoding.t

    val add_operation :
      ?check_signature:bool ->
      ?conflict_handler:conflict_handler ->
      validation_info ->
      t ->
      Tezos_crypto.Hashed.Operation_hash.t * operation ->
      (t * add_result, add_error) result Lwt.t

    val remove_operation : t -> Tezos_crypto.Hashed.Operation_hash.t -> t

    val merge :
      ?conflict_handler:conflict_handler -> t -> t -> (t, merge_error) result

    val operations : t -> operation Tezos_crypto.Hashed.Operation_hash.Map.t
  end
end
