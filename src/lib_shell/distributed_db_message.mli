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

(** Tezos Shell - Network message for the gossip P2P protocol. *)

(** We must make sure that we are able to associate a response to the
    corresponding request. Thus, we are able to discrimate between
    expected responses from the unexpected ones. *)
type t =
  | Get_current_branch of Chain_id.t
  | Current_branch of Chain_id.t * Block_locator.t
  | Deactivate of Chain_id.t
  | Get_current_head of Chain_id.t
  | Current_head of Chain_id.t * Block_header.t * Mempool.t
  | Get_block_headers of Block_hash.t list
  | Block_header of Block_header.t
  | Get_operations of Operation_hash.t list
  | Operation of Operation.t
  | Get_protocols of Protocol_hash.t list
  | Protocol of Protocol.t
  | Get_operations_for_blocks of (Block_hash.t * int) list
  | Operations_for_block of
      Block_hash.t * int * Operation.t list * Operation_list_list_hash.path
  | Get_checkpoint of Chain_id.t
  | Checkpoint of Chain_id.t * Block_header.t
  | Get_protocol_branch of Chain_id.t * int
  | Protocol_branch of Chain_id.t * int * Block_locator.t
  | Get_predecessor_header of Block_hash.t * int32
  | Predecessor_header of Block_hash.t * int32 * Block_header.t

val encoding : t P2p_params.app_message_encoding list

val distributed_db_versions : Distributed_db_version.t list

val cfg : Distributed_db_version.Name.t -> t P2p_params.message_config

val pp_json : Format.formatter -> t -> unit

module Bounded_encoding : sig
  val set_block_header_max_size : int -> unit

  val set_block_locator_max_length : int -> unit

  val set_operation_max_size : int option -> unit

  val set_operation_list_max_size : int option -> unit

  val set_operation_list_max_length : int option -> unit

  val set_operation_max_pass : int option -> unit

  val set_protocol_max_size : int option -> unit

  val set_mempool_max_operations : int option -> unit
end
