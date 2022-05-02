(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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

(** The node encountered a fatal error which prevents it from working \
    properly. *)
type error += Tx_rollup_fatal

(** Add the [Tx_rollup_fatal] error to any error occurring in the promise. *)
val trace_fatal : 'a tzresult Lwt.t -> 'a tzresult Lwt.t

(** Internal error in rollup node *)
type error += Tx_rollup_internal of string

(** Error issued when the rollup referenced by its hash has not been created
    on the block referenced by its hash. The node computes a state from the
    block that created the rollup. *)
type error +=
  | Tx_rollup_not_originated_in_the_given_block of
      Protocol.Alpha_context.Tx_rollup.t

(** Error issued when the rollup genesis block is (or moves) to a different
    branch. *)
type error += Tx_rollup_originated_in_fork

(** Error issued when the configuration file does not exists. *)
type error += Tx_rollup_configuration_file_does_not_exists of string

(** Error issued when the configuration file cannot be write. *)
type error += Tx_rollup_unable_to_write_configuration_file of string

(** Error issued when the Tx rollup node try to parse an invalid rollup l2 address. *)
type error += Tx_rollup_invalid_l2_address of Micheline.canonical_location

(** Error issued when a ticket amount is invalid. *)
type error += Tx_rollup_invalid_ticket_amount of Z.t

(** Error issued when a deposit is invalid. *)
type error += Tx_rollup_invalid_deposit

(** Error issued when context cannot be retrieved. *)
type error +=
  | Tx_rollup_cannot_checkout_context of Protocol.Tx_rollup_l2_context_hash.t

(** Error issued when the Tx rollup node starts without a rollup origination
    stored on disk and when there is no given rollup genesis. *)
type error += Tx_rollup_no_rollup_info_on_disk_and_no_rollup_genesis_given

(** Error issued when the Tx rollup node starts with a rollup origination stored on disk
    different from the given rollup genesis. *)
type error +=
  | Tx_rollup_different_disk_stored_origination_rollup_and_given_rollup_genesis of {
      disk_rollup_origination : Block_hash.t;
      given_rollup_genesis : Block_hash.t;
    }

(** Error when operation metadata is not available. *)
type error += Tx_rollup_no_operation_metadata of Operation_hash.t

(** Error when rollup stored on disk is different from the expected one. *)
type error += Tx_rollup_mismatch

(** Error when Tezos block cannot be fetched. *)
type error += Tx_rollup_cannot_fetch_tezos_block of Block_hash.t

(** Error when the tree is not found in the context. *)
type error += Tx_rollup_tree_not_found

(** Error when the kinded key is not found in the tree. *)
type error += Tx_rollup_tree_kinded_key_not_found

(** Error when a message position does not exist in the inbox for the proof RPC *)
type error += Tx_rollup_invalid_message_position_in_inbox of int

(** Error when the injector has no worker for the source which must inject an
    operation. *)
type error += No_worker_for_source of Signature.Public_key_hash.t

(** Error when we want to interact with the batcher but it was not started. *)
type error += No_batcher

(** Error when a ticket is not registered for a ticket index *)
type error +=
  | Tx_rollup_unknown_ticket of
      Protocol.Tx_rollup_l2_context_sig.Ticket_indexable.either

(** Error when the tezos node does not know the inbox *)
type error +=
  | Tx_rollup_no_proto_inbox of
      Protocol.Alpha_context.Tx_rollup_level.t * Block_hash.t

(** Error when the node reconstructed a different inbox than the one stored on L1 *)
type error +=
  | Tx_rollup_inbox_mismatch of {
      level : Protocol.Alpha_context.Tx_rollup_level.t;
      reconstructed_inbox : Protocol.Alpha_context.Tx_rollup_inbox.t;
      protocol_inbox : Protocol.Alpha_context.Tx_rollup_inbox.t;
    }

(** Error when the transaction submitted to the batcher produces a too large
    message regarding the layer1 limit. *)
type error += Transaction_too_large of {actual : int; limit : int}

(** Mismatch between the chosen mode and the provided signers. *)
type error +=
  | Tx_rollup_mismatch_mode_signers of {
      mode : string;
      missing_signers : string list;
      extra_signers : string list;
    }

(** Error returned when the rollup node is not authorized to make deposits *)
type error += Tx_rollup_deposit_not_allowed

(** Error (fatal) when we are slashed *)
type error += Tx_rollup_deposit_slashed of Operation_hash.t
