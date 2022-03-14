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

(** Error issued when the rollup referenced by its hash has not been created
    on the block referenced by its hash. The node computes a state from the
    block that created the rollup. *)
type error +=
  | Tx_rollup_not_originated_in_the_given_block of
      Protocol.Alpha_context.Tx_rollup.t

(** Error issued when the rollup genesis block is (or moves) to a different
    branch. *)
type error += Tx_rollup_originated_in_fork

(** Error issued when the daemon attempts to process the operations of a block
    for which the predecessor has not yet been processed. *)
type error += Tx_rollup_block_predecessor_not_processed of Block_hash.t

(** Error issued when the encoding of a value, in order to be persisted in the
    store, fails. *)
type error +=
  | Tx_rollup_unable_to_encode_storable_value of string * Data_encoding.Json.t

(** Error issued when decoding a persisted value in the store fails. *)
type error += Tx_rollup_unable_to_decode_stored_value of string * string

(** Error issued when an error occurs on Irmin side. *)
type error += Tx_rollup_irmin_error of string

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
type error +=
  | Tx_rollup_no_rollup_origination_on_disk_and_no_rollup_genesis_given

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
