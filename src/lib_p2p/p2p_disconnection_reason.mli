(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(* Some disconnection reasons are the same for reading or writing. This type
   prevent duplicating these reasons. *)
type read_write = Read | Write

(* The reasons to trigger a disconnection *)
type t =
  | Connection_lost of read_write
  | Connection_closed_by_peer of read_write
  | Connection_closed_by_unexpected_error of (read_write * string)
  | TCP_connection_refused
  | TCP_connection_unreachable
  | TCP_connection_canceled
  | TCP_connection_failed_unexpected_error of string
  | Scheduled_pop_unexpected_error of tztrace
  | Scheduled_push_unexpected_error of tztrace
  | IO_scheduler_closed
  | IO_scheduler_shutdown
  | Accept_write_error of tztrace
  | Ack_read_error of tztrace
  | Authentication_rejected_no_common_protocol
  | Authentication_rejected of P2p_rejection.t
  | Authentication_rejected_by_peer of P2p_rejection.t
  | Authentication_rejected_error of tztrace
  | Incoming_connection_too_many
  | Incoming_connection_banned
  | Unexpected_peer_id
  | IP_manually_banned
  | Peer_id_manually_banned
  | Pool_destroyed
  | Peer_swapped
  | Explicit_RPC
  | Maintenance_too_many
  | User of string
  | Unknown_reason

(* Encoding of disconnection reasons *)
val encoding : t Data_encoding.encoding

(* Pretty printer of disconnection reasons *)
val pp : Format.formatter -> t -> unit
