(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type read_write = Read | Write

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

let encoding =
  let open Data_encoding in
  let encoding_read_write =
    union
      [
        case
          (Tag 0x01)
          ~title:"Read"
          (constant "Read")
          (function Read -> Some () | _ -> None)
          (fun () -> Read);
        case
          (Tag 0x02)
          ~title:"Write"
          (constant "Write")
          (function Write -> Some () | _ -> None)
          (fun () -> Write);
      ]
  in
  union
    [
      case
        (Tag 0x01)
        ~title:"Connection_lost"
        (obj2
           (req "reason" (constant "Connection_lost"))
           (req "while" encoding_read_write))
        (function Connection_lost rw -> Some ((), rw) | _ -> None)
        (fun ((), rw) -> Connection_lost rw);
      case
        (Tag 0x02)
        ~title:"Connection_closed_by_peer"
        (obj2
           (req "reason" (constant "Connection_closed_by_peer"))
           (req "while" encoding_read_write))
        (function Connection_closed_by_peer rw -> Some ((), rw) | _ -> None)
        (fun ((), rw) -> Connection_closed_by_peer rw);
      case
        (Tag 0x03)
        ~title:"Connection_closed_by_unexpected_error"
        (obj3
           (req "reason" (constant "Connection_closed_by_unexpected_error"))
           (req "while" encoding_read_write)
           (req "error" string))
        (function
          | Connection_closed_by_unexpected_error (rw, err) -> Some ((), rw, err)
          | _ -> None)
        (fun ((), rw, err) -> Connection_closed_by_unexpected_error (rw, err));
      case
        (Tag 0x04)
        ~title:"TCP_connection_refused"
        (constant "TCP_connection_refused")
        (function TCP_connection_refused -> Some () | _ -> None)
        (fun () -> TCP_connection_refused);
      case
        (Tag 0x05)
        ~title:"TCP_connection_unreachable"
        (constant "TCP_connection_unreachable")
        (function TCP_connection_unreachable -> Some () | _ -> None)
        (fun () -> TCP_connection_unreachable);
      case
        (Tag 0x06)
        ~title:"TCP_connection_canceled"
        (constant "TCP_connection_canceled")
        (function TCP_connection_canceled -> Some () | _ -> None)
        (fun () -> TCP_connection_canceled);
      case
        (Tag 0x07)
        ~title:"TCP_connection_failed_unexpected_error"
        (obj2
           (req "reason" (constant "TCP_connection_failed_unexpected_error"))
           (req "error" string))
        (function
          | TCP_connection_failed_unexpected_error exn -> Some ((), exn)
          | _ -> None)
        (fun ((), exn) -> TCP_connection_failed_unexpected_error exn);
      case
        (Tag 0x08)
        ~title:"Scheduled_pop_unexpected_error"
        (obj2
           (req "reason" (constant "Scheduled_pop_unexpected_error"))
           (req "trace" Error_monad.trace_encoding))
        (function
          | Scheduled_pop_unexpected_error trace -> Some ((), trace) | _ -> None)
        (fun ((), trace) -> Scheduled_pop_unexpected_error trace);
      case
        (Tag 0x09)
        ~title:"Scheduled_push_unexpected_error"
        (obj2
           (req "reason" (constant "Scheduled_push_unexpected_error"))
           (req "trace" Error_monad.trace_encoding))
        (function
          | Scheduled_push_unexpected_error trace -> Some ((), trace)
          | _ -> None)
        (fun ((), trace) -> Scheduled_push_unexpected_error trace);
      case
        (Tag 0x10)
        ~title:"IO_scheduler_closed"
        (constant "IO_scheduler_closed")
        (function IO_scheduler_closed -> Some () | _ -> None)
        (fun () -> IO_scheduler_closed);
      case
        (Tag 0x11)
        ~title:"IO_scheduler_shutdown"
        (constant "IO_scheduler_shutdown")
        (function IO_scheduler_shutdown -> Some () | _ -> None)
        (fun () -> IO_scheduler_shutdown);
      case
        (Tag 0x12)
        ~title:"Accept_write_error"
        (obj2
           (req "reason" (constant "Accept_write_error"))
           (req "trace" Error_monad.trace_encoding))
        (function Accept_write_error trace -> Some ((), trace) | _ -> None)
        (fun ((), trace) -> Accept_write_error trace);
      case
        (Tag 0x13)
        ~title:"Ack_read_error"
        (obj2
           (req "reason" (constant "Ack_read_error"))
           (req "" Error_monad.trace_encoding))
        (function Ack_read_error trace -> Some ((), trace) | _ -> None)
        (fun ((), trace) -> Ack_read_error trace);
      case
        (Tag 0x14)
        ~title:"Authentication_rejected_no_common_protocol"
        (constant "Authentication_rejected_no_common_protocol")
        (function
          | Authentication_rejected_no_common_protocol -> Some () | _ -> None)
        (fun () -> Authentication_rejected_no_common_protocol);
      case
        (Tag 0x15)
        ~title:"Authentication_rejected"
        (obj2
           (req "reason" (constant "Authentication_rejected"))
           (req "" P2p_rejection.encoding))
        (function
          | Authentication_rejected motive -> Some ((), motive) | _ -> None)
        (fun ((), motive) -> Authentication_rejected motive);
      case
        (Tag 0x16)
        ~title:"Authentication_rejected_by_peer"
        (obj2
           (req "reason" (constant "Authentication_rejected_by_peer"))
           (req "" P2p_rejection.encoding))
        (function
          | Authentication_rejected_by_peer motive -> Some ((), motive)
          | _ -> None)
        (fun ((), motive) -> Authentication_rejected_by_peer motive);
      case
        (Tag 0x17)
        ~title:"Authentication_rejected_error"
        (obj2
           (req "reason" (constant "Authentication_rejected_error"))
           (req "" Error_monad.trace_encoding))
        (function
          | Authentication_rejected_error err -> Some ((), err) | _ -> None)
        (fun ((), err) -> Authentication_rejected_error err);
      case
        (Tag 0x18)
        ~title:"Incoming_connection_too_many"
        (constant "Incoming_connection_too_many")
        (function Incoming_connection_too_many -> Some () | _ -> None)
        (fun () -> Incoming_connection_too_many);
      case
        (Tag 0x19)
        ~title:"Incoming_connection_banned"
        (constant "Incoming_connection_banned")
        (function Incoming_connection_banned -> Some () | _ -> None)
        (fun () -> Incoming_connection_banned);
      case
        (Tag 0x20)
        ~title:"Unexpected_peer_id"
        (constant "Unexpected_peer_id")
        (function Unexpected_peer_id -> Some () | _ -> None)
        (fun () -> Unexpected_peer_id);
      case
        (Tag 0x21)
        ~title:"IP_manually_banned"
        (constant "IP_manually_banned")
        (function IP_manually_banned -> Some () | _ -> None)
        (fun () -> IP_manually_banned);
      case
        (Tag 0x22)
        ~title:"Peer_id_manually_banned"
        (constant "Peer_id_manually_banned")
        (function Peer_id_manually_banned -> Some () | _ -> None)
        (fun () -> Peer_id_manually_banned);
      case
        (Tag 0x23)
        ~title:"Pool_destroyed"
        (constant "Pool_destroyed")
        (function Pool_destroyed -> Some () | _ -> None)
        (fun () -> Pool_destroyed);
      case
        (Tag 0x24)
        ~title:"Peer_swapped"
        (constant "Peer_swapped")
        (function Peer_swapped -> Some () | _ -> None)
        (fun () -> Peer_swapped);
      case
        (Tag 0x25)
        ~title:"Explicit_RPC"
        (constant "Explicit_RPC")
        (function Explicit_RPC -> Some () | _ -> None)
        (fun () -> Explicit_RPC);
      case
        (Tag 0x26)
        ~title:"Maintenance_too_many"
        (constant "Maintenance_too_many")
        (function Maintenance_too_many -> Some () | _ -> None)
        (fun () -> Maintenance_too_many);
      case
        (Tag 0x27)
        ~title:"User"
        (obj2 (req "reason" (constant "User")) (req "" string))
        (function User reason -> Some ((), reason) | _ -> None)
        (fun ((), reason) -> User reason);
      case
        (Tag 0x28)
        ~title:"Unknown_reason"
        (constant "Unknown_reason")
        (function Unknown_reason -> Some () | _ -> None)
        (fun () -> Unknown_reason);
    ]

let pp fmt =
  let pp_read_write fmt = function
    | Read -> Format.fprintf fmt "receiving"
    | Write -> Format.fprintf fmt "sending"
  in
  function
  | Connection_lost rw ->
      Format.fprintf fmt "connection lost while %a data" pp_read_write rw
  | Connection_closed_by_peer rw ->
      Format.fprintf
        fmt
        "connection closed by peer while %a data"
        pp_read_write
        rw
  | Connection_closed_by_unexpected_error (rw, exn) ->
      Format.fprintf
        fmt
        "connection closed by unexpected error while %a data: %s"
        pp_read_write
        rw
        exn
  | TCP_connection_refused -> Format.fprintf fmt "TCP connection refused"
  | TCP_connection_unreachable ->
      Format.fprintf fmt "TCP connection, peer unreachable"
  | TCP_connection_canceled -> Format.fprintf fmt "TCP connection cancelled"
  | TCP_connection_failed_unexpected_error ex ->
      Format.fprintf fmt "TCP connection failed with unexpected error, %s" ex
  | Scheduled_pop_unexpected_error trace ->
      Format.fprintf
        fmt
        "unexpected error when poping on scheduled connection, %a"
        Error_monad.pp_print_top_error_of_trace
        trace
  | Scheduled_push_unexpected_error trace ->
      Format.fprintf
        fmt
        "unexpected error when pushing on scheduled connection, %a"
        Error_monad.pp_print_top_error_of_trace
        trace
  | IO_scheduler_closed -> Format.fprintf fmt "IO scheduler is closed"
  | IO_scheduler_shutdown -> Format.fprintf fmt "IO scheduler is shutting down"
  | Accept_write_error trace ->
      Format.fprintf
        fmt
        "error when sending accept, %a"
        Error_monad.pp_print_top_error_of_trace
        trace
  | Ack_read_error trace ->
      Format.fprintf
        fmt
        "error when receiving ack, %a"
        Error_monad.pp_print_top_error_of_trace
        trace
  | Authentication_rejected_no_common_protocol ->
      Format.fprintf fmt "authentication rejected by us: no common protocol"
  | Authentication_rejected motive ->
      Format.fprintf
        fmt
        "authentication rejected by us with motive: %a"
        P2p_rejection.pp_short
        motive
  | Authentication_rejected_by_peer motive ->
      Format.fprintf
        fmt
        "authentication rejected by peer with motive: %a"
        P2p_rejection.pp_short
        motive
  | Authentication_rejected_error err ->
      Format.fprintf
        fmt
        "authentication has been rejected from error: %a"
        Error_monad.pp_print_top_error_of_trace
        err
  | Incoming_connection_too_many ->
      Format.fprintf
        fmt
        "incoming connection not accepted: too many incoming connections"
  | Incoming_connection_banned ->
      Format.fprintf fmt "incoming connection not accepted: point is banned"
  | Unexpected_peer_id -> Format.fprintf fmt "unexpected peer id"
  | IP_manually_banned -> Format.fprintf fmt "IP manually banned"
  | Peer_id_manually_banned -> Format.fprintf fmt "peer_id manually banned"
  | Pool_destroyed -> Format.fprintf fmt "pool destroyed"
  | Peer_swapped -> Format.fprintf fmt "peer has been swapped"
  | Explicit_RPC -> Format.fprintf fmt "disconnected using a RPC"
  | Maintenance_too_many ->
      Format.fprintf fmt "maintenance detected too many connections"
  | User reason -> Format.fprintf fmt "%s" reason
  | Unknown_reason -> Format.fprintf fmt "connection canceled without reason"
