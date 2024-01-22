(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(************************ p2p io scheduler ********************************)

type error += Connection_closed

type error += Connection_error

let () =
  (* Connection closed *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_io_scheduler.connection_closed"
    ~title:"Connection closed"
    ~description:"IO error: connection with a peer is closed."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "IO error: connection with a peer is closed.")
    Data_encoding.empty
    (function Connection_closed -> Some () | _ -> None)
    (fun () -> Connection_closed) ;
  register_error_kind
    `Permanent
    ~id:"node.p2p_io_scheduler.connection_error"
    ~title:"Connection error"
    ~description:"IO error: connection error while reading from a peer."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "IO error: connection error while reading from a peer.")
    Data_encoding.empty
    (function Connection_error -> Some () | _ -> None)
    (fun () -> Connection_error)

(***************************** p2p socket *********************************)

type error += Decipher_error

type error += Invalid_message_size

type error += Invalid_incoming_ciphertext_size

type error += Rejected_socket_connection

type error +=
  | Rejected_by_nack of {
      motive : P2p_rejection.t;
      alternative_points : P2p_point.Id.t list option;
    }

type error += Rejected_no_common_protocol of {announced : Network_version.t}

type error += Decoding_error of Data_encoding.Binary.read_error

type error += Myself of P2p_connection.Id.t

type error += Not_enough_proof_of_work of P2p_peer.Id.t

type error += Invalid_auth

type error += Invalid_chunks_size of {value : int; min : int; max : int}

let () =
  (* Decipher error *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.decipher_error"
    ~title:"Decipher error"
    ~description:"An error occurred while deciphering."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "An error occurred while deciphering.")
    Data_encoding.empty
    (function Decipher_error -> Some () | _ -> None)
    (fun () -> Decipher_error) ;
  (* Invalid message size *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_message_size"
    ~title:"Invalid message size"
    ~description:"The size of the message to be written is invalid."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "The size of the message to be written is invalid.")
    Data_encoding.empty
    (function Invalid_message_size -> Some () | _ -> None)
    (fun () -> Invalid_message_size) ;
  (* Invalid incoming ciphertext size *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_incoming_ciphertext_size"
    ~title:"Invalid incoming ciphertext size"
    ~description:"The announced size for the incoming ciphertext is invalid."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The announced size for the incoming ciphertext is invalid.")
    Data_encoding.empty
    (function Invalid_incoming_ciphertext_size -> Some () | _ -> None)
    (fun () -> Invalid_incoming_ciphertext_size) ;
  (* Rejected socket connection *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejected_socket_connection"
    ~title:"Rejected socket connection"
    ~description:"Rejected peer connection: rejected socket connection."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Rejected peer connection: rejected socket connection.")
    Data_encoding.empty
    (function Rejected_socket_connection -> Some () | _ -> None)
    (fun () -> Rejected_socket_connection) ;
  (* Rejected socket connection, peer gave us with alternative points *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejected_by_nack"
    ~title:"Rejected socket connection by Nack"
    ~description:
      "Rejected peer connection: The peer rejected the socket connection by \
       Nack with a list of alternative peers."
    ~pp:(fun ppf (motive, alt_points) ->
      Format.fprintf
        ppf
        "Rejected peer connection: Peer rejected us on motive \"%a\" and \
         proposed %a alternative peers."
        P2p_rejection.pp
        motive
        (fun ppf alt_points ->
          match alt_points with
          | None -> Format.pp_print_string ppf "no"
          | Some l -> Format.pp_print_int ppf @@ List.length l)
        alt_points)
    Data_encoding.(
      obj2
        (req "motive" P2p_rejection.encoding)
        (opt "alternative_points" (list P2p_point.Id.encoding)))
    (function
      | Rejected_by_nack {motive; alternative_points} ->
          Some (motive, alternative_points)
      | _ -> None)
    (fun (motive, alternative_points) ->
      Rejected_by_nack {motive; alternative_points}) ;
  (* Rejected socket connection, no common network protocol *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.rejected_no_common_protocol"
    ~title:"Rejected socket connection - no common network protocol"
    ~description:
      "Rejected peer connection: rejected socket connection as we have no \
       common network protocol with the peer."
    ~pp:(fun ppf _lst ->
      Format.fprintf ppf "Rejected peer connection: no common network protocol.")
    Data_encoding.(obj1 (req "announced_version" Network_version.encoding))
    (function
      | Rejected_no_common_protocol {announced} -> Some announced | _ -> None)
    (fun announced -> Rejected_no_common_protocol {announced}) ;
  (* Decoding error *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.decoding_error"
    ~title:"Decoding error"
    ~description:"An error occurred while decoding."
    ~pp:(fun ppf re ->
      Format.fprintf
        ppf
        "An error occurred while decoding: %a."
        Data_encoding.Binary.pp_read_error
        re)
    Data_encoding.(obj1 @@ req "read_error" Binary.read_error_encoding)
    (function Decoding_error re -> Some re | _ -> None)
    (fun re -> Decoding_error re) ;
  (* Myself *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.myself"
    ~title:"Myself"
    ~description:"Remote peer is actually yourself."
    ~pp:(fun ppf id ->
      Format.fprintf
        ppf
        "Remote peer %a cannot be authenticated: peer is actually yourself."
        P2p_connection.Id.pp
        id)
    Data_encoding.(obj1 (req "connection_id" P2p_connection.Id.encoding))
    (function Myself id -> Some id | _ -> None)
    (fun id -> Myself id) ;
  (* Not enough proof of work *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.not_enough_proof_of_work"
    ~title:"Not enough proof of work"
    ~description:
      "Remote peer cannot be authenticated: not enough proof of work."
    ~pp:(fun ppf id ->
      Format.fprintf
        ppf
        "Remote peer %a cannot be authenticated: not enough proof of work."
        P2p_peer.Id.pp
        id)
    Data_encoding.(obj1 (req "peer_id" P2p_peer.Id.encoding))
    (function Not_enough_proof_of_work id -> Some id | _ -> None)
    (fun id -> Not_enough_proof_of_work id) ;
  (* Invalid authentication *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_auth"
    ~title:"Invalid authentication"
    ~description:"Rejected peer connection: invalid authentication."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "Rejected peer connection: invalid authentication.")
    Data_encoding.empty
    (function Invalid_auth -> Some () | _ -> None)
    (fun () -> Invalid_auth) ;
  (* Invalid chunks size *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_socket.invalid_chunks_size"
    ~title:"Invalid chunks size"
    ~description:"Size of chunks is not valid."
    ~pp:(fun ppf (value, min, max) ->
      Format.fprintf
        ppf
        "Size of chunks is invalid: should be between %d and %d but is %d"
        min
        max
        value)
    Data_encoding.(obj3 (req "value" int31) (req "min" int31) (req "max" int31))
    (function
      | Invalid_chunks_size {value; min; max} -> Some (value, min, max)
      | _ -> None)
    (fun (value, min, max) -> Invalid_chunks_size {value; min; max})

(***************************** p2p conn ***********************************)

type error += Peer_discovery_disabled

let () =
  (* Peer discovery disabled *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_conn.peer_discovery_disabled"
    ~title:"Peer discovery disabled"
    ~description:
      "The peer discovery is disabled, sending advertise messages is forbidden."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The peer discovery is disabled, sending advertise messages is \
         forbidden.")
    Data_encoding.empty
    (function Peer_discovery_disabled -> Some () | _ -> None)
    (fun () -> Peer_discovery_disabled)

(***************************** p2p pool ***********************************)

type error += Pending_connection

type error += Connected

type error += Connection_failed

type error += Rejected of {peer : P2p_peer.Id.t; motive : P2p_rejection.t}

type error += Too_many_connections

type error += Private_mode

type error += Point_banned of P2p_point.Id.t

type error += Peer_banned of P2p_peer.Id.t

type error += P2p_layer_disabled

type error +=
  | Identity_check_failure of {
      point : P2p_point.Id.t;
      expected_peer_id : P2p_peer.Id.t;
      received_peer_id : P2p_peer.Id.t;
    }

let () =
  (* Pending connection *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.pending_connection"
    ~title:"Pending connection"
    ~description:"Fail to connect with a peer: a connection is already pending."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Fail to connect with a peer: a connection is already pending.")
    Data_encoding.empty
    (function Pending_connection -> Some () | _ -> None)
    (fun () -> Pending_connection) ;
  (* Connected *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.connected"
    ~title:"Connected"
    ~description:
      "Fail to connect with a peer: a connection is already established."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "Fail to connect with a peer: a connection is already established.")
    Data_encoding.empty
    (function Connected -> Some () | _ -> None)
    (fun () -> Connected) ;
  (* Connected failed *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.connection_failed"
    ~title:"TCP connection failed"
    ~description:"TCP connection failed (refused or no route to host)."
    ~pp:(fun ppf () ->
      Format.fprintf ppf "TCP connection failed (refused or no route to host.")
    Data_encoding.empty
    (function Connection_failed -> Some () | _ -> None)
    (fun () -> Connection_failed) ;
  (* Rejected *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.rejected"
    ~title:"Rejected peer"
    ~description:"Connection to peer was rejected by us."
    ~pp:(fun ppf (peer, motive) ->
      Format.fprintf
        ppf
        "Connection to peer %a was rejected by us on motive: %a."
        P2p_peer.Id.pp
        peer
        P2p_rejection.pp
        motive)
    Data_encoding.(
      obj2
        (req "peer_id" P2p_peer.Id.encoding)
        (req "motive" P2p_rejection.encoding))
    (function Rejected {peer; motive} -> Some (peer, motive) | _ -> None)
    (fun (peer, motive) -> Rejected {peer; motive}) ;
  (* Too many connections *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.too_many_connections"
    ~title:"Too many connections"
    ~description:"Too many connections."
    ~pp:(fun ppf () -> Format.fprintf ppf "Too many connections.")
    Data_encoding.empty
    (function Too_many_connections -> Some () | _ -> None)
    (fun () -> Too_many_connections) ;
  (* Private mode *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.private_mode"
    ~title:"Private mode"
    ~description:"Node is in private mode."
    ~pp:(fun ppf () -> Format.fprintf ppf "Node is in private mode.")
    Data_encoding.empty
    (function Private_mode -> Some () | _ -> None)
    (fun () -> Private_mode) ;
  (* Point Banned *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.point_banned"
    ~title:"Point Banned"
    ~description:"The address you tried to connect is banned."
    ~pp:(fun ppf (addr, _port) ->
      Format.fprintf
        ppf
        "The address you tried to connect (%a) is banned."
        P2p_addr.pp
        addr)
    Data_encoding.(obj1 (req "point" P2p_point.Id.encoding))
    (function Point_banned point -> Some point | _ -> None)
    (fun point -> Point_banned point) ;
  (* Peer Banned *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.peer_banned"
    ~title:"Peer Banned"
    ~description:"The peer identity you tried to connect is banned."
    ~pp:(fun ppf peer_id ->
      Format.fprintf
        ppf
        "The peer identity you tried to connect (%a) is banned."
        P2p_peer.Id.pp
        peer_id)
    Data_encoding.(obj1 (req "peer" P2p_peer.Id.encoding))
    (function Peer_banned peer_id -> Some peer_id | _ -> None)
    (fun peer_id -> Peer_banned peer_id) ;
  (* P2p_layer_disabled *)
  register_error_kind
    `Permanent
    ~id:"node.p2p_pool.disabled"
    ~title:"P2P layer disabled"
    ~description:"The P2P layer on this node is not active."
    ~pp:(fun ppf () -> Format.fprintf ppf "P2P layer disabled.")
    Data_encoding.empty
    (function P2p_layer_disabled -> Some () | _ -> None)
    (fun () -> P2p_layer_disabled) ;
  register_error_kind
    `Permanent
    ~id:"node.p2p_connect_handler.identity_check_failure"
    ~title:"Unexpected peer identity"
    ~description:
      "Peer announced an identity which does not match the one specified on \
       the command-line."
    ~pp:(fun ppf (point, expected_peer_id, received_peer_id) ->
      Format.fprintf
        ppf
        "For point `%a`: Expected peer identity `%a` and received peer \
         identity `%a`."
        P2p_point.Id.pp
        point
        P2p_peer.Id.pp
        expected_peer_id
        P2p_peer.Id.pp
        received_peer_id)
    Data_encoding.(
      obj3
        (req "point" P2p_point.Id.encoding)
        (req "expected_peer_id" P2p_peer.Id.encoding)
        (req "received_peer_id" P2p_peer.Id.encoding))
    (function
      | Identity_check_failure {point; expected_peer_id; received_peer_id} ->
          Some (point, expected_peer_id, received_peer_id)
      | _ -> None)
    (fun (point, expected_peer_id, received_peer_id) ->
      Identity_check_failure {point; expected_peer_id; received_peer_id})

(****************************** p2p maintenance ******************************)

type error += Maintenance_disabled

let () =
  (* Maintenance_disabled *)
  let description =
    "Attempt to trigger the maintenance failed as the maintenance is disabled."
  in

  register_error_kind
    `Permanent
    ~id:"node.p2p_maintenance.disabled"
    ~title:"Maintenance disabled"
    ~description
    ~pp:(fun ppf () -> Format.fprintf ppf "%s" description)
    Data_encoding.empty
    (function Maintenance_disabled -> Some () | _ -> None)
    (fun () -> Maintenance_disabled)
