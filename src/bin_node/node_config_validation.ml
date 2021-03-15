(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* The type for a node configuration warning/error. *)

type alert =
  | Alert : {
      event : level:Internal_event.Level.t -> 'a Internal_event.Simple.t;
      level : Internal_event.Level.t;
      payload : 'a;
    }
      -> alert

let is_error (Alert {level; _}) = level = Error

let is_warning (Alert {level; _}) = level = Warning

(* The type for a configuration validation report. *)

type t = alert list

let empty = []

let has_error t = List.exists is_error t

let has_warning t = List.exists is_warning t

module Event = struct
  include Internal_event.Simple

  let section = ["node"; "config"; "validation"]

  let disabled_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_disabled"
      ~msg:"the node configuration validation is disabled."
      ~level:Notice
      ()

  let success_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_success"
      ~msg:"the node configuration has been successfully validated."
      ~level:Notice
      ()

  let error_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_error"
      ~msg:
        "found the following error(s) while validating the node configuration."
      ~level:Error
      ()

  let warning_event =
    Internal_event.Simple.declare_0
      ~section
      ~name:"node_config_validation_warning"
      ~msg:
        "found the following warning(s) while validating the node \
         configuration."
      ~level:Warning
      ()

  let emit_all t =
    Lwt_list.iter_s
      (function Alert {level; event; payload} -> emit (event ~level) payload)
      t

  let report t =
    let errors = List.filter is_error t in
    let warnings = List.filter is_warning t in
    ( match errors with
    | [] ->
        Lwt.return_unit
    | xs ->
        emit error_event () >>= fun () -> emit_all xs )
    >>= fun () ->
    match warnings with
    | [] ->
        Lwt.return_unit
    | xs ->
        emit warning_event () >>= fun () -> emit_all xs
end

let mk_alert ~event ~level ~payload = Alert {event; level; payload}

let when_ condition ~event ~level ~payload =
  if not condition then [] else [mk_alert ~event ~level ~payload]
  [@@ocaml.warning "-32"]

let unless condition ~event ~level ~payload =
  if condition then [] else [mk_alert ~event ~level ~payload]
  [@@ocaml.warning "-32"]

(* The following parts consist in node configuration validations. *)

(* Validate expected proof-of-work. *)

let invalid_pow ~level =
  Internal_event.Simple.declare_1
    ~section:Event.section
    ~name:"invalid_pow"
    ~level
    ~msg:
      (Format.sprintf
         "the expected proof-of-work must be between 0 and 256 (inclusive), \
          but found the value {proof-of-work} in field '%s'."
         "p2p.expected-proof-of-work")
    ("proof-of-work", Data_encoding.float)

let validate_expected_pow (config : Node_config_file.t) : t tzresult Lwt.t =
  let t =
    unless
      (0. <= config.p2p.expected_pow && config.p2p.expected_pow <= 256.)
      ~event:invalid_pow
      ~level:Error
      ~payload:config.p2p.expected_pow
  in
  return t

(* Validate addresses. *)

let cannot_parse_addr ~level =
  Internal_event.Simple.declare_3
    ~section:Event.section
    ~name:"cannot_parse_addr"
    ~msg:"failed to parse address '{addr}' in field '{field}': {why}."
    ~level
    ("addr", Data_encoding.string)
    ("field", Data_encoding.string)
    ("why", Data_encoding.string)

let cannot_resolve_addr ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"cannot_resolve_addr"
    ~msg:"failed to resolve address '{addr}' in field '{field}'."
    ~level
    ("addr", Data_encoding.string)
    ("field", Data_encoding.string)

let validate_addr ~level ~field ~addr ~resolver =
  resolver addr
  >>= function
  | Error [Node_config_file.Failed_to_parse_address (addr, why)] ->
      return_some
        (mk_alert
           ~event:cannot_parse_addr
           ~level:Error
           ~payload:(addr, field, why))
  | Ok [] ->
      return_some
        (mk_alert ~event:cannot_resolve_addr ~level ~payload:(addr, field))
  | Ok _ ->
      return_none
  | Error _ as e ->
      Lwt.return e

let validate_addr_opt ~field ~level ~addr ~resolver =
  Option.fold addr ~none:return_none ~some:(fun addr ->
      validate_addr ~field ~level ~addr ~resolver)
  >|=? Option.to_list

let validate_rpc_listening_addrs (config : Node_config_file.t) =
  let aux addr =
    validate_addr
      ~field:"rpc.listen-addrs"
      ~level:Error
      ~addr
      ~resolver:Node_config_file.resolve_rpc_listening_addrs
  in
  List.filter_map_ep aux config.rpc.listen_addrs

let validate_p2p_listening_addrs (config : Node_config_file.t) =
  validate_addr_opt
    ~field:"p2p.listen-addr"
    ~level:Error
    ~addr:config.p2p.listen_addr
    ~resolver:Node_config_file.resolve_listening_addrs

let validate_p2p_discovery_addr (config : Node_config_file.t) =
  validate_addr_opt
    ~field:"p2p.discovery-addr"
    ~level:Error
    ~addr:config.p2p.discovery_addr
    ~resolver:Node_config_file.resolve_discovery_addrs

let validate_p2p_bootstrap_addrs ~field peers =
  let aux addr =
    validate_addr ~level:Error ~field ~addr ~resolver:(fun x ->
        Node_config_file.resolve_bootstrap_addrs [x])
  in
  List.filter_map_ep aux peers

let validate_p2p_bootstrap_peers (config : Node_config_file.t) =
  match config.p2p.bootstrap_peers with
  | None ->
      validate_p2p_bootstrap_addrs
        ~field:"network.default_bootstrap-peers"
        config.blockchain_network.default_bootstrap_peers
  | Some peers ->
      validate_p2p_bootstrap_addrs ~field:"p2p.bootstrap-peers" peers

let validate_addresses config : t tzresult Lwt.t =
  List.fold_left_es
    (fun acc f -> f config >>=? fun res -> return (res @ acc))
    empty
    [ validate_rpc_listening_addrs;
      validate_p2p_bootstrap_peers;
      validate_p2p_listening_addrs;
      validate_p2p_discovery_addr ]

(* Validate connections setup. *)

let connections_min_expected ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"minimum_connections_greater_than_expected"
    ~level
    ~msg:
      (Format.sprintf
         "the minimum number of connections found in field '%s' ({minimum}) \
          is greater than the expected number of connections found in field \
          '%s' ({expected})."
         "p2p.limits.min-connections"
         "p2p.limits.expected-connections")
    ("minimum", Data_encoding.int16)
    ("expected", Data_encoding.int16)

let connections_expected_max ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"expected_connections_greater_than_maximum"
    ~level
    ~msg:
      (Format.sprintf
         "the expected number of connections found in field '%s' ({expected}) \
          is greater than the maximum number of connections found in field \
          '%s' ({maximum})."
         "p2p.limits.expected-connections"
         "p2p.limits.max-connections")
    ("expected", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_peers_greater_than_maximum ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"target_number_of_known_peers_greater_than_maximum"
    ~level
    ~msg:
      (Format.sprintf
         "in field '%s', the target number of known peer ids ({target}) is \
          greater than the maximum number of known peers ids ({maximum})."
         "p2p.limits.max_known_peer_ids")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_peers_lower_than_maximum_conn ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"target_number_of_known_peers_greater_than_maximum_conn"
    ~level
    ~msg:
      (Format.sprintf
         "the target number of known peer ids ({target}) in field '%s', is \
          lower than the maximum number of connections ({maximum}) found in \
          field '%s'."
         "p2p.limits.max_known_peer_ids"
         "p2p.limits.max-connections")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_points_greater_than_maximum ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"target_number_of_known_points_greater_than_maximum"
    ~level
    ~msg:
      (Format.sprintf
         "in field '%s', the target number of known point ids ({target}) is \
          greater than the maximum number of known points ids ({maximum})."
         "p2p.limits.max_known_points")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_points_lower_than_maximum_conn ~level =
  Internal_event.Simple.declare_2
    ~section:Event.section
    ~name:"target_number_of_known_points_greater_than_maximum_conn"
    ~level
    ~msg:
      (Format.sprintf
         "the target number of known point ids ({target}) found in field '%s' \
          is lower than the maximum number of connections ({maximum}) found \
          in '%s'."
         "p2p.limits.max_known_points"
         "p2p.limits.max-connections")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let validate_connections (config : Node_config_file.t) =
  let limits = config.p2p.limits in
  when_
    (limits.min_connections > limits.expected_connections)
    ~level:Error
    ~event:connections_min_expected
    ~payload:(limits.min_connections, limits.expected_connections)
  @ when_
      (limits.expected_connections > limits.max_connections)
      ~level:Error
      ~event:connections_expected_max
      ~payload:(limits.expected_connections, limits.max_connections)
  @ Option.fold
      limits.max_known_peer_ids
      ~none:[]
      ~some:(fun (max_known_peer_ids, target_known_peer_ids) ->
        when_
          (target_known_peer_ids > max_known_peer_ids)
          ~event:target_number_of_known_peers_greater_than_maximum
          ~level:Error
          ~payload:(target_known_peer_ids, max_known_peer_ids)
        @ when_
            (limits.max_connections > target_known_peer_ids)
            ~event:target_number_of_known_peers_lower_than_maximum_conn
            ~level:Error
            ~payload:(target_known_peer_ids, limits.max_connections))
  @ Option.fold
      limits.max_known_points
      ~none:[]
      ~some:(fun (max_known_points, target_known_points) ->
        when_
          (target_known_points > max_known_points)
          ~event:target_number_of_known_points_greater_than_maximum
          ~level:Error
          ~payload:(max_known_points, target_known_points)
        @ when_
            (limits.max_connections > target_known_points)
            ~event:target_number_of_known_points_lower_than_maximum_conn
            ~level:Error
            ~payload:(target_known_points, limits.max_connections))
  |> return

(* Main validation passes. *)

let validation_passes =
  [validate_expected_pow; validate_addresses; validate_connections]

let validate_passes config =
  List.fold_left_es
    (fun acc f -> f config >>=? fun res -> return (res @ acc))
    empty
    validation_passes

(* Main validation functions. *)

let check config =
  if config.Node_config_file.disable_config_validation then
    Event.(emit disabled_event ()) >>= fun () -> return_unit
  else
    validate_passes config
    >>=? fun t ->
    if has_error t then Event.report t >>= fun () -> exit 1
    else if has_warning t then Event.report t >>= fun () -> return_unit
    else Event.(emit success_event ()) >>= fun () -> return_unit
