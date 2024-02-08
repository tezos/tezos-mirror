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

(* Overlay for simple events that allows us to retrieve the level. *)

let section = ["node"; "config"; "validation"]

module E : sig
  type 'a t

  val level : 'a t -> Internal_event.level

  val event : 'a t -> 'a Internal_event.Simple.t

  val declare_0 :
    name:string -> msg:string -> level:Internal_event.level -> unit -> unit t

  val declare_1 :
    name:string ->
    msg:string ->
    level:Internal_event.level ->
    string * 'a Data_encoding.t ->
    'a t

  val declare_2 :
    name:string ->
    msg:string ->
    level:Internal_event.level ->
    string * 'a Data_encoding.t ->
    string * 'b Data_encoding.t ->
    ('a * 'b) t

  val declare_3 :
    name:string ->
    msg:string ->
    level:Internal_event.level ->
    string * 'a Data_encoding.t ->
    string * 'b Data_encoding.t ->
    string * 'c Data_encoding.t ->
    ('a * 'b * 'c) t
end = struct
  type 'a t = {
    level : Internal_event.Level.t;
    event : 'a Internal_event.Simple.t;
  }

  let level {level; _} = level

  let event {event; _} = event

  let prefix_with_level level msg =
    Format.sprintf "%s: %s" (Internal_event.Level.to_string level) msg

  let declare_0 ~name ~msg ~level x =
    let msg = prefix_with_level level msg in
    {
      level;
      event = Internal_event.Simple.declare_0 ~section ~name ~msg ~level x;
    }

  let declare_1 ~name ~msg ~level x =
    let msg = prefix_with_level level msg in
    {
      level;
      event = Internal_event.Simple.declare_1 ~section ~name ~msg ~level x;
    }

  let declare_2 ~name ~msg ~level x y =
    let msg = prefix_with_level level msg in
    {
      level;
      event = Internal_event.Simple.declare_2 ~section ~name ~msg ~level x y;
    }

  let declare_3 ~name ~msg ~level x y z =
    let msg = prefix_with_level level msg in
    {
      level;
      event = Internal_event.Simple.declare_3 ~section ~name ~msg ~level x y z;
    }
end

(* The type for a node configuration warning/error. *)

type alert = Alert : {event : 'a E.t; payload : 'a} -> alert

let is_error (Alert {event; _}) = E.level event = Error

let is_warning (Alert {event; _}) = E.level event = Warning

(* Errors *)

type error += Invalid_node_configuration

let () =
  register_error_kind
    `Permanent
    ~id:"config_validation.invalid_node_configuration"
    ~title:"Invalid node configuration"
    ~description:"The node configuration is invalid."
    ~pp:(fun ppf () ->
      Format.fprintf
        ppf
        "The node configuration is invalid, use `%s config validate [options]`"
        Sys.argv.(0))
    Data_encoding.unit
    (function Invalid_node_configuration -> Some () | _ -> None)
    (fun () -> Invalid_node_configuration)

(* The type for a configuration validation report. *)

type t = alert list

let has_error t = List.exists is_error t

let has_warning t = List.exists is_warning t

module Event = struct
  open Internal_event.Simple

  let emit = emit

  let disabled_event =
    declare_0
      ~section
      ~name:"config_validation_disabled"
      ~msg:"the node configuration validation is disabled."
      ~level:Notice
      ()

  let success_event =
    declare_0
      ~section
      ~name:"config_validation_success"
      ~msg:"the node configuration has been successfully validated."
      ~level:Notice
      ()

  let error_event =
    declare_0
      ~section
      ~name:"config_validation_error"
      ~msg:
        "found the following error(s) while validating the node configuration."
      ~level:Error
      ()

  let warning_event =
    declare_0
      ~section
      ~name:"config_validation_warning"
      ~msg:
        "found the following warning(s) while validating the node \
         configuration."
      ~level:Warning
      ()

  let emit_all t =
    Lwt_list.iter_s
      (function Alert {event; payload} -> emit (E.event event) payload)
      t

  let report t =
    let open Lwt_syntax in
    let errors = List.filter is_error t in
    let warnings = List.filter is_warning t in
    let* () =
      match errors with
      | [] -> Lwt.return_unit
      | xs ->
          let* () = emit error_event () in
          emit_all xs
    in
    match warnings with
    | [] -> Lwt.return_unit
    | xs ->
        let* () = emit warning_event () in
        emit_all xs
end

let mk_alert ~event ~payload = Alert {event; payload}

let when_ condition ~event ~payload =
  if not condition then [] else [mk_alert ~event ~payload]

let unless condition ~event ~payload =
  if condition then [] else [mk_alert ~event ~payload]

(* The following parts consist in node configuration validations. *)

(* Validate expected proof-of-work. *)

let invalid_pow =
  E.declare_1
    ~name:"invalid_pow"
    ~level:Error
    ~msg:
      (Format.sprintf
         "the expected proof-of-work must be between 0 and 256 (inclusive), \
          but found the value {proof-of-work} in field '%s'."
         "p2p.expected-proof-of-work")
    ("proof-of-work", Data_encoding.float)

let validate_expected_pow (config : Config_file.t) : (t, 'error) result Lwt.t =
  unless
    (0. <= config.p2p.expected_pow && config.p2p.expected_pow <= 256.)
    ~event:invalid_pow
    ~payload:config.p2p.expected_pow
  |> Lwt.return_ok

(* Validate addresses. *)

let cannot_parse_addr =
  E.declare_3
    ~name:"cannot_parse_addr"
    ~msg:"failed to parse address '{addr}' in field '{field}': {why}."
    ~level:Error
    ("addr", Data_encoding.string)
    ("field", Data_encoding.string)
    ("why", Data_encoding.string)

let cannot_resolve_addr =
  E.declare_2
    ~name:"cannot_resolve_addr"
    ~msg:"failed to resolve address '{addr}' in field '{field}'."
    ~level:Warning
    ("addr", Data_encoding.string)
    ("field", Data_encoding.string)

let cannot_resolve_bootstrap_peer_addr =
  E.declare_2
    ~name:"cannot_resolve_bootstrap_peer_addr"
    ~msg:
      "failed to resolve the bootstrap peer address '{addr}' in field \
       '{field}', the node will not use this bootstrap peer"
    ~level:Warning
    ("addr", Data_encoding.string)
    ("field", Data_encoding.string)

let validate_addr ?e_resolve ?e_parse ~field ~addr resolver =
  let open Lwt_result_syntax in
  let*! r = resolver addr in
  match r with
  | Error [P2p_resolve.Failed_to_parse_address (addr, why)] ->
      return_some
        (mk_alert
           ~event:(Option.value e_parse ~default:cannot_parse_addr)
           ~payload:(addr, field, why))
  | Ok [] ->
      return_some
        (mk_alert
           ~event:(Option.value e_resolve ~default:cannot_resolve_addr)
           ~payload:(addr, field))
  | Ok _ -> return_none
  | Error _ as e -> Lwt.return e

let validate_addr_opt ?e_resolve ?e_parse ~field ~addr resolver =
  let addr = Option.to_list addr in
  List.filter_map_es
    (fun addr -> validate_addr ?e_resolve ?e_parse ~field ~addr resolver)
    addr

let validate_rpc_listening_addrs (config : Config_file.t) =
  let aux addr =
    validate_addr
      ~field:"rpc.listen-addrs"
      ~addr
      Config_file.resolve_rpc_listening_addrs
  in
  List.filter_map_ep aux config.rpc.listen_addrs

let validate_p2p_listening_addrs (config : Config_file.t) =
  validate_addr_opt
    ~field:"p2p.listen-addr"
    ~addr:config.p2p.listen_addr
    Config_file.resolve_listening_addrs

let validate_p2p_discovery_addr (config : Config_file.t) =
  validate_addr_opt
    ~field:"p2p.discovery-addr"
    ~addr:config.p2p.discovery_addr
    Config_file.resolve_discovery_addrs

let validate_p2p_bootstrap_addrs ~field peers =
  let aux addr =
    validate_addr
      ~e_resolve:cannot_resolve_bootstrap_peer_addr
      ~field
      ~addr
      (fun x -> Config_file.resolve_bootstrap_addrs [x])
  in
  List.filter_map_ep aux peers

let validate_p2p_bootstrap_peers (config : Config_file.t) =
  match config.p2p.bootstrap_peers with
  | None ->
      validate_p2p_bootstrap_addrs
        ~field:"network.default_bootstrap-peers"
        config.blockchain_network.default_bootstrap_peers
  | Some peers ->
      validate_p2p_bootstrap_addrs ~field:"p2p.bootstrap-peers" peers

let validate_addresses config : t tzresult Lwt.t =
  List.concat_map_es
    (fun f -> f config)
    [
      validate_rpc_listening_addrs;
      validate_p2p_bootstrap_peers;
      validate_p2p_listening_addrs;
      validate_p2p_discovery_addr;
    ]

(* Validate connections setup. *)

let connections_min_expected =
  E.declare_2
    ~name:"minimum_connections_greater_than_expected"
    ~level:Error
    ~msg:
      (Format.sprintf
         "the minimum number of connections found in field '%s' ({minimum}) is \
          greater than the expected number of connections found in field '%s' \
          ({expected})."
         "p2p.limits.min-connections"
         "p2p.limits.expected-connections")
    ("minimum", Data_encoding.int16)
    ("expected", Data_encoding.int16)

let connections_expected_max =
  E.declare_2
    ~name:"expected_connections_greater_than_maximum"
    ~level:Error
    ~msg:
      (Format.sprintf
         "the expected number of connections found in field '%s' ({expected}) \
          is greater than the maximum number of connections found in field \
          '%s' ({maximum})."
         "p2p.limits.expected-connections"
         "p2p.limits.max-connections")
    ("expected", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_peers_greater_than_maximum =
  E.declare_2
    ~name:"target_number_of_known_peers_greater_than_maximum"
    ~level:Error
    ~msg:
      (Format.sprintf
         "in field '%s', the target number of known peer ids ({target}) is \
          greater than the maximum number of known peers ids ({maximum})."
         "p2p.limits.max_known_peer_ids")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_peers_lower_than_maximum_conn =
  E.declare_2
    ~name:"target_number_of_known_peers_greater_than_maximum_conn"
    ~level:Error
    ~msg:
      (Format.sprintf
         "the target number of known peer ids ({target}) in field '%s', is \
          lower than the maximum number of connections ({maximum}) found in \
          field '%s'."
         "p2p.limits.max_known_peer_ids"
         "p2p.limits.max-connections")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_points_greater_than_maximum =
  E.declare_2
    ~name:"target_number_of_known_points_greater_than_maximum"
    ~level:Error
    ~msg:
      (Format.sprintf
         "in field '%s', the target number of known point ids ({target}) is \
          greater than the maximum number of known points ids ({maximum})."
         "p2p.limits.max_known_points")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let target_number_of_known_points_lower_than_maximum_conn =
  E.declare_2
    ~name:"target_number_of_known_points_greater_than_maximum_conn"
    ~level:Error
    ~msg:
      (Format.sprintf
         "the target number of known point ids ({target}) found in field '%s' \
          is lower than the maximum number of connections ({maximum}) found in \
          '%s'."
         "p2p.limits.max_known_points"
         "p2p.limits.max-connections")
    ("target", Data_encoding.int16)
    ("maximum", Data_encoding.int16)

let validate_connections (config : Config_file.t) =
  let validated_connections =
    let limits = config.p2p.limits in
    when_
      (limits.min_connections > limits.expected_connections)
      ~event:connections_min_expected
      ~payload:(limits.min_connections, limits.expected_connections)
    @ when_
        (limits.expected_connections > limits.max_connections)
        ~event:connections_expected_max
        ~payload:(limits.expected_connections, limits.max_connections)
    @ Option.fold
        limits.max_known_peer_ids
        ~none:[]
        ~some:(fun (max_known_peer_ids, target_known_peer_ids) ->
          when_
            (target_known_peer_ids > max_known_peer_ids)
            ~event:target_number_of_known_peers_greater_than_maximum
            ~payload:(target_known_peer_ids, max_known_peer_ids)
          @ when_
              (limits.max_connections > target_known_peer_ids)
              ~event:target_number_of_known_peers_lower_than_maximum_conn
              ~payload:(target_known_peer_ids, limits.max_connections))
    @ Option.fold
        limits.max_known_points
        ~none:[]
        ~some:(fun (max_known_points, target_known_points) ->
          when_
            (target_known_points > max_known_points)
            ~event:target_number_of_known_points_greater_than_maximum
            ~payload:(max_known_points, target_known_points)
          @ when_
              (limits.max_connections > target_known_points)
              ~event:target_number_of_known_points_lower_than_maximum_conn
              ~payload:(target_known_points, limits.max_connections))
  in
  Lwt.return_ok validated_connections

let maintenance_disabled =
  E.declare_0
    ~name:"maintenance_disabled"
    ~level:Warning
    ~msg:
      "The P2P maintenance is disabled. The P2P maintenance should only be \
       disabled for testing purposes."
    ()

let warn_maintenance_deactivated (config : Config_file.t) =
  when_
    (config.p2p.limits.maintenance_idle_time = None)
    ~event:maintenance_disabled
    ~payload:()
  |> Lwt_result.return

(* Deprecated argument *)

let testchain_is_deprecated =
  E.declare_0
    ~level:Warning
    ~name:"enable_testchain_is_deprecated_in_configuration_file"
    ~msg:"The option `p2p.enable_testchain` is deprecated."
    ()

let local_listen_addrs_is_deprecated =
  E.declare_0
    ~level:Warning
    ~name:"local_listen_addrs_is_deprecated_in_configuration_file"
    ~msg:
      "The option `rpc.local_listen_addrs` is deprecated. Use `listen_addrs` \
       instead and remove `local_listen_addrs` from your config file."
    ()

let warn_deprecated_testchain (config : Config_file.t) =
  when_ config.p2p.enable_testchain ~event:testchain_is_deprecated ~payload:()
  |> Lwt_result.return

let warn_deprecated_local_listen_addrs (config : Config_file.t) =
  when_
    (not (config.rpc.local_listen_addrs = []))
    ~event:local_listen_addrs_is_deprecated
    ~payload:()
  |> Lwt_result.return

(* Main validation passes. *)

let validation_passes ignore_testchain_warning =
  [
    validate_expected_pow;
    validate_addresses;
    validate_connections;
    warn_maintenance_deactivated;
  ]
  @ (if ignore_testchain_warning then [] else [warn_deprecated_testchain])
  @ [warn_deprecated_local_listen_addrs]

let validate_passes ?(ignore_testchain_warning = false) config =
  List.concat_map_es
    (fun f -> f config)
    (validation_passes ignore_testchain_warning)

(* Main validation functions. *)

let check ?ignore_testchain_warning config =
  let open Lwt_result_syntax in
  if config.Config_file.disable_config_validation then
    let*! () = Event.(emit disabled_event ()) in
    return_unit
  else
    let* t = validate_passes ?ignore_testchain_warning config in
    if has_error t then
      let*! () = Event.report t in
      tzfail Invalid_node_configuration
    else if has_warning t then
      let*! () = Event.report t in
      return_unit
    else
      let*! () = Event.(emit success_event ()) in
      return_unit
