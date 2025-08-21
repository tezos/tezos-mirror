(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 Functori,     <contact@functori.com>                   *)
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

let init_identity_file configuration =
  let open Lwt_result_syntax in
  let check_data_dir ~data_dir:_ =
    (* FIXME: https://gitlab.com/tezos/tezos/-/issues/5566

       Do some checks about the shape of the node's data dir as done for Octez
       node ? *)
    return_unit
  in
  let identity_file = Configuration_file.identity_file configuration in
  Identity_file.init
    ~check_data_dir
    ~identity_file
    ~expected_pow:configuration.expected_pow

let p2p_config configuration =
  let open Lwt_result_syntax in
  let open Gossipsub.Transport_layer.Default_parameters in
  let* identity = init_identity_file configuration in
  let p2p_addr, p2p_port = configuration.listen_addr in
  let reconnection_config =
    let open P2p_reconnection_config in
    Point_reconnection_config.
      {factor; initial_delay; disconnection_delay; increase_cap}
  in
  let proof_of_work_target' =
    Crypto_box.make_pow_target configuration.expected_pow
  in
  let config =
    let open P2p_config in
    {
      P2p.listening_port = Some p2p_port;
      listening_addr = Some p2p_addr;
      discovery_port;
      discovery_addr;
      advertised_port = Some p2p_port;
      trusted_points;
      peers_file = Configuration_file.peers_file configuration;
      private_mode;
      identity;
      proof_of_work_target = proof_of_work_target';
      trust_discovered_peers;
      reconnection_config;
      disable_peer_discovery;
    }
  in
  return config

let p2p_limits =
  let open Gossipsub.Transport_layer.Default_parameters.P2p_limits in
  {
    P2p_limits.connection_timeout;
    authentication_timeout;
    greylist_timeout;
    maintenance_idle_time;
    min_connections;
    expected_connections;
    max_connections;
    backlog;
    max_incoming_connections;
    max_download_speed;
    max_upload_speed;
    read_buffer_size;
    read_queue_size;
    write_queue_size;
    incoming_app_message_queue_size;
    incoming_message_queue_size;
    outgoing_message_queue_size;
    max_known_points;
    max_known_peer_ids;
    peer_greylist_size;
    ip_greylist_size_in_kilobytes;
    ip_greylist_cleanup_delay;
    swap_linger;
    binary_chunks_size;
  }
