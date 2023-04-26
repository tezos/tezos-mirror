(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

(** Testing
    -------
    Component:    Shell (Node)
    Invocation:   dune exec src/lib_shell/test/main.exe \
                  -- --file test_node.ml
    Dependencies: src/lib_shell/test/shell_test_helpers.ml
    Subject:      Unit tests for node. Currently only tests that
                  events are emitted.
*)

let section = Some (Internal_event.Section.make_sanitized ["node"])

let filter = Some section

let init_config (* (f : 'a -> unit -> unit Lwt.t) *) f test_dir switch () :
    unit Lwt.t =
  let sandbox_parameters : Data_encoding.json = `Null in
  let config : Node.config =
    let open Filename.Infix in
    {
      genesis = Shell_test_helpers.genesis;
      chain_name = Distributed_db_version.Name.zero;
      sandboxed_chain_name = Distributed_db_version.Name.zero;
      user_activated_upgrades = [];
      user_activated_protocol_overrides = [];
      operation_metadata_size_limit = Unlimited;
      internal_events = Tezos_base.Internal_event_config.lwt_log;
      data_dir = test_dir;
      store_root = test_dir // "store";
      context_root = test_dir // "context";
      protocol_root = test_dir // "protocol";
      patch_context = None;
      p2p = None;
      target = None;
      disable_mempool = false;
      enable_testchain = true;
      dal = Tezos_crypto_dal.Cryptobox.Config.default;
    }
  in
  f sandbox_parameters config switch ()

let default_p2p : P2p.config =
  {
    listening_port = None;
    listening_addr = Some (P2p_addr.of_string_exn "[::]");
    advertised_port = None;
    discovery_port = None;
    discovery_addr = Some Ipaddr.V4.any;
    trusted_points = [];
    peers_file = "";
    private_mode = true;
    identity = P2p_identity.generate_with_pow_target_0 ();
    proof_of_work_target = Tezos_crypto.Crypto_box.default_pow_target;
    trust_discovered_peers = false;
    reconnection_config = Point_reconnection_config.default;
    disable_peer_discovery = false;
  }

let default_p2p_limits =
  P2p_limits.
    {
      default with
      ip_greylist_size_in_kilobytes = 256 (* default/8 *);
      ip_greylist_cleanup_delay = Ptime.Span.of_int_s 3600 (* default/24 *);
    }

let default_p2p = Some (default_p2p, default_p2p_limits)

let wrap f _switch () =
  Tztest.with_empty_mock_sink (fun _ ->
      Lwt_utils_unix.with_tempdir "tezos_test_" (fun test_dir ->
          init_config f test_dir _switch ()))

(** Start tests *)

let ( let*?? ) m f =
  let open Lwt_syntax in
  let* r = m in
  match r with
  | Ok v -> f v
  | Error error ->
      Format.printf "Error:\n   %a\n" pp_print_trace error ;
      Format.print_flush () ;
      Lwt.return_unit

(** Node creation in sandbox. Expects one event with status
    [p2p_layer_disabled]. *)
let node_sandbox_initialization_events sandbox_parameters config _switch () =
  let*?? n =
    Node.create
      ~sandboxed:true
      ~sandbox_parameters
      ~singleprocess:true
      (* Tezos_shell.Node.config *)
      config
      (* Tezos_shell.Node.peer_validator_limits *)
      Shell_limits.default_peer_validator_limits
      (* Tezos_shell.Node.block_validator_limits *)
      Shell_limits.default_block_validator_limits
      (* Tezos_shell.Node.prevalidator_limits *)
      Shell_limits.default_prevalidator_limits
      (* Tezos_shell.Node.chain_validator_limits *)
      Shell_limits.default_chain_validator_limits
      (* Tezos_shell_services.History_mode.t option *)
      None
  in
  (* Start tests *)
  let evs = Mock_sink.get_events ?filter () in
  Alcotest.(check int) "should have one event" 1 (List.length evs) ;
  Mock_sink.Pattern.(
    assert_event
      {
        level = Some Internal_event.Notice;
        section = Some section;
        name = "p2p-initialization";
      })
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth evs 0) ;
  (* End tests *)
  Node.shutdown n

(** Node creation. Expects two events with statuses
    [bootstrapping] and [p2p_maintain_started]. *)
let node_initialization_events _sandbox_parameters config _switch () =
  let*?? n =
    Node.create
      ~sandboxed:false
      ~singleprocess:true
      (* Tezos_shell.Node.config *)
      {config with p2p = default_p2p}
      (* Tezos_shell.Node.peer_validator_limits *)
      Shell_limits.default_peer_validator_limits
      (* Tezos_shell.Node.block_validator_limits *)
      Shell_limits.default_block_validator_limits
      (* Tezos_shell.Node.prevalidator_limits *)
      Shell_limits.default_prevalidator_limits
      (* Tezos_shell.Node.chain_validator_limits *)
      Shell_limits.default_chain_validator_limits
      (* Tezos_shell_services.History_mode.t option *)
      None
  in
  (* Start tests *)
  let evs = Mock_sink.get_events ?filter () in
  Alcotest.(check int) "should have two events" 2 (List.length evs) ;
  Mock_sink.Pattern.(
    assert_event
      {
        level = Some Internal_event.Notice;
        section = Some section;
        name = "p2p-initialization";
      })
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth evs 0) ;
  Mock_sink.Pattern.(
    assert_event
      {
        level = Some Internal_event.Notice;
        section = Some section;
        name = "p2p-initialization";
      })
    (WithExceptions.Option.get ~loc:__LOC__ @@ List.nth evs 1) ;
  (* End tests *)
  Node.shutdown n

let node_store_known_protocol_events _sandbox_parameters config _switch () =
  let*?? n =
    Node.create
      ~sandboxed:false
      ~singleprocess:true
      (* Tezos_shell.Node.config *)
      {config with p2p = default_p2p}
      (* Tezos_shell.Node.peer_validator_limits *)
      Shell_limits.default_peer_validator_limits
      (* Tezos_shell.Node.block_validator_limits *)
      Shell_limits.default_block_validator_limits
      (* Tezos_shell.Node.prevalidator_limits *)
      Shell_limits.default_prevalidator_limits
      (* Tezos_shell.Node.chain_validator_limits *)
      Shell_limits.default_chain_validator_limits
      (* Tezos_shell_services.History_mode.t option *)
      None
  in
  (* Start tests *)
  Mock_sink.(
    assert_has_event
      "Should have a store_protocol_incorrect_hash event"
      ?filter
      Pattern.
        {
          level = Some Internal_event.Info;
          section = Some section;
          name = "store_protocol_incorrect_hash";
        }) ;
  (* END tests *)
  Node.shutdown n

let tests =
  [
    Alcotest_lwt.test_case
      "node_sandbox_initialization_events"
      `Quick
      (wrap node_sandbox_initialization_events);
    Alcotest_lwt.test_case
      "node_initialization_events"
      `Quick
      (wrap node_initialization_events);
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-shell" [("test node", tests)]
  |> Lwt_main.run
