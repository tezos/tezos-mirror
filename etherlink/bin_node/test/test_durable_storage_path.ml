(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   cd etherlink/bin_node/test/ ;
                  dune exec ./test_durable_storage_path.exe
    Subject:      Storage-version gating of the trace paths
                  ([Durable_storage_path.Trace] and [Http_trace]) and of
                  the gas backlog path. *)

open Evm_node_lib_dev

let check_path ~storage_version got expected =
  Check.(
    (got = expected)
      string
      ~error_msg:(sf "V%d: expected %%R, got %%L" storage_version))

let test_trace_input () =
  List.iter
    (fun (storage_version, expected) ->
      check_path
        ~storage_version
        (Durable_storage_path.Trace.input ~storage_version)
        expected)
    [
      (59, "/evm/trace/input");
      (60, "/base/trace/input");
      (69, "/base/trace/input");
      (70, "/base/trace/input");
      (72, "/base/trace/input");
    ]

let test_trace_root () =
  List.iter
    (fun (storage_version, transaction_hash, expected) ->
      check_path
        ~storage_version
        (Durable_storage_path.Trace.root_indexed_by_hash
           ~storage_version
           ~transaction_hash)
        expected)
    [
      (59, None, "/evm/trace");
      (60, None, "/base/trace");
      (69, Some "ab", "/base/trace/ab");
      (70, None, "/evm/world_state/trace");
      (70, Some "ab", "/evm/world_state/trace/ab");
      (71, None, "/evm/world_state/trace");
      (71, Some "ab", "/evm/world_state/trace/ab");
      (72, None, "/common_world_state/trace");
      (72, Some "ab", "/common_world_state/trace/ab");
    ]

let test_http_trace_for_tx () =
  List.iter
    (fun (storage_version, expected) ->
      check_path
        ~storage_version
        (Durable_storage_path.Http_trace.for_tx
           ~storage_version
           ~transaction_hash:"ab")
        expected)
    [
      (59, "/evm/world_state/__http_trace/traces/ab");
      (60, "/base/__http_trace/traces/ab");
      (69, "/base/__http_trace/traces/ab");
      (70, "/evm/world_state/__http_trace/traces/ab");
      (71, "/evm/world_state/__http_trace/traces/ab");
      (72, "/common_world_state/__http_trace/traces/ab");
    ]

let test_backlog () =
  List.iter
    (fun (storage_version, expected) ->
      check_path
        ~storage_version
        (Durable_storage_path.backlog ~storage_version)
        expected)
    [
      (59, "/evm/world_state/fees/backlog");
      (72, "/evm/world_state/fees/backlog");
      (73, "/common_world_state/fees/backlog");
    ]

let tests =
  [
    ( "durable_storage_path",
      [
        ("tracer input", `Quick, test_trace_input);
        ("trace root", `Quick, test_trace_root);
        ("http trace per transaction", `Quick, test_http_trace_for_tx);
        ("gas backlog", `Quick, test_backlog);
      ] );
  ]

let () = Alcotest.run ~__FILE__ "durable_storage_path" tests

let () = Test.run ()
