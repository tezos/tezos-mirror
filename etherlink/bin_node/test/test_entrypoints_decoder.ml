(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Bin_evm_node
    Invocation:   cd etherlink/bin_node/test/ ;
                  dune exec ./test_entrypoints_decoder.exe
    Subject:      Tests for [Tezos_backend.decode_entrypoints_result],
                  in particular the legacy-shape fallback that keeps
                  /script working when the EVM node updates ahead of
                  the rollup kernel upgrade introducing the
                  entries+views shape (!21936). *)

open Evm_node_lib_dev.Tezos_backend
module Tezlink_imports = Evm_node_lib_dev_tezlink.Tezlink_imports
module Imported_protocol = Tezlink_imports.Imported_protocol
module Imported_context = Tezlink_imports.Imported_context
module Prim = Imported_protocol.Michelson_v1_primitives
module Rlp = Evm_node_lib_dev_encoding.Rlp

(* Encode a Micheline type expression to the same byte representation
   the kernel uses on the wire ([Script.expr_encoding]). Use the
   primitives of the protocol [Tezlink_imports] points at, so the
   produced bytes match what the kernel would write. *)
let type_bytes expr =
  Data_encoding.Binary.to_bytes_exn Imported_context.Script.expr_encoding expr

let prim0 p = Tezos_micheline.Micheline.Prim (0, p, [], [])

let string_type_bytes =
  type_bytes (Tezos_micheline.Micheline.strip_locations (prim0 Prim.T_string))

let bytes_type_bytes =
  type_bytes (Tezos_micheline.Micheline.strip_locations (prim0 Prim.T_bytes))

let pair_string_bytes_bytes =
  type_bytes
    (Tezos_micheline.Micheline.strip_locations
       (Prim (0, Prim.T_pair, [prim0 Prim.T_string; prim0 Prim.T_bytes], [])))

let value s = Rlp.Value (Bytes.of_string s)

let entry name ty = Rlp.List [value name; Rlp.Value ty]

let view name pty rty = Rlp.List [value name; Rlp.Value pty; Rlp.Value rty]

(* Pre-!21936 kernels emit `List [entries_list]`. *)
let legacy_bytes entries = Rlp.encode (Rlp.List [Rlp.List entries])

(* !21936+ kernels emit `List [[entries_list; views_list]]`. *)
let new_bytes ~entries ~views =
  Rlp.encode (Rlp.List [Rlp.List [Rlp.List entries; Rlp.List views]])

let none_bytes = Rlp.encode (Rlp.List [])

let check_entry_names entries expected =
  let names = List.map fst entries |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  if names <> expected then
    Test.fail
      "Expected entrypoint names %s, got %s"
      (String.concat "," expected)
      (String.concat "," names)

let check_view_names views expected =
  let names = List.map (fun (n, _, _) -> n) views |> List.sort String.compare in
  let expected = List.sort String.compare expected in
  if names <> expected then
    Test.fail
      "Expected view names %s, got %s"
      (String.concat "," expected)
      (String.concat "," names)

(* `None` (empty outer list) must decode to [None]. *)
let test_none () =
  let open Lwt_syntax in
  let* result = decode_entrypoints_result none_bytes in
  match result with
  | Ok None -> Lwt.return_unit
  | Ok (Some _) -> Test.fail "expected None, got Some"
  | Error _ -> Test.fail "expected Ok None, got Error"

(* New-shape decoding works for a typical gateway-like payload. *)
let test_new_shape () =
  let open Lwt_syntax in
  let bytes =
    new_bytes
      ~entries:
        [entry "default" string_type_bytes; entry "approve" bytes_type_bytes]
      ~views:[view "staticcall_evm" pair_string_bytes_bytes bytes_type_bytes]
  in
  let* result = decode_entrypoints_result bytes in
  match result with
  | Ok (Some (_, entries, views)) ->
      check_entry_names entries ["default"; "approve"] ;
      check_view_names views ["staticcall_evm"] ;
      Lwt.return_unit
  | Ok None -> Test.fail "expected Some, got None"
  | Error _ -> Test.fail "expected Ok Some, got Error"

(* Legacy-shape decoding (the case that motivated this test): an old
   kernel emits `[entries]` with no [views] element, and the new node
   must decode it as `entries + []` rather than 500. *)
let test_legacy_shape () =
  let open Lwt_syntax in
  let bytes =
    legacy_bytes
      [entry "default" string_type_bytes; entry "approve" bytes_type_bytes]
  in
  let* result = decode_entrypoints_result bytes in
  match result with
  | Ok (Some (_, entries, views)) ->
      check_entry_names entries ["default"; "approve"] ;
      if views <> [] then
        Test.fail
          "legacy decode should yield no views, got %d"
          (List.length views) ;
      Lwt.return_unit
  | Ok None -> Test.fail "expected Some, got None"
  | Error _ -> Test.fail "expected Ok Some, got Error"

(* Legacy with a SINGLE entry: the inner list has length 1, which the
   new-shape pattern (which requires length 2) cannot match — covered
   by the catch-all fallback. *)
let test_legacy_shape_single_entry () =
  let open Lwt_syntax in
  let bytes = legacy_bytes [entry "default" string_type_bytes] in
  let* result = decode_entrypoints_result bytes in
  match result with
  | Ok (Some (_, entries, [])) ->
      check_entry_names entries ["default"] ;
      Lwt.return_unit
  | Ok (Some (_, _, _ :: _)) -> Test.fail "legacy decode should yield no views"
  | Ok None -> Test.fail "expected Some, got None"
  | Error _ -> Test.fail "expected Ok Some, got Error"

(* Legacy with EXACTLY two entries: the boundary case where the inner
   list has length 2 like the new shape, but its first child is a
   pair (`[Value; Value]`) — not a list — so the discriminator
   correctly picks the legacy branch. *)
let test_legacy_shape_two_entries () =
  let open Lwt_syntax in
  let bytes =
    legacy_bytes
      [entry "default" string_type_bytes; entry "transfer" bytes_type_bytes]
  in
  let* result = decode_entrypoints_result bytes in
  match result with
  | Ok (Some (_, entries, [])) ->
      check_entry_names entries ["default"; "transfer"] ;
      Lwt.return_unit
  | Ok (Some (_, _, _ :: _)) ->
      Test.fail "two-entry legacy decode must yield no views"
  | Ok None -> Test.fail "expected Some, got None"
  | Error _ -> Test.fail "expected Ok Some, got Error"

(* New shape with no synthetic views (the ERC-20 wrapper today). *)
let test_new_shape_no_views () =
  let open Lwt_syntax in
  let bytes =
    new_bytes
      ~entries:
        [
          entry "transfer" pair_string_bytes_bytes;
          entry "approve" pair_string_bytes_bytes;
        ]
      ~views:[]
  in
  let* result = decode_entrypoints_result bytes in
  match result with
  | Ok (Some (_, entries, [])) ->
      check_entry_names entries ["transfer"; "approve"] ;
      Lwt.return_unit
  | Ok (Some (_, _, _ :: _)) ->
      Test.fail "new-shape with empty views must yield no views"
  | Ok None -> Test.fail "expected Some, got None"
  | Error _ -> Test.fail "expected Ok Some, got Error"

let lwt_to_alcotest f () = Lwt_main.run (f ())

let tests =
  [
    ( "decode_entrypoints_result",
      [
        ("None encoding", `Quick, lwt_to_alcotest test_none);
        ("new shape with views", `Quick, lwt_to_alcotest test_new_shape);
        ( "new shape with empty views",
          `Quick,
          lwt_to_alcotest test_new_shape_no_views );
        ("legacy shape", `Quick, lwt_to_alcotest test_legacy_shape);
        ( "legacy shape with one entry",
          `Quick,
          lwt_to_alcotest test_legacy_shape_single_entry );
        ( "legacy shape with two entries (boundary)",
          `Quick,
          lwt_to_alcotest test_legacy_shape_two_entries );
      ] );
  ]

let () = Alcotest.run ~__FILE__ "decode_entrypoints_result" tests

let () = Test.run ()
