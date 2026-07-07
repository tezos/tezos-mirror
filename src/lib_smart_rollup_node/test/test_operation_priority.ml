(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Functori <contact@functori.com>              *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Smart rollup node
    Invocation:   dune exec src/lib_smart_rollup_node/test/main.exe \
                  -- -f src/lib_smart_rollup_node/test/test_operation_priority.ml
    Subject:      Test priority ordering of injected operation kinds
*)

open Operation_kind

(* Expected priority classes, from highest priority to lowest. Kinds in
   the same class have equal priority. *)
let expected_classes =
  [
    [Timeout];
    [Refute];
    [Cement];
    [Publish; Publish_dal_commitment];
    [Recover];
    [Add_messages];
    [Execute_outbox_message];
  ]

let check_lt k1 k2 =
  if compare_priority k1 k2 >= 0 then
    Assert.fail_msg
      "%s should have a strictly higher priority than %s"
      (to_string k1)
      (to_string k2)

let check_eq k1 k2 =
  if compare_priority k1 k2 <> 0 then
    Assert.fail_msg
      "%s should have the same priority as %s"
      (to_string k1)
      (to_string k2)

let test_cement_before_publish () = assert (compare_priority Cement Publish < 0)

let test_documented_chain () =
  check_lt Timeout Refute ;
  check_lt Refute Cement ;
  check_lt Cement Publish ;
  check_eq Publish Publish_dal_commitment

let test_total_ordering () =
  (* The expected classes cover exactly the operation kinds of {!all}. *)
  let expected_kinds = List.flatten expected_classes in
  assert (List.compare_lengths expected_kinds all = 0) ;
  List.iter
    (fun k -> assert (List.mem ~equal:Stdlib.( = ) k expected_kinds))
    all ;
  (* Kinds within a class compare equal. *)
  List.iter
    (fun cls -> List.iter (fun k1 -> List.iter (check_eq k1) cls) cls)
    expected_classes ;
  (* Classes are strictly ordered. *)
  let rec check_classes = function
    | cls1 :: (cls2 :: _ as rest) ->
        List.iter (fun k1 -> List.iter (check_lt k1) cls2) cls1 ;
        check_classes rest
    | [_] | [] -> ()
  in
  check_classes expected_classes

let tests =
  [
    Alcotest.test_case
      "cement has priority over publish"
      `Quick
      test_cement_before_publish;
    Alcotest.test_case "documented ordering chain" `Quick test_documented_chain;
    Alcotest.test_case
      "total ordering over all kinds"
      `Quick
      test_total_ordering;
  ]

let () =
  Alcotest.run ~__FILE__ "lib_smart_rollup_node" [("Operation_priority", tests)]
