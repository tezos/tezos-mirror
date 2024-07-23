(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    bin_node
    Invocation:   dune exec etherlink/bin_node/test/main.exe -- --file test_call_tracer_algo.ml
    Subject:      Tests for implementation of call tracer algorithm
    *)

(* TODO: These tests need to be refactored in tezt, Alcote*t has been deprecated *)

open Evm_node_lib_dev.Tracer.CallTracerRead

type call_mock = {value : string; calls : call_mock list}

module Mock = struct
  let get_next storage counter =
    let open Lwt_result_syntax in
    (* we want to start at the end of the list *)
    let index = (List.length storage) - 1 - counter in
    if index < 0 then return None else
    match List.nth_opt storage index with
    | None -> return None
    | Some (value, depth) -> return (Some ({value; calls = []}, depth))

  let end_call {value; _} call_list = {value; calls = call_list}

  let rec pp_call out {value; calls} =
    Format.fprintf
      out
      "%s[%a]"
      value
      (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out "; ") pp_call)
      calls

  let check result expected msg =
    match result with
    | Error e ->
        Test.fail "Failed to rebuild call trace : %a" (Format.pp_print_list pp) e 
    | Ok res ->
        Check.is_true
          (res = expected)
          ~error_msg:
            (Format.asprintf
               "%s (actual = %a , expected = %a)"
               msg
               pp_call
               res
               pp_call
               expected) ;
        Lwt.return_unit

  let check_failed result msg =
    match result with
    | Ok trace ->
        Test.fail
          "%s ; Managed to rebuild %s"
          msg
          (Format.asprintf "%a" pp_call trace)
    | Error _ -> Lwt.return_unit
end

let register_unit_test =
  Protocol.register_test
    ~__FILE__
    ~uses_client:false
    ~uses_node:false
    ~uses_admin_client:false

let test_top_call =
  register_unit_test
    ~title:"CallTracer: Test with only a top call"
    ~tags:["call_tracer"; "debug"; "top_call"]
    (fun _protocol ->
      let storage = [("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check res {value = "A"; calls = []} "Should have been empty top call")

let test_order =
  register_unit_test
    ~title:"CallTracer: Test order of children"
    ~tags:["call_tracer"; "debug"; "top_call"]
    (fun _protocol ->
      let expected =
        {
          value = "A";
          calls =
            [
              {value = "B"; calls = []};
              {value = "C"; calls = []};
              {value = "D"; calls = []};
            ];
        }
      in
      let storage = [("B",1);("C",1);("D",1);("A",0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check res expected "Children should be in order")

let test_rake =
  register_unit_test
    ~title:"CallTracer: Test rake"
    ~tags:["call_tracer"; "debug"; "rake"]
    (fun _protocol ->
      let expected =
        {
          value = "A";
          calls =
            [
              {
                value = "B";
                calls =
                  [
                    {
                      value = "C";
                      calls =
                        [{value = "D"; calls = [{value = "E"; calls = []}]}];
                    };
                  ];
              };
            ];
        }
      in
      let storage = [("E", 4); ("D", 3); ("C", 2); ("B", 1); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check res expected "Should have been equal")

let test_complex =
  register_unit_test
    ~title:"CallTracer: Test more complex tree"
    ~tags:["call_tracer"; "debug"; "top_call"]
    (fun _protocol ->
      let expected =
        {
          value = "A";
          calls =
            [
              {
                value = "B";
                calls = [{value = "C1"; calls = []}; {value = "C2"; calls = []}];
              };
              {value = "D"; calls = []};
            ];
        }
      in
      let storage = [("C1", 2); ("C2", 2); ("B", 1); ("D", 1); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check res expected "Should have been equal")

let test_fail_too_many_top_call =
  register_unit_test
    ~title:"CallTracer: Test too many top calls"
    ~tags:["call_tracer"; "debug"; "fail"; "top_call"]
    (fun _protocol ->
      let storage = [("A", 0); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check_failed res "Should have failed")

let test_fail_wrong_start_depth =
  register_unit_test
    ~title:"CallTracer: Test wrong start depth"
    ~tags:["call_tracer"; "debug"; "fail"; "depth"]
    (fun _protocol ->
      let storage = [("A", 2)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check_failed res "Should have failed")

let test_fail_wrong_depth =
  register_unit_test
    ~title:"CallTracer: Test wrong child depth"
    ~tags:["call_tracer"; "debug"; "fail"; "depth"]
    (fun _protocol ->
        let storage = [ ("B", 2);("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check_failed res "Should have failed")

let test_fail_wrong_depth_2 =
  register_unit_test
    ~title:"CallTracer: Test wrong child depth (deeper)"
    ~tags:["call_tracer"; "debug"; "fail"; "depth"]
    (fun _protocol ->
      let storage = [("C1", 2); ("C2", 3); ("B", 1); ("D", 1); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check_failed res "Should have failed")

let protocols = Protocol.all

let () =
  test_top_call protocols ;
  test_order protocols ;
  test_rake protocols ;
  test_complex protocols ;
  test_fail_too_many_top_call protocols ;
  test_fail_wrong_start_depth protocols ;
  test_fail_wrong_depth protocols ;
  test_fail_wrong_depth_2 protocols ;
  ()
