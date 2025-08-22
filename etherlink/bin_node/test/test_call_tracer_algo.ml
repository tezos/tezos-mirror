(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    bin_node
    Invocation:   dune exec etherlink/bin_node/test/test_call_tracer_algo.exe
    Subject:      Tests for implementation of call tracer algorithm
    *)

open Evm_node_lib_dev.Tracer.CallTracerRead

type call_mock = {value : string; calls : call_mock list}

module Mock = struct
  let get_next storage counter =
    let open Lwt_result_syntax in
    (* we want to start at the end of the list *)
    let index = List.length storage - 1 - counter in
    if index < 0 then return None
    else
      match List.nth_opt storage index with
      | None -> return None
      | Some (value, depth) -> return (Some ({value; calls = []}, depth))

  let end_call {value; _} call_list = {value; calls = call_list}

  let rec pp_call out {value; calls} =
    Format.fprintf
      out
      "%s[%a]"
      value
      (Format.pp_print_list
         ~pp_sep:(fun out () -> Format.fprintf out "; ")
         pp_call)
      calls

  let check result expected msg =
    match result with
    | Error e ->
        Test.fail
          "Failed to rebuild call trace : %a"
          (Format.pp_print_list pp)
          e
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
      let storage = [("B", 1); ("C", 1); ("D", 1); ("A", 0)] in
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

let complex_example =
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

let complex_example2 =
  {
    value = "E";
    calls =
      [
        {
          value = "F";
          calls = [{value = "G1"; calls = []}; {value = "G2"; calls = []}];
        };
        {value = "H"; calls = []};
      ];
  }

let test_complex =
  register_unit_test
    ~title:"CallTracer: Test more complex tree"
    ~tags:["call_tracer"; "debug"; "top_call"]
    (fun _protocol ->
      let storage = [("C1", 2); ("C2", 2); ("B", 1); ("D", 1); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check res complex_example "Should have been equal")

let test_fail_too_many_top_call =
  register_unit_test
    ~title:"CallTracer: Test too many top calls"
    ~tags:["call_tracer"; "debug"; "fail"; "top_call"]
    (fun _protocol ->
      let storage = [("A", 0); ("A", 0)] in
      let* res = build_calltrace Mock.end_call (Mock.get_next storage) in
      Mock.check_failed res "Should have failed")

let test_succeed_trace_block =
  register_unit_test
    ~title:"CallTracer: Test trace block"
    ~tags:["call_tracer"; "debug"; "block"; "top_call"]
    (fun _protocol ->
      let trace1 = [("C1", 2); ("C2", 2); ("B", 1); ("D", 1); ("A", 0)] in
      let trace2 = [("G1", 2); ("G2", 2); ("F", 1); ("H", 1); ("E", 0)] in
      let storage = trace1 @ trace2 in
      let* res = build_calltraces Mock.end_call (Mock.get_next storage) in
      match res with
      | Ok [res1; res2] ->
          let* () =
            Mock.check (Ok res1) complex_example "Couldn't rebuild first trace"
          in
          Mock.check (Ok res2) complex_example2 "Couldn't rebuild second trace"
      | _ -> Test.fail "Could not trace block")

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
      let storage = [("B", 2); ("A", 0)] in
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

let to_string call =
  Data_encoding.Json.to_string
  @@ Data_encoding.Json.construct
       Evm_node_lib_dev_encoding.Tracer_types.CallTracer.output_encoding
       call

let make_string n s = String.concat "" (List.repeat n s)

let make_hex n s =
  Evm_node_lib_dev_encoding.Ethereum_types.Hex (make_string n s)

let test_decoding_rlp =
  register_unit_test
    ~title:"CallTracer: Test decoding call"
    ~tags:["call_tracer"; "debug"; "encoding"; "rlp"]
    (fun _protocol ->
      let open Evm_node_lib_dev_encoding.Tracer_types in
      let open Evm_node_lib_dev_encoding.Ethereum_types in
      let bytes =
        Hex.to_bytes
          (`Hex
             "f8d88443414c4c941919191919191919191919191919191919191919d5941919191919191919191919191919191919191919a03dd5030000000000000000000000000000000000000000000000000000000000c988881300000000000088881300000000000083000102c483000102c483000102f861f85ff85d941919191919191919191919191919191919191919f842a01919191919191919191919191919191919191919191919191919191919191919a00d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d83000102820200")
      in
      (*
        Comes from the following rust:

        let logs = Log {
            address: H160::from([25; 20]),
            topics: vec![H256::from([25; 32]), H256::from([13; 32])],
            data: vec![0x00, 0x01, 0x02],
        };
        let call_trace = CallTrace {
            type_: "CALL".into(),
            from: H160::from([25; 20]),
            to: Some(H160::from([25; 20])),
            value: U256::from(251197),
            gas: Some(5000),
            gas_used: 5000,
            input: vec![0x00, 0x01, 0x02],
            output: Some(vec![0x00, 0x01, 0x02]),
            error: Some(vec![0x00, 0x01, 0x02]),
            revert_reason: None,
            logs: Some(vec![logs]),
        };*)
      let logs =
        Some
          [
            CallTracer.
              {
                address = Address (Hex (make_string 20 "19"));
                topics = [make_hex 32 "19"; make_hex 32 "0d"];
                data = Hex "000102";
              };
          ]
      in
      let expected =
        CallTracer.
          {
            calls = [];
            type_ = "CALL";
            from = Address (Hex (make_string 20 "19"));
            to_ = Some (Address (Hex (make_string 20 "19")));
            value = Z.of_int 251197;
            gas = Some (Z.of_int 5000);
            gas_used = Z.of_int 5000;
            input = Hex "000102";
            output = Some (Hex "000102");
            error = Some "\000\001\002";
            revert_reason = None;
            logs;
          }
      in
      let expected_depth = 2 in
      match
        CallTracer.decode_call
          (Option.value bytes ~default:(Bytes.of_string "\x01"))
      with
      | Error e ->
          Test.fail "Failed to rebuild call %a" (Format.pp_print_list pp) e
      | Ok (call, depth) ->
          Check.(
            (depth = expected_depth)
              int
              ~error_msg:"wrong depth, expected %R but got %L") ;
          Check.is_true
            (call = expected)
            ~error_msg:
              (Format.asprintf
                 "error decoding call, expected \n%s \n but got \n%s "
                 (to_string expected)
                 (CallTracer.to_string call)) ;
          Lwt.return_unit)

let _test_decoding_rlp_min =
  register_unit_test
    ~title:"CallTracer: Test decoding call with few values"
    ~tags:["call_tracer"; "debug"; "encoding"; "rlp"]
    (fun _protocol ->
      let open Evm_node_lib_dev_encoding.Tracer_types in
      let open Evm_node_lib_dev_encoding.Ethereum_types in
      let bytes =
        Hex.to_bytes
          (`Hex
             "f8518443414c4c941919191919191919191919191919191919191919c0a03dd5030000000000000000000000000000000000000000000000000000000000c088881300000000000083000102c0c0c0c0820200")
      in
      (*
        Comes from the following rust:

        let call_trace_none = CallTrace {
            type_: "CALL".into(),
            from: H160::from([25; 20]),
            to: None,
            value: U256::from(251197),
            gas: None,
            gas_used: 5000,
            input: vec![0x00, 0x01, 0x02],
            output: None,
            error: None,
            revert_reason: None,
            logs: None,
            depth: 2,
        };
        *)
      let expected =
        CallTracer.
          {
            calls = [];
            type_ = "CALL";
            from = Address (Hex (make_string 20 "19"));
            to_ = Some (Address (Hex (make_string 20 "19")));
            value = Z.of_int 251197;
            gas = Some (Z.of_int 5000);
            gas_used = Z.of_int 5000;
            input = Hex "000102";
            output = Some (Hex "000102");
            error = Some "\000\001\002";
            revert_reason = None;
            logs = None;
          }
      in
      let expected_depth = 2 in
      match
        CallTracer.decode_call
          (Option.value bytes ~default:(Bytes.of_string "\x01"))
      with
      | Error e ->
          Test.fail "Failed to rebuild call %a" (Format.pp_print_list pp) e
      | Ok (call, depth) ->
          Check.(
            (depth = expected_depth)
              int
              ~error_msg:"wrong depth, expected %R but got %L") ;
          Check.is_true
            (call = expected)
            ~error_msg:
              (Format.asprintf
                 "error decoding call, expected \n%s \n but got \n%s "
                 (to_string expected)
                 (CallTracer.to_string call)) ;
          Lwt.return_unit)

let test_decoding_rlp_revert_reason =
  register_unit_test
    ~title:"CallTracer: Test decoding call with a revert reason"
    ~tags:["call_tracer"; "debug"; "encoding"; "rlp"; "revert_reason"]
    (fun _protocol ->
      let open Evm_node_lib_dev_encoding.Tracer_types in
      let open Evm_node_lib_dev_encoding.Ethereum_types in
      let bytes =
        Hex.to_bytes
          (`Hex
             "f901258443414c4c941919191919191919191919191919191919191919d5941919191919191919191919191919191919191919a03dd5030000000000000000000000000000000000000000000000000000000000c988881300000000000088881300000000000083000102f84bb84908c379a0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000056572726f72c9885265766572746564f861f85ff85d941919191919191919191919191919191919191919f842a01919191919191919191919191919191919191919191919191919191919191919a00d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d83000102820200")
      in
      (*
        Comes from the following rust:

        let logs = Log {
            address: H160::from([25; 20]),
            topics: vec![H256::from([25; 32]), H256::from([13; 32])],
            data: vec![0x00, 0x01, 0x02],
        };
        let call_trace = CallTrace {
            type_: "CALL".into(),
            from: H160::from([25; 20]),
            to: Some(H160::from([25; 20])),
            value: U256::from(251197),
            gas: Some(5000),
            gas_used: 5000,
            input: vec![0x00, 0x01, 0x02],
            output: Some("08c379a0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000056572726f72"),
            error: Some("Reverted"),
            logs: Some(vec![logs]),
        };*)
      let logs =
        Some
          [
            CallTracer.
              {
                address = Address (Hex (make_string 20 "19"));
                topics = [make_hex 32 "19"; make_hex 32 "0d"];
                data = Hex "000102";
              };
          ]
      in
      let expected =
        CallTracer.
          {
            calls = [];
            type_ = "CALL";
            from = Address (Hex (make_string 20 "19"));
            to_ = Some (Address (Hex (make_string 20 "19")));
            value = Z.of_int 251197;
            gas = Some (Z.of_int 5000);
            gas_used = Z.of_int 5000;
            input = Hex "000102";
            output =
              Some
                (Hex
                   "08c379a0000000000000000000000000000000000000000000000000000000000000002000000000000000000000000000000000000000000000000000000000000000056572726f72");
            error = Some "execution reverted";
            revert_reason = Some "error";
            logs;
          }
      in
      let expected_depth = 2 in
      match
        CallTracer.decode_call
          (Option.value bytes ~default:(Bytes.of_string "\x01"))
      with
      | Error e ->
          Test.fail "Failed to rebuild call %a" (Format.pp_print_list pp) e
      | Ok (call, depth) ->
          Check.(
            (depth = expected_depth)
              int
              ~error_msg:"wrong depth, expected %R but got %L") ;
          Check.is_true
            (call = expected)
            ~error_msg:
              (Format.asprintf
                 "error decoding call, expected \n%s \n but got \n%s "
                 (to_string expected)
                 (CallTracer.to_string call)) ;
          Lwt.return_unit)

let protocols = Protocol.all

let () =
  test_top_call protocols ;
  test_order protocols ;
  test_rake protocols ;
  test_complex protocols ;
  test_fail_too_many_top_call protocols ;
  test_succeed_trace_block protocols ;
  test_fail_wrong_start_depth protocols ;
  test_fail_wrong_depth protocols ;
  test_fail_wrong_depth_2 protocols ;
  test_decoding_rlp protocols ;
  test_decoding_rlp_revert_reason protocols ;
  ()

let () = Test.run ()
