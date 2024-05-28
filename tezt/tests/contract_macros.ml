(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(* Testing
   -------
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_macros.ml
   Subject:      Tests for contracts using macros that do not require origination.
*)

let test_macros_tests parameterization protocols =
  Fun.flip List.iter parameterization
  @@ fun (script, storage, input, expected) ->
  ( Protocol.register_test
      ~__FILE__
      ~title:
        (sf
           "macros [%s--storage%d--input%d]"
           script
           (Hashtbl.hash storage)
           (Hashtbl.hash input))
      ~tags:["michelson"; "macros"]
      ~uses_node:false
  @@ fun protocol ->
    let client = Client.create_with_mode Mockup in
    let* {storage = run_script_res_storage; _} =
      Client.run_script_at
        ~protocol_hash:(Protocol.hash protocol)
        ~storage
        ~input
        client
        ["macros"; script]
        protocol
    in
    let error_msg =
      "Expected storage %R, got %L when executing"
      ^ sf " %s with input %s and storage %s" script input storage
    in
    Check.((run_script_res_storage = expected) string ~__LOC__ ~error_msg) ;
    unit )
    protocols

let test_macros_tests =
  test_macros_tests
    [
      (* FORMAT: contract_file storage input expected_result *)
      ("build_list", "{}", "0", "{ 0 }");
      ("build_list", "{}", "3", "{ 0 ; 1 ; 2 ; 3 }");
      ( "build_list",
        "{}",
        "10",
        "{ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 ; 8 ; 9 ; 10 }" );
      (* Find maximum int in list -- returns None if not found *)
      ("max_in_list", "None", "{}", "None");
      ("max_in_list", "None", "{ 1 }", "(Some 1)");
      ("max_in_list", "None", "{ -1 }", "(Some -1)");
      ("max_in_list", "None", "{ 10 ; -1 ; -20 ; 100 ; 0 }", "(Some 100)");
      ("max_in_list", "None", "{ -10 ; -1 ; -20 ; -100 }", "(Some -1)");
      (* Test comparisons on tez { EQ ; GT ; LT ; GE ; LE } *)
      ( "compare",
        "{}",
        "(Pair 1000000 2000000)",
        "{ False ; False ; True ; False ; True }" );
      ( "compare",
        "{}",
        "(Pair 2000000 1000000)",
        "{ False ; True ; False ; True ; False }" );
      ( "compare",
        "{}",
        "(Pair 2370000 2370000)",
        "{ True ; False ; False ; True ; True }" );
      (* Test ASSERT *)
      ("assert", "Unit", "True", "Unit");
      (* ASSERT_{OP} *)
      ("assert_eq", "Unit", "(Pair -1 -1)", "Unit");
      ("assert_neq", "Unit", "(Pair 0 -1)", "Unit");
      ("assert_lt", "Unit", "(Pair -1 0)", "Unit");
      ("assert_le", "Unit", "(Pair 0 0)", "Unit");
      ("assert_le", "Unit", "(Pair -1 0)", "Unit");
      ("assert_gt", "Unit", "(Pair 0 -1)", "Unit");
      ("assert_ge", "Unit", "(Pair 0 0)", "Unit");
      ("assert_ge", "Unit", "(Pair 0 -1)", "Unit");
      (* ASSERT_CMP{OP} *)
      ("assert_cmpeq", "Unit", "(Pair -1 -1)", "Unit");
      ("assert_cmpneq", "Unit", "(Pair 0 -1)", "Unit");
      ("assert_cmplt", "Unit", "(Pair -1 0)", "Unit");
      ("assert_cmple", "Unit", "(Pair -1 0)", "Unit");
      ("assert_cmple", "Unit", "(Pair 0 0)", "Unit");
      ("assert_cmpgt", "Unit", "(Pair 0 -1)", "Unit");
      ("assert_cmpge", "Unit", "(Pair 0 -1)", "Unit");
      ("assert_cmpge", "Unit", "(Pair 0 0)", "Unit");
      (* Tests the SET_CAR and SET_CDR instructions *)
      ( "set_caddaadr",
        "(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)",
        "3000000",
        "(Pair (Pair 1 2 (Pair (Pair 3 3000000) 4) 5) 6)" );
      ( "map_caddaadr",
        "(Pair (Pair 1 2 (Pair (Pair 3 0) 4) 5) 6)",
        "Unit",
        "(Pair (Pair 1 2 (Pair (Pair 3 1000000) 4) 5) 6)" );
      (* Test comparisons on bytes { EQ ; GT ; LT ; GE ; LE } *)
      ( "compare_bytes",
        "{}",
        "(Pair 0x33 0x34)",
        "{ False ; False ; True ; False ; True }" );
      ( "compare_bytes",
        "{}",
        "(Pair 0x33 0x33aa)",
        "{ False ; False ; True ; False ; True }" );
      ( "compare_bytes",
        "{}",
        "(Pair 0x33 0x33)",
        "{ True ; False ; False ; True ; True }" );
      ( "compare_bytes",
        "{}",
        "(Pair 0x34 0x33)",
        "{ False ; True ; False ; True ; False }" );
    ]

let check_script_failwith =
  Process.check_error ~msg:(rex "script reached FAILWITH instruction")

let test_macros_failures protocols =
  List.iter
    (fun (script, storage, input) ->
      ( Protocol.register_test
          ~__FILE__
          ~title:
            (sf
               "macro failure [%s--storage%d--input%d]"
               script
               (Hashtbl.hash storage)
               (Hashtbl.hash input))
          ~tags:["michelson"; "macros"]
          ~uses_node:false
      @@ fun protocol ->
        let client = Client.create_with_mode Mockup in
        Client.spawn_run_script_at
          ~protocol_hash:(Protocol.hash protocol)
          ~storage
          ~input
          client
          ["macros"; script]
          protocol
        |> check_script_failwith )
        protocols)
    [
      ("assert", "Unit", "False");
      ("assert_eq", "Unit", "(Pair 0 -1)");
      ("assert_neq", "Unit", "(Pair -1 -1)");
      ("assert_lt", "Unit", "(Pair 0 -1)");
      ("assert_lt", "Unit", "(Pair 0 0)");
      ("assert_le", "Unit", "(Pair 0 -1)");
      ("assert_gt", "Unit", "(Pair -1 0)");
      ("assert_gt", "Unit", "(Pair 0 0)");
      ("assert_ge", "Unit", "(Pair -1 0)");
      ("assert_cmpeq", "Unit", "(Pair 0 -1)");
      ("assert_cmpneq", "Unit", "(Pair -1 -1)");
      ("assert_cmplt", "Unit", "(Pair 0 0)");
      ("assert_cmplt", "Unit", "(Pair 0 -1)");
      ("assert_cmple", "Unit", "(Pair 0 -1)");
      ("assert_cmpgt", "Unit", "(Pair 0 0)");
      ("assert_cmpgt", "Unit", "(Pair -1 0)");
      ("assert_cmpge", "Unit", "(Pair -1 0)");
    ]

let transfer client ~receiver ?(giver = Constant.bootstrap1) arg =
  {
    value =
      Client.spawn_transfer
        client
        ~burn_cap:Tez.one
        ~giver:giver.alias
        ~amount:Tez.zero
        ~receiver
        ~arg;
    run = Process.check;
  }

let check_script_failwith {value; _} = check_script_failwith value

let test_guestbook =
  Protocol.register_test
    ~__FILE__
    ~title:"test guestbook.tz"
    ~tags:["michelson"; "macros"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* guestbook, _address =
    Client.originate_contract_at
      ~amount:(Tez.of_int 100)
      ~src:Constant.bootstrap1.alias
      ~init:{|{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" None }|}
      ~burn_cap:Tez.one
      client
      ["macros"; "guestbook"]
      protocol
  in
  let transfer = transfer client ~receiver:guestbook in
  let* () =
    transfer ~giver:Constant.bootstrap2 {|"Pas moi"|} |> check_script_failwith
  in
  let*! () = transfer ~giver:Constant.bootstrap1 {|"Coucou"|} in
  let* storage = Client.contract_storage guestbook client in
  Check.(
    (String.trim storage
   = {|{ Elt "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" (Some "Coucou") }|})
      string
      ~__LOC__
      ~error_msg:"Expected guestbook storage %R, got %L") ;
  let* () =
    transfer ~giver:Constant.bootstrap3 {|"Pas moi non plus"|}
    |> check_script_failwith
  in
  transfer ~giver:Constant.bootstrap1 {|"Recoucou ?"|} |> check_script_failwith

let test_big_map =
  Protocol.register_test
    ~__FILE__
    ~title:"test big_map"
    ~tags:["michelson"; "macros"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* big_map_contract, _address =
    Client.originate_contract_at
      ~amount:(Tez.of_int 100)
      ~src:Constant.bootstrap1.alias
      ~init:"(Pair { Elt 1 Unit ; Elt 2 Unit ; Elt 3 Unit } Unit)"
      ~burn_cap:Tez.one
      client
      ["macros"; "big_map_mem"]
      protocol
  in
  let transfer = transfer client ~receiver:big_map_contract in
  let*! () = transfer "(Pair 0 False)" in
  let* () = transfer "(Pair 0 True)" |> check_script_failwith in
  let*! () = transfer "(Pair 1 True)" in
  let* () = transfer "(Pair 1 False)" |> check_script_failwith in
  let*! () = transfer "(Pair 2 True)" in
  let* () = transfer "(Pair 2 False)" |> check_script_failwith in
  let*! () = transfer "(Pair 3 True)" in
  let* () = transfer "(Pair 3 False)" |> check_script_failwith in
  let*! () = transfer "(Pair 4 False)" in
  let* () = transfer "(Pair 4 True)" |> check_script_failwith in
  unit

let test_big_map_get_add =
  Protocol.register_test
    ~__FILE__
    ~title:"test big_map_get_add"
    ~tags:["michelson"; "macros"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* big_map_contract, _address =
    Client.originate_contract_at
      ~amount:(Tez.of_int 100)
      ~src:Constant.bootstrap1.alias
      ~init:"(Pair { Elt 0 1 ; Elt 1 2 ; Elt 2 3 } Unit)"
      ~burn_cap:Tez.one
      client
      ["macros"; "big_map_get_add"]
      protocol
  in
  Lwt_list.iter_s
    (fun parameter ->
      Runnable.run @@ transfer client ~receiver:big_map_contract parameter)
    [
      "(Pair (Pair 200 (Some 2)) (Pair 200 (Some 2)))";
      "(Pair (Pair 200 None) (Pair 200 None))";
      "(Pair (Pair 200 None) (Pair 300 None))";
      "(Pair (Pair 1 None) (Pair 200 None))";
      "(Pair (Pair 1 (Some 2)) (Pair 0 (Some 1)))";
      "(Pair (Pair 400 (Some 1232)) (Pair 400 (Some 1232)))";
      "(Pair (Pair 401 (Some 0)) (Pair 400 (Some 1232)))";
    ]

let test_macro_expansion protocols =
  List.iter
    (fun protocol ->
      List.iter
        (fun script ->
          Protocol.register_regression_test
            ~__FILE__
            ~title:(sf "Macro expansion: %s" (Michelson_script.name_s script))
            ~tags:["michelson"; "macros"]
            ~uses_node:false
            (fun protocol ->
              let client = Client.create_with_mode Mockup in
              let script_path = Michelson_script.path script in
              let* (expansion : string) =
                Client.expand_macros
                  ~protocol_hash:(Protocol.hash protocol)
                  ~no_base_dir_warnings:true
                  client
                  script_path
              in
              Regression.capture expansion ;
              unit)
            [protocol])
        Michelson_script.(find_all_in protocol ["macros"]))
    protocols

let register ~protocols =
  test_macros_tests protocols ;
  test_macros_failures protocols ;
  test_guestbook protocols ;
  test_big_map protocols ;
  test_big_map_get_add protocols ;
  test_macro_expansion protocols
