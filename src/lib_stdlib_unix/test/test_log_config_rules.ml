(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
   Component:    Log config rules
   Invocation:   dune exec src/lib_stdlib_unix/test/main.exe
   Subject:      Log config rules parsing
*)

open Level_config_rules

let level_typ = Check.(convert Internal_event.Level.to_string string)

let test_case rules expected =
  let rules = rules |> parse_rules in
  Check.(
    (expected = rules)
      (list (tuple2 string level_typ))
      ~error_msg:"Expected %L, got %R"
      ~__LOC__)

let test_case_error rules exc =
  try
    let _ = rules |> parse_rules in
    Assert.assert_false "This parsing rule should fail" true
  with e ->
    let expected = Printexc.to_string exc in
    let e = Printexc.to_string e in
    Check.(
      (expected = e)
        string
        ~error_msg:"Wrong exception: expected %L, got %R"
        ~__LOC__)

let () =
  Tezt_core.Test.register
    ~__FILE__
    ~title:"Log_config_rules: parsing rules"
    ~tags:["log"; "config"]
  @@ fun () ->
  let open Internal_event in
  test_case
    "toto -> notice; foo -> debug; bar -> error;"
    [("toto", Notice); ("foo", Debug); ("bar", Error)] ;
  test_case "toto*titi -> notice" [("toto*titi", Notice)] ;
  test_case_error "toto" Incorrect_log_rules_not_a_level ;
  test_case_error "toto -> titi" Incorrect_log_rules_not_a_level ;
  test_case "debug; titi -> Warning" [("", Debug); ("titi", Warning)] ;
  test_case "" [] ;
  test_case "   toto   ->  notice  " [("toto", Notice)] ;
  test_case_error "toto -> tata -> error" Incorrect_log_rules_syntax ;
  test_case "info" [("", Info)] ;
  test_case_error "notice ->" Incorrect_log_rules_missing_level ;
  test_case_error "-> info" Incorrect_log_rules_missing_pattern ;
  test_case_error "  -> info" Incorrect_log_rules_missing_pattern ;
  test_case_error "  ->  " Incorrect_log_rules_missing_pattern ;
  unit
