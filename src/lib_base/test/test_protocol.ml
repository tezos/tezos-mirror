(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
   Component:    Base, Protocol
   Invocation:   dune exec src/lib_base/test/main.exe
   Subject:      Check the ordering of protocol versions
*)

let all_env_versions = Protocol.[V0; V1; V2; V3]

let env_v_eq_check () =
  List.iter
    (fun v -> assert (Protocol.compare_version v v = 0))
    all_env_versions

let env_v_lt_check () =
  let rec check = function
    | [] -> ()
    | v :: vs ->
        assert (List.for_all (fun w -> Protocol.compare_version v w < 0) vs) ;
        check vs
  in
  check all_env_versions

let env_v_gt_check () =
  let rec check = function
    | [] -> ()
    | v :: vs ->
        assert (List.for_all (fun w -> Protocol.compare_version w v > 0) vs) ;
        check vs
  in
  check all_env_versions

let env_v_comparison_checks =
  let open Alcotest in
  [
    test_case "equal" `Quick env_v_eq_check;
    test_case "less-than" `Quick env_v_lt_check;
    test_case "greater-than" `Quick env_v_gt_check;
  ]

let () =
  Alcotest.run
    "Protocol"
    [("environment-version-comparison", env_v_comparison_checks)]
