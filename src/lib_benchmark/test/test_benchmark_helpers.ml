(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

open Tezos_benchmark

(** Helper for Data_encoding.json alcotest print and equality test *)
let json_t : Data_encoding.json Alcotest.testable =
  (module struct
    type t = Data_encoding.json

    let equal = ( = )

    let pp = Data_encoding.Json.pp
  end)

let exn_t : exn Alcotest.testable =
  (module struct
    type t = exn

    let equal = ( = )

    let pp ppf x = Format.pp_print_string ppf (Printexc.to_string x)
  end)

let load_missing_json_err () =
  let file = "./__doesnotexist" in
  let expected =
    Result.error
    @@ Sys_error (Printf.sprintf "%s: No such file or directory" file)
  in
  let actual () = Benchmark_helpers.load_json file in
  let result_t = Alcotest.result json_t exn_t in
  Alcotest.(check result_t) "missing file returns Error " (actual ()) expected

let tests =
  [
    Alcotest_lwt.test_case_sync
      "raise exception when file is missing"
      `Quick
      load_missing_json_err;
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-benchmark" [("benchmark_helpers", tests)]
  |> Lwt_main.run
