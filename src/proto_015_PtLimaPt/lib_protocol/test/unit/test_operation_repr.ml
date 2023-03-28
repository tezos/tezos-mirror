(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Marigold <contact@marigold.dev>                        *)
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
    Component:    Operation_repr
    Invocation:   dune exec src/proto_015_PtLimaPt/lib_protocol/test/unit/main.exe
    Dependencies: --
    Subject:      To test the modules (including the top-level)
                  in operation_repr.ml as individual units, particularly
                  failure cases. Superficial goal: increase coverage percentage.
*)
open Protocol

open Tztest

module Test_operation_repr = struct
  open Operation_repr

  let test_of_list_single_case () =
    Environment.wrap_tzresult
    @@ of_list
         [
           Contents
             (Manager_operation
                {
                  fee = Obj.magic 0;
                  operation = Obj.magic 0;
                  gas_limit = Obj.magic 0;
                  storage_limit = Obj.magic 0;
                  counter = Obj.magic 0;
                  source = Obj.magic 0;
                });
         ]
    >>?= fun contents_list ->
    match contents_list with
    | Contents_list (Single _) -> return_unit
    | _ -> failwith "Unexpected value"

  let test_of_list_multiple_case () =
    Environment.wrap_tzresult
    @@ of_list
         [
           Contents
             (Manager_operation
                {
                  fee = Obj.magic 0;
                  operation = Obj.magic 0;
                  gas_limit = Obj.magic 0;
                  storage_limit = Obj.magic 0;
                  counter = Obj.magic 0;
                  source = Obj.magic 0;
                });
           Contents
             (Manager_operation
                {
                  fee = Obj.magic 0;
                  operation = Obj.magic 0;
                  gas_limit = Obj.magic 0;
                  storage_limit = Obj.magic 0;
                  counter = Obj.magic 0;
                  source = Obj.magic 0;
                });
         ]
    >>?= fun contents_list ->
    match contents_list with
    | Contents_list (Cons (_, Single _)) -> return_unit
    | _ -> failwith "Unexpected value"

  let test_of_list_empty_case () =
    match of_list [] with
    | Ok _ -> failwith "of_list of an empty list was expected to fail"
    | Error _ -> return_unit
end

let tests =
  [
    tztest
      "of_list: single element input list"
      `Quick
      Test_operation_repr.test_of_list_single_case;
    tztest
      "of_list: multiple element input list"
      `Quick
      Test_operation_repr.test_of_list_multiple_case;
    tztest
      "of_list: empty input list"
      `Quick
      Test_operation_repr.test_of_list_empty_case;
  ]
