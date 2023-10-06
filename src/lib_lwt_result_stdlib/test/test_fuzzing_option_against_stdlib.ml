(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Stdlib
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe \
                  -- --file test_fuzzing_option_against_stdlib.ml
   Subject:      Test fuzzing option against stdlib
*)

open Test_fuzzing_lib

let map =
  test_of_ty
    "Option.map"
    DSL.([[data] @-> monad data; option data] --> monad (option data))
    Stdlib.Option.map
    Support.Lib.Option.map
    Support.Lib.Option.map_s
    Support.Lib.Option.map_e
    Support.Lib.Option.map_es

(* NOTE: The testing framework ([Test_fuzzing_lib]) is focused on testing the
   return values of a set of functions. [iter] (and friends) always return [()]
   (modulo the monad). As a result, in a simple test, all calls would be
   equivalent and the simple test would not mean anything. Instead we implement
   [fold] on top of [iter] and test this. *)
let iter =
  test_of_ty
    "Option.iter"
    DSL.([[data] @-> monad data; option data] --> monad (option data))
    (fun f o ->
      let acc = ref None in
      Stdlib.Option.iter
        (fun x ->
          let y = f x in
          acc := Some y ;
          ())
        o ;
      !acc)
    (fun f o ->
      let acc = ref None in
      Support.Lib.Option.iter
        (fun x ->
          let y = f x in
          acc := Some y ;
          ())
        o ;
      !acc)
    (fun f o ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref None in
      let* () =
        Support.Lib.Option.iter_s
          (fun x ->
            let* y = f x in
            acc := Some y ;
            return_unit)
          o
      in
      return !acc)
    (fun f o ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref None in
      let* () =
        Support.Lib.Option.iter_e
          (fun x ->
            let* y = f x in
            acc := Some y ;
            return_unit)
          o
      in
      return !acc)
    (fun f o ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref None in
      let* () =
        Support.Lib.Option.iter_es
          (fun x ->
            let* y = f x in
            acc := Some y ;
            return_unit)
          o
      in
      return !acc)

let filter =
  test_of_ty
    "Option.filter"
    DSL.([[data] @-> monad bool; option data] --> monad (option data))
    (fun f o ->
      match Stdlib.Option.map f o with
      | None | Some false -> None
      | Some true -> o)
    Support.Lib.Option.filter
    Support.Lib.Option.filter_s
    Support.Lib.Option.filter_e
    Support.Lib.Option.filter_es

(* In the specific case of the option monad, the function [filter_map] also
   happens to be the function [bind] (modulo the order of parameters). The
   former is not available in the Stdlib so we test against the latter. *)
let filter_map =
  test_of_ty
    "Option.filter_map"
    DSL.([[data] @-> monad (option data); option data] --> monad (option data))
    (fun f o -> Stdlib.Option.bind o f)
    Support.Lib.Option.filter_map
    Support.Lib.Option.filter_map_s
    Support.Lib.Option.filter_map_e
    Support.Lib.Option.filter_map_es

let filter_ok =
  test_vanilla_of_ty
    "Option.filter_ok"
    DSL.([option (result data data)] --> option data)
    (function Some (Ok v) -> Some v | _ -> None)
    Support.Lib.Option.filter_ok

let filter_error =
  test_vanilla_of_ty
    "Option.filter_error"
    DSL.([option (result data data)] --> option data)
    (function Some (Error v) -> Some v | _ -> None)
    Support.Lib.Option.filter_error

let filter_left =
  test_vanilla_of_ty
    "Option.filter_left"
    DSL.([option (either data data)] --> option data)
    (function Some (Either.Left v) -> Some v | _ -> None)
    Support.Lib.Option.filter_left

let filter_right =
  test_vanilla_of_ty
    "Option.filter_right"
    DSL.([option (either data data)] --> option data)
    (function Some (Either.Right v) -> Some v | _ -> None)
    Support.Lib.Option.filter_right

let all_option : unit Alcotest.test_case list =
  [
    map;
    iter;
    filter;
    filter_map;
    filter_ok;
    filter_error;
    filter_left;
    filter_right;
  ]

let () = Alcotest.run ~__FILE__ "FuzzRef" [("Option", all_option)]
