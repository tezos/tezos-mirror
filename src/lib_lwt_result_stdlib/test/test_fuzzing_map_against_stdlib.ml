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
   Invocation:   dune exec src/lib_lwt_result_stdlib/test/main.exe
   Subject:      Test fuzzing map against stdlib
*)

open Test_fuzzing_lib

(* The test framework [Test_fuzzing_lib] uses [char] as the type of values
   stored inside the collections being tested. For variety, we use [int] for the
   type of keys, but we keep [char] for all the elements of all the maps tested
   below. *)
module StdlibMap = Stdlib.Map.Make (Int)
module Map = Support.Lib.Map.Make (Int)

(* A large part of Map is defined by inclusion of Stdlib's Map. We only test the
   rest. *)

(* NOTE: The testing framework ([Test_fuzzing_lib]) is focused on testing the
   return values of a set of functions. [iter] (and friends) always return [()]
   (modulo the monad). As a result, in a simple test, all calls would be
   equivalent and the simple test would not mean anything. Instead we implement
   [fold] on top of [iter] and test this. *)
let iter =
  test_of_ty_with_p
    "Map.iter"
    DSL.([data; [int; data] @-> monad data; list (int * data)] --> monad data)
    (fun init f xs ->
      let acc = ref init in
      StdlibMap.iter
        (fun k x ->
          let y = f k x in
          acc := y ;
          ())
        (StdlibMap.of_seq (List.to_seq xs)) ;
      !acc)
    (fun init f xs ->
      let acc = ref init in
      Map.iter
        (fun k x ->
          let y = f k x in
          acc := y ;
          ())
        (Map.of_seq (List.to_seq xs)) ;
      !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Map.iter_s
          (fun k x ->
            let* y = f k x in
            acc := y ;
            return ())
          (Map.of_seq (List.to_seq xs))
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Map.iter_p
          (fun k x ->
            let* y = f k x in
            acc := y ;
            return ())
          (Map.of_seq (List.to_seq xs))
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Map.iter_e
          (fun k x ->
            let* y = f k x in
            acc := y ;
            return ())
          (Map.of_seq (List.to_seq xs))
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Map.iter_es
          (fun k x ->
            let* y = f k x in
            acc := y ;
            return ())
          (Map.of_seq (List.to_seq xs))
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Map.iter_ep
          (fun k x ->
            let* y = f k x in
            acc := y ;
            return ())
          (Map.of_seq (List.to_seq xs))
      in
      return !acc)

let fold =
  test_of_ty
    "Map.fold"
    DSL.(
      [[int; data; data] @-> monad data; list (int * data); data] --> monad data)
    (fun f xs init -> StdlibMap.fold f (StdlibMap.of_seq (List.to_seq xs)) init)
    (fun f xs init -> Map.fold f (Map.of_seq (List.to_seq xs)) init)
    (fun f xs init -> Map.fold_s f (Map.of_seq (List.to_seq xs)) init)
    (fun f xs init -> Map.fold_e f (Map.of_seq (List.to_seq xs)) init)
    (fun f xs init -> Map.fold_es f (Map.of_seq (List.to_seq xs)) init)

let all_map : unit Alcotest.test_case list = [iter; fold]

let () = Alcotest.run ~__FILE__ "FuzzRef" [("Map", all_map)]
