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
                  -- --file test_fuzzing_set_against_stdlib.ml
   Subject:      Test fuzzing set against stdlib
*)

open Test_fuzzing_lib

(* The testing framework [Test_fuzzing_lib] uses [char] as the type of values
   stored inside the collections being tested. Correspondigly, we make sets by
   applying the relevant functors to the [Char] module. *)
module StdlibSet = Stdlib.Set.Make (Char)
module Set = Support.Lib.Set.Make (Char)

(* A large part of Set is defined by inclusion of Stdlib's Set. We only test the
   rest. *)

(* NOTE: The testing framework ([Test_fuzzing_lib]) is focused on testing the
   return values of a set of functions. [iter] (and friends) always return [()]
   (modulo the monad). As a result, in a simple test, all calls would be
   equivalent and the simple test would not mean anything. Instead we implement
   [fold] on top of [iter] and test this. *)
let iter =
  test_of_ty_with_p
    "Set.iter"
    DSL.([data; [data] @-> monad data; list data] --> monad data)
    (fun init f xs ->
      let acc = ref init in
      StdlibSet.iter
        (fun x ->
          let y = f x in
          acc := y ;
          ())
        (StdlibSet.of_list xs) ;
      !acc)
    (fun init f xs ->
      let acc = ref init in
      Set.iter
        (fun x ->
          let y = f x in
          acc := y ;
          ())
        (Set.of_list xs) ;
      !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Set.iter_s
          (fun x ->
            let* y = f x in
            acc := y ;
            return ())
          (Set.of_list xs)
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Set.iter_p
          (fun x ->
            let* y = f x in
            acc := y ;
            return ())
          (Set.of_list xs)
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Set.iter_e
          (fun x ->
            let* y = f x in
            acc := y ;
            return ())
          (Set.of_list xs)
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Set.iter_es
          (fun x ->
            let* y = f x in
            acc := y ;
            return ())
          (Set.of_list xs)
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Set.iter_ep
          (fun x ->
            let* y = f x in
            acc := y ;
            return ())
          (Set.of_list xs)
      in
      return !acc)

let fold =
  test_of_ty
    "Set.fold"
    DSL.([[data; data] @-> monad data; list data; data] --> monad data)
    (fun f xs init -> StdlibSet.fold f (StdlibSet.of_list xs) init)
    (fun f xs init -> Set.fold f (Set.of_list xs) init)
    (fun f xs init -> Set.fold_s f (Set.of_list xs) init)
    (fun f xs init -> Set.fold_e f (Set.of_list xs) init)
    (fun f xs init -> Set.fold_es f (Set.of_list xs) init)

let all_set : unit Alcotest.test_case list = [iter; fold]

let () = Alcotest.run ~__FILE__ "FuzzRef" [("Set", all_set)]
