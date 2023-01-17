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

open Test_fuzzing_lib

let fold_left =
  test_of_ty
    "Seq.fold_left"
    DSL.([[data; data] @-> monad data; data; seq data] --> monad data)
    Stdlib.Seq.fold_left
    Support.Lib.Seq.fold_left
    Support.Lib.Seq.fold_left_s
    Support.Lib.Seq.fold_left_e
    Support.Lib.Seq.fold_left_es

(* NOTE: The testing framework ([Test_fuzzing_lib]) is focused on testing the
   return values of a set of functions. [iter] (and friends) always return [()]
   (modulo the monad). As a result, in a simple test, all calls would be
   equivalent and the simple test would not mean anything. Instead we implement
   [fold_left] on top of [iter] and test this. *)
let iter =
  test_of_ty_with_p
    "Seq.iter"
    DSL.([data; [data; data] @-> monad data; seq data] --> monad data)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Stdlib.Seq.iter
          (fun x ->
            let y = f !acc x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let acc = ref init in
      let () =
        Support.Lib.Seq.iter
          (fun x ->
            let y = f !acc x in
            acc := y ;
            ())
          xs
      in
      !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.Seq.iter_s
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.Seq.iter_p
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.Seq.iter_e
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.Seq.iter_es
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)
    (fun init f xs ->
      let open Support.Lib.Monad.Lwt_result_syntax in
      let acc = ref init in
      let* () =
        Support.Lib.Seq.iter_ep
          (fun x ->
            let* y = f !acc x in
            acc := y ;
            return ())
          xs
      in
      return !acc)

let all_seq : unit Alcotest.test_case list = [fold_left; iter]

let () = Alcotest.run "FuzzRef" [("Seq", all_seq)]
