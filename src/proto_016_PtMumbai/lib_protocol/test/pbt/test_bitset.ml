(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
    Component:    Protocol Library
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/pbt/main.exe \
                  -- --file test_bitset.ml
    Subject:      Bitset structure
*)

open Qcheck2_helpers
open Protocol.Bitset

let gen_ofs = QCheck2.Gen.int_bound (64 * 10)

let value_of res =
  match res with
  | Ok v -> v
  | Error e ->
      Alcotest.failf
        "An unxpected error %a occurred when generating Bitset.t"
        Environment.Error_monad.pp_trace
        e

let gen_storage =
  let open QCheck2.Gen in
  let* int_vector = list @@ int_bound 64 in
  from_list int_vector |> value_of |> return

let test_get_set (c, ofs) =
  List.for_all
    (fun ofs' ->
      let open Result_syntax in
      value_of
      @@ let* c' = add c ofs in
         let* v = mem c ofs' in
         let* v' = mem c' ofs' in
         return (if ofs = ofs' then v' = true else v = v'))
    (0 -- 63)

let test_inter (c1, c2) =
  let c3 = inter c1 c2 in
  List.for_all
    (fun ofs ->
      let open Result_syntax in
      value_of
      @@ let* v1 = mem c1 ofs in
         let* v2 = mem c2 ofs in
         let* v3 = mem c3 ofs in
         return ((v1 && v2) = v3))
    (0 -- 63)

let test_diff (c1, c2) =
  let c3 = diff c1 c2 in
  List.for_all
    (fun ofs ->
      let open Result_syntax in
      value_of
      @@ let* v1 = mem c1 ofs in
         let* v2 = mem c2 ofs in
         let* v3 = mem c3 ofs in
         return ((v1 && not v2) = v3))
    (0 -- 63)

let test_fill =
  let two = Z.of_int 2 in
  fun length ->
    let f1 = fill ~length |> value_of |> Internal_for_tests.to_z in
    let f2 =
      from_list (0 -- (length - 1)) |> value_of |> Internal_for_tests.to_z
    in
    let f3 = Z.(pow two length |> pred) in
    Z.equal f1 f2 && Z.equal f2 f3

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [
      ( "quantity",
        qcheck_wrap
          [
            QCheck2.Test.make
              ~count:10000
              ~name:"get set"
              QCheck2.Gen.(pair gen_storage gen_ofs)
              test_get_set;
            QCheck2.Test.make
              ~count:10000
              ~name:"inter"
              QCheck2.Gen.(pair gen_storage gen_storage)
              test_inter;
            QCheck2.Test.make
              ~count:10000
              ~name:"diff"
              QCheck2.Gen.(pair gen_storage gen_storage)
              test_diff;
            QCheck2.Test.make
              ~count:10000
              ~name:"fill"
              QCheck2.Gen.(small_nat)
              test_fill;
          ] );
    ]
