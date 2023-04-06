(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs. <contact@nomadic-labs.com>               *)
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
module V = Sparse_vec.String

let ( =:= ) = V.equal

let ( =/= ) x y = not (V.equal x y)

let ( + ) = V.add

let ( - ) x y = V.add x (V.neg y)

let ( |* ) = V.smul

let vec0 = V.of_list [("saucisse", 42.0); ("fatigue", 100.0)]

let vec1 = V.of_list [("saucisse", 43.0); ("fatigue", 100.0)]

let vec2 = V.of_list [("fatigue", 100.0); ("saucisse", 42.0)]

let vec3 = V.of_list [("fatigue", 99.0); ("saucisse", 42.0); ("fatigue", 100.0)]

let vec4 = V.of_list [("fatigue", 100.0); ("saucisse", 42.0); ("fatigue", 99.0)]

let vec5 = V.of_list [("saucisse", 42.0 *. 2.); ("fatigue", 100.0 *. 2.)]

let vec6 = V.of_list [("saucisse", 100.); ("fatigue", 42.0)]

let tests =
  [
    (Test.tztest_assert "refl" `Quick @@ fun () -> vec0 =:= vec0);
    (Test.tztest_assert "neq1" `Quick @@ fun () -> vec0 =/= vec1);
    (Test.tztest_assert "of_list1" `Quick @@ fun () -> vec0 =:= vec2);
    (Test.tztest_assert "of_list2" `Quick @@ fun () -> vec2 =:= vec3);
    (Test.tztest_assert "of_list3" `Quick @@ fun () -> vec2 =/= vec4);
    (Test.tztest_assert "smul" `Quick @@ fun () -> 2. |* vec0 =:= vec5);
    (Test.tztest_assert "add1" `Quick @@ fun () -> vec0 + vec0 =:= vec5);
    (Test.tztest_assert "add2" `Quick @@ fun () -> vec0 + vec0 =/= vec0);
    ( Test.tztest_assert "add3" `Quick @@ fun () ->
      vec3 =:= vec4 + V.of_list [("fatigue", 1.0)] );
    (Test.tztest_assert "sub1" `Quick @@ fun () -> vec5 - vec0 =:= vec0);
    (Test.tztest_assert "sub2" `Quick @@ fun () -> vec0 - vec0 =:= V.zero);
    ( Test.tztest_assert "swap" `Quick @@ fun () ->
      V.swap vec0 "saucisse" "fatigue" =:= vec6 );
    ( Test.tztest_assert "eval1" `Quick @@ fun () ->
      V.eval vec0 "saucisse" = 42.0 );
    ( Test.tztest_assert "eval2" `Quick @@ fun () ->
      V.eval vec0 "fatigue" = 100.0 );
    ( Test.tztest_assert "eval_absent" `Quick @@ fun () ->
      V.eval vec0 "lentilles" = 0.0 );
  ]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-benchmark" [("sparse_vec", tests)]
  |> Lwt_main.run
