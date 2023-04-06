(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Invocation:   dune exec src/proto_016_PtMumbai/lib_protocol/test/pbt/main.exe
    Subject:      Operations in Tez_repr
*)

open Protocol.Alpha_context
open Test_tez

let z_mutez_min = Z.zero

let z_mutez_max = Z.of_int64 Int64.max_int

let tez_to_z (tez : Tez.t) : Z.t = Z.of_int64 (Tez.to_mutez tez)

let z_in_mutez_bounds (z : Z.t) : bool =
  Z.Compare.(z_mutez_min <= z && z <= z_mutez_max)

let compare (c' : Z.t) (c : Tez.t tzresult) : bool =
  match (z_in_mutez_bounds @@ c', c) with
  | true, Ok c ->
      Qcheck2_helpers.qcheck_eq'
        ~pp:Z.pp_print
        ~expected:c'
        ~actual:(tez_to_z c)
        ()
  | true, Error _ ->
      QCheck2.Test.fail_reportf
        "@[<h 0>Results are in Z bounds, but tez operation fails.@]"
  | false, Ok _ ->
      QCheck2.Test.fail_reportf
        "@[<h 0>Results are not in Z bounds, but tez operation did not fail.@]"
  | false, Error _ -> true

(* [prop_binop f f' (a, b)] compares the function [f] in Tez with a model
   function function [f'] in [Z].

   If [f' a' b'] falls outside Tez bounds, it is true if [f a b] has
   failed.  If not, it it is true if [f a b = f' a' b'] where [a']
   (resp. [b']) are [a] (resp. [b']) in [Z]. *)
let prop_binop (f : Tez.t -> Tez.t -> Tez.t tzresult) (f' : Z.t -> Z.t -> Z.t)
    ((a, b) : Tez.t * Tez.t) : bool =
  compare (f' (tez_to_z a) (tez_to_z b)) (f a b)

(* [prop_binop64 f f' (a, b)] is as [prop_binop] but for binary operations
   where the second operand is of type [int64]. *)
let prop_binop64 (f : Tez.t -> int64 -> Tez.t tzresult) (f' : Z.t -> Z.t -> Z.t)
    ((a, b) : Tez.t * int64) : bool =
  compare (f' (tez_to_z a) (Z.of_int64 b)) (f a b)

(** Generator for int64 by conversion from int32 *)
let gen_int64_of32 : int64 QCheck2.Gen.t =
  QCheck2.Gen.(map Int64.of_int32 int32)

(** Generator for int64 mixing small positive integers,
    int64s from int32 and arbitrary int64 with equal frequency *)
let gen_int64_sizes : int64 QCheck2.Gen.t =
  let open QCheck2.Gen in
  oneof [map Int64.of_int (int_range (-10) 10); gen_int64_of32; int64]

(** Generator for positive int64, mixing small positive integers,
    int64s from int32 and arbitrary int64 with equal frequency *)
let gen_ui64_sizes : int64 QCheck2.Gen.t =
  let open QCheck2.Gen in
  map
    (fun i ->
      let v = if i = Int64.min_int then Int64.max_int else Int64.abs i in
      assert (v >= 0L) ;
      v)
    gen_int64_sizes

(** Generator for tez based on [gen_tez_sizes] *)
let gen_tez_sizes =
  let open QCheck2.Gen in
  map Tez.of_mutez_exn gen_ui64_sizes

let test_coherent_mul =
  QCheck2.Test.make
    ~name:"Tez.(*?) is coherent w.r.t. Z.(*)"
    QCheck2.Gen.(pair gen_tez_sizes gen_ui64_sizes)
    (prop_binop64 ( *? ) Z.( * ))

let test_coherent_sub =
  QCheck2.Test.make
    ~name:"Tez.(-?) is coherent w.r.t. Z.(-)"
    QCheck2.Gen.(pair gen_tez_sizes gen_tez_sizes)
    (prop_binop ( -? ) Z.( - ))

let test_coherent_add =
  QCheck2.Test.make
    ~name:"Tez.(+?) is coherent w.r.t. Z.(+)"
    QCheck2.Gen.(pair gen_tez_sizes gen_tez_sizes)
    (prop_binop ( +? ) Z.( + ))

let test_coherent_div =
  QCheck2.Test.make
    ~name:"Tez.(/?) is coherent w.r.t. Z.(/)"
    QCheck2.Gen.(pair gen_tez_sizes gen_ui64_sizes)
    (fun (a, b) ->
      QCheck2.assume (b > 0L) ;
      prop_binop64 ( /? ) Z.( / ) (a, b))

let tests =
  [test_coherent_mul; test_coherent_sub; test_coherent_add; test_coherent_div]

let () =
  Alcotest.run
    ~__FILE__
    "protocol > pbt > tez_repr"
    [(Protocol.name ^ ": Tez_repr", Qcheck2_helpers.qcheck_wrap tests)]
