(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
    Invocation:   dune exec src/proto_017_PtNairob/lib_protocol/test/pbt/main.exe \
                  -- --file test_sc_rollup_tick_repr.ml
    Subject:      Operations in Tick_repr
*)

open Protocol.Alpha_context.Sc_rollup
open QCheck2

(** A generator for ticks *)
let tick =
  let open Gen in
  let+ n = nat in
  Option.value ~default:Tick.initial (Tick.of_int n)

(** For all x, x = initial \/ x > initial. *)
let test_initial_is_bottom =
  Test.make ~name:"x = initial \\/ x > initial" tick @@ fun x ->
  Tick.(x = initial || x > initial)

(** For all x, next x > x. *)
let test_next_is_monotonic =
  Test.make ~name:"next x > x" tick @@ fun x -> Tick.(next x > x)

(** Distance from self to self is zero *)
let test_distance_from_self =
  Test.make ~name:"distance from x to x is 0" tick (fun x ->
      Z.(equal (Tick.distance x x) zero))

(** Distance from non-self is non-zero. *)
let test_distance_from_non_self =
  Test.make
    ~name:"distance from non-self is non-zero"
    (Gen.pair tick tick)
    (fun (x, y) ->
      let dist = Tick.distance x y in
      if x = y then Compare.Z.(dist = Z.zero) else Compare.Z.(dist <> Z.zero))

(** Distance is symmetric . *)
let test_distance_symmetry =
  Test.make
    ~name:"distance is a distance (symmetry)"
    (Gen.pair tick tick)
    (fun (x, y) -> Z.(equal (Tick.distance x y) (Tick.distance y x)))

(** Distance satisfies triangular inequality. *)
let test_distance_triangle_inequality =
  Test.make
    ~name:"distance is a distance (triangle inequality)"
    (Gen.triple tick tick tick)
    (fun (x, y, z) ->
      Tick.(Z.(geq (distance x y + distance y z) (distance x z))))

(** Test that [of_int x = Some t] iff [x >= 0] *)
let test_of_int =
  Test.make ~name:"of_int only accepts natural numbers" Gen.int (fun x ->
      match Tick.of_int x with None -> x < 0 | Some _ -> x >= 0)

(** Test [of_int o to_int = identity]. *)
let test_of_int_to_int =
  Test.make ~name:"to_int o of_int = identity" tick @@ fun x ->
  Tick.(
    match to_int x with
    | None -> (* by the tick generator definition. *) assert false
    | Some i -> ( match of_int i with Some y -> y = x | None -> false))

let tests =
  [
    test_next_is_monotonic;
    test_initial_is_bottom;
    test_distance_from_self;
    test_distance_from_non_self;
    test_distance_symmetry;
    test_distance_triangle_inequality;
    test_of_int;
    test_of_int_to_int;
  ]

let () =
  Alcotest.run
    ~__FILE__
    Protocol.name
    [("Tick_repr", Qcheck2_helpers.qcheck_wrap tests)]
