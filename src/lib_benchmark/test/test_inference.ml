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

(* Test the inference module on a dummy problem: regress the function

   x \mapsto 33. + 0.042 * x^2 *)

(* ------------------------------------------------------------------------- *)
(* Define the problem and generate some data *)

(* Fix an rng_state to make tests deterministic *)
let rng_state = Random.State.make [|219731; 86876; 87612|]

let truth x = 33. +. (0.042 *. x *. x)

type workload = Input of float

module T () = struct
  (* 10_000 points are a bit overkill to fit a quadratic function. *)
  let input : workload array =
    Array.init 10_000 (fun i -> Input (float_of_int i))

  let data : workload Measure.workload_data =
    Array.map
      (fun (Input x as workload) ->
        (* This 'noise' is crappy but you get the idea *)
        let noise = Random.State.float rng_state 2.0 -. 1. in
        Measure.
          {workload; measures = Maths.vector_of_array [|noise +. truth x|]})
      input
    |> Array.to_list

  (* ----------------------------------------------------------------------- *)
  (* Define the model we're going to use to fit *)

  let fv_const = Free_variable.of_string "test/const"

  let fv_quad = Free_variable.of_string "test/quadratic_term"

  let quadratic_affine =
    let open Model in
    let module M = struct
      let name = Namespace.(make root "test") "quadratic_affine"

      type arg_type = int * unit

      module Def (X : Costlang.S) = struct
        open X

        type model_type = size -> size

        let arity = arity_1

        let model =
          lam ~name:"size" @@ fun size ->
          free ~name:fv_const + (free ~name:fv_quad * (size * size))
      end
    end in
    (module M : Model_impl with type arg_type = int * unit)

  let model : workload Model.t =
    Model.make
      ~conv:(fun (Input x) -> (int_of_float x, ()))
      ~model:quadratic_affine

  (* ----------------------------------------------------------------------- *)

  let problem = Inference.make_problem ~data ~model ~overrides:(fun _ -> None)

  let {Inference.mapping; weights = _; scores = _} =
    Inference.solve_problem
      ~is_constant_input:false
      problem
      (Inference.Lasso {alpha = 1.0; positive = false})

  let const =
    List.assoc ~equal:Free_variable.equal fv_const mapping
    |> WithExceptions.Option.get ~loc:__LOC__

  let quadratic_term =
    List.assoc ~equal:Free_variable.equal fv_quad mapping
    |> WithExceptions.Option.get ~loc:__LOC__
end

(* ------------------------------------------------------------------------- *)

let ( =~ ) x y = abs_float (x -. y) < 0.001

let test () =
  Py.initialize () ;
  let module T = T () in
  let () = Format.printf "%f %f@." T.const T.quadratic_term in
  (* the intercept is quite noisy! *)
  30.0 < T.const && T.const < 36.0 && T.quadratic_term =~ 0.042

let tests = [Test.tztest_assert "regression" `Quick @@ test]

let () =
  Alcotest_lwt.run ~__FILE__ "tezos-benchmark" [("inference", tests)]
  |> Lwt_main.run
