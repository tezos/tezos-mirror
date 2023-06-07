(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(*
  First step: defining the benchmark. This is done in
  the following library.
*)

open Tezos_benchmark_examples

(*
  Second step: performing measurements. Above, we kept the type of workloads
  abstract so we need to work under an existential quantifier. We proceed
  this way to mimick what `tezos-snoop`
*)

let bench_opts =
  let open Measure in
  {
    seed = Some 1337;
    nsamples = 3000;
    bench_number = 100;
    minor_heap_size = `words (256 * 1024);
    config_file = None;
  }

(* Perform timing measurements, existentially pack it *)
let measurement =
  match Benchmark.ex_unpack (module Blake2b.Blake2b_bench) with
  | Ex bench ->
      let workload_data = Measure.perform_benchmark bench_opts bench in
      let measurement =
        {Measure.bench_opts; workload_data; date = Unix.gmtime (Unix.time ())}
      in
      Measure.Measurement (bench, measurement)

(*
  Third step: performing inference.
*)
let solution =
  match measurement with
  | Measure.Measurement ((module Bench), {workload_data; _}) ->
      let model =
        List.assoc ~equal:String.equal "blake2b" Bench.models
        |> WithExceptions.Option.get ~loc:__LOC__
      in
      let problem =
        Inference.make_problem ~data:workload_data ~model ~overrides:(fun _ ->
            None)
      in
      let solver = Inference.Lasso {alpha = 1.0; positive = true} in
      (* Initialize Python to have access to Scikit's Lasso solver *)
      Pyinit.pyinit () ;
      Inference.solve_problem ~is_constant_input:false problem solver

(*
  Fourth and last step: exploiting results.
*)

(* Code generation *)
let () =
  let model = Blake2b.model_blake2b in
  let solution =
    Codegen.
      {
        inference_model_name = "blake2b";
        map = Free_variable.Map.of_seq (List.to_seq solution.mapping);
        scores_list = [];
      }
  in
  (match
     Codegen.codegen
       (Model.Model model)
       solution
       (module Costlang.Identity)
       (Namespace.of_string "blake2b_model")
   with
  | None -> assert false
  | Some code -> Format.printf "%a@." Codegen.pp_code code) ;
  let module FPT = Fixed_point_transform.Apply (struct
    let options = {Fixed_point_transform.default_options with precision = 5}
  end) in
  match
    Codegen.codegen
      (Model.Model model)
      solution
      (module FPT)
      (Namespace.of_string "blake2b_model_fixed_point")
  with
  | None -> assert false
  | Some code -> Format.printf "%a@." Codegen.pp_code code
