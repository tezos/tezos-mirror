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

module String_table = String.Hashtbl

let bench_table : Benchmark.t String_table.t = String_table.create 51

let clic_table : unit Clic.command list ref = ref []

let codegen_table : Model.for_codegen String_table.t = String_table.create 51

let register ((module Bench) : Benchmark.t) =
  if String_table.mem bench_table Bench.name then (
    Format.eprintf "Benchmark %s already registered! exiting@." Bench.name ;
    exit 1)
  else String_table.add bench_table Bench.name (module Bench)

let register_for_codegen name model =
  if String_table.mem codegen_table name then
    Format.eprintf
      "Model %s already registered for code generation! (overloaded \
       instruction?) Ignoring.@."
      name
  else String_table.add codegen_table name model

let add_command cmd = clic_table := cmd :: !clic_table

let all_benchmarks () : Benchmark.t list =
  String_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.sort (fun b1 b2 ->
         String.compare (Benchmark.name b1) (Benchmark.name b2))

let all_tags () : string list =
  String_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.map (fun b -> Benchmark.tags b)
  |> List.flatten
  |> List.sort_uniq (fun t1 t2 -> String.compare t1 t2)

let all_benchmarks_with_all_of (tags : string list) : Benchmark.t list =
  String_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         List.for_all
           (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
           tags)
  |> List.sort (fun b1 b2 ->
         String.compare (Benchmark.name b1) (Benchmark.name b2))

let rec list_equal l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | x :: t, y :: u -> String.equal x y && list_equal t u
  | _ -> false

let all_benchmarks_with_exactly (tags : string list) : Benchmark.t list =
  let sorted_requested_tags = List.sort String.compare tags in
  String_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         let benchmark_tags = List.sort String.compare (Benchmark.tags b) in
         list_equal sorted_requested_tags benchmark_tags)
  |> List.sort (fun b1 b2 ->
         String.compare (Benchmark.name b1) (Benchmark.name b2))

let all_benchmarks_with_any_of (tags : string list) : Benchmark.t list =
  String_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         List.exists
           (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
           tags)
  |> List.sort (fun b1 b2 ->
         String.compare (Benchmark.name b1) (Benchmark.name b2))

let all_registered_models () =
  String_table.to_seq codegen_table
  |> List.of_seq
  |> List.sort (fun (s, _) (s', _) -> String.compare s s')

let all_model_names () =
  let module String_set = String.Set in
  List.fold_left
    (fun acc (name, _) -> String.Set.add name acc)
    String.Set.empty
    (all_registered_models ())
  |> String.Set.to_seq |> List.of_seq

let all_custom_commands () = !clic_table

let find_benchmark name = String_table.find bench_table name

let find_model name = String_table.find codegen_table name
