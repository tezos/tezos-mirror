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
module Name_table = Namespace.Hashtbl

exception Benchmark_not_found of string

let bench_table : Benchmark.t Name_table.t = Name_table.create 51

let clic_table : unit Tezos_clic.command list ref = ref []

let codegen_table : Model.for_codegen String_table.t = String_table.create 51

(* Table for the sub-namespaces of benchmarks *)
let namespace_table : unit Name_table.t = Name_table.create 51

let parameter_table : string list Name_table.t = Name_table.create 51

let register_namespace (name : Namespace.t) =
  let rec aux ns name_list =
    match name_list with
    | h :: t ->
        Name_table.add namespace_table (ns h) () ;
        aux (Namespace.make ns h) t
    | [] -> ()
  in
  let name_list = Option.value ~default:[] (List.tl (Namespace.to_list name)) in
  aux Namespace.root name_list

let () = Name_table.add namespace_table Namespace.empty ()

let register_parameter model_name (param : Free_variable.t) =
  let ns = Free_variable.to_namespace param in
  match Name_table.find_opt parameter_table ns with
  | None -> Name_table.add parameter_table ns [model_name]
  | Some l -> Name_table.replace parameter_table ns (model_name :: l)

let aux_register_model name (model : 'a Model.t) =
  match model with
  | Preapplied _ -> ()
  | Packaged {model; _} ->
      let module M = (val model) in
      let module T0 = Costlang.Fold_constants (Costlang.Free_variables) in
      let module T1 = Costlang.Beta_normalize (T0) in
      let module R = M.Def (T1) in
      let fv_set = T0.prj @@ T1.prj R.model in
      Free_variable.Set.iter (register_parameter name) fv_set

let register ((module Bench) : Benchmark.t) =
  register_namespace Bench.name ;
  if Name_table.mem bench_table Bench.name then (
    Format.eprintf
      "Benchmark %a already registered! exiting@."
      Namespace.pp
      Bench.name ;
    exit 1)
  else () ;
  List.iter (fun (name, m) -> aux_register_model name m) Bench.models ;
  Name_table.add bench_table Bench.name (module Bench)

let register_for_codegen name model =
  if String_table.mem codegen_table name then
    Format.eprintf
      "Model %s already registered for code generation! (overloaded \
       instruction?) Ignoring.@."
      name
  else String_table.add codegen_table name model

let add_command cmd = clic_table := cmd :: !clic_table

let all_namespaces () : Namespace.t list =
  Name_table.to_seq namespace_table
  |> Seq.map fst |> List.of_seq
  |> List.sort (fun b1 b2 -> Namespace.compare b1 b2)

let all_benchmarks () : Benchmark.t list =
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.sort (fun b1 b2 ->
         Namespace.compare (Benchmark.name b1) (Benchmark.name b2))

let all_tags () : string list =
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.map (fun b -> Benchmark.tags b)
  |> List.flatten
  |> List.sort_uniq (fun t1 t2 -> String.compare t1 t2)

let all_benchmarks_with_all_of (tags : string list) : Benchmark.t list =
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         List.for_all
           (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
           tags)
  |> List.sort (fun b1 b2 ->
         Namespace.compare (Benchmark.name b1) (Benchmark.name b2))

let rec list_equal l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | x :: t, y :: u -> String.equal x y && list_equal t u
  | _ -> false

let all_benchmarks_with_exactly (tags : string list) : Benchmark.t list =
  let sorted_requested_tags = List.sort String.compare tags in
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         let benchmark_tags = List.sort String.compare (Benchmark.tags b) in
         list_equal sorted_requested_tags benchmark_tags)
  |> List.sort (fun b1 b2 ->
         Namespace.compare (Benchmark.name b1) (Benchmark.name b2))

let all_benchmarks_with_any_of (tags : string list) : Benchmark.t list =
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.filter (fun b ->
         List.exists
           (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
           tags)
  |> List.sort (fun b1 b2 ->
         Namespace.compare (Benchmark.name b1) (Benchmark.name b2))

let all_registered_models () =
  String_table.to_seq codegen_table
  |> List.of_seq
  |> List.sort (fun (s, _) (s', _) -> String.compare s s')

let all_model_names () =
  let module String_set = String.Set in
  List.fold_left
    (fun acc (name, _) -> String_set.add name acc)
    String_set.empty
    (all_registered_models ())
  |> String_set.to_seq |> List.of_seq

let all_registered_parameters () =
  Name_table.to_seq parameter_table
  |> List.of_seq
  |> List.sort (fun (p1, _) (p2, _) -> Namespace.compare p1 p2)

let all_custom_commands () = !clic_table

let find_benchmarks_in_namespace pattern =
  let pattern = Namespace.of_string pattern in
  (Name_table.fold (fun name bench acc ->
       if Namespace.name_match pattern name then bench :: acc else acc))
    bench_table
    []

let find_namespace (name : string) =
  let name = Namespace.of_string name in
  Option.map (fun () -> name) (Name_table.find namespace_table name)

let find_benchmark_exn name =
  let n = Namespace.of_string name in
  match Name_table.find bench_table n with
  | None ->
      Format.eprintf "No benchmark named %s found.@." name ;
      raise (Benchmark_not_found name)
  | Some b -> b

let find_model name = String_table.find codegen_table name
