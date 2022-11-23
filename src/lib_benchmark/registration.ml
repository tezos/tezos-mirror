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

module Name_table = Namespace.Hashtbl

exception Benchmark_not_found of string

exception Model_not_found of string

exception Parameter_not_found of string

let bench_table : Benchmark.t Name_table.t = Name_table.create 51

let clic_table : unit Tezos_clic.command list ref = ref []

(* An abstract model name maps to a model, and a list of (bench * constructed model)
   names that refer to it *)
let model_table :
    (Model.packed_model * (Namespace.t * string) list) Name_table.t =
  Name_table.create 51

(* Table for the sub-namespaces of benchmarks *)
let namespace_table : unit Name_table.t = Name_table.create 51

(* A parameter name maps to the list of abstract models that contain it *)
let parameter_table : Namespace.t list Name_table.t = Name_table.create 51

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

let register_param_from_model (model : Model.packed_model) =
  match model with
  | Model model ->
      let module M = (val model) in
      let fv_set = Model.get_free_variable_set model in
      Free_variable.Set.iter (register_parameter M.name) fv_set

let register_model (type a) bench_name model_local_name (model : a Model.t) :
    unit =
  (* We assume that models with the same name are the same model *)
  let register_packed_model = function
    | Model.Model m as model -> (
        let module M = (val m) in
        let name = M.name in
        match Name_table.find_opt model_table name with
        | None ->
            register_param_from_model model ;
            Name_table.add
              model_table
              name
              (model, [(bench_name, model_local_name)])
        | Some (m, l) ->
            Name_table.replace
              model_table
              name
              (m, (bench_name, model_local_name) :: l))
  in
  (* We don't register constructed models, only their sub-models *)
  match model with
  | Aggregate {sub_models; _} -> List.iter register_packed_model sub_models
  | Abstract {model; _} -> register_packed_model (Model.Model model)

let register ((module Bench) : Benchmark.t) =
  register_namespace Bench.name ;
  if Name_table.mem bench_table Bench.name then (
    Format.eprintf
      "Benchmark %a already registered! exiting@."
      Namespace.pp
      Bench.name ;
    exit 1)
  else () ;
  (* We do a little benchmark edition. We add the timer latency to all models, which makes all
     models constructed *)
  let module Bench = struct
    include Bench

    let models =
      List.map
        (fun (s, m) ->
          ( s,
            Model.(
              add_model m Builtin_models.timer_model
              |> precompose (fun w -> (w, ()))) ))
        models
  end in
  List.iter
    (fun (model_local_name, m) -> register_model Bench.name model_local_name m)
    Bench.models ;
  Name_table.add bench_table Bench.name (module Bench)

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
  Name_table.to_seq model_table
  |> List.of_seq
  |> List.sort (fun (s, _) (s', _) -> Namespace.compare s s')

let all_model_names () = all_registered_models () |> List.map fst

let all_registered_parameters () =
  Name_table.to_seq parameter_table
  |> List.of_seq
  |> List.sort (fun (p1, _) (p2, _) -> Namespace.compare p1 p2)

let all_local_model_names () =
  all_benchmarks ()
  |> List.map (fun (module B : Benchmark.S) -> List.map fst B.models)
  |> List.flatten
  |> List.filter (fun s -> not (String.equal s "*"))

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

let find_model name = Name_table.find model_table name

let find_model_exn name =
  let n = Namespace.of_string name in
  match find_model n with
  | None ->
      Format.eprintf "No model named %s found.@." name ;
      raise (Model_not_found name)
  | Some m -> m

let find_parameter name = Name_table.find parameter_table name

let find_parameter_exn name =
  let n = Namespace.of_string name in
  match find_parameter n with
  | None ->
      Format.eprintf "No parameter %s found.@." name ;
      raise (Parameter_not_found name)
  | Some m -> m
