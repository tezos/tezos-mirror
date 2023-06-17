(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs. <contact@nomadic-labs.com>               *)
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

exception Benchmark_not_found of Namespace.t

exception Model_not_found of Namespace.t

exception Local_model_not_found of String.t

exception Parameter_not_found of Free_variable.t

(*---------------------------------------------------------------------------*)
(* Type definitions *)

type benchmark_info = Benchmark.t

type model_info = {model : Model.packed_model; from : local_model_info list}

and local_model_info = {bench_name : Namespace.t; local_model_name : string}

type parameter_info = Namespace.t list

type local_model_benchmark_names = Namespace.Set.t
(*---------------------------------------------------------------------------*)
(* Table initialization *)

let bench_table : benchmark_info Name_table.t = Name_table.create 51

(* An abstract model name maps to a model, and a list of (bench * aggregated model)
   names that refer to it *)
let model_table : model_info Name_table.t = Name_table.create 51

(* An abstract local model name maps to a set of benchmark names that refer to
   it *)
let local_model_table : local_model_benchmark_names String.Hashtbl.t =
  String.Hashtbl.create 51

(* A parameter name maps to the list of abstract models that contain it *)
let parameter_table : parameter_info Name_table.t = Name_table.create 51

let clic_table : unit Tezos_clic.command list ref = ref []

(*---------------------------------------------------------------------------*)
(* Registration functions *)

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

let register_model (type a) bench_name local_model_name (model : a Model.t) :
    unit =
  let register_local_model bench_name local_model_name : unit =
    match String.Hashtbl.find_opt local_model_table local_model_name with
    | None ->
        String.Hashtbl.add
          local_model_table
          local_model_name
          (Namespace.Set.singleton bench_name)
    | Some bench_names ->
        String.Hashtbl.replace
          local_model_table
          local_model_name
          (Namespace.Set.add bench_name bench_names)
  in
  (* We assume that models with the same name are the same model *)
  let register_packed_model = function
    | Model.Model m as model -> (
        let module M = (val m) in
        let name = M.name in
        match Name_table.find_opt model_table name with
        | None ->
            register_param_from_model model ;
            register_local_model bench_name local_model_name ;
            Name_table.add
              model_table
              name
              {model; from = [{bench_name; local_model_name}]}
        | Some {model = Model m'; from} ->
            (* Check equality of models by their free variables *)
            if
              not
                (Free_variable.Set.equal
                   (Model.get_free_variable_set m)
                   (Model.get_free_variable_set m'))
            then
              Format.eprintf
                "Warning: Registered different model with same name %a@."
                Namespace.pp
                name ;
            Name_table.replace
              model_table
              name
              {model; from = {bench_name; local_model_name} :: from})
  in
  (* We don't register aggregated models, only their sub-models *)
  match model with
  | Aggregate {sub_models; _} -> List.iter register_packed_model sub_models
  | Abstract {model; _} -> register_packed_model (Model.Model model)

let register_model_for_code_generation local_model_name model =
  (* Expected there is no benchmark has this model.
     So it gives the benchmark name as "dummy" *)
  register_model (Namespace.of_string "dummy") local_model_name model

let register ?(add_timer = true) ((module Bench) : Benchmark.t) =
  if Name_table.mem bench_table Bench.name then (
    Format.eprintf
      "Benchmark %a already registered! exiting@."
      Namespace.pp
      Bench.name ;
    exit 1)
  else () ;

  let ((module Bench) : Benchmark.t) =
    if add_timer then
      (* We do a little benchmark edition. We add the timer latency to all models, which makes
         models aggregated *)
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
      (module Bench)
    else (module Bench)
  in
  let module Bench = struct
    include Bench

    let purpose =
      match purpose with
      | Other_purpose _ -> purpose
      | Generate_code destination ->
          let destination =
            Filename.concat "src/proto_alpha/lib_protocol"
            @@ destination ^ "_costs_generated.ml"
          in
          Generate_code destination
  end in
  List.iter
    (fun (model_local_name, m) -> register_model Bench.name model_local_name m)
    Bench.models ;
  Name_table.add bench_table Bench.name (module Bench)

let add_command cmd = clic_table := cmd :: !clic_table

(*---------------------------------------------------------------------------*)
(* Listing functions *)

let all_benchmarks () : (Namespace.t * benchmark_info) list =
  Name_table.to_seq bench_table
  |> List.of_seq
  |> List.sort (fun (b1, _) (b2, _) -> Namespace.compare b1 b2)

let all_tags () : string list =
  Name_table.to_seq bench_table
  |> Seq.map snd |> List.of_seq
  |> List.map (fun b -> Benchmark.tags b)
  |> List.flatten
  |> List.sort_uniq (fun t1 t2 -> String.compare t1 t2)

let all_models () =
  Name_table.to_seq model_table
  |> List.of_seq
  |> List.sort (fun (s, _) (s', _) -> Namespace.compare s s')

let all_model_names () = all_models () |> List.map fst

let all_parameters () =
  Name_table.to_seq parameter_table
  |> List.of_seq
  |> List.sort (fun (p1, _) (p2, _) -> Namespace.compare p1 p2)
  |> List.map (fun (a, b) -> (Free_variable.of_namespace a, b))

let all_local_model_names () =
  String.Hashtbl.to_seq_keys local_model_table
  |> List.of_seq
  |> List.filter (fun s -> not (String.equal s "*"))
  |> List.sort_uniq String.compare

let all_custom_commands () = !clic_table

(* -------------------------------------------------------------------------- *)
(* Search functions *)

let find_benchmarks_with_tags ~mode tag_list =
  let filter =
    match mode with
    | `All ->
        fun (_, b) ->
          List.for_all
            (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
            tag_list
    | `Exact ->
        fun (_, b) ->
          let benchmark_tags = List.sort String.compare (Benchmark.tags b) in
          List.equal
            String.equal
            (List.sort String.compare tag_list)
            benchmark_tags
    | `Any ->
        fun (_, b) ->
          List.exists
            (fun tag -> List.mem ~equal:String.equal tag (Benchmark.tags b))
            tag_list
  in
  Name_table.to_seq bench_table
  |> List.of_seq |> List.filter filter
  |> List.sort (fun (b1, _) (b2, _) -> Namespace.compare b1 b2)

let find_in_namespace table pattern =
  Name_table.fold
    (fun name e acc ->
      if Namespace.name_match pattern name then (name, e) :: acc else acc)
    table
    []

let find_benchmark name = Name_table.find bench_table name

let find_benchmark_exn name =
  match find_benchmark name with
  | None ->
      Format.eprintf "No benchmark named %a found.@." Namespace.pp name ;
      raise (Benchmark_not_found name)
  | Some b -> b

let find_benchmarks_in_namespace = find_in_namespace bench_table

let find_model name = Name_table.find model_table name

let find_model_exn name =
  match find_model name with
  | None ->
      Format.eprintf "No model named %a found.@." Namespace.pp name ;
      raise (Model_not_found name)
  | Some m -> m

let find_local_model name =
  String.Hashtbl.find local_model_table name
  |> Option.map (fun benches -> Namespace.Set.to_seq benches |> List.of_seq)

let find_local_model_exn name =
  match find_local_model name with
  | None ->
      Format.eprintf
        "No local model named %a found.@."
        Format.pp_print_string
        name ;
      raise (Local_model_not_found name)
  | Some m -> m

let find_models_in_namespace = find_in_namespace model_table

let find_parameter name =
  let name = Free_variable.to_namespace name in
  Name_table.find parameter_table name

let find_parameter_exn name =
  match find_parameter name with
  | None ->
      Format.eprintf "No parameter %a found.@." Free_variable.pp name ;
      raise (Parameter_not_found name)
  | Some m -> m

let find_parameters_in_namespace ns =
  find_in_namespace parameter_table ns
  |> List.map (fun (x, y) -> (Free_variable.of_namespace x, y))
