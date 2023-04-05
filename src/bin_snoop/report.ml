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

open Costlang
module Hashtbl = Stdlib.Hashtbl

(* Automatic report generation. *)

type context =
  | Add
  | Mul
  | Div
  | Lam_body
  | Arg_app
  | Fun_app
  | If_cond
  | If_branch

let equal_context : context -> context -> bool = Stdlib.( = )

type printed = Format.formatter -> context -> unit

let pp c fmtr printed = Format.fprintf fmtr (printed c)

let unprotect_in_context ctxts f fmtr c =
  if List.mem ~equal:equal_context c ctxts then Format.fprintf fmtr "%a" f ()
  else Format.fprintf fmtr "(%a)" f ()

let to_string (x : printed) = Format.asprintf "%a" x Lam_body

(* Custom pretty-printing implementation *)
module Pp_impl : S with type 'a repr = printed and type size = string = struct
  type 'a repr = printed

  type size = string

  let false_ fmtr _c = Format.pp_print_bool fmtr false

  let true_ fmtr _c = Format.pp_print_bool fmtr true

  let float x fmtr _c = Format.pp_print_float fmtr x

  let int x fmtr _c = Format.pp_print_int fmtr x

  let ( + ) x y =
    unprotect_in_context [Add; Lam_body] (fun fmtr () ->
        Format.fprintf fmtr "%a +@, %a" x Add y Add)

  let sat_sub x y =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "sat_sub %a %a" x Arg_app y Arg_app)

  let ( * ) x y =
    unprotect_in_context [Mul; Lam_body] (fun fmtr () ->
        Format.fprintf fmtr "%a * %a" x Mul y Mul)

  let ( / ) x y =
    unprotect_in_context [Mul; Lam_body] (fun fmtr () ->
        Format.fprintf fmtr "%a / %a" x Div y Div)

  let max x y =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "max %a %a" x Arg_app y Arg_app)

  let min x y =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "min %a %a" x Arg_app y Arg_app)

  let shift_left x i =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "%a lsl %d" x Arg_app i)

  let shift_right x i =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "%a lsr %d" x Arg_app i)

  let log2 x =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "log2 @[<h>%a@]" x Arg_app)

  let sqrt x =
    unprotect_in_context [Lam_body; Add; Mul; Div] (fun fmtr () ->
        Format.fprintf fmtr "sqrt @[<h>%a@]" x Arg_app)

  let free ~name fmtr _c =
    Format.fprintf fmtr "free(%a)" Free_variable.pp_short name

  let lt x y =
    unprotect_in_context [Lam_body; If_cond] (fun fmtr () ->
        Format.fprintf fmtr "@[<h>%a@] < @[<h>%a@]" x Arg_app y Arg_app)

  let eq x y =
    unprotect_in_context [Lam_body; If_cond] (fun fmtr () ->
        Format.fprintf fmtr "@[<h>%a@] = @[<h>%a@]" x Arg_app y Arg_app)

  let lam ~name f =
    unprotect_in_context [Lam_body] (fun fmtr () ->
        Format.fprintf
          fmtr
          "@[<hov 1>λ%s.@[<v>%a@]@]"
          name
          (f (fun fmtr _ -> Format.pp_print_string fmtr name))
          Lam_body)

  let app f arg =
    unprotect_in_context [Fun_app] (fun fmtr () ->
        Format.fprintf fmtr "%a %a" f Fun_app arg Arg_app)

  let let_ ~name m f fmtr c =
    match c with
    | Lam_body ->
        Format.fprintf
          fmtr
          "@[<v>let %s = @[<h>%a@] in@;@[<h>%a@]@]"
          name
          m
          Lam_body
          (f (fun fmtr _ -> Format.pp_print_string fmtr name))
          Lam_body
    | _ ->
        Format.fprintf
          fmtr
          "(@[<v>let %s = @[<h>%a@] in@;@[<h>%a@]@])"
          name
          m
          Lam_body
          (f (fun fmtr _ -> Format.pp_print_string fmtr name))
          Lam_body

  let if_ cond ift iff =
    unprotect_in_context [Lam_body] (fun fmtr () ->
        Format.fprintf
          fmtr
          "if %a then %a else %a"
          cond
          If_cond
          ift
          If_branch
          iff
          If_branch)
end

module Pp_impl_abstract : S with type 'a repr = printed and type size = string =
struct
  include Pp_impl

  let app f _arg =
    unprotect_in_context [Fun_app] (fun fmtr () ->
        Format.fprintf fmtr "%a" f Fun_app)
end

let ( % ) g f x = g (f x)

let escape_underscore (s : string) =
  Str.global_replace (Str.regexp_string "_") "\\_" s

let splice sep list =
  List.fold_right (fun elt acc -> sep :: elt :: acc) list [sep]

(* let verb x = L.verbatim_inline (L.text x) *)

let normal_text s =
  let open Latex_syntax in
  Text_blob (Normal, s)

let emph_text s =
  let open Latex_syntax in
  Text_blob (Emph, s)

let bold_text s =
  let open Latex_syntax in
  Text_blob (Bold, s)

let maths s =
  let open Latex_syntax in
  Inline_math_blob s

let benchmark_options_table (bench_opts : Measure.options) =
  let seed =
    match bench_opts.seed with
    | None -> normal_text "self-init"
    | Some seed -> normal_text (string_of_int seed)
  in
  let nsamples =
    let s = string_of_int bench_opts.nsamples in
    normal_text s
  in
  let open Latex_syntax in
  let rows =
    [
      Hline;
      Row [[normal_text "seed"]; [seed]];
      Row [[normal_text "nsamples"]; [nsamples]];
      Hline;
    ]
  in
  ([Vbar; L; Vbar; L; Vbar], rows)

let inferred_params_table (solution : Inference.solution) =
  match Inference.solution_to_csv solution with
  | None -> None
  | Some solution_csv -> (
      match solution_csv with
      | [] | [[]] -> assert false
      | column_names :: lines ->
          let spec_data =
            (* we do not actually care about the content of the column_names,
                just matching things one-to-one for equal length. *)
            List.rev_map (fun _ -> Latex_syntax.L) column_names
          in
          let spec = splice Latex_syntax.Vbar spec_data in
          let hdr =
            Latex_syntax.Row (List.map (fun x -> [normal_text x]) column_names)
          in
          let data =
            List.map
              (fun l -> Latex_syntax.Row (List.map (fun x -> [maths x]) l))
              lines
          in
          let rows =
            (Latex_syntax.Hline :: hdr :: data) @ [Latex_syntax.Hline]
          in
          Some (spec, rows))

let overrides_table (overrides : float Free_variable.Map.t) =
  if Free_variable.Map.is_empty overrides then None
  else
    let spec = Latex_syntax.[Vbar; L; Vbar; L; Vbar] in
    let hdr =
      Latex_syntax.(Row [[normal_text "var"]; [normal_text "value (ns)"]])
    in
    let data =
      Free_variable.Map.fold
        (fun var value acc ->
          let var = Format.asprintf "%a" Free_variable.pp_short var in
          Latex_syntax.Row [[maths var]; [maths (string_of_float value)]] :: acc)
        overrides
        []
    in
    let rows = (Latex_syntax.Hline :: hdr :: data) @ [Latex_syntax.Hline] in
    Some (spec, rows)

module Int_set = Set.Make (Int)

let average_qty (qtyies : float list) =
  let open Stats in
  Emp.of_raw_data (Array.of_list qtyies) |> Emp.Float.empirical_mean

let pp_vec =
  Sparse_vec.String.pp
    ~pp_basis:Format.pp_print_string
    ~pp_element:Format.pp_print_float

let workloads_table (type c t) ((module Bench) : (c, t) Benchmark.poly)
    (workload_data : t Measure.workload_data) =
  let open Latex_syntax in
  let table = Hashtbl.create 41 in
  List.iter
    (fun {Measure.workload; measures; _} ->
      let qties = Hashtbl.find_opt table workload |> Option.value ~default:[] in
      Hashtbl.replace table workload (Maths.vector_to_array measures :: qties))
    workload_data ;
  let compute_avg s qtyies =
    let average = Stats.Emp.Float.empirical_mean qtyies in
    Row [[normal_text s]; [normal_text @@ string_of_float average]]
  in
  let list = List.of_seq (Hashtbl.to_seq table) in
  let row_of_table =
    List.map
      (fun (workload, timings) ->
        let timings = Array.concat timings in
        let vec = Bench.workload_to_vector workload in
        let s = Format.asprintf "@[<h> %a@]" pp_vec vec in
        compute_avg s timings)
      list
  in
  let head = Row [[normal_text "workload"]; [normal_text "average"]] in
  let rows = [head] @ row_of_table in
  Some ([Vbar; L; Vbar; L; Vbar], splice Hline rows)

let model_table (type c t) ((module Bench) : (c, t) Benchmark.poly) =
  let open Latex_syntax in
  let rows =
    List.filter_map
      (fun (model_name, model) ->
        match model with
        | Tezos_benchmark.Model.Aggregate _ -> None
        | Tezos_benchmark.Model.Abstract {model; _} ->
            let module M = (val model) in
            let module Model = M.Def (Pp_impl_abstract) in
            let printed = to_string Model.model in
            let printed = Format.asprintf "%s: %s" model_name printed in
            Some (Row [[normal_text printed]]))
      Bench.models
  in
  ([Vbar; L; Vbar], splice Hline rows)

let report ~(measure : Measure.packed_measurement)
    ~(solution : Inference.solution) ~(figs_files : string list)
    ~(overrides_map : float Free_variable.Map.t) ~short : Latex_syntax.section =
  let (Measure.Measurement ((module Bench), measurement)) = measure in
  let {Measure.bench_opts; workload_data; date = _} = measurement in
  (* let pp_step_model = model (module Pp) in *)
  let open Latex_syntax in
  let preamble : section_content =
    let text =
      Format.asprintf "Results for benchmark %a." Namespace.pp Bench.name
    in
    Text [normal_text text; normal_text "Options used:"]
  in
  let overrides_table : section_content =
    match overrides_table overrides_map with
    | None -> Text [normal_text "None."]
    | Some table -> Table table
  in
  let inferred_params : section_content =
    match inferred_params_table solution with
    | None -> Text [normal_text "None. All free parameters already set."]
    | Some table -> Table table
  in
  let benchmark_options : section_content =
    Table (benchmark_options_table bench_opts)
  in
  let figure =
    List.map
      (fun figs_file ->
        Figure
          ( [normal_text (Namespace.basename Bench.name)],
            {filename = Filename.basename figs_file; size = Some (Width_cm 17)}
          ))
      figs_files
  in
  let model_table : section_content = Table (model_table (module Bench)) in
  let short_table =
    [
      preamble;
      benchmark_options;
      Text [normal_text "Model (sample):"];
      model_table;
      Text [normal_text "Inferred parameters:"];
      inferred_params;
    ]
  in
  let sections =
    if short then short_table
    else
      short_table
      @ [
          Text
            [
              normal_text
                "Overrides used in inference (previously solved variables):";
            ];
          overrides_table;
        ]
      @ Option.fold
          ~none:[]
          ~some:(fun contents ->
            [Text [normal_text "Recorded workloads:"]; Table contents])
          (workloads_table (module Bench) workload_data)
  in
  Section (Namespace.basename Bench.name, sections @ figure)

type t = Latex_syntax.t

let create_empty ~name = Latex_syntax.{title = name; sections = []}

let add_section ~(measure : Measure.packed_measurement) ~(model_name : string)
    ~(problem : Inference.problem) ~(solution : Inference.solution)
    ~overrides_map ~short ~display_options document =
  let (Measure.Measurement ((module Bench), _)) = measure in
  let figs_files =
    let plot_target = Display.Save in
    let save_directory = display_options.Display.save_directory in
    (match Unix.stat save_directory with
    | exception Unix.Unix_error _ ->
        Format.eprintf "Folder %s does not exist, creating it.@." save_directory ;
        Unix.mkdir save_directory 0o700
    | {st_kind = S_DIR; _} -> ()
    | _ ->
        Format.eprintf "%s is not a folder, exiting.@." save_directory ;
        exit 1) ;
    Format.eprintf "Saving plot in folder %s@." save_directory ;
    Display.perform_plot
      ~measure
      ~model_name
      ~problem
      ~solution
      ~plot_target
      ~options:display_options
  in
  let section = report ~measure ~solution ~figs_files ~overrides_map ~short in
  let open Latex_syntax in
  {document with sections = document.sections @ [section]}

(* backend-specific functions *)

let to_latex document =
  let document = Latex_syntax.map_string escape_underscore document in
  Format.asprintf "%a" Latex_pp.pp document
