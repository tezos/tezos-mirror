(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
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

let models = Perform_inference.models

(* ./octez-snoop generate code for solutions inferred_XXX.sol *)

let cost_function_ml fp model_name =
  (if fp then Printf.sprintf "inferred_%s.ml"
  else Printf.sprintf "inferred_%s_no_fp.ml")
    model_name

let cleanup model_name =
  let inference_root = Files.(working_dir // inference_results_dir) in
  let files =
    [
      inference_root // cost_function_ml false model_name;
      inference_root // cost_function_ml true model_name;
    ]
  in
  List.iter Files.unlink_if_present files

let solution_fn inference_root model_name =
  Files.(inference_root // solution_bin model_name)

let prepare_fp_json inference_root =
  let fn = inference_root // "fp.json" in
  Base.write_file
    fn
    ~contents:
      {|{ "precision": 3,
      "max_relative_error": 0.5,
      "cast_mode": "Round",
      "inverse_scaling": 3,
      "resolution": 5 }|} ;
  fn

let main () =
  Log.info "Entering Perform_codegen.main" ;
  let snoop = Snoop.create () in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let fp_json_fn = prepare_fp_json inference_root in
  Lwt_list.iter_s
    (fun model_name ->
      let open Lwt.Syntax in
      let saved_model_name =
        String.split_on_char '/' model_name |> String.concat "__"
      in
      cleanup saved_model_name ;
      let solution_fn = solution_fn inference_root saved_model_name in
      let* no_fp =
        Snoop.generate_code_using_solution ~solution:solution_fn snoop
      in
      Base.write_file
        (inference_root // cost_function_ml false saved_model_name)
        ~contents:no_fp ;
      let+ fp =
        Snoop.generate_code_using_solution
          ~solution:solution_fn
          ~fixed_point:fp_json_fn
          snoop
      in
      Base.write_file
        (inference_root // cost_function_ml true saved_model_name)
        ~contents:fp)
    models
