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

let cleanup model_name =
  let open Files in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let files =
    [
      inference_root // solution_csv model_name;
      inference_root // solution_bin model_name;
      inference_root // report_tex model_name;
      inference_root // dep_graph model_name;
    ]
  in
  List.iter Files.unlink_if_present files

let rec retry ?max_tries closure =
  Lwt.catch closure (fun e ->
      match max_tries with
      | None -> retry closure
      | Some k -> if k <= 0 then raise e else retry ~max_tries:(k - 1) closure)

let main () =
  Log.info "Entering Perform_inference.main" ;
  let snoop = Snoop.create () in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let models =
    [
      "interpreter";
      "gas_translator_model";
      "size_translator_model";
      "micheline";
      "micheline_bytes";
      "encoding";
      "Set_add";
      "Set_elements";
      "Script_expr_hash_of_b58check_opt";
      "Global_constants_storage_expr_to_address_in_context";
      "Global_constants_storage_expand_constant_branch";
      "Global_constants_storage_expand_no_constant_branch";
      "cache/CACHE_UPDATE/model";
      "ir_size_model";
      "carbonated_map";
      "size_collect_tickets_step_model";
      "size_has_tickets_model";
      "compare_tickets";
      "list_key_values";
      "skip_list/next/model";
      "skip_list/hash_cell/model";
      "sc_rollup/Sc_rollup_deserialize_output_proof_benchmark/model";
      "sc_rollup/Sc_rollup_verify_output_proof_benchmark/model";
    ]
  in
  Lwt_list.iter_s
    (fun model_name ->
      let saved_model_name =
        String.split_on_char '/' model_name |> String.concat "__"
      in
      cleanup saved_model_name ;
      (* Python bindings tend to crash, hence the need to retry and resume inference midway,
         multiple times. When python bindings are removed, this needs to be removed as well
         See https://gitlab.com/tezos/tezos/-/issues/1523
      *)
      retry ~max_tries:5 (fun () ->
          Snoop.infer_parameters
            ~model_name
            ~workload_data:Files.(working_dir // benchmark_results_dir)
            ~regression_method:Snoop.(Lasso {positive = true})
            ~dump_csv:Files.(inference_root // solution_csv saved_model_name)
            ~solution:Files.(inference_root // solution_bin saved_model_name)
            ~report:Files.(inference_root // report_tex saved_model_name)
            ~graph:Files.(inference_root // dep_graph saved_model_name)
            snoop))
    models
