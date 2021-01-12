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
    [ inference_root // solution_csv model_name;
      inference_root // solution_bin model_name;
      inference_root // report_tex model_name;
      inference_root // dep_graph model_name ]
  in
  List.iter Files.unlink_if_present files

let main () =
  Log.info "Entering Perform_inference.main" ;
  let snoop = Snoop.create () in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let* () =
    let model_name = "interpreter" in
    cleanup model_name ;
    Snoop.infer_parameters
      ~model_name
      ~workload_data:Files.(working_dir // benchmark_results_dir)
      ~regression_method:Snoop.(Lasso {positive = true})
      ~dump_csv:Files.(inference_root // solution_csv model_name)
      ~solution:Files.(inference_root // solution_bin model_name)
      ~report:Files.(inference_root // report_tex model_name)
      ~graph:Files.(inference_root // dep_graph model_name)
      snoop
  in
  (* TODO: translator, encoding *)
  return ()
