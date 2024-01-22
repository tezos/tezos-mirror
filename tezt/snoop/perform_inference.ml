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

let rec retry ?max_tries closure =
  Lwt.catch closure (fun e ->
      match max_tries with
      | None -> retry closure
      | Some k -> if k <= 0 then raise e else retry ~max_tries:(k - 1) closure)

let main () =
  Log.info "Entering Perform_inference.main" ;
  let snoop = Snoop.create () in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let* () = Files.cleanup inference_root in

  (* Python bindings tend to crash, hence the need to retry and resume inference midway,
     multiple times. When python bindings are removed, this needs to be removed as well
     See https://gitlab.com/tezos/tezos/-/issues/1523
  *)
  retry ~max_tries:5 (fun () ->
      Snoop.infer_parameters
        ~workload_data:Files.(working_dir // benchmark_results_dir)
        ~regression_method:Snoop.(Lasso {positive = true})
        ~dump_csv:Files.(inference_root // solution_csv)
        ~solution:Files.(inference_root // solution_bin)
        ~report:Files.(inference_root // report_tex)
        ~graph:Files.(inference_root // dep_graph)
        snoop)
