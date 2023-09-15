(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 DaiLambda, Inc. <contact@dailambda.jp>                 *)
(* Copyright (c) 2023  Marigold <contact@marigold.dev>                       *)
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

let prepare_fp_json inference_root =
  let fn = inference_root // "fp.json" in
  Base.write_file
    fn
    ~contents:
      {|{ "precision": 6,
      "max_relative_error": 0.1,
      "cast_mode": "Round",
      "inverse_scaling": 10,
      "resolution": 5 }|} ;
  fn

let destination = Files.(working_dir // codegen_results_dir)

let main () =
  Log.info "Entering Perform_codegen.main" ;
  let snoop = Snoop.create () in
  let* () = Files.cleanup destination in
  let inference_root = Files.(working_dir // inference_results_dir) in
  let fp_json_fn = prepare_fp_json inference_root in
  let open Lwt.Syntax in
  let solution_fn = inference_root in
  let* _ =
    Snoop.generate_code_for_solutions
      ~solution:solution_fn
      ~split_to:destination
      snoop
  in
  let* _ =
    Snoop.generate_code_for_solutions
      ~solution:solution_fn
      ~split_to:destination
      ~fixed_point:fp_json_fn
      snoop
  in
  Lwt.return_unit
