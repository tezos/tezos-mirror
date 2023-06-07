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

(* Testing
   -------
   Component:    Snoop
   Invocation:   dune exec tezt/tests/main.exe -- --file snoop_codegen.ml
   Subject:      Tests for snoop codegen.
*)

let generate_code_using_solution_test () =
  Test.register ~title:"snoop codegen" ~tags:["snoop"; "codegen"] ~__FILE__
  @@ fun () ->
  let open Lwt.Syntax in
  let snoop = Snoop.create () in
  let* res =
    Snoop.generate_code_using_solution
      ~solution:"tezt/tests/snoop_codegen/lsl_bytes.sol"
      ~fixed_point:"tezt/tests/snoop_codegen/fp.json"
      snoop
  in
  let outfn = Temp.file "codegen.ml" in
  let oc = open_out outfn in
  output_string oc res ;
  close_out oc ;
  let diff = Diff.files outfn "tezt/tests/snoop_codegen/lsl_bytes.ml.expect" in
  if diff.different then (
    Diff.log ~level:Error diff ;
    assert false)
  else Lwt.return_unit

let register_protocol_independent () = generate_code_using_solution_test ()

let perform_check_definitions snoop proto =
  let files =
    List.map
      (fun fn ->
        project_root
        // Printf.sprintf "src/%s/lib_protocol/%s" (Protocol.directory proto) fn)
      ["michelson_v1_gas_costs.ml"; "michelson_v1_gas_costs_generated.ml"]
  in
  Snoop.check_definitions ~files snoop

let check_definitions_test =
  Protocol.register_test
    ~__FILE__
    ~title:"check of the cost function definitions"
    ~tags:["snoop"; "codegen"]
  @@ fun protocol ->
  Log.info "Checking the cost function definitions" ;
  let snoop = Snoop.create () in
  perform_check_definitions snoop protocol

let register ~protocols = check_definitions_test protocols
