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
  Test.register
    ~title:"snoop codegen"
    ~tags:["codegen"]
    ~uses:[Constant.octez_snoop]
    ~uses_node:false
    ~uses_client:false
    ~uses_admin_client:false
    ~__FILE__
  @@ fun () ->
  let open Lwt.Syntax in
  let snoop = Snoop.create () in
  let outfn = Temp.file "codegen.ml" in
  close_out @@ open_out outfn ;
  let* _ =
    Snoop.generate_code_for_solutions
      ~solution:"tezt/tests/snoop_codegen/lsl_bytes.sol"
      ~fixed_point:"tezt/tests/snoop_codegen/fp.json"
      ~save_to:outfn
      snoop
  in
  let diff = Diff.files "tezt/tests/snoop_codegen/lsl_bytes.ml.expect" outfn in
  if diff.different then (
    Diff.log ~level:Error diff ;
    assert false)
  else Lwt.return_unit

let register_protocol_independent () = generate_code_using_solution_test ()
