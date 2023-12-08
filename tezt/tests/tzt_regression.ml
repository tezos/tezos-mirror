(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Components: Michelson / TZT
   Invocation: dune exec tezt/tests/main.exe -- --file tzt_regression.ml
   Subject: Regression testing of the Octez TZT runner on the reference test suite.
*)

(* Using the lighter hook that only scrubs the clients [--base-dir] *)
let hooks =
  Tezos_regression.hooks_custom
    ~scrubbed_global_options:["--base-dir"; "-d"]
    ~replace_variables:Fun.id
    ()

let test_tzt protocols =
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Run TZT")
    ~tags:["client"; "michelson"; "tzt"]
    ~supports:(Protocol.From_protocol 019)
    (fun protocol ->
      let tests =
        List.map
          Michelson_script.path
          (Michelson_script.find_all_tzt_tests protocol)
      in
      let* client = Client.init_mockup ~protocol () in
      let* _ =
        Client.spawn_run_tzt_unit_tests
          ~tests
          ~protocol_hash:(Protocol.hash protocol)
          ~hooks
          ~no_base_dir_warnings:true
          client
        |> Process.wait
      in
      unit)
    protocols

let register ~protocols = test_tzt protocols
