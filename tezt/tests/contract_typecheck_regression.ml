(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold <contact@marigold.dev>                        *)
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
   Components: Michelson
   Invocation: dune exec tezt/tests/main.exe -- --file contract_typecheck_regression.ml
   Subject: Regression testing of Michelson typechecking
*)

(* Using the lighter hook that only scrubs the clients [--base-dir] *)
let hooks =
  Tezos_regression.hooks_custom
    ~scrubbed_global_options:["--base-dir"; "-d"]
    ~replace_variables:Fun.id
    ()

let test_typecheck_contract protocol contract =
  Protocol.register_regression_test
    ~__FILE__
    ~title:(sf "Tc %s" contract)
    ~tags:["client"; "michelson"; "typechecking"]
    (fun _protocol ->
      let client = Client.create_with_mode Mockup in
      Client.typecheck_script
        ~script:contract
        ~protocol_hash:(Protocol.hash protocol)
        ~hooks
        ~no_base_dir_warnings:true
        ~details:true
        client)
    [protocol]

let test_typecheck protocols =
  List.iter
    (fun protocol ->
      (* Type check regression tests for all contracts (except "ill_typed" and "legacy") *)
      let pytest_dir =
        sf
          "tests_python/contracts_%s"
          (match protocol with
          | Protocol.Alpha -> "alpha"
          | _ -> sf "%03d" @@ Protocol.number protocol)
      in
      List.iter
        (fun subdir ->
          Array.iter
            (fun script ->
              test_typecheck_contract protocol (pytest_dir // subdir // script))
            (Sys.readdir (pytest_dir // subdir)))
        [
          "attic";
          "entrypoints";
          "opcodes";
          "macros";
          "mini_scenarios";
          "non_regression";
        ])
    protocols

let register ~protocols = test_typecheck protocols
