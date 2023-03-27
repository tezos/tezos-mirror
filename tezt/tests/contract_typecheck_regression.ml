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

let test_typecheck_contract protocol scripts =
  List.iter
    (fun script ->
      Protocol.register_regression_test
        ~__FILE__
        ~title:(sf "Tc %s" (Michelson_script.name_s script))
        ~tags:["client"; "michelson"; "typechecking"]
        (fun _protocol ->
          (* Register constants for scripts that require it *)
          let constants =
            match Michelson_script.name script with
            | ["mini_scenarios"; "999_constant"] -> ["999"]
            | ["mini_scenarios"; "constant_unit"] -> ["Unit"; "unit"]
            | ["mini_scenarios"; "constant_entrypoints"] -> ["unit"]
            | _ -> []
          in
          let* client =
            match constants with
            | [] -> return (Client.create_with_mode Mockup)
            | constants ->
                let* client = Client.init_mockup ~protocol () in
                let* (_exprs : string list) =
                  Lwt_list.map_s
                    (fun value ->
                      Client.register_global_constant
                        ~src:Constant.bootstrap1.alias
                        ~value
                        ~burn_cap:Tez.one
                        client)
                    constants
                in
                return client
          in
          Client.typecheck_script
            ~scripts:[Michelson_script.path script]
            ~protocol_hash:(Protocol.hash protocol)
            ~hooks
            ~no_base_dir_warnings:true
            ~details:true
            client)
        [protocol])
    scripts

let test_typecheck protocols =
  List.iter
    (fun protocol ->
      (* Type check regression tests for all well-typed contracts *)
      test_typecheck_contract
        protocol
        (Michelson_script.find_all_well_typed protocol))
    protocols

let register ~protocols = test_typecheck protocols
