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
   Component:    Michelson
   Invocation:   dune exec tezt/tests/main.exe -- --file tzip4_view.ml
   Subject:      Tests for the "run tzip4 view" command
*)

let init_with_contract ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap1.alias
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["mini_scenarios"; "tzip4_view"]
      protocol
  in
  Lwt.return (client, contract)

let test_run_view_generic ?unlimited_gas ~protocol ~entrypoint ~input ~expected
    () =
  let* client, contract = init_with_contract ~protocol () in
  let* view =
    Client.run_tzip4_view ?unlimited_gas ~entrypoint ~contract ?input client
  in
  Check.(
    (String.trim view = expected)
      string
      ~__LOC__
      ~error_msg:"Expected view result %R, got %L") ;
  unit

let test_run_view_const ~protocol () =
  test_run_view_generic
    ~protocol
    ~entrypoint:"view_const"
    ~input:(Some "Unit")
    ~expected:"5"
    ()

let test_run_view_add ~protocol () =
  test_run_view_generic
    ~protocol
    ~entrypoint:"view_add"
    ~input:(Some "Pair 1 3")
    ~expected:"4"
    ()

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "tzip4_view"]
        ~uses_node:false
        (fun protocol -> test_function ~protocol ())
        protocols)
    [
      ("Run tzip4_view `tzip4_view_const`", test_run_view_const);
      ("Run tzip4_view `tzip4_view_add`", test_run_view_add);
    ]
