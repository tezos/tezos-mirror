(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Component:    Client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file timelock_disabled.ml
   Subject:      Tests checking contracts with timelock cannot be originated
*)

let test_contract_not_originable ~protocol () =
  let* client = Client.init_mockup ~protocol () in
  let _alias, result =
    Client.spawn_originate_contract_at
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~burn_cap:(Tez.of_int 1)
      ~init:"0xaa"
      client
      ["ill_typed"; "timelock"]
      protocol
  in
  let msg =
    rex
      "Origination of contracts containing time lock related instructions is \
       disabled in the client because of a vulnerability."
  in
  Process.check_error ~exit_code:1 ~msg result

let register ~protocols =
  List.iter
    (fun (title, test_function) ->
      Protocol.register_test
        ~__FILE__
        ~title
        ~tags:["client"; "michelson"; "timelock"]
        ~uses_node:false
        (fun protocol -> test_function ~protocol ())
        protocols)
    [
      ( "Check a contract containing timelock operations is forbidden",
        test_contract_not_originable );
    ]
