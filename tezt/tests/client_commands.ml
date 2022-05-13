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

(* Testing
   -------
   Component:    Client commands
   Invocation:   dune exec tezt/tests/main.exe -- --file client_commands.ml
   Subject:      Tests for the Tezos client
*)

module Helpers = struct
  let originate_fail_on_false client =
    let* contract =
      Client.originate_contract
        ~wait:"none"
        ~init:"Unit"
        ~alias:"deserialization_gas"
        ~amount:Tez.zero
        ~burn_cap:Tez.one
        ~src:Constant.bootstrap1.alias
        ~prg:
          "parameter bool; storage unit; code { UNPAIR; IF { NIL operation; \
           PAIR } { DROP; PUSH string \"bang\"; FAILWITH } }"
        client
    in
    let* () = Client.bake_for_and_wait client in
    return contract
end

module Simulation = struct
  let transfer ~arg ?simulation ?force k protocol =
    let* _node, client = Client.init_with_protocol `Client ~protocol () in
    let* contract = Helpers.originate_fail_on_false client in
    Client.spawn_transfer
      ~amount:(Tez.of_int 2)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:contract
      ~arg
      ?simulation
      ?force
      client
    |> k

  let successful =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of successful operation"
      ~tags:["client"; "simulation"; "success"]
    @@ transfer ~arg:"True" ~simulation:true Process.check

  let failing =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of failing operation"
      ~tags:["client"; "simulation"; "failing"]
    @@ transfer ~arg:"False" ~simulation:true
    @@ Process.check_error ~exit_code:1 ~msg:(rex "with \"bang\"")

  let failing_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Simulation of failing operation with force"
      ~tags:["client"; "simulation"; "failing"; "force"]
    @@ transfer ~arg:"False" ~simulation:true ~force:true
    @@ fun p ->
    let* stdout = Process.check_and_read_stdout ~expect_failure:false p in
    if stdout =~! rex "This operation FAILED" then
      Test.fail "Did not report operation failure" ;
    unit

  let injection_force =
    Protocol.register_test
      ~__FILE__
      ~title:"Injecting of failing operation with force"
      ~tags:["client"; "injection"; "failing"; "force"]
    @@ transfer ~arg:"False" ~force:true
    @@ Process.check_error
         ~exit_code:1
         ~msg:(rex "--gas-limit option is required")

  let register protocol =
    successful protocol ;
    failing protocol ;
    failing_force protocol ;
    injection_force protocol
end

let register ~protocols = Simulation.register protocols
