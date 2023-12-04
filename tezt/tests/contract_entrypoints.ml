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
   Invocation:   dune exec tezt/tests/main.exe -- --file contract_entrypoints.ml
   Subject:      Tests for the contract entrypoints
*)

let extract_new_contract client_output =
  match client_output =~* rex "New contract ?(KT1\\w{33})" with
  | None ->
      Test.fail
        "Cannot extract new contract from client_output: %s"
        client_output
  | Some c -> return c

(* Originates a contract that when called, creates a contract with a
   rootname annotation. Such annotations comes in two flavors, thus the
   parameterization. Then calls the first contract and verifies the
   existence and type of the root entrypoint of the create contract. *)
let test_create_contract_rootname_originate ~contract protocol =
  let* client = Client.init_mockup ~protocol () in
  let* _alias, contract_rootname =
    Client.originate_contract_at
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap1.alias
      ~init:"None"
      ~burn_cap:Tez.one
      client
      ["opcodes"; contract]
      protocol
  in
  let process =
    Client.spawn_transfer
      ~burn_cap:(Tez.of_int 10)
      ~amount:Tez.zero
      ~giver:"bootstrap1"
      ~receiver:contract_rootname
      ~arg:"Unit"
      client
  in
  let* client_output = Process.check_and_read_stdout process in
  let* kt_1 = extract_new_contract client_output in
  let* entrypoint_type =
    Client.contract_entrypoint_type ~entrypoint:"root" ~contract:kt_1 client
  in
  let expected = "Entrypoint root: unit" in
  let () =
    Check.(
      (String.trim entrypoint_type = String.trim expected)
        string
        ~__LOC__
        ~error_msg:
          ("the entrypoint 'root' of the originated contract should exist"
         ^ " with type unit"))
  in
  unit

let register_create_contract_rootname protocols =
  ["create_contract_rootname"; "create_contract_rootname_alt"]
  |> List.iter @@ fun contract ->
     Protocol.register_test
       ~__FILE__
       ~title:(sf "test %s" contract)
       ~tags:["client"; "michelson"]
       ~uses_node:false
       (test_create_contract_rootname_originate ~contract)
       protocols

(* Test CONTRACT with/without entrypoint annotation on literal address
   parameters with/without entrypoint annotation *)
let originate_simple_entrypoints client ~protocol =
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap5"
      ~init:"Unit"
      ~burn_cap:Tez.one
      client
      ["entrypoints"; "simple_entrypoints"]
      protocol
  in
  Lwt.return (client, contract)

let script contract_annotation contract_type =
  sf
    {|
parameter address;
storage (option address);
code { CAR; CONTRACT %s %s; 
       IF_SOME { ADDRESS; SOME } { NONE address; }; NIL operation; PAIR }
|}
    contract_annotation
    contract_type

let test_simple_entrypoints client ~contract_annotation ~contract_type ~param
    ~expected_storage =
  let prg = script contract_annotation contract_type in
  let* {storage; _} =
    Client.run_script client ~prg ~storage:"None" ~input:param
  in
  Check.(
    (storage = expected_storage)
      ~__LOC__
      string
      ~error_msg:"Expected results %R, got %L") ;
  unit

let test_simple_entrypoints_parametrize client ~contract =
  (* tests passing adr to CONTRACT %A unit
     where adr has an entrypoint %A of type unit, is allowed. *)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"unit"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:(sf {|(Some "%s%%A")|} contract)
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%B"
      ~contract_type:"string"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:(sf {|(Some "%s%%B")|} contract)
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%C"
      ~contract_type:"nat"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:(sf {|(Some "%s%%C")|} contract)
  in
  (* tests passing adr%A to CONTRACT %A unit: redundant specification
     of entrypoint not allowed so CONTRACT returns None*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%A"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%B"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%D"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%A"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%B"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%A"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%D"
      ~contract_type:"unit"
      ~param:(sf {|"%s%%A"|} contract)
      ~expected_storage:"None"
  in
  (* tests passing adr%A to CONTRACT unit:
     where adr has an entrypoint %A of type unit, is allowed.*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"unit"
      ~param:(sf {|"%s%%A"|} contract)
      ~expected_storage:(sf {|(Some "%s%%A")|} contract)
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"string"
      ~param:(sf {|"%s%%B"|} contract)
      ~expected_storage:(sf {|(Some "%s%%B")|} contract)
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"nat"
      ~param:(sf {|"%s%%C"|} contract)
      ~expected_storage:(sf {|(Some "%s%%C")|} contract)
  in
  (* tests passing adr%B to CONTRACT unit:
     as entrypoint %B of simple_entrypoints.tz has type string,
     CONTRACT will return None.*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"unit"
      ~param:(sf {|"%s%%B"|} contract)
      ~expected_storage:"None"
  in
  (* tests passing adr%D to CONTRACT unit:
     as entrypoint %D does not exist in simple_entrypoints.tz,
     CONTRACT will return None.*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"unit"
      ~param:(sf {|"%s%%D"|} contract)
      ~expected_storage:"None"
  in
  (* tests passing adr to CONTRACT unit:
     as adr does not have type unit, CONTRACT returns None.*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:""
      ~contract_type:"unit"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:"None"
  in
  (* entrypoint that does not exist*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%D"
      ~contract_type:"unit"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:"None"
  in
  (* ill-typed entrypoints*)
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%A"
      ~contract_type:"int"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%B"
      ~contract_type:"unit"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:"None"
  in
  let* () =
    test_simple_entrypoints
      client
      ~contract_annotation:"%C"
      ~contract_type:"int"
      ~param:(sf {|"%s"|} contract)
      ~expected_storage:"None"
  in
  unit

let register_simple_entrypoints protocols =
  Protocol.register_test
    ~__FILE__
    ~title:"simple entrypoints"
    ~tags:["client"; "michelson"]
    ~uses_node:false
    (fun protocol ->
      let* client = Client.init_mockup ~protocol () in
      let* client, contract = originate_simple_entrypoints client ~protocol in
      test_simple_entrypoints_parametrize client ~contract)
    protocols

let register ~protocols =
  register_create_contract_rootname protocols ;
  register_simple_entrypoints protocols
