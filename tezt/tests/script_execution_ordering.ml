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
   Component:    Script execution ordering
   Invocation:   dune exec tezt/tests/main.exe -- --file script_execution_ordering.ml
   Subject:      Tests for contract execution order.
*)

let contract_path protocol kind contract =
  sf
    "tests_python/contracts_%s/%s/%s"
    (match protocol with
    | Protocol.Alpha -> "alpha"
    | _ -> sf "%03d" @@ Protocol.number protocol)
    kind
    contract

type 'a tree = Leaf of 'a | Node of 'a tree list

(* This test to verifies contract execution order.
   There are 3 contracts:
   - Storer: Appends its argument to storage.
   - Caller: Calls the list of unit contracts in its storage.
   - Appender: Calls the string contract in its storage with a stored argument.

   For each test, there is one unique Storer. Each test is
   parameterized by a tree and the expected final storage of the
   Storer. A leaf in the tree is a string. Inner nodes are lists of
   leafs/inner nodes. The test maps maps over this tree to build a
   tree of contracts. Leaf nodes map to Appender contracts calling
   the Storer. Inner nodes map to Caller contract that calling
   children.

   Example. Given the tree: ["A", ["B"], "C"], we obtain
   Caller([Appender("A"), Caller([Appender("B")]), Appender("C")])
   Before the protocol 009, contract execution order was in BFS
   In BFS, Storer would've ended up with storage ACB.
   In DFS, Storer will end up with storage ABC. *)
let test_execution_ordering =
  Protocol.register_test
    ~__FILE__
    ~title:"Test contract execution order."
    ~tags:["client"; "script"]
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let originate_storer () =
    Client.originate_contract
      ~alias:"storer"
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:(contract_path protocol "mini_scenarios" "execution_order_storer.tz")
      ~init:{|""|}
      ~burn_cap:Tez.one
      ~force:true
      client
  in
  let originate_appender ~storer ~argument =
    Client.originate_contract
      ~alias:(sf "appender-%s" argument)
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:
        (contract_path protocol "mini_scenarios" "execution_order_appender.tz")
      ~init:(sf "Pair %S %S" storer argument)
      ~burn_cap:Tez.one
      ~force:true
      client
  in
  let originate_caller callees =
    let storage = sf "{%s}" (String.concat "; " (List.map (sf "%S") callees)) in
    Client.originate_contract
      ~alias:(sf "caller-%d" (Hashtbl.hash storage))
      ~amount:Tez.zero
      ~src:Constant.bootstrap1.alias
      ~prg:(contract_path protocol "mini_scenarios" "execution_order_caller.tz")
      ~init:storage
      ~burn_cap:Tez.one
      ~force:true
      client
  in
  let* () =
    [
      (* before 009, the result should be "DABCEFG". *)
      ( Node
          [
            Node [Leaf "A"; Leaf "B"; Leaf "C"];
            Leaf "D";
            Node [Leaf "E"; Leaf "F"; Leaf "G"];
          ],
        "ABCDEFG" );
      (* before 009; the result should be "ACB". *)
      (Node [Leaf "A"; Node [Leaf "B"]; Leaf "C"], "ABC");
      (* before 009; the result should be "ABDC". *)
      (Node [Leaf "A"; Node [Leaf "B"; Node [Leaf "C"]; Leaf "D"]], "ABCD");
      (Node [], "");
    ]
    |> Lwt_list.iter_s @@ fun (tree, expected) ->
       let* storer = originate_storer () in
       let rec deploy_tree = function
         | Leaf s -> originate_appender ~storer ~argument:s
         | Node tree ->
             let* children = Lwt_list.map_s deploy_tree tree in
             originate_caller children
       in
       let* root = deploy_tree tree in
       let* () =
         Client.transfer
           ~burn_cap:(Tez.of_int 5)
           ~amount:Tez.zero
           ~giver:Constant.bootstrap2.alias
           ~receiver:root
           client
       in
       let* storer_storage = Client.contract_storage storer client in
       return
       @@ Check.(
            (String.trim storer_storage = sf "%S" expected) ~__LOC__ string)
            ~error_msg:"expected %R, got %L"
  in
  unit

let register ~protocols = test_execution_ordering protocols
