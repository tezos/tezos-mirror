(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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
   Components: Client > Multiple transfers
   Invocation: dune exec tezt/tests/main.exe -- --file multiple_transfers.ml
   Subject: Tests the multiple tranfers function of the client
*)

(* Originate and return the alias of a manager script *)
let manager client ~protocol =
  let manager = Constant.bootstrap2.public_key_hash in
  let* alias, _address =
    Client.originate_contract_at
      ~init:(sf {|"%s"|} manager)
      ~amount:(Tez.of_int 1000)
      ~src:manager
      ~burn_cap:Tez.one
      client
      ["entrypoints"; "manager"]
      protocol
  in
  return alias

(** Originate and return the alias of a big_map_entrypoints contract *)
let big_map_entrypoints client ~protocol =
  let* alias, _address =
    Client.originate_contract_at
      ~init:(sf {|Pair {} {Elt "Hello" 42}|})
      ~amount:(Tez.of_int 1000)
      ~src:Constant.bootstrap1.alias
      ~burn_cap:Tez.one
      client
      ["entrypoints"; "big_map_entrypoints"]
      protocol
  in
  return alias

let test_empty =
  Protocol.register_test
    ~__FILE__
    ~title:"Test Empty"
    ~tags:["client"; "michelson"; "multiple"; "transfers"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let giver = Constant.bootstrap1.alias in
  let json_batch = "[]" in
  let*? transfer_proc = Client.multiple_transfers ~giver ~json_batch client in
  Process.check_error transfer_proc ~msg:(rex "Empty operation list")

(* Test the multiple transfers command with a single transfer to a contract's
   entrypoint, with implicit accounts or a manager contract as source as per
   parametrization. *)
let test_transfer_json_to_entrypoint_with_args =
  Protocol.register_test
    ~__FILE__
    ~title:"Test transfer JSON to entrypoint with args"
    ~tags:["client"; "michelson"; "multiple"; "transfers"; "args"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* big_map_entrypoints = big_map_entrypoints client ~protocol in
  let* manager = manager client ~protocol in
  let test (payer, source) =
    let* balance_source = Client.get_balance_for ~account:source client in
    let* balance_payer = Client.get_balance_for ~account:payer client in
    let fee = Tez.one in
    let json_batch =
      `A
        [
          `O
            [
              ("destination", `String big_map_entrypoints);
              ("amount", `String "0");
              ("fee", `String (Tez.to_string fee));
              ("gas-limit", `String "65942");
              ("storage-limit", `String "1024");
              ("arg", `String {|"Hello"|});
              ("entrypoint", `String "mem_right");
            ];
        ]
      |> JSON.encode_u
    in
    let*! () = Client.multiple_transfers ~giver:source ~json_batch client in
    let* new_balance_source = Client.get_balance_for ~account:source client in
    let* new_balance_payer = Client.get_balance_for ~account:payer client in
    (if payer <> source then
     Check.(
       (balance_source = new_balance_source)
         Tez.typ
         ~__LOC__
         ~error_msg:
           "Expected source's balance to be unchanged from %R, but got %L")) ;
    Check.(
      (Tez.(balance_payer - fee) = new_balance_payer)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected payer's balance to be %R, but got %L") ;
    unit
  in
  Lwt_list.iter_s test [("bootstrap2", manager); ("bootstrap4", "bootstrap4")]

(* Test a multiple transfers to implicit accounts, with implicit
   accounts or a manager contract as source as per parametrization. *)
let test_multiple_transfer =
  Protocol.register_test
    ~__FILE__
    ~title:"Test multiple transfers"
    ~tags:["client"; "michelson"; "multiple"; "transfers"]
    ~uses_node:false
  @@ fun protocol ->
  let* client = Client.init_mockup ~protocol () in
  let* manager = manager client ~protocol in
  let test (payer, source) =
    let* balance_source = Client.get_balance_for ~account:source client in
    let* balance_bootstrap1 =
      Client.get_balance_for ~account:Constant.bootstrap1.alias client
    in
    let* balance_bootstrap3 =
      Client.get_balance_for ~account:Constant.bootstrap3.alias client
    in
    let amount1 = Tez.of_mutez_int 10_100_000 in
    let amount3 = Tez.of_mutez_int 11_010_000 in
    let json_batch =
      `A
        [
          `O
            [
              ("destination", `String Constant.bootstrap1.alias);
              ("amount", `String (Tez.to_string amount1));
            ];
          `O
            [
              ("destination", `String Constant.bootstrap3.alias);
              ("amount", `String (Tez.to_string amount3));
            ];
        ]
      |> JSON.encode_u
    in
    let*! () = Client.multiple_transfers ~giver:source ~json_batch client in
    let* new_balance_source = Client.get_balance_for ~account:source client in
    let* new_balance_bootstrap1 =
      Client.get_balance_for ~account:Constant.bootstrap1.alias client
    in
    let* new_balance_bootstrap3 =
      Client.get_balance_for ~account:Constant.bootstrap3.alias client
    in
    let source_fee =
      if payer = source then Tez.of_mutez_int 0_000_434 else Tez.zero
    in
    Check.(
      (Tez.(balance_source - amount1 - amount3 - source_fee)
      = new_balance_source)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected source's balance to be %R, but got %L" ;
      (Tez.(balance_bootstrap1 + amount1) = new_balance_bootstrap1)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected bootstrap1's balance to be %R, but got %L" ;
      (Tez.(balance_bootstrap3 + amount3) = new_balance_bootstrap3)
        Tez.typ
        ~__LOC__
        ~error_msg:"Expected bootstrap3's balance to be %R, but got %L") ;
    unit
  in
  Lwt_list.iter_s test [("bootstrap2", manager); ("bootstrap4", "bootstrap4")]

let register ~protocols =
  test_empty protocols ;
  test_transfer_json_to_entrypoint_with_args protocols ;
  test_multiple_transfer protocols
