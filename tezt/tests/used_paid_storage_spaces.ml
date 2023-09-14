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
   Component: Used and paid storage spaces
   Invocation:
     dune exec tezt/tests/main.exe -- --file used_paid_storage_spaces.ml
   Subject: Tests for requesting the used and paid storage spaces of a smart
            contract.
*)

(* The test will check the values of the used and paid storage spaces, coming
   from the client CLI or RPC, and before/after some events on the blockchain.
   We gather the values in a single structure. *)
type space_values = {
  used_from_client : string;
  used_from_RPC : int;
  paid_from_client : string;
  paid_from_RPC : int;
}

(* Gather used and paid storage spaces for a [contract] and a [client], both as
   a client CLI and an RPC. *)
let get_space_values contract client =
  let call_rpc rpc = Client.RPC.call client @@ rpc contract in
  let* used_from_client = Client.used_storage_space ~contract client in
  let* used_from_RPC =
    call_rpc RPC.get_chain_block_context_contract_storage_used_space
  in
  let* paid_from_client = Client.paid_storage_space ~contract client in
  let* paid_from_RPC =
    call_rpc RPC.get_chain_block_context_contract_storage_paid_space
  in
  Lwt.return {used_from_client; used_from_RPC; paid_from_client; paid_from_RPC}

(* We'll read the output of the client CLI and RPC for used/paid storage spaces.
   One will return an integer as a string, the other one an OCamlinteger.
*)
type space_result = Client of string | RPC of int

(* [check level kind size ~initial_size ~increase] checks that a [kind]
   of storage space has indeed the correct [size], depending on the
   [initial_size] of the storage space and on the [level] at which the size was
   requested relatively to an [increase] of this storage space. *)
let check (level : [`Before | `After]) (kind : [`Used | `Paid])
    (size : space_result) ~initial_size ~increase =
  let increase =
    match (level, kind) with
    | `Before, _ | _, `Used -> 0
    | `After, `Paid -> increase
  in
  let level = match level with `Before -> "initial" | `After -> "new" in
  let kind = match kind with `Used -> "used" | `Paid -> "paid" in
  let source, value =
    match size with
    | Client value -> ("client CLI", int_of_string (String.trim value))
    | RPC value -> ("RPC", value)
  in
  Check.(value = initial_size + increase)
    Check.int
    ~error_msg:
      ("Unexpected " ^ level ^ " " ^ kind ^ " storage space from the " ^ source
     ^ ". Expected %R. Got %L")

(* The test consists in:
   1. originating a smart contract;
   2. storing its used and paid storage spaces from client CLI and RPC;
   3. increase its paid storage space;
   4. storing the new values for the used and paid storage spaces from client
      CLI and RPC;
   5. checking all the values. *)
let test_increase_paid_storage =
  Protocol.register_test
    ~__FILE__
    ~title:"used and paid storage spaces"
    ~tags:["storage"; "used_storage"; "paid_storage"]
    ~supports:(Protocol.From_protocol 015)
  @@ fun protocol ->
  (* 1. Originate the smart contract. We arbitrarily picked an existing file. *)
  let* _, client = Client.init_with_protocol ~protocol `Client () in
  let* _alias, contract =
    Client.originate_contract_at
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
      ["mini_scenarios"; "str_id"]
      protocol
  in
  let* () = Client.bake_for_and_wait client in

  (* 2. Storing the used and paid storage spaces from client CLI and RPC. *)
  let* before = get_space_values contract client in

  (* 3. Increase the paid storage space. *)
  let increase = 100 in
  let amount = increase in
  let payer = Constant.bootstrap1.alias in
  let* _ = Client.increase_paid_storage ~contract ~amount ~payer client in
  let* () = Client.bake_for_and_wait client in

  (* 4. Storing the new values for the used and paid storage spaces from client
        CLI and RPC. *)
  let* after = get_space_values contract client in

  (* 5. Checking all the values. *)
  (* 62 is the initial storage space after origination, obtained by testing it.
     We don't question this value here, it's the purpose of unit tests. *)
  let check = check ~initial_size:62 ~increase in
  check `Before `Used (Client before.used_from_client) ;
  check `Before `Used (RPC before.used_from_RPC) ;
  check `Before `Paid (Client before.paid_from_client) ;
  check `Before `Paid (RPC before.paid_from_RPC) ;

  check `After `Used (Client after.used_from_client) ;
  check `After `Used (RPC after.used_from_RPC) ;
  check `After `Paid (Client after.paid_from_client) ;
  check `After `Paid (RPC after.paid_from_RPC) ;

  unit

let register ~protocols = test_increase_paid_storage protocols
