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
   Component:    Signer
   Invocation:   dune exec tezt/tests/main.exe -- --file signer_test.ml
   Subject:      Run the baker and signer while performing transfers
*)

(* same as `baker_test`, `baker_test.ml` but using the signer *)
let signer_test protocol ~keys =
  (* init the signer and import all the bootstrap_keys *)
  let* signer = Signer.init ~keys () in
  let* parameter_file =
    Protocol.write_parameter_file
      ~bootstrap_accounts:(List.map (fun k -> (k, None)) keys)
      ~base:(Right (protocol, None))
      []
  in
  let* node, client =
    Client.init_with_protocol
      ~keys:[Constant.activator]
      `Client
      ~protocol
      ~timestamp:Now
      ~parameter_file
      ()
  in
  let* _ =
    (* tell the baker to ask the signer for the bootstrap keys *)
    let uri = Signer.uri signer in
    Lwt_list.iter_s
      (fun account ->
        let Account.{alias; public_key_hash; _} = account in
        Client.import_signer_key client ~alias ~public_key_hash uri)
      keys
  in
  let level_2_promise = Node.wait_for_level node 2 in
  let level_3_promise = Node.wait_for_level node 3 in
  let* _baker = Baker.init ~protocol node client in
  let* _ = level_2_promise in
  Log.info "New head arrive level 2" ;
  let* _ = level_3_promise in
  Log.info "New head arrive level 3" ;
  return client

let signer_simple_test =
  Protocol.register_test
    ~__FILE__
    ~title:"signer test"
    ~tags:["node"; "baker"; "tz1"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun protocol ->
  let* _ =
    signer_test protocol ~keys:(Account.Bootstrap.keys |> Array.to_list)
  in
  unit

let signer_bls_test =
  Protocol.register_test
    ~__FILE__
    ~title:"BLS signer test"
    ~tags:["node"; "baker"; "bls"]
    ~uses:(fun _ -> [Constant.octez_signer])
  @@ fun protocol ->
  let* _node, client = Client.init_with_protocol `Client ~protocol () in
  let* signer = Signer.init ~keys:[Constant.tz4_account] () in
  let* () =
    let uri = Signer.uri signer in
    let Account.{alias; public_key_hash; _} = Constant.tz4_account in
    Client.import_signer_key client ~alias ~public_key_hash uri
  in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 10)
      ~giver:Constant.bootstrap1.public_key_hash
      ~receiver:Constant.tz4_account.public_key_hash
      ~burn_cap:(Tez.of_int 1)
      client
  in
  let* () = Client.bake_for_and_wait client in
  let get_balance_tz4 client =
    Client.RPC.call client
    @@ RPC.get_chain_block_context_contract_balance
         ~id:Constant.tz4_account.public_key_hash
         ()
  in
  let* balance_0 = get_balance_tz4 client in
  let* () =
    Client.transfer
      ~amount:(Tez.of_int 5)
      ~giver:Constant.tz4_account.public_key_hash
      ~receiver:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for_and_wait client in
  let* balance_1 = get_balance_tz4 client in
  Check.((Tez.mutez_int64 balance_0 > Tez.mutez_int64 balance_1) int64)
    ~error_msg:"Tz4 sender %s has decreased balance after transfer" ;
  unit

let register ~protocols =
  signer_simple_test protocols ;
  signer_bls_test protocols
