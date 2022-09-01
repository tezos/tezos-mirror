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
      (fun account -> Client.import_signer_key client account uri)
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
    ~tags:["node"; "baker"; "signer"; "tz1"]
  @@ fun protocol ->
  let* _ =
    signer_test protocol ~keys:(Account.Bootstrap.keys |> Array.to_list)
  in
  unit

let signer_bls_test =
  Protocol.register_test
    ~__FILE__
    ~title:"BLS signer test"
    ~tags:["node"; "baker"; "signer"; "bls"]
  @@ fun protocol ->
  let* client0 = Client.init_mockup ~protocol () in
  Log.info "Generate BLS keys for client" ;
  let* keys =
    Lwt_list.map_s
      (fun i ->
        Client.gen_and_show_keys
          ~alias:(sf "bootstrap_bls_%d" i)
          ~sig_alg:"bls"
          client0)
      (Base.range 1 5)
  in
  let* client = signer_test protocol ~keys in
  Log.info "Checking that block was signed with BLS" ;
  let* block = RPC.Client.call client @@ RPC.get_chain_block () in
  let block_sig = JSON.(block |-> "header" |-> "signature" |> as_string) in
  let block_sig_prefix = String.sub block_sig 0 5 in
  Check.((block_sig_prefix = "BLsig") string)
    ~error_msg:"Signature starts with %L but should start with %R" ;
  let baker = JSON.(block |-> "metadata" |-> "baker" |> as_string) in
  let baker_prefix = String.sub baker 0 3 in
  Check.((baker_prefix = "tz4") string)
    ~error_msg:"Baker starts with %L but should start with %R" ;
  unit

let register ~protocols =
  signer_simple_test protocols ;
  signer_bls_test [Alpha]
