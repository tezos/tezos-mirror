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
   Component:    Tx_rollup_node
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup_node.ml
   Subject:      Various test scenarios for the Tx rollup node
*)

module Rollup = Rollup.Tx_rollup
module Rollup_node = Rollup_node.Tx_node

let hooks = Tezos_regression.hooks

let get_block_hash block_json =
  JSON.(block_json |-> "hash" |> as_string) |> return

let node_rpc node service =
  let* opt_get = RPC.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
      let url = Format.asprintf "%s/%s" (Rollup_node.endpoint node) service in
      let* result = get ~url in
      return (Some (result |> JSON.parse ~origin:service))

let get_node_inbox ?(block = "head") node =
  let* json = node_rpc node @@ "block/" ^ block ^ "/proto_inbox" in
  match json with
  | None -> return Rollup.{cumulated_size = 0; contents = []; hash = ""}
  | Some json ->
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list |> List.map as_string)
      in
      let hash = JSON.(json |-> "hash" |> as_string) in
      return Rollup.{cumulated_size; contents; hash}

let get_rollup_parameter_file ~protocol =
  let enable_tx_rollup = [(["tx_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  Protocol.write_parameter_file ~base enable_tx_rollup

let test_node_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: configuration"
    ~tags:["tx_rollup"; "configuration"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      (* Originate a rollup with a given operator *)
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_rollup_node =
        Rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* filename =
        Rollup_node.config_init tx_rollup_node tx_rollup_hash block_hash
      in
      let configuration =
        let open Ezjsonm in
        match from_channel @@ open_in filename with
        | `O fields ->
            `O
              (List.map
                 (fun (k, v) ->
                   let x =
                     if k = "data-dir" || k = "block-hash" || k = "rpc-port"
                     then `String "<variable>"
                     else v
                   in
                   (k, x))
                 fields)
            |> to_string
        | _ -> failwith "Unexpected configuration format"
      in
      let () = Regression.capture configuration in
      unit)

let test_tx_node_is_ready =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test if the node is ready"
    ~tags:["tx_rollup"; "ready"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_node =
        Rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Rollup_node.config_init tx_node tx_rollup_hash block_hash in
      let* () = Rollup_node.run tx_node in
      let* () = Rollup_node.wait_for_ready tx_node in
      unit)

let test_tx_node_store_inbox =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test"
    ~tags:["tx_rollup"; "test"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! rollup = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_node =
        Rollup_node.create
          ~rollup_id:rollup
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Rollup_node.config_init tx_node rollup block_hash in
      let* () = Rollup_node.run tx_node in
      (* Submit a batch *)
      let batch = "tezos" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~hooks
          ~content:batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* node_inbox_head = get_node_inbox tx_node in
      let* node_inbox_0 = get_node_inbox ~block:"0" tx_node in
      let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:0 client in
      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(
        (( = )
           node_inbox_head.cumulated_size
           inbox.cumulated_size
           ~error_msg:
             "Cumulated size of inboxes computed by the rollup node should be \
              equal to the cumulated size given by the RPC")
          int) ;
      Check.(
        ( = )
          node_inbox_head.contents
          inbox.contents
          ~error_msg:
            "Content of inboxes computed by the rollup node should be equal to \
             the contents given by the RPC"
          (list string)) ;
      Check.(
        ( = )
          node_inbox_head.contents
          node_inbox_0.contents
          ~error_msg:
            "Content of inbox at head (%L) and level 0 (%R) are different"
          (list string)) ;
      let snd_batch = "tezos_tezos" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~hooks
          ~content:snd_batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* node_inbox_head = get_node_inbox tx_node in
      let* node_inbox_1 = get_node_inbox ~block:"1" tx_node in
      let*! inbox = Rollup.get_inbox ~hooks ~rollup ~level:1 client in
      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      assert (Int.equal node_inbox_head.cumulated_size inbox.cumulated_size) ;
      assert (List.equal String.equal node_inbox_head.contents inbox.contents) ;
      Check.(
        ( = )
          node_inbox_head.contents
          node_inbox_1.contents
          ~error_msg:
            "Content of inbox at head (%L) and level 1 (%R) are different"
          (list string)) ;
      unit)

let register ~protocols =
  test_node_configuration protocols ;
  test_tx_node_is_ready protocols ;
  test_tx_node_store_inbox protocols
