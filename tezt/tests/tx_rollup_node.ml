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

let get_node_inbox node =
  let* json = node_rpc node "current_inbox" in
  match json with
  | None -> return Rollup.{cumulated_size = 0; contents = []}
  | Some json ->
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list |> List.map as_string)
      in
      return Rollup.{cumulated_size; contents}

let test ~__FILE__ ?output_file ?(tags = []) title k =
  match output_file with
  | None ->
      Protocol.register_test ~__FILE__ ~title ~tags:("tx_rollup" :: tags) k
  | Some output_file ->
      Protocol.register_regression_test
        ~output_file
        ~__FILE__
        ~title
        ~tags:("tx_rollup" :: tags)
        k

let setup f ~protocol =
  let enable_tx_rollup = [(["tx_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base enable_tx_rollup in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let bootstrap1_key = Constant.bootstrap1 in
  let bootstrap2_key = Constant.bootstrap2 in
  f node client bootstrap1_key bootstrap2_key

let test_with_setup ~__FILE__ ?output_file ?(tags = []) title f =
  test ~__FILE__ ?output_file ~tags title (fun protocol ->
      setup ~protocol (f protocol))

let test_node_configuration =
  let output_file = "tx_node_configuration" in
  test_with_setup
    ~__FILE__
    ~output_file
    "TX_rollup: configuration"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* tx_rollup_hash =
        Client.Tx_rollup.originate_tx_rollup ~src:operator client
      in
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
  test_with_setup
    ~__FILE__
    "TX_rollup: test if the node is ready"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* tx_rollup_hash =
        Client.Tx_rollup.originate_tx_rollup ~src:operator client
      in
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
  test_with_setup
    ~__FILE__
    "TX_rollup: test"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* rollup = Client.Tx_rollup.originate_tx_rollup ~src:operator client in
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
      let* () =
        Client.Tx_rollup.submit_tx_rollup_batch
          ~hooks
          ~content:batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* node_inbox = get_node_inbox tx_node in
      let* inbox = Rollup.get_inbox ~hooks ~rollup client in
      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(
        (( = )
           node_inbox.cumulated_size
           inbox.cumulated_size
           ~error_msg:
             "Cumulated size of inboxes computed by the rollup node should be \
              equal to the cumulated size given by the RPC")
          int) ;
      Check.(
        ( = )
          node_inbox.contents
          inbox.contents
          ~error_msg:
            "Content of inboxes computed by the rollup node should be equal to \
             the cumulated size given by the RPC"
          (list string)) ;
      let snd_batch = "tezos_tezos" in
      let* () =
        Client.Tx_rollup.submit_tx_rollup_batch
          ~hooks
          ~content:snd_batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* node_inbox = get_node_inbox tx_node in
      let* inbox = Rollup.get_inbox ~hooks ~rollup client in
      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      assert (Int.equal node_inbox.cumulated_size inbox.cumulated_size) ;
      assert (List.equal String.equal node_inbox.contents inbox.contents) ;
      unit)

let register ~protocols =
  test_node_configuration ~protocols ;
  test_tx_node_is_ready ~protocols ;
  test_tx_node_store_inbox ~protocols
