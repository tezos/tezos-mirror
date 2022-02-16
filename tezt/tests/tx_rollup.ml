(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 Marigold, <contact@marigold.dev>                       *)
(* Copyright (c) 2022 Oxhead Alpha <info@oxhead-alpha.com>                   *)
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
   Component:    Mempool
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup.ml
   Subject:      .
*)

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

open Rollup
module Rollup = Tx_rollup

let parameter_file protocol =
  Protocol.write_parameter_file
    ~base:(Either.right (protocol, None))
    [(["tx_rollup_enable"], Some "true")]

(* This module only registers regressions tests. Those regressions
   tests should be used to ensure there is no regressions with the
   various RPCs exported by the tx_rollups. *)
module Regressions = struct
  type t = {node : Node.t; client : Client.t; rollup : string}

  let init_with_tx_rollup ~protocol =
    let* parameter_file = parameter_file protocol in
    let* (node, client) =
      Client.init_with_protocol ~parameter_file `Client ~protocol ()
    in
    (* We originate a dumb rollup to be able to generate a paths for
       tx_rollups related RPCs. *)
    let* rollup =
      Client.originate_tx_rollup
        ~burn_cap:Tez.(of_int 9999999)
        ~storage_limit:60_000
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node 2 in
    return {node; client; rollup}

  let rpc_state ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_state"
      ~title:"RPC (tx_rollups, regression) - state"
      ~tags:["tx_rollup"; "rpc"]
      ~protocols
    @@ fun protocol ->
    let* {node = _; client; rollup} = init_with_tx_rollup ~protocol in
    let* _state = Rollup.get_state ~hooks ~rollup client in
    return ()

  let submit_batch ~batch {rollup; client; node} =
    let* () =
      Client.submit_tx_rollup_batch
        ~hooks
        ~content:batch
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_inbox ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_inbox"
      ~title:"RPC (tx_rollups, regression) - inbox"
      ~tags:["tx_rollup"; "rpc"; "inbox"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node = _} as state) =
      init_with_tx_rollup ~protocol
    in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let* _state = Rollup.get_inbox ~hooks ~rollup client in
    unit

  let submit_commitment ~level ~roots ~predecessor {rollup; client; node} =
    let* () =
      Client.submit_tx_rollup_commitment
        ~hooks
        ~level
        ~roots
        ~predecessor
        ~rollup
        ~src:Constant.bootstrap1.public_key_hash
        client
    in
    let current_level = Node.get_level node in
    let* () = Client.bake_for client in
    let* _ = Node.wait_for_level node (current_level + 1) in
    return ()

  let rpc_commitment ~protocols =
    Protocol.register_regression_test
      ~__FILE__
      ~output_file:"tx_rollup_rpc_commitment"
      ~title:"RPC (tx_rollups, regression) - commitment"
      ~tags:["tx_rollup"; "rpc"; "commitment"]
      ~protocols
    @@ fun protocol ->
    let* ({rollup; client; node} as state) = init_with_tx_rollup ~protocol in
    (* The content of the batch does not matter for the regression test. *)
    let batch = "blob" in
    let* () = submit_batch ~batch state in
    let batch_level = Node.get_level node in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       we introduce two bakes to ensure the block is finalised. This
       should be removed once we do not rely on Tenderbake anymore. *)
    let* () = Client.bake_for client in
    let* () = Client.bake_for client in
    (* FIXME https://gitlab.com/tezos/tezos/-/issues/2503

       At the same time we remove the dependency to Tenderbake for
       commitment, we will ensure the root is indeed the root of the
       previous inbox. I don't know yet how we will be able to do that
       yes, something is missing. *)
    let* () =
      submit_commitment
        ~level:batch_level
        ~roots:["root"]
        ~predecessor:None
        state
    in
    let offset = Node.get_level node - batch_level in
    let* _state =
      Rollup.get_commitments ~hooks ~block:"head" ~offset ~rollup client
    in
    unit

  let register ~protocols =
    rpc_state ~protocols ;
    rpc_inbox ~protocols ;
    rpc_commitment ~protocols
end

(** To be attached to process whose output needs to be captured by the
    regression framework. *)
let hooks = Tezos_regression.hooks

type state = {burn_per_byte : int}

type inbox = {cumulated_size : int; contents : string list}

let node_rpc node service =
  let* opt_get = RPC.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
      let url =
        Format.asprintf "%s/%s" (Tx_rollup_node.endpoint node) service
      in
      let* result = get ~url in
      return (Some (result |> JSON.parse ~origin:service))

let get_block_hash block_json =
  JSON.(block_json |-> "hash" |> as_string) |> return

let get_state ?hooks rollup client =
  (* The state is currently empty, but the RPC can fail if [tx_rollup]
     does not exist. *)
  let* json = RPC.Tx_rollup.get_state ?hooks ~rollup client in
  let burn_per_byte = JSON.(json |-> "burn_per_byte" |> as_int) in
  return {burn_per_byte}

let get_inbox ?hooks rollup client =
  let* json = RPC.Tx_rollup.get_inbox ?hooks ~rollup client in
  let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
  let contents = JSON.(json |-> "contents" |> as_list |> List.map as_string) in
  return {cumulated_size; contents}

let get_node_inbox node =
  let* json = node_rpc node "current_inbox" in
  match json with
  | None -> return {cumulated_size = 0; contents = []}
  | Some json ->
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list |> List.map as_string)
      in
      return {cumulated_size; contents}

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
      setup ~protocol (fun node client bootstrap_key bootstrap2_key ->
          f protocol node client bootstrap_key bootstrap2_key))

let test_submit_batch ~protocols =
  Protocol.register_test
    ~__FILE__
    ~title:"Simple use case"
    ~tags:["tx_rollup"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let* rollup =
    Client.originate_tx_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~storage_limit:60_000
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  (* We check the rollup exists by trying to fetch its state. Since it
     is a regression test, we can detect changes to this default
     state. *)
  let* _state = Rollup.get_state ~hooks ~rollup client in

  (* Submit a batch *)
  let batch = "tezos" in

  let* () =
    Client.submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:Constant.bootstrap1.public_key_hash
      client
  in
  let* () = Client.bake_for client in

  let* _ = Node.wait_for_level node 3 in

  (* Check the inbox has been created *)
  let* inbox = get_inbox ~hooks rollup client in
  Check.(
    (( = )
       (String.length batch)
       inbox.cumulated_size
       ~error_msg:
         "Cumulated size of inboxes should be equal of the length of the \
          submited message")
      int) ;

  unit

let test_invalid_rollup_address ~protocols =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit to an invalid rollup address should fail"
    ~tags:["tx_rollup"; "cli"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (_node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  let invalid_address = "this is an invalid tx rollup address" in
  let* () =
    Client.spawn_submit_tx_rollup_batch
      ~hooks
      ~content:""
      ~rollup:invalid_address
      ~src:Constant.bootstrap1.public_key_hash
      client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex
              ("Parameter '" ^ invalid_address
             ^ "' is an invalid tx rollup address"))
  in

  unit

let test_submit_from_originated_source ~protocols =
  let open Tezt_tezos in
  Protocol.register_test
    ~__FILE__
    ~title:"Submit from an originated contract should fail"
    ~tags:["tx_rollup"; "cli"]
    ~protocols
  @@ fun protocol ->
  let* parameter_file = parameter_file protocol in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ()
  in
  (* We begin by originating a contract *)
  let* originated_contract =
    Client.originate_contract
      ~alias:"originated_contract_simple"
      ~amount:Tez.zero
      ~src:"bootstrap1"
      ~prg:"file:./tezt/tests/contracts/proto_alpha/str_id.tz"
      ~init:"Some \"initial storage\""
      ~burn_cap:Tez.(of_int 3)
      client
  in

  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in

  (* We originate a tx_rollup using an implicit account *)
  let* rollup =
    Client.originate_tx_rollup
      ~burn_cap:Tez.(of_int 9999999)
      ~storage_limit:60_000
      ~src:Constant.bootstrap1.public_key_hash
      client
  in

  let* () = Client.bake_for client in

  let batch = "tezos" in

  (* Finally, we submit a batch to the tx_rollup from an originated contract *)
  let* () =
    Client.spawn_submit_tx_rollup_batch
      ~hooks
      ~content:batch
      ~rollup
      ~src:originated_contract
      client
    |> Process.check_error
         ~exit_code:1
         ~msg:
           (rex "Only implicit accounts can submit transaction rollup batches")
  in

  unit

let test_node_configuration =
  let output_file = "tx_node_configuration" in
  test_with_setup
    ~__FILE__
    ~output_file
    "TX_rollup: configuration"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* tx_rollup_hash =
        Client.originate_tx_rollup
          ~burn_cap:Tez.(of_int 9999999)
          ~storage_limit:60_000
          ~src:operator
          client
      in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_rollup_node =
        Tx_rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* filename =
        Tx_rollup_node.config_init tx_rollup_node tx_rollup_hash block_hash
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
  let open Tezt_tezos in
  test_with_setup
    ~__FILE__
    "TX_rollup: test if the node is ready"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* tx_rollup_hash =
        Client.originate_tx_rollup
          ~burn_cap:Tez.(of_int 9999999)
          ~storage_limit:60_000
          ~src:operator
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_node =
        Tx_rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Tx_rollup_node.config_init tx_node tx_rollup_hash block_hash in
      let* () = Tx_rollup_node.run tx_node in
      let* () = Tx_rollup_node.wait_for_ready tx_node in
      unit)

let test_tx_node_store_inbox =
  let open Tezt_tezos in
  test_with_setup
    ~__FILE__
    "TX_rollup: test"
    (fun _protocol node client bootstrap1_key _ ->
      let operator = bootstrap1_key.public_key_hash in
      let* tx_rollup_hash =
        Client.originate_tx_rollup
          ~burn_cap:Tez.(of_int 9999999)
          ~storage_limit:60_000
          ~src:operator
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_node =
        Tx_rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Tx_rollup_node.config_init tx_node tx_rollup_hash block_hash in
      let* () = Tx_rollup_node.run tx_node in
      (* Submit a batch *)
      let batch = "tezos" in

      let* () =
        Client.submit_tx_rollup_batch
          ~hooks
          ~content:batch
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* node_inbox = get_node_inbox tx_node in
      let* inbox = get_inbox ~hooks tx_rollup_hash client in

      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      Check.(
        (( = )
           node_inbox.cumulated_size
           inbox.cumulated_size
           ~error_msg:
             "Cumulated size of inboxes on the client side should be equal to \
              the cumulated size of inboxes on the node side")
          int) ;

      Check.(
        ( = )
          node_inbox.contents
          inbox.contents
          ~error_msg:
            "Content  of inboxes on the client side should be equal to the \
             content of inboxes on the node side"
          (list string)) ;

      let snd_batch = "tezos_tezos" in

      let* () =
        Client.submit_tx_rollup_batch
          ~hooks
          ~content:snd_batch
          ~rollup:tx_rollup_hash
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* node_inbox = get_node_inbox tx_node in
      let* inbox = get_inbox ~hooks tx_rollup_hash client in
      (* Enusre that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      assert (Int.equal node_inbox.cumulated_size inbox.cumulated_size) ;
      assert (List.equal String.equal node_inbox.contents inbox.contents) ;
      unit)

let register ~protocols =
  Regressions.register ~protocols ;
  test_submit_batch ~protocols ;
  test_invalid_rollup_address ~protocols ;
  test_submit_from_originated_source ~protocols ;
  test_node_configuration ~protocols ;
  test_tx_node_is_ready ~protocols ;
  test_tx_node_store_inbox ~protocols
