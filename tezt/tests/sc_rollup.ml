(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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
   Component:    Smart Contract Optimistic Rollups
   Invocation:   dune exec tezt/tests/main.exe -- --file sc_rollup.ml
*)

open Base

let hooks = Tezos_regression.hooks

(*

   Helpers
   =======

*)

let test ~__FILE__ ?output_file ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  match output_file with
  | Some output_file ->
      Protocol.register_regression_test ~output_file ~__FILE__ ~title ~tags f
  | None -> Protocol.register_test ~__FILE__ ~title ~tags f

let setup f ~protocol =
  let sc_rollup_enable = [(["sc_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base sc_rollup_enable in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let* (node, client) =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

let sc_rollup_node_rpc sc_node service =
  let* curl = RPC.Curl.get () in
  match curl with
  | None -> return None
  | Some curl ->
      let url =
        Printf.sprintf "%s/%s" (Sc_rollup_node.endpoint sc_node) service
      in
      let* response = curl ~url in
      return (Some response)

(*

   Tests
   =====

*)

(* Originate a new SCORU of the arithmetic kind
   --------------------------------------------

   - Rollup addresses are fully determined by operation hashes and origination nonce.

*)
let test_origination =
  let output_file _ = "sc_rollup_origination" in
  test
    ~__FILE__
    ~output_file
    "origination of a SCORU executes without error"
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* _rollup_address =
        Client.Sc_rollup.originate
          ~hooks
          ~burn_cap:Tez.(of_int 9999999)
          ~src:bootstrap1_key
          ~kind:"arith"
          ~boot_sector:""
          client
      in
      Client.bake_for client)

(* Configuration of a rollup node
   ------------------------------

   A rollup node has a configuration file that must be initialized.

*)
let with_fresh_rollup f tezos_node tezos_client bootstrap1_key =
  let* rollup_address =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"arith"
      ~boot_sector:""
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create tezos_node tezos_client ~operator_pkh:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node rollup_address
  in
  let* () = Client.bake_for tezos_client in
  f rollup_address sc_rollup_node configuration_filename

let with_fresh_rollups n f node client bootstrap1 =
  let rec go n addrs k =
    if n < 1 then k addrs
    else
      with_fresh_rollup
        (fun addr _ _ -> go (n - 1) (String_set.add addr addrs) k)
        node
        client
        bootstrap1
  in
  go n String_set.empty f

let test_rollup_node_configuration =
  let output_file _ = "sc_rollup_node_configuration" in
  test
    ~__FILE__
    ~output_file
    "configuration of a smart contract optimistic rollup node"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun _rollup_address _sc_rollup_node filename ->
      let read_configuration =
        let open Ezjsonm in
        match from_channel (open_in filename) with
        | `O fields ->
            (* Remove "data-dir" and "rpc-port" as they are non deterministic. *)
            `O
              (List.filter
                 (fun (s, _) ->
                   match s with "data-dir" | "rpc-port" -> false | _ -> true)
                 fields)
            |> to_string
        | _ ->
            failwith "The configuration file does not have the expected format."
      in
      Log.info "Read configuration:\n %s" read_configuration ;
      return ())

(* Launching a rollup node
   -----------------------

   A running rollup node can be asked the address of the rollup it is
   interacting with.

*)
let test_rollup_node_running =
  test
    ~__FILE__
    ~tags:["run"]
    "running a smart contract rollup node"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun rollup_address sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let* rollup_address_from_rpc =
        sc_rollup_node_rpc sc_rollup_node "sc_rollup_address"
      in
      match rollup_address_from_rpc with
      | None ->
          (* No curl, no check. *)
          failwith "Please install curl"
      | Some rollup_address_from_rpc ->
          let rollup_address_from_rpc =
            JSON.as_string rollup_address_from_rpc
          in
          if rollup_address_from_rpc <> rollup_address then
            failwith
              (Printf.sprintf
                 "Expecting %s, got %s when we query the sc rollup node RPC \
                  address"
                 rollup_address
                 rollup_address_from_rpc)
          else return ())

(* Interacting with a rollup node through a rollup client
   ------------------------------------------------------

   When a rollup node is running, a rollup client can ask this
   node its rollup address.

*)
let test_rollup_client_gets_address =
  let output_file _ = "sc_rollup_client_gets_address" in
  test
    ~__FILE__
    ~output_file
    ~tags:["run"; "client"]
    "getting a smart-contract rollup address through the client"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun rollup_address sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let sc_client = Sc_rollup_client.create sc_rollup_node in
      let* rollup_address_from_client =
        Sc_rollup_client.sc_rollup_address sc_client
      in
      if rollup_address_from_client <> rollup_address then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s when the client asks for the sc rollup \
              address"
             rollup_address
             rollup_address_from_client) ;
      return ())

(* Fetching the initial level of a sc rollup
   -----------------------------------------

  We can fetch the level when a smart contract rollup was
  originated from the context.
*)
let test_rollup_get_initial_level =
  let output_file _ = "sc_rollup_get_initial_level" in
  test
    ~__FILE__
    ~output_file
    ~tags:["initial_level"]
    "get initial level of a sc rollup"
    (fun protocol ->
      setup ~protocol @@ fun node client bootstrap ->
      let* current_level = RPC.get_current_level client in
      ( with_fresh_rollup @@ fun sc_rollup_address _sc_rollup_node _filename ->
        (* Bake 10 blocks to be sure that the initial level of rollup is different
           from the current level. *)
        let rec bake_blocks n =
          match n with
          | 0 -> return ()
          | _ ->
              Lwt.bind (Client.bake_for client) (fun _ -> bake_blocks (n - 1))
        in
        let* _ = bake_blocks 10 in
        let* initial_level =
          RPC.Sc_rollup.get_initial_level ~sc_rollup_address client
        in
        (* 1 Block for activating alpha + 1 block for originating the rollup
           the rollup initial level should be 2 *)
        Check.(
          (JSON.as_int initial_level
          = JSON.as_int (JSON.get "level" current_level) + 1)
            int
            ~error_msg:"expected value %L, got %R") ;
        return () )
        node
        client
        bootstrap)

(* Pushing message in the inbox
   ----------------------------

   A message can be pushed to a smart-contract rollup inbox through
   the Tezos node. Then we can observe that the messages are included in the
   inbox.
*)
let send_message client sc_rollup_address msg =
  let* () =
    Client.Sc_rollup.send_message
      ~hooks
      ~src:"bootstrap1"
      ~dst:sc_rollup_address
      ~msg
      client
  in
  Client.bake_for client

let send_messages n sc_rollup_address client =
  let messages =
    List.map
      (fun i ->
        let json = `A (List.map (fun _ -> `String "CAFEBABE") (range 1 i)) in
        "text:" ^ Ezjsonm.to_string json)
      (range 1 n)
  in
  Lwt_list.iter_s
    (fun msg -> send_message client sc_rollup_address msg)
    messages

let to_text_messages_arg msgs =
  let text_messages =
    List.map (fun msg -> Hex.of_string msg |> Hex.show) msgs
  in
  let json = Ezjsonm.list Ezjsonm.string text_messages in
  "text:" ^ Ezjsonm.to_string ~minify:true json

let send_text_messages client sc_rollup_address msgs =
  send_message client sc_rollup_address (to_text_messages_arg msgs)

let parse_inbox json =
  let go () =
    let inbox = JSON.as_object json in
    return
      ( List.assoc "current_messages_hash" inbox |> JSON.as_string,
        List.assoc "nb_available_messages" inbox |> JSON.as_int )
  in
  Lwt.catch go @@ fun exn ->
  failwith
    (Printf.sprintf
       "Unable to parse inbox %s\n%s"
       (JSON.encode json)
       (Printexc.to_string exn))

let get_inbox_from_tezos_node sc_rollup_address client =
  let* inbox = RPC.Sc_rollup.get_inbox ~sc_rollup_address client in
  parse_inbox inbox

let get_inbox_from_sc_rollup_node sc_rollup_node =
  let* inbox = sc_rollup_node_rpc sc_rollup_node "inbox" in
  match inbox with
  | None -> failwith "Unable to retrieve inbox from sc rollup node"
  | Some inbox -> parse_inbox inbox

let test_rollup_inbox_size =
  let output_file _ = "sc_rollup_inbox_size" in
  test
    ~__FILE__
    ~output_file
    ~tags:["inbox"]
    "pushing messages in the inbox - check inbox size"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup_address _sc_rollup_node _filename ->
        let n = 10 in
        let* () = send_messages n sc_rollup_address client in
        let* (_, inbox_size) =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        return
        @@ Check.(
             (inbox_size = n * (n + 1) / 2)
               int
               ~error_msg:"expected value %R, got %L") )
        node
        client)

module Sc_rollup_inbox = struct
  open Tezos_context_encoding.Context

  module Store = struct
    module Maker = Irmin_pack_mem.Maker (Conf)
    include Maker.Make (Schema)
    module Schema = Tezos_context_encoding.Context.Schema
  end

  include Tezos_context_helpers.Context.Make_tree (Conf) (Store)

  (*
      The hash for empty messages is the hash of empty bytes, and not of an empty
      tree.

      The hash for non-empty messages is the hash of the tree, where each message
      payload sits at the key [[message_index, "payload"]], where [message_index]
      is the index of the current message relative to the first message.

      The [message_counter] is reset to zero when the inbox level increments (and
      therefore [current_messages] are zero-indexed in the tree).
  *)
  let rec build_current_messages_tree counter tree messages =
    match messages with
    | [] -> return tree
    | message :: rest ->
        let key = Data_encoding.Binary.to_string_exn Data_encoding.z counter in
        let payload = Bytes.of_string message in
        let* tree = add tree [key; "payload"] payload in
        build_current_messages_tree (Z.succ counter) tree rest

  let predict_current_messages_hash = function
    | [] -> return @@ Tezos_crypto.Context_hash.hash_bytes []
    | current_messages ->
        let open Lwt.Syntax in
        let+ tree =
          build_current_messages_tree Z.zero (empty ()) current_messages
        in
        hash tree
end

let fetch_messages_from_block sc_rollup_address client =
  let* ops = RPC.get_operations client in
  let messages =
    ops |> JSON.as_list
    |> List.concat_map JSON.as_list
    |> List.concat_map (fun op -> JSON.(op |-> "contents" |> as_list))
    |> List.filter_map (fun op ->
           if
             JSON.(op |-> "kind" |> as_string) = "sc_rollup_add_messages"
             && JSON.(op |-> "rollup" |> as_string) = sc_rollup_address
           then Some JSON.(op |-> "message" |> as_list)
           else None)
    |> List.hd
    |> List.map (fun message -> JSON.(message |> as_string))
  in
  return messages

let test_rollup_inbox_current_messages_hash =
  let output_file _ = "sc_rollup_inbox_current_messages_hash" in
  test
    ~__FILE__
    ~output_file
    ~tags:["inbox"]
    "pushing messages in the inbox - current messages hash"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup_address _sc_rollup_node _filename ->
        let gen_message_batch from until =
          List.map
            (fun x ->
              Printf.sprintf "hello, message number %s" (Int.to_string x))
            (range from until)
        in
        let prepare_batch messages =
          messages
          |> List.map (Printf.sprintf "\"%s\"")
          |> String.concat ", " |> Printf.sprintf "text:[%s]"
        in
        let open Tezos_crypto.Context_hash in
        (* no messages have been sent *)
        let* (pristine_hash, _) =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        let* expected = Sc_rollup_inbox.predict_current_messages_hash [] in
        let () =
          Check.(
            (to_b58check expected = pristine_hash)
              string
              ~error_msg:"expected pristine hash %L, got %R")
        in
        (*
           send messages, and assert that
           - the hash has changed
           - the hash matches the 'predicted' hash from the messages we sent
        *)
        let fst_batch = gen_message_batch 0 4 in
        let* () =
          send_message client sc_rollup_address @@ prepare_batch fst_batch
        in
        let* (fst_batch_hash, _) =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        let () =
          Check.(
            (pristine_hash <> fst_batch_hash)
              string
              ~error_msg:
                "expected current messages hash to change when messages sent")
        in
        let* expected =
          Sc_rollup_inbox.predict_current_messages_hash fst_batch
        in
        let () =
          Check.(
            (to_b58check expected = fst_batch_hash)
              string
              ~error_msg:"expected first batch hash %L, got %R")
        in
        (*
           send more messages, and assert that
           - the messages can be retrieved from the latest block
           - the hash matches the 'predicted' hash from the messages we sent
        *)
        let snd_batch = gen_message_batch 5 10 in
        let* () =
          send_message client sc_rollup_address @@ prepare_batch snd_batch
        in
        let* messages = fetch_messages_from_block sc_rollup_address client in
        let () =
          Check.(
            (messages = snd_batch)
              (list string)
              ~error_msg:"expected messages:\n%R\nretrieved:\n%L")
        in
        let* (snd_batch_hash, _) =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        let* expected =
          Sc_rollup_inbox.predict_current_messages_hash snd_batch
        in
        let () =
          Check.(
            (Tezos_crypto.Context_hash.to_b58check expected = snd_batch_hash)
              string
              ~error_msg:"expected second batch hash %L, got %R")
        in
        (*
           send an empty list of messages, and assert that
           - the hash matches the 'pristine' hash: a.k.a there are no 'current messages'
        *)
        let* () = send_message client sc_rollup_address @@ prepare_batch [] in
        let* (empty_batch_hash, _) =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        let () =
          Check.(
            (pristine_hash = empty_batch_hash)
              string
              ~error_msg:"expected empty batch hash %L, got %R")
        in
        return () )
        node
        client)

(* Synchronizing the inbox in the rollup node
   ------------------------------------------

   For each new head set by the Tezos node, the rollup node retrieves
   the messages of its rollup and maintains its internal inbox in a
   persistent state stored in its data directory. This process can
   handle Tezos chain reorganization and can also catch up to ensure a
   tight synchronization between the rollup and the layer 1 chain.

   In addition, this maintenance includes the computation of a Merkle
   tree which must have the same root hash as the one stored by the
   protocol in the context.

*)
let test_rollup_inbox_of_rollup_node variant scenario =
  let output_file _ = "sc_rollup_inbox_of_rollup_node_" ^ variant in
  test
    ~__FILE__
    ~output_file
    ~tags:["inbox"; "node"; variant]
    (Printf.sprintf
       "observing the correct maintenance of inbox in the rollup node (%s)"
       variant)
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup_address sc_rollup_node _filename ->
        let* () =
          scenario protocol sc_rollup_node sc_rollup_address node client
        in
        let* inbox_from_sc_rollup_node =
          get_inbox_from_sc_rollup_node sc_rollup_node
        in
        let* inbox_from_tezos_node =
          get_inbox_from_tezos_node sc_rollup_address client
        in
        return
        @@ Check.(
             (inbox_from_sc_rollup_node = inbox_from_tezos_node)
               (tuple2 string int)
               ~error_msg:"expected value %R, got %L") )
        node
        client)

let basic_scenario _protocol sc_rollup_node sc_rollup_address _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent, we
       must end up at level 4. *)
    4
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages sc_rollup_address client in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node expected_level in
  return ()

let sc_rollup_node_stops_scenario _protocol sc_rollup_node sc_rollup_address
    _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent twice, we
       must end up at level 6. *)
    6
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages sc_rollup_address client in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () = send_messages num_messages sc_rollup_address client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node expected_level in
  return ()

let sc_rollup_node_handles_chain_reorg protocol sc_rollup_node sc_rollup_address
    node client =
  let num_messages = 1 in

  setup ~protocol @@ fun node' client' _ ->
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages sc_rollup_address client in
  (* Since we start at level 2, sending 1 message (which also bakes a block) must cause the nodes to
     observe level 3. *)
  let* _ = Node.wait_for_level node 3 in
  let* _ = Node.wait_for_level node' 3 in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node 3 in
  Log.info "Nodes are synchronized." ;

  let divergence () =
    let* identity' = Node.wait_for_identity node' in
    let* () = Client.Admin.kick_peer client ~peer:identity' in
    let* () = send_messages num_messages sc_rollup_address client in
    (* +1 block for [node] *)
    let* _ = Node.wait_for_level node 4 in

    let* () = send_messages num_messages sc_rollup_address client' in
    let* () = send_messages num_messages sc_rollup_address client' in
    (* +2 blocks for [node'] *)
    let* _ = Node.wait_for_level node' 5 in
    Log.info "Nodes are following distinct branches." ;
    return ()
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ = Node.wait_for_level node 5 in
    Log.info "Nodes are synchronized again." ;
    return ()
  in

  let* () = divergence () in
  let* () = trigger_reorg () in
  (* After bringing [node'] back, our SCORU node should see that there is a more attractive head at
     level 5. *)
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node 5 in
  return ()

let test_rollup_list =
  let open Lwt.Syntax in
  let go node client bootstrap1 =
    let* rollups = RPC.Sc_rollup.list client in
    let rollups = JSON.as_list rollups in
    let () =
      match rollups with
      | _ :: _ ->
          failwith "Expected initial list of originated SCORUs to be empty"
      | [] -> ()
    in

    with_fresh_rollups
      10
      (fun scoru_addresses ->
        let* () = Client.bake_for client in
        let+ rollups = RPC.Sc_rollup.list client in
        let rollups =
          JSON.as_list rollups |> List.map JSON.as_string |> String_set.of_list
        in
        Check.(
          (rollups = scoru_addresses)
            (comparable_module (module String_set))
            ~error_msg:"%L %R"))
      node
      client
      bootstrap1
  in

  test
    ~__FILE__
    ~output_file:(fun _ -> "sc_rollup_list")
    ~tags:["list"]
    "list originated rollups"
    (fun protocol -> setup ~protocol go)

(* Make sure the rollup node boots into the initial state.
   -------------------------------------------------------

   When a rollup node starts, we want to make sure that in the absence of
   messages it will boot into the initial state.

*)
let test_rollup_node_boots_into_initial_state =
  let go client sc_rollup_address sc_rollup_node =
    let* init_level =
      RPC.Sc_rollup.get_initial_level ~hooks ~sc_rollup_address client
    in
    let init_level = init_level |> JSON.as_int in

    let* () = Sc_rollup_node.run sc_rollup_node in
    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

    let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;

    let* ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
    Check.(ticks = 0)
      Check.int
      ~error_msg:"Unexpected initial tick count (%L = %R)" ;

    let* status = Sc_rollup_client.status ~hooks sc_rollup_client in
    Check.(status = "Halted")
      Check.string
      ~error_msg:"Unexpected PVM status (%L = %R)" ;

    Lwt.return_unit
  in

  let output_file _ = "sc_rollup_node_boots_into_initial_state" in
  test
    ~__FILE__
    ~output_file
    ~tags:["run"; "node"]
    "node boots into the initial state"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        (fun sc_rollup_address sc_rollup_node _filename ->
          go client sc_rollup_address sc_rollup_node)
        node
        client)

(* Ensure the PVM is transitioning upon incoming messages.
   -------------------------------------------------------

   When the rollup node receives messages, we like to see evidence that the PVM
   has advanced.

*)
let test_rollup_node_advances_pvm_state =
  let go client sc_rollup_address sc_rollup_node =
    let* init_level =
      RPC.Sc_rollup.get_initial_level ~hooks ~sc_rollup_address client
    in
    let init_level = init_level |> JSON.as_int in

    let* () = Sc_rollup_node.run sc_rollup_node in
    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

    let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
    Check.(level = init_level)
      Check.int
      ~error_msg:"Current level has moved past origination level (%L = %R)" ;

    let test_message i =
      let* prev_state_hash =
        Sc_rollup_client.state_hash ~hooks sc_rollup_client
      in
      let* prev_ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in

      let x = Int.to_string i in
      let y = Int.to_string ((i + 2) * 2) in
      let* () = send_text_messages client sc_rollup_address [x; y; "+"] in
      let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (level + i) in

      let* state_hash = Sc_rollup_client.state_hash ~hooks sc_rollup_client in
      Check.(state_hash <> prev_state_hash)
        Check.string
        ~error_msg:"State hash has not changed (%L <> %R)" ;

      let* ticks = Sc_rollup_client.total_ticks ~hooks sc_rollup_client in
      Check.(ticks >= prev_ticks)
        Check.int
        ~error_msg:"Tick counter did not advance (%L >= %R)" ;
      Lwt.return_unit
    in
    let* () = Lwt_list.iter_s test_message (range 1 10) in

    Lwt.return_unit
  in

  let output_file _ = "sc_rollup_node_advances_pvm_state" in
  test
    ~__FILE__
    ~output_file
    ~tags:["run"; "node"]
    "node advances PVM state with messages"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        (fun sc_rollup_address sc_rollup_node _filename ->
          go client sc_rollup_address sc_rollup_node)
        node
        client)

let register ~protocols =
  test_origination protocols ;
  test_rollup_node_configuration protocols ;
  test_rollup_node_running protocols ;
  test_rollup_client_gets_address protocols ;
  test_rollup_list protocols ;
  test_rollup_get_initial_level protocols ;
  test_rollup_inbox_size protocols ;
  test_rollup_inbox_current_messages_hash protocols ;
  test_rollup_inbox_of_rollup_node "basic" basic_scenario protocols ;
  test_rollup_inbox_of_rollup_node
    "stops"
    sc_rollup_node_stops_scenario
    protocols ;
  test_rollup_inbox_of_rollup_node
    "handles_chain_reorg"
    sc_rollup_node_handles_chain_reorg
    protocols ;
  test_rollup_node_boots_into_initial_state protocols ;
  test_rollup_node_advances_pvm_state protocols
