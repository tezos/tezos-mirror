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

(* Number of levels needed to process a head as finalized. This value should
   be the same as `node_context.block_finality_time`, where `node_context` is
   the `Node_context.t` used by the rollup node. For Tenderbake, the
   block finality time is 2. *)
let block_finality_time = 2

type sc_rollup_constants = {
  origination_size : int;
  challenge_window_in_blocks : int;
  max_number_of_messages_per_commitment_period : int;
  stake_amount : Tez.t;
  commitment_period_in_blocks : int;
  max_lookahead_in_blocks : int32;
  max_active_outbox_levels : int32;
  max_outbox_messages_per_level : int;
}

let get_sc_rollup_constants client =
  let* json = RPC.get_constants client in
  let open JSON in
  let origination_size = json |-> "sc_rollup_origination_size" |> as_int in
  let challenge_window_in_blocks =
    json |-> "sc_rollup_challenge_window_in_blocks" |> as_int
  in
  let max_number_of_messages_per_commitment_period =
    json |-> "sc_rollup_max_number_of_messages_per_commitment_period" |> as_int
  in
  let stake_amount =
    json |-> "sc_rollup_stake_amount" |> as_string |> Int64.of_string
    |> Tez.of_mutez_int64
  in
  let commitment_period_in_blocks =
    json |-> "sc_rollup_commitment_period_in_blocks" |> as_int
  in
  let max_lookahead_in_blocks =
    json |-> "sc_rollup_max_lookahead_in_blocks" |> as_int32
  in
  let max_active_outbox_levels =
    json |-> "sc_rollup_max_active_outbox_levels" |> as_int32
  in
  let max_outbox_messages_per_level =
    json |-> "sc_rollup_max_outbox_messages_per_level" |> as_int
  in
  return
    {
      origination_size;
      challenge_window_in_blocks;
      max_number_of_messages_per_commitment_period;
      stake_amount;
      commitment_period_in_blocks;
      max_lookahead_in_blocks;
      max_active_outbox_levels;
      max_outbox_messages_per_level;
    }

let make_parameter name value =
  Option.map (fun v -> ([name], Option.some @@ Int.to_string v)) value
  |> Option.to_list

let test ~__FILE__ ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  Protocol.register_test ~__FILE__ ~title ~tags f

let regression_test ~__FILE__ ?(tags = []) title f =
  let tags = "sc_rollup" :: tags in
  Protocol.register_regression_test ~__FILE__ ~title ~tags f

let setup ?commitment_period ?challenge_window f ~protocol =
  let parameters =
    make_parameter "sc_rollup_commitment_period_in_blocks" commitment_period
    @ make_parameter "sc_rollup_challenge_window_in_blocks" challenge_window
    @ [(["sc_rollup_enable"], Some "true")]
  in
  let base = Either.right (protocol, None) in
  let* parameter_file = Protocol.write_parameter_file ~base parameters in
  let nodes_args =
    Node.
      [
        Synchronisation_threshold 0; History_mode (Full None); No_bootstrap_peers;
      ]
  in
  let* node, client =
    Client.init_with_protocol ~parameter_file `Client ~protocol ~nodes_args ()
  in
  let bootstrap1_key = Constant.bootstrap1.public_key_hash in
  f node client bootstrap1_key

let get_sc_rollup_commitment_period_in_blocks client =
  let* constants = get_sc_rollup_constants client in
  return constants.commitment_period_in_blocks

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

type test = {variant : string; tags : string list; description : string}

let with_fresh_rollup f tezos_node tezos_client bootstrap1_key =
  let* sc_rollup =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"arith"
      ~boot_sector:""
      ~parameters_ty:"unit"
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create tezos_node tezos_client ~operator_pkh:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup
  in
  let* () = Client.bake_for_and_wait tezos_client in
  f sc_rollup sc_rollup_node configuration_filename

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2933
   Many tests can be refactored using test_scenario. *)
let test_scenario ?commitment_period ?challenge_window
    {variant; tags; description} scenario =
  let tags = tags @ [variant] in
  regression_test
    ~__FILE__
    ~tags
    (Printf.sprintf "%s (%s)" description variant)
    (fun protocol ->
      setup ?commitment_period ?challenge_window ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup sc_rollup_node _filename ->
        scenario protocol sc_rollup_node sc_rollup node client )
        node
        client)

let inbox_level (_hash, (commitment : Sc_rollup_client.commitment), _level) =
  commitment.inbox_level

let number_of_ticks (_hash, (commitment : Sc_rollup_client.commitment), _level)
    =
  commitment.number_of_ticks

let last_cemented_commitment_hash_with_level ~sc_rollup client =
  let* json =
    RPC.Client.call client
    @@ RPC.Sc_rollup.get_last_cemented_commitment_hash_with_level sc_rollup
  in
  let hash = JSON.(json |-> "hash" |> as_string) in
  let level = JSON.(json |-> "level" |> as_int) in
  return (hash, level)

let get_staked_on_commitment ~sc_rollup ~staker client =
  let* json =
    RPC.Client.call client
    @@ RPC.Sc_rollup.get_staked_on_commitment ~sc_rollup staker
  in
  let hash = JSON.(json |-> "hash" |> as_string) in
  return hash

let hash (hash, (_ : Sc_rollup_client.commitment), _level) = hash

let first_published_at_level (_hash, (_ : Sc_rollup_client.commitment), level) =
  level

let predecessor (_hash, {Sc_rollup_client.predecessor; _}, _level) = predecessor

let cement_commitment client ~sc_rollup ~hash =
  let* () =
    Client.Sc_rollup.cement_commitment
      ~hooks
      ~src:"bootstrap1"
      ~dst:sc_rollup
      ~hash
      client
  in
  Client.bake_for_and_wait client

let publish_commitment ?(src = Constant.bootstrap1.public_key_hash) ~commitment
    client sc_rollup =
  let ({compressed_state; inbox_level; predecessor; number_of_ticks}
        : Sc_rollup_client.commitment) =
    commitment
  in
  Client.Sc_rollup.publish_commitment
    ~hooks
    ~src
    ~sc_rollup
    ~compressed_state
    ~inbox_level
    ~predecessor
    ~number_of_ticks
    client

(*

   Tests
   =====

*)

(* Originate a new SCORU of the arithmetic kind
   --------------------------------------------

   - Rollup addresses are fully determined by operation hashes and origination nonce.
*)
let test_origination =
  regression_test
    ~__FILE__
    "origination of a SCORU executes without error"
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* _sc_rollup =
        Client.Sc_rollup.originate
          ~hooks
          ~burn_cap:Tez.(of_int 9999999)
          ~src:bootstrap1_key
          ~kind:"arith"
          ~parameters_ty:"unit"
          ~boot_sector:""
          client
      in
      Client.bake_for_and_wait client)

(* Configuration of a rollup node
   ------------------------------

   A rollup node has a configuration file that must be initialized.
*)
let with_fresh_rollup ?(boot_sector = "") f tezos_node tezos_client
    bootstrap1_key =
  let* sc_rollup =
    Client.Sc_rollup.originate
      ~hooks
      ~burn_cap:Tez.(of_int 9999999)
      ~src:bootstrap1_key
      ~kind:"arith"
      ~parameters_ty:"unit"
      ~boot_sector
      tezos_client
  in
  let sc_rollup_node =
    Sc_rollup_node.create tezos_node tezos_client ~operator_pkh:bootstrap1_key
  in
  let* configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node sc_rollup
  in
  let* () = Client.bake_for_and_wait tezos_client in
  f sc_rollup sc_rollup_node configuration_filename

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
  regression_test
    ~__FILE__
    "configuration of a smart contract optimistic rollup node"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun _sc_rollup _sc_rollup_node filename ->
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
      @@ fun sc_rollup sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let* sc_rollup_from_rpc =
        sc_rollup_node_rpc sc_rollup_node "global/sc_rollup_address"
      in
      match sc_rollup_from_rpc with
      | None ->
          (* No curl, no check. *)
          failwith "Please install curl"
      | Some sc_rollup_from_rpc ->
          let sc_rollup_from_rpc = JSON.as_string sc_rollup_from_rpc in
          if sc_rollup_from_rpc <> sc_rollup then
            failwith
              (Printf.sprintf
                 "Expecting %s, got %s when we query the sc rollup node RPC \
                  address"
                 sc_rollup
                 sc_rollup_from_rpc)
          else return ())

(* Interacting with a rollup node through a rollup client
   ------------------------------------------------------

   When a rollup node is running, a rollup client can ask this
   node its rollup address.
*)
let test_rollup_client_gets_address =
  regression_test
    ~__FILE__
    ~tags:["run"; "client"]
    "getting a smart-contract rollup address through the client"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun sc_rollup sc_rollup_node _filename ->
      let* () = Sc_rollup_node.run sc_rollup_node in
      let sc_client = Sc_rollup_client.create sc_rollup_node in
      let* sc_rollup_from_client =
        Sc_rollup_client.sc_rollup_address sc_client
      in
      if sc_rollup_from_client <> sc_rollup then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s when the client asks for the sc rollup \
              address"
             sc_rollup
             sc_rollup_from_client) ;
      return ())

(* Fetching the initial level of a sc rollup
    -----------------------------------------

   We can fetch the level when a smart contract rollup was
   originated from the context.
*)
let test_rollup_get_genesis_info =
  regression_test
    ~__FILE__
    ~tags:["genesis_info"]
    "get genesis info of a sc rollup"
    (fun protocol ->
      setup ~protocol @@ fun node client bootstrap ->
      let* current_level = RPC.get_current_level client in
      ( with_fresh_rollup @@ fun sc_rollup _sc_rollup_node _filename ->
        (* Bake 10 blocks to be sure that the initial level of rollup is different
           from the current level. *)
        let* _ = repeat 10 (fun () -> Client.bake_for_and_wait client) in
        let* genesis_info =
          RPC.Client.call client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
        in
        (* 1 Block for activating alpha + 1 block for originating the rollup
           the rollup initial level should be 2 *)
        Check.(
          (JSON.(genesis_info |-> "level" |> as_int)
          = JSON.as_int (JSON.get "level" current_level) + 1)
            int
            ~error_msg:"expected value %L, got %R") ;
        return () )
        node
        client
        bootstrap)

(* Fetching the last cemented commitment info for a sc rollup
    ----------------------------------------------------------

   We can fetch the hash and level of the last cemented commitment. Initially,
   this corresponds to `(Sc_rollup.Commitment_hash.zero, origination_level)`.
*)

(* TODO: https://gitlab.com/tezos/tezos/-/issues/2944
   Revisit this test once the rollup node can cement commitments. *)
let test_rollup_get_last_cemented_commitment_hash_with_level =
  regression_test
    ~__FILE__
    ~tags:["lcc_hash_with_level"]
    "get last cemented commitment hash and inbox level of a sc rollup"
    (fun protocol ->
      setup ~protocol @@ fun node client bootstrap ->
      ( with_fresh_rollup @@ fun sc_rollup _sc_rollup_node _filename ->
        let* origination_level = RPC.get_current_level client in

        (* Bake 10 blocks to be sure that the origination_level of rollup is different
           from the level of the head node. *)
        let* () = repeat 10 (fun () -> Client.bake_for_and_wait client) in
        let* hash, level =
          last_cemented_commitment_hash_with_level ~sc_rollup client
        in
        let* genesis_info =
          RPC.Client.call client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
        in
        let genesis_hash =
          JSON.(genesis_info |-> "commitment_hash" |> as_string)
        in
        Check.(
          (hash = genesis_hash) string ~error_msg:"expected value %L, got %R") ;
        (* The level of the last cemented commitment should correspond to the
           rollup origination level. *)
        Check.(
          (level = JSON.(origination_level |-> "level" |> as_int))
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
let send_message client sc_rollup msg =
  let* () =
    Client.Sc_rollup.send_message
      ~hooks
      ~src:"bootstrap1"
      ~dst:sc_rollup
      ~msg
      client
  in
  Client.bake_for_and_wait client

let send_messages ?batch_size n sc_rollup client =
  let messages =
    List.map
      (fun i ->
        let batch_size = match batch_size with None -> i | Some v -> v in
        let json =
          `A (List.map (fun _ -> `String "CAFEBABE") (range 1 batch_size))
        in
        "text:" ^ Ezjsonm.to_string json)
      (range 1 n)
  in
  Lwt_list.iter_s (fun msg -> send_message client sc_rollup msg) messages

let to_text_messages_arg msgs =
  let text_messages =
    List.map (fun msg -> Hex.of_string msg |> Hex.show) msgs
  in
  let json = Ezjsonm.list Ezjsonm.string text_messages in
  "text:" ^ Ezjsonm.to_string ~minify:true json

let send_text_messages client sc_rollup msgs =
  send_message client sc_rollup (to_text_messages_arg msgs)

let parse_inbox json =
  let go () =
    let inbox = JSON.as_object json in
    return
      ( List.assoc "current_level_hash" inbox |> JSON.as_string,
        List.assoc "nb_messages_in_commitment_period" inbox |> JSON.as_int )
  in
  Lwt.catch go @@ fun exn ->
  failwith
    (Printf.sprintf
       "Unable to parse inbox %s\n%s"
       (JSON.encode json)
       (Printexc.to_string exn))

let get_inbox_from_tezos_node sc_rollup client =
  let* inbox = RPC.Client.call client @@ RPC.Sc_rollup.get_inbox sc_rollup in
  parse_inbox inbox

let get_inbox_from_sc_rollup_node sc_rollup_node =
  let* inbox = sc_rollup_node_rpc sc_rollup_node "global/inbox" in
  match inbox with
  | None -> failwith "Unable to retrieve inbox from sc rollup node"
  | Some inbox -> parse_inbox inbox

let test_rollup_inbox_size =
  regression_test
    ~__FILE__
    ~tags:["inbox"]
    "pushing messages in the inbox - check inbox size"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup _sc_rollup_node _filename ->
        let n = 10 in
        let* () = send_messages n sc_rollup client in
        let* _, inbox_msg_during_commitment_period =
          get_inbox_from_tezos_node sc_rollup client
        in
        return
        @@ Check.(
             (inbox_msg_during_commitment_period = n * (n + 1) / 2)
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

  (* An external message is prefixed with a tag whose length is one byte, and
     whose value is 1. *)
  let encode_external_message message =
    let prefix = "\001" in
    Bytes.of_string (prefix ^ message)

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
        let payload = encode_external_message message in
        let* tree = add tree ["message"; key] payload in
        build_current_messages_tree (Z.succ counter) tree rest

  module P = Tezos_protocol_alpha.Protocol

  let predict_current_messages_hash level current_messages =
    let open P.Alpha_context.Sc_rollup in
    let open Lwt.Syntax in
    let level_bytes =
      Data_encoding.Binary.to_bytes_exn
        P.Raw_level_repr.encoding
        (P.Raw_level_repr.of_int32_exn level)
    in
    let* tree = add (empty ()) ["level"] level_bytes in
    let* tree = build_current_messages_tree Z.zero tree current_messages in
    let context_hash = hash tree in
    let test =
      Data_encoding.Binary.to_bytes_exn
        Tezos_base.TzPervasives.Context_hash.encoding
        context_hash
    in
    return (Inbox.Hash.of_bytes_exn test)
end

let fetch_messages_from_block sc_rollup client =
  let* ops = RPC.get_operations client in
  let messages =
    ops |> JSON.as_list
    |> List.concat_map JSON.as_list
    |> List.concat_map (fun op -> JSON.(op |-> "contents" |> as_list))
    |> List.filter_map (fun op ->
           if
             JSON.(op |-> "kind" |> as_string) = "sc_rollup_add_messages"
             && JSON.(op |-> "rollup" |> as_string) = sc_rollup
           then Some JSON.(op |-> "message" |> as_list)
           else None)
    |> List.hd
    |> List.map (fun message -> JSON.(message |> as_string))
  in
  return messages

let test_rollup_inbox_current_messages_hash =
  regression_test
    ~__FILE__
    ~tags:["inbox"]
    "pushing messages in the inbox - current messages hash"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup _sc_rollup_node _filename ->
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
        let open Tezos_protocol_alpha.Protocol.Alpha_context.Sc_rollup in
        (* no messages have been sent *)
        let* pristine_hash, nb_available_messages =
          get_inbox_from_tezos_node sc_rollup client
        in
        let () =
          Check.((nb_available_messages = 0) int)
            ~error_msg:"0 messages expected in the inbox"
        in
        let* expected = Sc_rollup_inbox.predict_current_messages_hash 2l [] in
        let () =
          Check.(
            (Inbox.Hash.to_b58check expected = pristine_hash)
              string
              ~error_msg:"FIRST: expected pristine hash %L, got %R")
        in
        (*
           send messages, and assert that
           - the hash has changed
           - the hash matches the 'predicted' hash from the messages we sent
        *)
        let fst_batch = gen_message_batch 0 4 in
        let* () = send_message client sc_rollup @@ prepare_batch fst_batch in
        let* fst_batch_hash, _ = get_inbox_from_tezos_node sc_rollup client in
        let () =
          Check.(
            (pristine_hash <> fst_batch_hash)
              string
              ~error_msg:
                "expected current messages hash to change when messages sent")
        in
        let* expected =
          Sc_rollup_inbox.predict_current_messages_hash 3l fst_batch
        in
        let () =
          Check.(
            (Inbox.Hash.to_b58check expected = fst_batch_hash)
              string
              ~error_msg:"2 expected first batch hash %L, got %R")
        in
        (*
           send more messages, and assert that
           - the messages can be retrieved from the latest block
           - the hash matches the 'predicted' hash from the messages we sent
        *)
        let snd_batch = gen_message_batch 5 10 in
        let* () = send_message client sc_rollup @@ prepare_batch snd_batch in
        let* messages = fetch_messages_from_block sc_rollup client in
        let () =
          Check.(
            (messages = snd_batch)
              (list string)
              ~error_msg:"expected messages:\n%R\nretrieved:\n%L")
        in
        let* snd_batch_hash, _ = get_inbox_from_tezos_node sc_rollup client in
        let* expected =
          Sc_rollup_inbox.predict_current_messages_hash 4l snd_batch
        in
        let () =
          Check.(
            (Inbox.Hash.to_b58check expected = snd_batch_hash)
              string
              ~error_msg:"expected second batch hash %L, got %R")
        in
        unit )
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
  regression_test
    ~__FILE__
    ~tags:["inbox"; "node"; variant]
    (Printf.sprintf
       "observing the correct maintenance of inbox in the rollup node (%s)"
       variant)
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      ( with_fresh_rollup @@ fun sc_rollup sc_rollup_node _filename ->
        let* () = scenario protocol sc_rollup_node sc_rollup node client in
        let* inbox_from_sc_rollup_node =
          get_inbox_from_sc_rollup_node sc_rollup_node
        in
        let* inbox_from_tezos_node =
          get_inbox_from_tezos_node sc_rollup client
        in
        return
        @@ Check.(
             (inbox_from_sc_rollup_node = inbox_from_tezos_node)
               (tuple2 string int)
               ~error_msg:"expected value %R, got %L") )
        node
        client)

let basic_scenario _protocol sc_rollup_node sc_rollup _node client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent, we
       must end up at level 4. *)
    4
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  Log.info "before sending messages\n" ;
  let* () = send_messages num_messages sc_rollup client in
  let* level = Client.level client in
  Log.info "level: %d\n" level ;
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node expected_level in
  return ()

let sc_rollup_node_stops_scenario _protocol sc_rollup_node sc_rollup _node
    client =
  let num_messages = 2 in
  let expected_level =
    (* We start at level 2 and each message also bakes a block. With 2 messages being sent twice, we
       must end up at level 6. *)
    6
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages sc_rollup client in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  let* () = send_messages num_messages sc_rollup client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node expected_level in
  return ()

let sc_rollup_node_handles_chain_reorg protocol sc_rollup_node sc_rollup node
    client =
  let num_messages = 1 in

  setup ~protocol @@ fun node' client' _ ->
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node in
  let* () = send_messages num_messages sc_rollup client in
  (* Since we start at level 2, sending 1 message (which also bakes a block) must cause the nodes to
     observe level 3. *)
  let* _ = Node.wait_for_level node 3 in
  let* _ = Node.wait_for_level node' 3 in
  let* _ = Sc_rollup_node.wait_for_level sc_rollup_node 3 in
  Log.info "Nodes are synchronized." ;

  let divergence () =
    let* identity' = Node.wait_for_identity node' in
    let* () = Client.Admin.kick_peer client ~peer:identity' in
    let* () = send_messages num_messages sc_rollup client in
    (* +1 block for [node] *)
    let* _ = Node.wait_for_level node 4 in

    let* () = send_messages num_messages sc_rollup client' in
    let* () = send_messages num_messages sc_rollup client' in
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

(* One can retrieve the list of originated SCORUs.
   -----------------------------------------------
*)
let test_rollup_list =
  let open Lwt.Syntax in
  let go node client bootstrap1 =
    let* rollups = RPC.Client.call client @@ RPC.Sc_rollup.list () in
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
        let* () = Client.bake_for_and_wait client in
        let+ rollups = RPC.Client.call client @@ RPC.Sc_rollup.list () in
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

  regression_test
    ~__FILE__
    ~tags:["list"]
    "list originated rollups"
    (fun protocol -> setup ~protocol go)

(* Make sure the rollup node boots into the initial state.
   -------------------------------------------------------

   When a rollup node starts, we want to make sure that in the absence of
   messages it will boot into the initial state.
*)
let test_rollup_node_boots_into_initial_state =
  let go client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

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

  regression_test
    ~__FILE__
    ~tags:["run"; "node"]
    "node boots into the initial state"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        (fun sc_rollup sc_rollup_node _filename ->
          go client sc_rollup sc_rollup_node)
        node
        client)

(* Ensure the PVM is transitioning upon incoming messages.
   -------------------------------------------------------

   When the rollup node receives messages, we like to see evidence that the PVM
   has advanced.
*)
let test_rollup_node_advances_pvm_state =
  let go client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

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
      let* () =
        send_message
          client
          sc_rollup
          (Printf.sprintf "[\"%d %d + value\"]" i ((i + 2) * 2))
      in
      let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (level + i) in
      let* encoded_value =
        Sc_rollup_client.state_value ~hooks sc_rollup_client ~key:"vars/value"
      in
      let value =
        match Data_encoding.(Binary.of_bytes int31) @@ encoded_value with
        | Error error ->
            failwith
              (Format.asprintf
                 "The arithmetic PVM has an unexpected state: %a"
                 Data_encoding.Binary.pp_read_error
                 error)
        | Ok x -> x
      in
      Check.(
        (value = i + ((i + 2) * 2))
          int
          ~error_msg:"Invalid value in rollup state (%L <> %R)") ;
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

  regression_test
    ~__FILE__
    ~tags:["run"; "node"]
    "node advances PVM state with messages"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        (fun sc_rollup sc_rollup_node _filename ->
          go client sc_rollup sc_rollup_node)
        node
        client)

(* Ensure that commitments are stored and published properly.
   ----------------------------------------------------------

   Every 20 level, a commitment is computed and stored by the
   rollup node. The rollup node will also publish previously
   computed commitments on the layer1, in a first in first out
   fashion. To ensure that commitments are robust to chain
   reorganisations, only finalized block are processed when
   trying to publish a commitment.
*)

let bake_levels n client = repeat n @@ fun () -> Client.bake_for_and_wait client

let check_eq_commitment (c1 : Sc_rollup_client.commitment)
    (c2 : Sc_rollup_client.commitment) =
  Check.(c1.predecessor = c2.predecessor)
    Check.string
    ~error_msg:"Commitments differ in inbox_level (%L = %R)" ;
  Check.(c1.compressed_state = c2.compressed_state)
    Check.string
    ~error_msg:"Commitments differ in inbox_level (%L = %R)" ;
  Check.(c1.inbox_level = c2.inbox_level)
    Check.int
    ~error_msg:"Commitments differ in inbox_level (%L = %R)" ;
  Check.(c1.number_of_ticks = c2.number_of_ticks)
    Check.int
    ~error_msg:"Commitments differ in inbox_level (%L = %R)"

let tezos_client_get_commitment client sc_rollup commitment_hash =
  let* output =
    Client.rpc
      Client.GET
      [
        "chains";
        "main";
        "blocks";
        "head";
        "context";
        "sc_rollup";
        sc_rollup;
        "commitment";
        commitment_hash;
      ]
      client
  in
  Lwt.return @@ Sc_rollup_client.commitment_from_json output

let check_published_commitment_in_l1 ?(force_new_level = true) sc_rollup client
    published_commitment =
  let* () =
    if force_new_level then
      (* Triggers injection into the L1 context *)
      bake_levels 1 client
    else Lwt.return_unit
  in
  let* commitment_in_l1 =
    match published_commitment with
    | None -> Lwt.return_none
    | Some (hash, _commitment, _level) ->
        tezos_client_get_commitment client sc_rollup hash
  in
  Option.iter (fun (c1, c2) -> check_eq_commitment c1 c2)
  @@ Option.bind published_commitment (fun (_, c1, _level) ->
         Option.map (fun c2 -> (c1, c2)) commitment_in_l1) ;
  Lwt.return_unit

let test_commitment_scenario ?commitment_period ?challenge_window variant =
  test_scenario
    ?commitment_period
    ?challenge_window
    {
      tags = ["commitment"; "node"];
      variant;
      description =
        "observing the correct handling of commitments in the rollup node";
    }

let commitment_stored _protocol sc_rollup_node sc_rollup _node client =
  (* The rollup is originated at level `init_level`, and it requires
     `sc_rollup_commitment_period_in_blocks` levels to store a commitment.
     There is also a delay of `block_finality_time` before storing a
     commitment, to avoid including wrong commitments due to chain
     reorganisations. Therefore the commitment will be stored and published
     when the [Commitment] module processes the block at level
     `init_level + sc_rollup_commitment_period_in_blocks +
     levels_to_finalise`.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let store_commitment_level =
    init_level + levels_to_commitment + block_finality_time
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () =
    (* at init_level + i we publish i messages, therefore at level
       init_level + i a total of 1+..+i = (i*(i+1))/2 messages will have been
       sent.
    *)
    send_messages levels_to_commitment sc_rollup client
  in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (init_level + levels_to_commitment)
  in
  (* Bake [block_finality_time] additional levels to ensure that block number
     [init_level + sc_rollup_commitment_period_in_blocks] is
     processed by the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node store_commitment_level
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (levels_to_commitment + init_level))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  Option.iter (fun (c1, c2) -> check_eq_commitment c1 c2)
  @@ Option.bind published_commitment (fun (_hash, c1, _level) ->
         Option.map (fun (_, c2, _level) -> (c1, c2)) stored_commitment) ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let commitment_not_stored_if_non_final _protocol sc_rollup_node sc_rollup _node
    client =
  (* The rollup is originated at level `init_level`, and it requires
     `sc_rollup_commitment_period_in_blocks` levels to store a commitment.
     There is also a delay of `block_finality_time` before storing a
     commitment, to avoid including wrong commitments due to chain
     reorganisations. Therefore the commitment will be stored and published
     when the [Commitment] module processes the block at level
     `init_level + sc_rollup_commitment_period_in_blocks +
     levels_to_finalise`. At the level before, the commitment will not be
     neither stored nor published.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let levels_to_finalize = block_finality_time - 1 in
  let store_commitment_level = init_level + levels_to_commitment in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = send_messages levels_to_commitment sc_rollup client in
  let* _ =
    Sc_rollup_node.wait_for_level sc_rollup_node store_commitment_level
  in
  let* () = bake_levels levels_to_finalize client in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (store_commitment_level + levels_to_finalize)
  in
  let* commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level commitment in
  Check.(stored_inbox_level = None)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  let* commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let published_inbox_level = Option.map inbox_level commitment in
  Check.(published_inbox_level = None)
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been published at a level different than expected (%L = \
       %R)" ;
  Lwt.return_unit

let commitments_messages_reset _protocol sc_rollup_node sc_rollup _node client =
  (* For `sc_rollup_commitment_period_in_blocks` levels after the sc rollup
     origination, i messages are sent to the rollup, for a total of
     `sc_rollup_commitment_period_in_blocks *
     (sc_rollup_commitment_period_in_blocks + 1)/2` messages. These will be
     the number of messages in the first commitment published by the rollup
     node. Then, for other `sc_rollup_commitment_period_in_blocks` levels,
     no messages are sent to the sc-rollup address. The second commitment
     published by the sc-rollup node will contain 0 messages. Finally,
     `block_finality_time` empty levels are baked which ensures that two
     commitments are stored and published by the rollup node.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () =
    (* At init_level + i we publish i messages, therefore at level
       init_level + 20 a total of 1+..+20 = (20*21)/2 = 210 messages
       will have been sent.
    *)
    send_messages levels_to_commitment sc_rollup client
  in
  (* Bake other `sc_rollup_commitment_period_in_blocks +
     block_finality_time` levels with no messages. The first
     `sc_rollup_commitment_period_in_blocks` levels contribute to the second
     commitment stored by the rollup node. The last `block_finality_time`
     levels ensure that the second commitment is stored and published by the
     rollup node.
  *)
  let* () = bake_levels (levels_to_commitment + block_finality_time) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (init_level + (2 * levels_to_commitment) + block_finality_time)
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (init_level + (2 * levels_to_commitment)))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  (let stored_number_of_ticks = Option.map number_of_ticks stored_commitment in
   Check.(stored_number_of_ticks = Some 0)
     (Check.option Check.int)
     ~error_msg:
       "Number of messages processed by commitment is different from the \
        number of messages expected (%L = %R)") ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  Option.iter (fun (c1, c2) -> check_eq_commitment c1 c2)
  @@ Option.bind published_commitment (fun (_hash, c1, _level) ->
         Option.map (fun (_hash, c2, _level) -> (c1, c2)) stored_commitment) ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

let commitments_reorgs protocol sc_rollup_node sc_rollup node client =
  (* No messages are published after origination, for
     `sc_rollup_commitment_period_in_blocks - 1` levels. Then a divergence
     occurs:  in the first branch one message is published for
     `block_finality_time - 1` blocks. In the second branch no messages are
     published for `block_finality_time` blocks. The second branch is
     the more attractive one, and will be chosen when a reorganisation occurs.
     One more level is baked to ensure that the rollup node stores and
     publishes the commitment. The final commitment should have
     no messages and no ticks.
  *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* levels_to_commitment =
    get_sc_rollup_commitment_period_in_blocks client
  in
  let num_empty_blocks = block_finality_time in
  let num_messages = 1 in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in

  setup ~protocol @@ fun node' client' _ ->
  let* () = Client.Admin.trust_address client ~peer:node'
  and* () = Client.Admin.trust_address client' ~peer:node in
  let* () = Client.Admin.connect_address client ~peer:node' in

  let* () = Sc_rollup_node.run sc_rollup_node in
  (* We bake `sc_rollup_commitment_period_in_blocks - 1` levels, which
     should cause both nodes to observe level
     `sc_rollup_commitment_period_in_blocks + init_level - 1 . *)
  let* () = bake_levels (levels_to_commitment - 1) client in
  let* _ = Node.wait_for_level node (init_level + levels_to_commitment - 1) in
  let* _ = Node.wait_for_level node' (init_level + levels_to_commitment - 1) in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (init_level + levels_to_commitment - 1)
  in
  Log.info "Nodes are synchronized." ;

  let divergence () =
    let* identity' = Node.wait_for_identity node' in
    let* () = Client.Admin.kick_peer client ~peer:identity' in
    let* () = send_messages num_messages sc_rollup client in
    (* `block_finality_time - 1` blocks with message for [node] *)
    let* _ =
      Node.wait_for_level
        node
        (init_level + levels_to_commitment - 1 + num_messages)
    in

    let* () = bake_levels num_empty_blocks client' in
    (* `block_finality_time` blocks with no messages for [node'] *)
    let* _ =
      Node.wait_for_level
        node'
        (init_level + levels_to_commitment - 1 + num_empty_blocks)
    in
    Log.info "Nodes are following distinct branches." ;
    return ()
  in

  let trigger_reorg () =
    let* () = Client.Admin.connect_address client ~peer:node' in
    let* _ =
      Node.wait_for_level
        node
        (init_level + levels_to_commitment - 1 + num_empty_blocks)
    in
    Log.info "Nodes are synchronized again." ;
    return ()
  in

  let* () = divergence () in
  let* () = trigger_reorg () in
  (* After triggering a reorganisation the node should see that there is a more
     attractive head at level `init_level +
     sc_rollup_commitment_period_in_blocks + block_finality_time - 1`.
  *)
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (init_level + levels_to_commitment - 1 + num_empty_blocks)
  in
  (* exactly one level left to finalize the commitment in the node. *)
  let* () = bake_levels (block_finality_time - num_empty_blocks + 1) client in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (init_level + levels_to_commitment + block_finality_time)
  in
  let* stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let stored_inbox_level = Option.map inbox_level stored_commitment in
  Check.(stored_inbox_level = Some (init_level + levels_to_commitment))
    (Check.option Check.int)
    ~error_msg:
      "Commitment has been stored at a level different than expected (%L = %R)" ;
  (let stored_number_of_ticks = Option.map number_of_ticks stored_commitment in
   Check.(stored_number_of_ticks = Some 0)
     (Check.option Check.int)
     ~error_msg:
       "Number of messages processed by commitment is different from the \
        number of messages expected (%L = %R)") ;
  let* published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  Option.iter (fun (c1, c2) -> check_eq_commitment c1 c2)
  @@ Option.bind published_commitment (fun (_hash, c1, _level) ->
         Option.map (fun (_hash, c2, _level) -> (c1, c2)) stored_commitment) ;
  check_published_commitment_in_l1 sc_rollup client published_commitment

type balances = {liquid : int; frozen : int}

let contract_balances ~pkh client =
  let*! liquid = RPC.Contracts.get_balance ~contract_id:pkh client in
  let*! frozen = RPC.Contracts.get_frozen_bonds ~contract_id:pkh client in
  return {liquid = JSON.as_int liquid; frozen = JSON.as_int frozen}

(** This helper allow to attempt recovering bond for SCORU rollup operator.
    if [expect_failure] is set to some string then, we expect the command to fail
    with an error that contains that string. *)
let attempt_withdraw_stake =
  let check_eq_int a b =
    Check.((a = b) int ~error_msg:"expected value %L, got %R")
  in
  fun ?expect_failure ~sc_rollup client ->
    (* placehoders *)
    (* TODO/Fixme:
        - Shoud provide the rollup operator key (bootstrap1_key) as an
          argument to scenarios.
    *)
    let bootstrap1_key = Constant.bootstrap1.public_key_hash in
    let* constants = RPC.get_constants ~hooks client in
    let recover_bond_unfreeze =
      JSON.(constants |-> "sc_rollup_stake_amount" |> as_int)
    in
    let recover_bond_fee = 1_000_000 in
    let inject_op () =
      Client.Sc_rollup.submit_recover_bond
        ~hooks
        ~rollup:sc_rollup
        ~src:bootstrap1_key
        ~fee:(Tez.of_mutez_int recover_bond_fee)
        client
    in
    match expect_failure with
    | None ->
        let*! () = inject_op () in
        let* old_bal = contract_balances ~pkh:bootstrap1_key client in
        let* () = Client.bake_for_and_wait ~keys:["bootstrap2"] client in
        let* new_bal = contract_balances ~pkh:bootstrap1_key client in
        let expected_liq_new_bal =
          old_bal.liquid - recover_bond_fee + recover_bond_unfreeze
        in
        check_eq_int new_bal.liquid expected_liq_new_bal ;
        check_eq_int new_bal.frozen (old_bal.frozen - recover_bond_unfreeze) ;
        unit
    | Some failure_string ->
        let*? p = inject_op () in
        Process.check_error ~msg:(rex failure_string) p

(* FIXME: https://gitlab.com/tezos/tezos/-/issues/2942
   Do not pass an explicit value for `?commitment_period until
   https://gitlab.com/tezos/tezos/-/merge_requests/5212 has been merged. *)
(* Test that nodes do not publish commitments before the last cemented commitment. *)
let commitment_before_lcc_not_published _protocol sc_rollup_node sc_rollup node
    client =
  let* constants = get_sc_rollup_constants client in
  let commitment_period = constants.commitment_period_in_blocks in
  let challenge_window = constants.challenge_window_in_blocks in
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in

  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = bake_levels commitment_period client in
  let* commitment_inbox_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + commitment_period)
  in
  (* Bake `block_finality_time` additional level to ensure that block number
     `init_level + sc_rollup_commitment_period_in_blocks` is processed by
     the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* commitment_finalized_level =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (commitment_inbox_level + block_finality_time)
  in
  let* rollup_node1_stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let () =
    Check.(
      Option.map inbox_level rollup_node1_published_commitment
      = Some commitment_inbox_level)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  (* Cement commitment manually: the commitment can be cemented after
     `challenge_window_levels` have passed since the commitment was published
     (that is at level `commitment_finalized_level`). Note that at this point
     we are already at level `commitment_finalized_level`, hence cementation of
     the commitment can happen. *)
  let levels_to_cementation = challenge_window + 1 in
  let cemented_commitment_hash =
    Option.map hash rollup_node1_published_commitment
    |> Option.value
         ~default:"scc12XhSULdV8bAav21e99VYLTpqAjTd7NU8Mn4zFdKPSA8auMbggG"
  in
  let* () = bake_levels levels_to_cementation client in
  let* cemented_commitment_level =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (commitment_finalized_level + levels_to_cementation)
  in

  (* Withdraw stake before cementing should fail *)
  let* () =
    attempt_withdraw_stake
      ~sc_rollup
      client
      ~expect_failure:
        "Attempted to withdraw while not staked on the last cemented \
         commitment."
  in

  let* () =
    cement_commitment client ~sc_rollup ~hash:cemented_commitment_hash
  in
  let* level_after_cementation =
    Sc_rollup_node.wait_for_level sc_rollup_node (cemented_commitment_level + 1)
  in

  (* Withdraw stake after cementing should succeed *)
  let* () = attempt_withdraw_stake ~sc_rollup client in

  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create node client' ~operator_pkh:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create sc_rollup_node' in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node' sc_rollup
  in
  let* () = Sc_rollup_node.run sc_rollup_node' in

  let* rollup_node2_catchup_level =
    Sc_rollup_node.wait_for_level sc_rollup_node' level_after_cementation
  in
  Check.(rollup_node2_catchup_level = level_after_cementation)
    Check.int
    ~error_msg:"Current level has moved past cementation inbox level (%L = %R)" ;
  (* Check that no commitment was published. *)
  let* rollup_node2_last_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    Option.map inbox_level rollup_node2_last_published_commitment
  in
  let () =
    Check.(rollup_node2_last_published_commitment_inbox_level = None)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  (* Check that the commitment stored by the second rollup node
     is the same commmitment stored by the first rollup node. *)
  let* rollup_node2_stored_commitment =
    Sc_rollup_client.last_stored_commitment ~hooks sc_rollup_client'
  in
  let () =
    Check.(
      Option.map hash rollup_node1_stored_commitment
      = Option.map hash rollup_node2_stored_commitment)
      (Check.option Check.string)
      ~error_msg:
        "Commitment stored by first and second rollup nodes differ (%L = %R)"
  in

  (* Bake other commitment_period levels and check that rollup_node2 is
     able to publish a commitment. *)
  let* () = bake_levels commitment_period client' in
  let commitment_inbox_level = commitment_inbox_level + commitment_period in
  let* _ =
    Sc_rollup_node.wait_for_level
      sc_rollup_node'
      (level_after_cementation + commitment_period)
  in
  let* rollup_node2_last_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let rollup_node2_last_published_commitment_inbox_level =
    Option.map inbox_level rollup_node2_last_published_commitment
  in
  let () =
    Check.(
      rollup_node2_last_published_commitment_inbox_level
      = Some commitment_inbox_level)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)"
  in
  let () =
    Check.(
      Option.map predecessor rollup_node2_last_published_commitment
      = Some cemented_commitment_hash)
      (Check.option Check.string)
      ~error_msg:
        "Predecessor fo commitment published by rollup_node2 should be the \
         cemented commitment (%L = %R)"
  in
  return ()

(* Test that the level when a commitment was first published is fetched correctly
   by rollup nodes. *)
let first_published_level_is_global _protocol sc_rollup_node sc_rollup node
    client =
  (* Rollup node 1 processes messages, produces and publishes two commitments. *)
  let* genesis_info =
    RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
  in
  let init_level = JSON.(genesis_info |-> "level" |> as_int) in
  let* commitment_period = get_sc_rollup_commitment_period_in_blocks client in
  let* () = Sc_rollup_node.run sc_rollup_node in
  let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
  let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in
  Check.(level = init_level)
    Check.int
    ~error_msg:"Current level has moved past origination level (%L = %R)" ;
  let* () = bake_levels commitment_period client in
  let* commitment_inbox_level =
    Sc_rollup_node.wait_for_level sc_rollup_node (init_level + commitment_period)
  in
  (* Bake `block_finality_time` additional level to ensure that block number
     `init_level + sc_rollup_commitment_period_in_blocks` is processed by
     the rollup node as finalized. *)
  let* () = bake_levels block_finality_time client in
  let* commitment_finalized_level =
    Sc_rollup_node.wait_for_level
      sc_rollup_node
      (commitment_inbox_level + block_finality_time)
  in
  let* rollup_node1_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client
  in
  let () =
    Check.(
      Option.map inbox_level rollup_node1_published_commitment
      = Some commitment_inbox_level)
      (Check.option Check.int)
      ~error_msg:
        "Commitment has been published at a level different than expected (%L \
         = %R)" ;
    Check.(
      Option.bind rollup_node1_published_commitment first_published_at_level
      <> None)
      (Check.option Check.int)
      ~error_msg:
        "Level at which commitment has first been published is undefined"
  in
  let* () = Sc_rollup_node.terminate sc_rollup_node in
  (* Rollup node 2 starts and processes enough levels to publish a commitment.*)
  let bootstrap2_key = Constant.bootstrap2.public_key_hash in
  let* client' = Client.init ?endpoint:(Some (Node node)) () in
  let sc_rollup_node' =
    Sc_rollup_node.create node client' ~operator_pkh:bootstrap2_key
  in
  let sc_rollup_client' = Sc_rollup_client.create sc_rollup_node' in
  let* _configuration_filename =
    Sc_rollup_node.config_init sc_rollup_node' sc_rollup
  in
  let* () = Sc_rollup_node.run sc_rollup_node' in

  let* rollup_node2_catchup_level =
    Sc_rollup_node.wait_for_level sc_rollup_node' commitment_finalized_level
  in
  Check.(rollup_node2_catchup_level = commitment_finalized_level)
    Check.int
    ~error_msg:"Current level has moved past cementation inbox level (%L = %R)" ;
  (* Check that no commitment was published. *)
  let* rollup_node2_published_commitment =
    Sc_rollup_client.last_published_commitment ~hooks sc_rollup_client'
  in
  let* () =
    Option.iter (fun (c1, c2) -> check_eq_commitment c1 c2)
    @@ Option.bind rollup_node1_published_commitment (fun (_, c1, _level) ->
           rollup_node2_published_commitment
           |> Option.map (fun (_, c2, _level) -> (c1, c2))) ;
    Lwt.return_unit
  in
  let () =
    Check.(
      Option.bind rollup_node1_published_commitment first_published_at_level
      = Option.bind rollup_node2_published_commitment first_published_at_level)
      (Check.option Check.int)
      ~error_msg:
        "Rollup nodes do not agree on level when commitment was first \
         published (%L = %R)"
  in
  return ()

(* Check that the SC rollup is correctly originated with a boot sector.
   -------------------------------------------------------

   Originate a rollup with a custom boot sector and check if the RPC returns it.
*)
let test_rollup_origination_boot_sector =
  let boot_sector = "10 10 10 + +" in

  let go client sc_rollup =
    let* client_boot_sector =
      RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_boot_sector sc_rollup
    in
    let client_boot_sector = JSON.as_string client_boot_sector in
    Check.(boot_sector = client_boot_sector)
      Check.string
      ~error_msg:"expected value %L, got %R" ;
    Lwt.return_unit
  in

  regression_test
    ~__FILE__
    ~tags:["run"]
    "originate with boot sector"
    (fun protocol ->
      setup ~protocol @@ fun node client ->
      with_fresh_rollup
        ~boot_sector
        (fun sc_rollup _sc_rollup_node _filename -> go client sc_rollup)
        node
        client)

(* Check that a node makes use of the boot sector.
   -------------------------------------------------------

   Originate 2 rollups with different boot sectors to check if the are
   actually different.
*)
let test_rollup_node_uses_boot_sector =
  let go_boot client sc_rollup sc_rollup_node =
    let* genesis_info =
      RPC.Client.call ~hooks client @@ RPC.Sc_rollup.get_genesis_info sc_rollup
    in
    let init_level = JSON.(genesis_info |-> "level" |> as_int) in

    let* () = Sc_rollup_node.run sc_rollup_node in

    let sc_rollup_client = Sc_rollup_client.create sc_rollup_node in
    let* level = Sc_rollup_node.wait_for_level sc_rollup_node init_level in

    let* () = send_text_messages client sc_rollup ["10 +"] in
    let* _ = Sc_rollup_node.wait_for_level sc_rollup_node (level + 1) in

    Sc_rollup_client.state_hash ~hooks sc_rollup_client
  in

  let with_booted ~boot_sector node client =
    with_fresh_rollup
      ~boot_sector
      (fun sc_rollup sc_rollup_node _filename ->
        go_boot client sc_rollup sc_rollup_node)
      node
      client
  in

  regression_test
    ~__FILE__
    ~tags:["run"; "node"]
    "ensure boot sector is used"
    (fun protocol ->
      setup ~protocol @@ fun node client x ->
      let* state_hash1 =
        with_booted ~boot_sector:"10 10 10 + +" node client x
      in
      let* state_hash2 = with_booted ~boot_sector:"31" node client x in
      Check.(state_hash1 <> state_hash2)
        Check.string
        ~error_msg:"State hashes should be different! (%L, %R)" ;

      Lwt.return_unit)

(* Initializes a client with an existing account being
   [Constants.tz4_account]. *)
let client_with_initial_keys ~protocol =
  setup ~protocol @@ with_fresh_rollup
  @@ fun _sc_rollup sc_rollup_node _filename ->
  let sc_client = Sc_rollup_client.create sc_rollup_node in
  let account = Constant.tz4_account in
  let* () = Sc_rollup_client.import_secret_key account sc_client in
  return (sc_client, account)

(* Check that the client can show the address of a registered account.
   -------------------------------------------------------------------
*)
let test_rollup_client_show_address =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Shows the address of a registered account"
    (fun protocol ->
      let* sc_client, account = client_with_initial_keys ~protocol in
      let* shown_account =
        Sc_rollup_client.show_address
          ~alias:account.Account.aggregate_alias
          sc_client
      in
      if
        account.aggregate_public_key_hash
        <> shown_account.aggregate_public_key_hash
      then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as public key hash from the client."
             account.aggregate_public_key_hash
             shown_account.aggregate_public_key_hash)
      else if account.aggregate_public_key <> shown_account.aggregate_public_key
      then
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as public key from the client."
             account.aggregate_public_key
             shown_account.aggregate_public_key)
      else if account.aggregate_secret_key <> shown_account.aggregate_secret_key
      then
        let (Unencrypted sk) = shown_account.aggregate_secret_key in
        let (Unencrypted expected_sk) = shown_account.aggregate_secret_key in
        failwith
          (Printf.sprintf
             "Expecting %s, got %s as secret key from the client."
             expected_sk
             sk)
      else return ())

(* Check that the client can generate keys.
   ----------------------------------------
*)
let test_rollup_client_generate_keys =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Generates new tz4 keys"
    (fun protocol ->
      setup ~protocol @@ with_fresh_rollup
      @@ fun _sc_rollup sc_rollup_node _filename ->
      let sc_client = Sc_rollup_client.create sc_rollup_node in
      let alias = "test_key" in
      let* () = Sc_rollup_client.generate_keys ~alias sc_client in
      let* _account = Sc_rollup_client.show_address ~alias sc_client in
      return ())

(* Check that the client can list keys.
   ------------------------------------
*)
let test_rollup_client_list_keys =
  test
    ~__FILE__
    ~tags:["run"; "client"]
    "Lists known aliases in the client"
    (fun protocol ->
      let* sc_client, account = client_with_initial_keys ~protocol in
      let* maybe_keys = Sc_rollup_client.list_keys sc_client in
      let expected_keys =
        [(account.aggregate_alias, account.aggregate_public_key_hash)]
      in
      if List.equal ( = ) expected_keys maybe_keys then return ()
      else
        let pp ppf l =
          Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "\n")
            (fun ppf (a, k) -> Format.fprintf ppf "%s: %s" a k)
            ppf
            l
        in
        Test.fail
          ~__LOC__
          "Expecting\n@[%a@]\ngot\n@[%a@]\nas keys from the client."
          pp
          expected_keys
          pp
          maybe_keys)

let publish_dummy_commitment ~inbox_level ~predecessor ~sc_rollup ~src client =
  let commitment : Sc_rollup_client.commitment =
    {
      compressed_state = Constant.sc_rollup_compressed_state;
      inbox_level;
      predecessor;
      number_of_ticks = 1;
    }
  in

  let*! () = publish_commitment ~src ~commitment client sc_rollup in
  Client.bake_for_and_wait client

let test_consecutive_commitments =
  regression_test
    ~__FILE__
    ~tags:["l1"; "commitment"]
    "consecutive commitments"
    (fun protocol ->
      setup ~protocol @@ fun _node client bootstrap1_key ->
      let* sc_rollup =
        Client.Sc_rollup.originate
          ~hooks
          ~burn_cap:Tez.(of_int 9999999)
          ~src:bootstrap1_key
          ~kind:"arith"
          ~parameters_ty:"unit"
          ~boot_sector:""
          client
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* inbox_level = Client.level client in
      let* {commitment_period_in_blocks; _} = get_sc_rollup_constants client in
      let* () = Client.bake_for_and_wait client in
      (* As we did no publish any commitment yet, this is supposed to fail. *)
      let*? process =
        RPC.Client.spawn client
        @@ RPC.Sc_rollup.get_staked_on_commitment ~sc_rollup operator
      in
      let* () = Process.check_error ~msg:(rex "Unknown staker") process in
      let* predecessor, _ =
        last_cemented_commitment_hash_with_level ~sc_rollup client
      in
      let* () =
        publish_dummy_commitment
          ~inbox_level:(inbox_level + commitment_period_in_blocks + 1)
          ~predecessor
          ~sc_rollup
          ~src:operator
          client
      in
      (* We get the predecessor's hash by getting the commitment on which
         the operator just staked on. *)
      let* predecessor =
        get_staked_on_commitment ~sc_rollup ~staker:operator client
      in
      let* () =
        publish_dummy_commitment
          ~inbox_level:(inbox_level + (2 * commitment_period_in_blocks) + 1)
          ~predecessor
          ~sc_rollup
          ~src:operator
          client
      in
      unit)

let register ~protocols =
  test_origination protocols ;
  test_rollup_node_configuration protocols ;
  test_rollup_node_running protocols ;
  test_rollup_client_gets_address protocols ;
  test_rollup_list protocols ;
  test_rollup_get_genesis_info protocols ;
  test_rollup_get_last_cemented_commitment_hash_with_level protocols ;
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
  test_rollup_node_advances_pvm_state protocols ;
  test_commitment_scenario "commitment_is_stored" commitment_stored protocols ;
  test_commitment_scenario
    ~commitment_period:15
    ~challenge_window:10080
    "node_use_proto_param"
    commitment_stored
    protocols ;
  test_commitment_scenario
    "non_final_level"
    commitment_not_stored_if_non_final
    protocols ;
  test_commitment_scenario "messages_reset" commitments_messages_reset protocols ;
  test_commitment_scenario "handles_chain_reorgs" commitments_reorgs protocols ;
  test_commitment_scenario
    ~challenge_window:1
    "no_commitment_publish_before_lcc"
    (* TODO: https://gitlab.com/tezos/tezos/-/issues/2976
       change tests so that we do not need to repeat custom parameters. *)
    commitment_before_lcc_not_published
    protocols ;
  test_commitment_scenario
    "first_published_at_level_global"
    first_published_level_is_global
    protocols ;
  test_consecutive_commitments protocols ;
  test_rollup_origination_boot_sector protocols ;
  test_rollup_node_uses_boot_sector protocols ;
  test_rollup_client_show_address protocols ;
  test_rollup_client_generate_keys protocols ;
  test_rollup_client_list_keys protocols
