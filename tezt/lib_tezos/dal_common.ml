(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

module Cryptobox = Tezos_crypto_dal.Cryptobox

module Parameters = struct
  type t = {
    feature_enabled : bool;
    incentives_enabled : bool;
    cryptobox : Cryptobox.parameters;
    number_of_slots : int;
    attestation_lag : int;
    attestation_threshold : int;
    blocks_per_epoch : int;
        (* TODO: https://gitlab.com/tezos/tezos/-/issues/6923
           To be removed when [Protocol.previous_protocol Alpha >= P]. *)
  }

  let parameter_file protocol =
    let args = [(["dal_parametric"; "feature_enable"], `Bool true)] in
    Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args

  let from_protocol_parameters json =
    let json = JSON.(json |-> "dal_parametric") in
    let number_of_shards = JSON.(json |-> "number_of_shards" |> as_int) in
    let redundancy_factor = JSON.(json |-> "redundancy_factor" |> as_int) in
    let slot_size = JSON.(json |-> "slot_size" |> as_int) in
    let page_size = JSON.(json |-> "page_size" |> as_int) in
    let number_of_slots = JSON.(json |-> "number_of_slots" |> as_int) in
    let attestation_lag = JSON.(json |-> "attestation_lag" |> as_int) in
    let attestation_threshold =
      JSON.(json |-> "attestation_threshold" |> as_int)
    in
    let blocks_per_epoch =
      JSON.(json |-> "blocks_per_epoch" |> as_int_opt)
      |> Option.value ~default:1
    in
    let feature_enabled = JSON.(json |-> "feature_enable" |> as_bool) in
    let incentives_enabled =
      JSON.(json |-> "incentives_enable" |> as_bool_opt)
      |> Option.value ~default:false
    in
    {
      feature_enabled;
      incentives_enabled;
      cryptobox =
        Cryptobox.Verifier.
          {number_of_shards; redundancy_factor; slot_size; page_size};
      number_of_slots;
      attestation_lag;
      attestation_threshold;
      blocks_per_epoch;
    }

  let from_client client =
    let* json =
      Client.RPC.call client @@ RPC.get_chain_block_context_constants ()
    in
    from_protocol_parameters json |> return

  let full_storage_period_with_refutation_in_cycles ~proto_parameters =
    let blocks_per_cycle =
      JSON.(proto_parameters |-> "blocks_per_cycle" |> as_int)
    in
    let challenge_window =
      JSON.(
        proto_parameters |-> "smart_rollup_challenge_window_in_blocks" |> as_int)
    in
    let commitment_period =
      JSON.(
        proto_parameters |-> "smart_rollup_commitment_period_in_blocks"
        |> as_int)
    in
    let validity_lag =
      JSON.(
        proto_parameters |-> "smart_rollup_reveal_activation_level"
        |-> "dal_attested_slots_validity_lag" |> as_int)
    in
    let attestation_lag =
      JSON.(
        proto_parameters |-> "dal_parametric" |-> "attestation_lag" |> as_int)
    in
    let blocks =
      (2 * (challenge_window + commitment_period + validity_lag))
      + attestation_lag + 1
    in
    if blocks mod blocks_per_cycle = 0 then blocks / blocks_per_cycle
    else 1 + (blocks / blocks_per_cycle)

  let initial_storage_period_with_refutation_in_cycles ~proto_parameters =
    let blocks_per_cycle =
      JSON.(proto_parameters |-> "blocks_per_cycle" |> as_int)
    in
    let attestation_lag =
      JSON.(
        proto_parameters |-> "dal_parametric" |-> "attestation_lag" |> as_int)
    in
    let blocks = (3 * attestation_lag) + 1 in
    if blocks mod blocks_per_cycle = 0 then blocks / blocks_per_cycle
    else 1 + (blocks / blocks_per_cycle)

  let storage_period_without_refutation_in_cycles ~proto_parameters =
    let blocks_per_cycle =
      JSON.(proto_parameters |-> "blocks_per_cycle" |> as_int)
    in
    let attestation_lag =
      JSON.(
        proto_parameters |-> "dal_parametric" |-> "attestation_lag" |> as_int)
    in
    let blocks = 2 * attestation_lag in
    if blocks mod blocks_per_cycle = 0 then blocks / blocks_per_cycle
    else 1 + (blocks / blocks_per_cycle)
end

module Committee = struct
  type member = {attester : string; indexes : int list}

  type t = member list

  let typ =
    let open Check in
    list
    @@ convert
         (fun {attester; indexes} -> (attester, indexes))
         (tuple2 string (list int))

  let at_level node ?level ?delegates () =
    let* json =
      Node.RPC.call node
      @@ RPC.get_chain_block_context_dal_shards ?level ?delegates ()
    in
    return
    @@ List.map
         (fun json ->
           let attester = JSON.(json |-> "delegate" |> as_string) in
           let indexes =
             JSON.(json |-> "indexes" |> as_list |> List.map as_int)
           in
           {attester; indexes})
         (JSON.as_list json)

  let check_is_in ~__LOC__ node ?(inside = true) ?level pkh =
    let* committee = at_level node ?level ~delegates:[pkh] () in
    let in_committee =
      List.exists (fun member -> String.equal member.attester pkh) committee
    in
    Check.(
      (in_committee = inside)
        ~__LOC__
        bool
        ~error_msg:"The account is in the DAL committee? Expected %R, got %L") ;
    unit
end

module Dal_RPC = struct
  type default_uri_provider = (Dal_node.t, Endpoint.t) Either.t

  type local_uri_provider = Dal_node.t

  type remote_uri_provider = Endpoint.t

  let make ?data ?query_string = RPC_core.make ?data ?query_string

  let decode_hex_string_to_bytes s = Hex.to_string (`Hex s)

  let get_bytes_from_json_string_node json =
    JSON.as_string json |> decode_hex_string_to_bytes

  type commitment = string

  type operator_profile =
    | Attester of string
    | Producer of int
    | Observer of int

  type operator_profiles = operator_profile list

  type profile = Bootstrap | Operator of operator_profiles

  type slot_header = {
    slot_level : int;
    slot_index : int;
    commitment : string;
    status : string;
  }

  let slot_header_of_json json =
    let open JSON in
    {
      slot_level = json |-> "slot_level" |> as_int;
      slot_index = json |-> "slot_index" |> as_int;
      commitment = json |-> "commitment" |> as_string;
      status = json |-> "status" |> as_string;
    }

  let slot_header_to_json_u h : JSON.u =
    `O
      [
        ("slot_level", `Float (float_of_int h.slot_level));
        ("slot_index", `Float (float_of_int h.slot_index));
        ("commitment", `String h.commitment);
        ("status", `String h.status);
      ]

  let slot_headers_of_json json =
    JSON.as_list json |> List.map slot_header_of_json

  let as_empty_object_or_fail t =
    match JSON.as_object t with
    | [] -> ()
    | _ -> JSON.error t "Not an empty object"

  (* Converts a possibly invalid UTF-8 string into a JSON object using
     Data-encoding's unistring representation. *)
  let unistring_to_json s =
    let l =
      String.to_seq s
      |> Seq.map (fun c -> `Float (float_of_int @@ Char.code c))
      |> List.of_seq
    in
    `O [("invalid_utf8_string", `A l)]

  let post_slot ?slot_index slot =
    let data : RPC_core.data = Data (unistring_to_json slot) in
    make
      ~data
      ?query_string:
        (Option.map
           (fun slot_index -> [("slot_index", string_of_int slot_index)])
           slot_index)
      POST
      ["slots"]
      JSON.(
        fun json ->
          ( json |-> "commitment" |> as_string,
            json |-> "commitment_proof" |> as_string ))

  let get_level_slot_content ~slot_level ~slot_index =
    make
      GET
      [
        "levels";
        string_of_int slot_level;
        "slots";
        string_of_int slot_index;
        "content";
      ]
      get_bytes_from_json_string_node

  let get_level_slot_pages ~published_level ~slot_index =
    make
      GET
      [
        "levels";
        string_of_int published_level;
        "slots";
        string_of_int slot_index;
        "pages";
      ]
      (fun pages ->
        pages |> JSON.as_list |> List.map get_bytes_from_json_string_node)

  type commitment_proof = string

  let get_level_index_commitment ~slot_level ~slot_index =
    make
      GET
      [
        "levels";
        string_of_int slot_level;
        "slots";
        string_of_int slot_index;
        "commitment";
      ]
      JSON.as_string

  let json_of_operator_profile list =
    let attesters, producers, observers =
      List.fold_left
        (fun (attesters, producers, observers) -> function
          | Attester pkh -> (`String pkh :: attesters, producers, observers)
          | Producer slot_index ->
              ( attesters,
                `Float (float_of_int slot_index) :: producers,
                observers )
          | Observer slot_index ->
              ( attesters,
                producers,
                `Float (float_of_int slot_index) :: observers ))
        ([], [], [])
        list
    in
    `O
      [
        ("attesters", `A (List.rev attesters));
        ("operators", `A (List.rev producers));
        ("observers", `A (List.rev observers));
      ]

  let operator_profile_of_json json =
    let open JSON in
    let attesters = json |-> "attesters" |> as_list |> List.map as_string in
    let producers = json |-> "operators" |> as_list |> List.map as_int in
    let observers = json |-> "observers" |> as_list |> List.map as_int in
    List.map (fun pkh -> Attester pkh) attesters
    @ List.map (fun i -> Producer i) producers
    @ List.map (fun i -> Observer i) observers

  let profiles_of_json json =
    let open JSON in
    match json |-> "kind" |> as_string with
    | "bootstrap" -> Bootstrap
    | "controller" ->
        let operator_profiles =
          operator_profile_of_json (json |-> "controller_profiles")
        in
        Operator operator_profiles
    | _ -> failwith "invalid case"

  let patch_profiles profiles =
    let data : RPC_core.data = Data (json_of_operator_profile profiles) in
    make ~data PATCH ["profiles"] as_empty_object_or_fail

  let get_profiles () = make GET ["profiles"] profiles_of_json

  let get_level_slot_status ~slot_level ~slot_index =
    make
      GET
      [
        "levels";
        string_of_int slot_level;
        "slots";
        string_of_int slot_index;
        "status";
      ]
      JSON.as_string

  let get_assigned_shard_indices ~level ~pkh =
    make
      GET
      [
        "profiles";
        pkh;
        "attested_levels";
        string_of_int level;
        "assigned_shard_indices";
      ]
      (fun json -> JSON.(json |> as_list |> List.map as_int))

  type slot_set = bool list

  type attestable_slots = Not_in_committee | Attestable_slots of slot_set

  let get_attestable_slots ~attester ~attested_level =
    make
      GET
      [
        "profiles";
        attester.Account.public_key_hash;
        "attested_levels";
        string_of_int attested_level;
        "attestable_slots";
      ]
      (fun json ->
        JSON.(
          match get "kind" json |> as_string with
          | "not_in_committee" -> Not_in_committee
          | "attestable_slots_set" ->
              let json = get "attestable_slots_set" json in
              Attestable_slots (json |> as_list |> List.map as_bool)
          | _ -> failwith "invalid case"))

  let delete_p2p_peer_disconnect ~peer_id =
    make DELETE ["p2p"; "peers"; "disconnect"; peer_id] as_empty_object_or_fail

  let patch_p2p_peers_by_id ~peer_id ?acl () =
    let data =
      Option.map (fun acl -> RPC_core.Data (`O [("acl", `String acl)])) acl
    in
    make ?data PATCH ["p2p"; "peers"; "by-id"; peer_id] (fun _json -> ())

  type topic = {topic_slot_index : int; topic_pkh : string}

  let get_topics () =
    let open JSON in
    let as_topic json =
      let topic_slot_index = get "slot_index" json |> as_int in
      let topic_pkh = get "pkh" json |> as_string in
      {topic_slot_index; topic_pkh}
    in
    make ~query_string:[] GET ["p2p"; "gossipsub"; "topics"] (fun json ->
        JSON.(json |> as_list |> List.map as_topic))

  let get_topics_peers ~subscribed =
    let open JSON in
    let query_string = if subscribed then [("subscribed", "true")] else [] in
    let as_topic json =
      let topic_slot_index = get "slot_index" json |> as_int in
      let topic_pkh = get "pkh" json |> as_string in
      {topic_slot_index; topic_pkh}
    in
    let as_topic_and_peers json =
      let topic = get "topic" json |> as_topic in
      let peers = get "peers" json |> as_list |> List.map as_string in
      (topic, peers)
    in
    make ~query_string GET ["p2p"; "gossipsub"; "topics"; "peers"] (fun json ->
        JSON.(json |> as_list |> List.map as_topic_and_peers))

  let get_slot_indexes_peers ~subscribed =
    let open JSON in
    let query_string = if subscribed then [("subscribed", "true")] else [] in
    let as_slot_indexes_and_peers json =
      let topic = get "slot_index" json |> as_int in
      let peers = get "peers" json |> as_list |> List.map as_string in
      (topic, peers)
    in
    make
      ~query_string
      GET
      ["p2p"; "gossipsub"; "slot_indexes"; "peers"]
      (fun json -> JSON.(json |> as_list |> List.map as_slot_indexes_and_peers))

  let get_pkhs_peers ~subscribed =
    let open JSON in
    let query_string = if subscribed then [("subscribed", "true")] else [] in
    let as_pkhs_and_peers json =
      let topic = get "pkh" json |> as_string in
      let peers = get "peers" json |> as_list |> List.map as_string in
      (topic, peers)
    in
    make ~query_string GET ["p2p"; "gossipsub"; "pkhs"; "peers"] (fun json ->
        JSON.(json |> as_list |> List.map as_pkhs_and_peers))

  let get_gossipsub_connections () =
    make ~query_string:[] GET ["p2p"; "gossipsub"; "connections"] (fun x -> x)

  type peer_score = {peer : string; score : float}

  let get_scores () =
    make GET ["p2p"; "gossipsub"; "scores"] (fun json ->
        JSON.(
          json |> as_list
          |> List.map (fun json ->
                 {
                   peer = get "peer" json |> as_string;
                   score = get "score" json |> as_float;
                 })))

  let get_plugin_commitments_history_hash ~proto_hash ~hash () =
    make GET ["plugin"; proto_hash; "commitments_history"; "hash"; hash] Fun.id

  let get_level_slot_shard_content ~slot_level ~slot_index ~shard_index =
    make
      GET
      [
        "levels";
        string_of_int slot_level;
        "slots";
        string_of_int slot_index;
        "shards";
        string_of_int shard_index;
        "content";
      ]
    @@ fun json -> json |> JSON.encode

  module Local : RPC_core.CALLERS with type uri_provider := local_uri_provider =
  struct
    let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        node rpc =
      RPC_core.call
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (Dal_node.as_rpc_endpoint node)
        rpc

    let call_raw ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        ?extra_headers node rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        ?extra_headers
        (Dal_node.as_rpc_endpoint node)
        rpc

    let call_json ?rpc_hooks ?log_request ?log_response_status
        ?log_response_body node rpc =
      RPC_core.call_json
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        (Dal_node.as_rpc_endpoint node)
        rpc
  end

  module Remote :
    RPC_core.CALLERS with type uri_provider := remote_uri_provider = struct
    let call ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        endpoint rpc =
      RPC_core.call
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        endpoint
        rpc

    let call_raw ?rpc_hooks ?log_request ?log_response_status ?log_response_body
        ?extra_headers endpoint rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        ?extra_headers
        endpoint
        rpc

    let call_json ?rpc_hooks ?log_request ?log_response_status
        ?log_response_body endpoint rpc =
      RPC_core.call_json
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
        endpoint
        rpc
  end
end

let pad n message =
  let padding = String.make n '\000' in
  message ^ padding

module Commitment = struct
  let dummy_commitment
      ?(on_error =
        function
        | `Slot_wrong_size str ->
            Test.fail "Dal_common.dummy_commitment failed: %s" str
        | ( `Invalid_degree_strictly_less_than_expected _
          | `Prover_SRS_not_loaded ) as commit_error ->
            Test.fail "%s" (Cryptobox.string_of_commit_error commit_error))
      cryptobox message =
    let parameters = Cryptobox.Verifier.parameters cryptobox in
    let padding_length = parameters.slot_size - String.length message in
    let padded_message =
      if padding_length > 0 then pad padding_length message else message
    in
    let slot = String.to_bytes padded_message in
    let open Tezos_error_monad.Error_monad.Result_syntax in
    (let* p = Cryptobox.polynomial_from_slot cryptobox slot in
     let* cm = Cryptobox.commit cryptobox p in
     let* proof = Cryptobox.prove_commitment cryptobox p in
     Ok (cm, proof))
    |> function
    | Ok res -> res
    | Error e -> on_error e

  let to_string commitment = Cryptobox.Commitment.to_b58check commitment

  let of_string commitment =
    Cryptobox.Commitment.of_b58check_opt commitment
    |> mandatory "The b58check-encoded slot commitment is not valid"

  let proof_to_string proof =
    Data_encoding.Json.construct Cryptobox.Commitment_proof.encoding proof
    |> Data_encoding.Json.to_string

  let proof_of_string proof =
    Data_encoding.Json.destruct
      Cryptobox.Commitment_proof.encoding
      (`String proof)
end

module Helpers = struct
  let endpoint dal_node =
    Printf.sprintf
      "http://%s:%d"
      (Dal_node.rpc_host dal_node)
      (Dal_node.rpc_port dal_node)

  type slot = string

  let make_slot ?(padding = true) ~slot_size slot =
    let actual_slot_size = String.length slot in
    if actual_slot_size < slot_size && padding then
      pad (slot_size - actual_slot_size) slot
    else slot

  let content_of_slot slot =
    (* We make the assumption that the content of a slot (for test
       purpose only) does not contain two `\000` in a row. This
       invariant is ensured by [make_slot]. *)
    String.split_on_char '\000' slot
    |> List.filter (fun str -> not (str = String.empty))
    |> String.concat "\000"

  let slot_of_pages ~slot_size pages =
    let slot = String.concat "" pages in
    assert (String.length slot = slot_size) ;
    slot

  let pp_cryptobox_error fmt = function
    | `Fail message -> Format.fprintf fmt "Fail: %s" message
    | `Not_enough_shards message ->
        Format.fprintf fmt "Not enough shards: %s" message
    | `Shard_index_out_of_range message ->
        Format.fprintf fmt "Shard index out of range: %s" message
    | `Invalid_shard_length message ->
        Format.fprintf fmt "Invalid shard length: %s" message
    | `Invalid_page -> Format.fprintf fmt "Invalid page"
    | `Page_index_out_of_range -> Format.fprintf fmt "Page index out of range"
    | `Invalid_degree_strictly_less_than_expected _ ->
        Format.fprintf fmt "Invalid degree strictly less than expected"
    | `Page_length_mismatch -> Format.fprintf fmt "Page length mismatch"
    | `Slot_wrong_size message ->
        Format.fprintf fmt "Slot wrong size: %s" message
    | `Prover_SRS_not_loaded -> Format.fprintf fmt "Prover SRS not loaded"
    | `Shard_length_mismatch -> Format.fprintf fmt "Shard length mismatch"
    | `Invalid_shard -> Format.fprintf fmt "Invalid shard"

  let make_cryptobox
      ?(on_error =
        fun msg -> Test.fail "Dal_common.make: Unexpected error: %s" msg)
      parameters =
    match Cryptobox.make parameters with
    | Ok cryptobox -> return cryptobox
    | Error (`Fail msg) ->
        let parameters_json =
          Data_encoding.Json.construct Cryptobox.parameters_encoding parameters
        in
        on_error
        @@ Format.asprintf
             "%s,@ parameters: %a"
             msg
             Data_encoding.Json.pp
             parameters_json

  let publish_commitment ?dont_wait ?counter ?force ?source ?fee ?error ~index
      ~commitment ~proof client =
    (* We scale the fees to match the actual gas cost of publishing a slot header.
       Doing this here allows to keep the diff small as gas cost for
       publishing slot header is adjusted. *)
    let fee = Option.map (fun x -> x * 13) fee in
    Operation.Manager.(
      inject
        ?dont_wait
        ?error
        ?force
        [
          make ?source ?fee ?counter
          @@ dal_publish_commitment ~index ~commitment ~proof;
        ]
        client)

  let store_slot dal_node_or_endpoint slot =
    let call = function
      | Either.Left node -> Dal_RPC.Local.call node
      | Either.Right endpoint -> Dal_RPC.Remote.call endpoint
    in
    call dal_node_or_endpoint @@ Dal_RPC.post_slot slot

  let store_slot_uri dal_node_endpoint slot =
    store_slot (Either.Right dal_node_endpoint) slot

  (* We override store slot so that it uses a DAL node in this file. *)
  let store_slot dal_node slot =
    match Dal_node.runner dal_node with
    | None -> store_slot (Either.Left dal_node) slot
    | Some runner ->
        let endpoint =
          Endpoint.
            {
              host = runner.Runner.address;
              scheme = "http";
              port = Dal_node.rpc_port dal_node;
            }
        in
        store_slot (Either.Right endpoint) slot

  let publish_and_store_slot ?dont_wait ?counter ?force ?(fee = 1_200) client
      dal_node source ~index content =
    (* We override store slot so that it uses a DAL node in this file. *)
    let* commitment_string, proof = store_slot dal_node content in
    let commitment = Commitment.of_string commitment_string in
    let proof = Commitment.proof_of_string proof in
    let* _ =
      publish_commitment
        ?dont_wait
        ?counter
        ?force
        ~source
        ~fee
        ~index
        ~commitment
        ~proof
        client
    in
    return commitment_string

  let wait_for_gossipsub_worker_event ~name dal_node lambda =
    Dal_node.wait_for dal_node (sf "gossipsub_worker_event-%s.v0" name) lambda

  let check_new_connection_event ~main_node ?other_peer_id ~other_node
      ~is_trusted () =
    let ( let*?? ) a b = Option.bind a b in
    let check_expected expected found =
      if expected <> found then None else Some ()
    in
    let* peer_id =
      wait_for_gossipsub_worker_event
        ~name:"new_connection"
        main_node
        (fun event ->
          let*?? peer_id = JSON.(event |-> "peer" |> as_string_opt) in
          let*?? () =
            check_expected is_trusted JSON.(event |-> "trusted" |> as_bool)
          in
          match other_peer_id with
          | None ->
              (* No expected peer id, event is considered valid and its peer id is returned *)
              Some peer_id
          | Some other_peer_id ->
              if other_peer_id = peer_id then Some peer_id
              else
                (* A connection was received from an unexpected peer,
                   discard the event. *)
                None)
    in
    let* other_node_id = Dal_node.read_identity other_node in
    let () =
      Check.(peer_id = other_node_id)
        ~__LOC__
        Check.string
        ~error_msg:"Expected a connection from the peer of id %R, got %L."
    in
    unit

  let connect_nodes_via_p2p ?(init_config = false) dal_node1 dal_node2 =
    let* () =
      if init_config then
        Dal_node.init_config ~peers:[Dal_node.listen_addr dal_node1] dal_node2
      else Lwt.return_unit
    in
    (* We ensure that [dal_node1] connects to [dal_node2]. *)
    let conn_ev_in_node1 =
      check_new_connection_event
        ~main_node:dal_node1
        ~other_node:dal_node2
        ~is_trusted:false
        ()
    in
    let* () = Dal_node.run dal_node2 in
    Log.info
      "Node %s started. Waiting for connection with node %s"
      (Dal_node.name dal_node2)
      (Dal_node.name dal_node1) ;
    conn_ev_in_node1
end

module Check = struct
  open Dal_RPC

  let profiles_typ : profile Check.typ =
    let pp_operator_profile ppf = function
      | Attester pkh -> Format.fprintf ppf "Attester %s" pkh
      | Producer slot_index -> Format.fprintf ppf "Producer %d" slot_index
      | Observer slot_index -> Format.fprintf ppf "Observer %d" slot_index
    in
    let equal_operator_profile op1 op2 =
      match (op1, op2) with
      | Attester pkh1, Attester pkh2 -> String.equal pkh1 pkh2
      | Producer slot_index1, Producer slot_index2 ->
          Int.equal slot_index1 slot_index2
      | Observer slot_index1, Observer slot_index2 ->
          Int.equal slot_index1 slot_index2
      | _, _ -> false
    in
    let pp ppf = function
      | Bootstrap -> Format.fprintf ppf "Bootstrap"
      | Operator operator_profiles ->
          Format.fprintf
            ppf
            "Operator [%a]"
            (Format.pp_print_list pp_operator_profile)
            operator_profiles
    in
    let equal p1 p2 =
      match (p1, p2) with
      | Bootstrap, Bootstrap -> true
      | Operator ops1, Operator ops2 ->
          let ops1 = List.sort compare ops1 in
          let ops2 = List.sort compare ops2 in
          List.equal equal_operator_profile ops1 ops2
      | _, _ -> false
    in
    Check.equalable pp equal

  let topic_typ : topic Check.typ =
    Check.convert
      (fun {topic_slot_index; topic_pkh} -> (topic_slot_index, topic_pkh))
      (Check.tuple2 Check.int Check.string)

  let topics_peers_typ : (topic * string list) list Check.typ =
    Check.list (Check.tuple2 topic_typ (Check.list Check.string))

  let slot_header_typ : slot_header Check.typ =
    Check.convert slot_header_to_json_u Check.json_u

  let slot_headers_typ : slot_header list Check.typ = Check.list slot_header_typ
end

module RPC = Dal_RPC
