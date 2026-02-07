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

(* Use the node for RPC call when possible because it is much faster *)
module Parameters = struct
  type t = {
    feature_enabled : bool;
    incentives_enabled : bool;
    cryptobox : Cryptobox.parameters;
    number_of_slots : int;
    attestation_lag : int;
    attestation_lags : int list;
    attestation_threshold : int;
  }

  let parameter_file protocol =
    let args = [(["dal_parametric"; "feature_enable"], `Bool true)] in
    Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args

  let from_protocol_parameters protocol json =
    let json = JSON.(json |-> "dal_parametric") in
    let number_of_shards = JSON.(json |-> "number_of_shards" |> as_int) in
    let redundancy_factor = JSON.(json |-> "redundancy_factor" |> as_int) in
    let slot_size = JSON.(json |-> "slot_size" |> as_int) in
    let page_size = JSON.(json |-> "page_size" |> as_int) in
    let number_of_slots = JSON.(json |-> "number_of_slots" |> as_int) in
    let attestation_lag = JSON.(json |-> "attestation_lag" |> as_int) in
    let attestation_lags =
      if Protocol.number protocol >= 025 then
        JSON.(json |-> "attestation_lags" |> as_list |> List.map as_int)
      else [attestation_lag]
    in
    let attestation_threshold =
      JSON.(json |-> "attestation_threshold" |> as_int)
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
      attestation_lags;
      attestation_threshold;
    }

  let from_client protocol ?block client =
    let* json =
      Client.RPC.call_via_endpoint client
      @@ RPC.get_chain_block_context_constants ?block ()
    in
    from_protocol_parameters protocol json |> return

  let from_endpoint protocol endpoint =
    let* json =
      RPC_core.call endpoint @@ RPC.get_chain_block_context_constants ()
    in
    from_protocol_parameters protocol json |> return

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
    let default_block_storage = 150 in
    if default_block_storage mod blocks_per_cycle = 0 then
      default_block_storage / blocks_per_cycle
    else 1 + (default_block_storage / blocks_per_cycle)

  let storage_period_without_refutation_in_cycles ~proto_parameters =
    let blocks_per_cycle =
      JSON.(proto_parameters |-> "blocks_per_cycle" |> as_int)
    in
    let default_block_storage = 150 in
    if default_block_storage mod blocks_per_cycle = 0 then
      default_block_storage / blocks_per_cycle
    else 1 + (default_block_storage / blocks_per_cycle)
end

(* Encoding/decoding of the multi-lag Attestations format used in attestation
   operations (Dal_attestations_repr.t).

   Note that:
   - [number_of_lags = List.length attestation_lags]
   - [lag_index i] corresponds to [attestation_lags[i]]

   This module handles two distinct formats used by the protocol:

   1. Pre-025 format (simple bitset):
      - Each bit i represents whether slot i is attested
      - Example: "5" = 0b101 means slots 0 and 2 are attested

   2. Post-025 format (multi-lag):
      - Supports attestations for slots published at different levels
      - Each lag index corresponds to a different published level:
        * lag_index i -> slots published at (attested_level - attestation_lags[i])

   Encoding format:
   - Prefix: first [number_of_lags] bits indicate which lag indices have non-empty
     attestations
   - Data: for each non-empty lag (in order), [number_of_slots] bits representing
     which slots are attested for that lag *)
module Attestations = struct
  (* Encode using the before 025 simple bitset format. *)
  let encode_single_lag_before_025 dal_attestation =
    let aux (acc, n) b =
      let bit = if b then 1 else 0 in
      (acc lor (bit lsl n), n + 1)
    in
    Array.fold_left aux (0, 0) dal_attestation |> fst |> string_of_int

  (* Encode using the post-025 multi-lag format.

     [lag_index] specifies the only lag index to encode the attestations for.
     This determines the position of the prefix bit and the data offset.

     For a single set of attestations (bool array), we encode them at the
     specified [lag_index]. *)
  let encode_single_lag_after_025 ~lag_index ~number_of_lags dal_attestation =
    (* Check if any slot is attested *)
    let has_attestation = Array.exists Fun.id dal_attestation in
    if not has_attestation then "0"
    else
      (* Prefix: set the bit at [lag_index] to indicate this lag has data *)
      let prefix = Z.shift_left Z.one lag_index in
      (* Add data bits at offset [number_of_lags] *)
      let result =
        Array.fold_left
          (fun (slot_idx, z) is_attested ->
            let z' =
              if is_attested then
                Z.logor z (Z.shift_left Z.one (number_of_lags + slot_idx))
              else z
            in
            (slot_idx + 1, z'))
          (0, prefix)
          dal_attestation
        |> snd
      in
      Z.to_string result

  let encode_for_one_lag protocol ?lag_index dal_parameters dal_attestation =
    if Protocol.number protocol < 025 then
      encode_single_lag_before_025 dal_attestation
    else
      let number_of_lags =
        List.length dal_parameters.Parameters.attestation_lags
      in
      (* Default to max lag (last element of the list) if not specified *)
      let lag_index = Option.value ~default:(number_of_lags - 1) lag_index in
      encode_single_lag_after_025 ~lag_index ~number_of_lags dal_attestation

  let encode_multiple_lags_after_025 ~number_of_slots attestations_per_lag =
    let number_of_lags = Array.length attestations_per_lag in
    let lag_indices = List.init number_of_lags Fun.id in
    (* Build the prefix: bit i is set if lag_index i has any attestations *)
    let prefix =
      List.fold_left
        (fun acc lag_index ->
          let slots_array = attestations_per_lag.(lag_index) in
          let has_attestation = Array.exists Fun.id slots_array in
          if has_attestation then Z.logor acc (Z.shift_left Z.one lag_index)
          else acc)
        Z.zero
        lag_indices
    in
    if Z.equal prefix Z.zero then "0"
    else
      (* Build the data segments: for each non-empty lag, add number_of_slots bits *)
      let slot_indices = List.init number_of_slots Fun.id in
      let _, result =
        List.fold_left
          (fun (data_offset, acc) lag_index ->
            let slots_array = attestations_per_lag.(lag_index) in
            let has_attestation = Array.exists Fun.id slots_array in
            if not has_attestation then (data_offset, acc)
            else
              (* Add bits for each attested slot *)
              let acc' =
                List.fold_left
                  (fun z slot_idx ->
                    if slots_array.(slot_idx) then
                      Z.logor z (Z.shift_left Z.one (data_offset + slot_idx))
                    else z)
                  acc
                  slot_indices
              in
              (data_offset + number_of_slots, acc'))
          (number_of_lags, prefix)
          lag_indices
      in
      Z.to_string result

  let encode protocol parameters attestations_per_lag =
    if Protocol.number protocol < 025 then
      match attestations_per_lag with
      | [|dal_attestation|] -> encode_single_lag_before_025 dal_attestation
      | _ ->
          Test.fail
            "Multiple lags encoding only supported for protocols >= 025."
    else
      encode_multiple_lags_after_025
        ~number_of_slots:parameters.Parameters.number_of_slots
        attestations_per_lag

  (* Decode before 025 format: simple bitset where bit[i] = slot i is attested *)
  let decode_before_025 str =
    let attestation = Z.of_string str in
    let length = Z.numbits attestation in
    let array = Array.make length false in
    List.iter
      (fun i -> if Z.testbit attestation i then array.(i) <- true)
      (range 0 (length - 1)) ;
    array

  (* Decode post-025 multi-lag format into per-lag arrays.

     Returns an array of size [number_of_lags], where element [i] is a bool
     array of size [number_of_slots] representing the attested slots at lag
     index [i].

     The encoded format is:
     - Prefix: first [number_of_lags] bits indicate which lag indices have
       non-empty attestations (bit [i] set means lag index [i] has data)
     - Data: for each non-empty lag (in order of lag_index), [number_of_slots]
       bits representing attested slots. *)
  let decode_after_025 ~number_of_lags ~number_of_slots str =
    let z = Z.of_string str in
    let result =
      Array.init number_of_lags (fun _ -> Array.make number_of_slots false)
    in
    let lag_indices = List.init number_of_lags Fun.id in
    let slot_indices = List.init number_of_slots Fun.id in
    (* Parse prefix to find which lags have data, then extract slot bits.
       We track [data_offset] which advances by [number_of_slots] for each
       lag that has data. *)
    let _final_offset =
      List.fold_left
        (fun data_offset lag_index ->
          let lag_has_data = Z.testbit z lag_index in
          if lag_has_data then (
            List.iter
              (fun slot_index ->
                if Z.testbit z (data_offset + slot_index) then
                  result.(lag_index).(slot_index) <- true)
              slot_indices ;
            data_offset + number_of_slots)
          else data_offset)
        number_of_lags
        lag_indices
    in
    result

  let decode protocol parameters str =
    if Protocol.number protocol < 025 then [|decode_before_025 str|]
    else
      let number_of_lags = List.length parameters.Parameters.attestation_lags in
      let number_of_slots = parameters.number_of_slots in
      decode_after_025 ~number_of_lags ~number_of_slots str
end

(* Decoding of the multi-lag Slot_availability.t bitset format. It has the same
   format as Attestations. *)
module Slot_availability = struct
  let decode = Attestations.decode
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

  type controller_profile =
    | Attester of string
    | Operator of int
    | Observer of int

  type controller_profiles = controller_profile list

  type profile = Bootstrap | Controller of controller_profiles

  type slot_id_status =
    | Waiting_attestation
    | Attested of int (* of attestation lag *)
    | Unattested
    | Unpublished

  type slot_header = {
    slot_level : int;
    slot_index : int;
    commitment : string;
    status : string;
  }

  let pp_slot_id_status fmt = function
    | Waiting_attestation -> Format.fprintf fmt "Waiting_attestation"
    | Attested lag -> Format.fprintf fmt "Attested(lag:%d)" lag
    | Unattested -> Format.fprintf fmt "Unattested"
    | Unpublished -> Format.fprintf fmt "Unpublished"

  let slot_id_status_of_json =
    let legacy_attestation_lag = 8 in
    fun json ->
      match String.lowercase_ascii @@ JSON.as_string json with
      | "waiting_attestation" -> Waiting_attestation
      | "attested" -> Attested legacy_attestation_lag
      | "unattested" -> Unattested
      | "unpublished" -> Unpublished
      | exception _exn -> (
          match
            JSON.
              ( json |-> "kind" |> as_string,
                json |-> "attestation_lag" |> as_int )
          with
          | "attested", lag -> Attested lag
          | (exception _) | _ ->
              failwith @@ Format.sprintf "Unknown slot_id status")
      | s -> failwith @@ Format.sprintf "Unknown slot_id status %s" s

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

  let get_level_slot_commitment ~slot_level ~slot_index =
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

  let json_of_controller_profile list =
    let attesters, operators, observers =
      List.fold_left
        (fun (attesters, operators, observers) -> function
          | Attester pkh -> (`String pkh :: attesters, operators, observers)
          | Operator slot_index ->
              ( attesters,
                `Float (float_of_int slot_index) :: operators,
                observers )
          | Observer slot_index ->
              ( attesters,
                operators,
                `Float (float_of_int slot_index) :: observers ))
        ([], [], [])
        list
    in
    `O
      [
        ("attesters", `A (List.rev attesters));
        ("operators", `A (List.rev operators));
        ("observers", `A (List.rev observers));
      ]

  let controller_profile_of_json json =
    let open JSON in
    let attesters = json |-> "attesters" |> as_list |> List.map as_string in
    let operators = json |-> "operators" |> as_list |> List.map as_int in
    let observers = json |-> "observers" |> as_list |> List.map as_int in
    List.map (fun pkh -> Attester pkh) attesters
    @ List.map (fun i -> Operator i) operators
    @ List.map (fun i -> Observer i) observers

  let profiles_of_json json =
    let open JSON in
    match json |-> "kind" |> as_string with
    | "bootstrap" -> Bootstrap
    | "controller" ->
        let controller_profiles =
          controller_profile_of_json (json |-> "controller_profiles")
        in
        Controller controller_profiles
    | _ -> failwith "invalid case"

  let patch_profiles profiles =
    let data : RPC_core.data = Data (json_of_controller_profile profiles) in
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
      slot_id_status_of_json

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

  type trap = {delegate : string; slot_index : int}

  let trap_of_json json =
    JSON.
      {
        delegate = json |-> "delegate" |> as_string;
        slot_index = json |-> "slot_index" |> as_int;
      }

  let get_published_level_known_traps ~published_level ~pkh ~slot_index =
    let query_string =
      [("delegate", pkh); ("slot_index", string_of_int slot_index)]
    in
    make
      GET
      ["published_levels"; string_of_int published_level; "known_traps"]
      ~query_string
      (fun json -> JSON.(json |> as_list |> List.map trap_of_json))

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

  let get_topics_peers ?(all = false) () =
    let open JSON in
    let query_string = if all then [("all", "true")] else [] in
    let as_topic json =
      let topic_slot_index = get "slot_index" json |> as_int in
      let topic_pkh = get "pkh" json |> as_string in
      {topic_slot_index; topic_pkh}
    in
    let as_topic_and_peers json =
      let topic = get "topic" json |> as_topic in
      let peers =
        get "peers" json |> as_list
        |> List.map (fun x -> x |-> "peer_id" |> as_string)
      in
      (topic, peers)
    in
    make ~query_string GET ["p2p"; "gossipsub"; "topics"; "peers"] (fun json ->
        JSON.(json |> as_list |> List.map as_topic_and_peers))

  let get_slot_indexes_peers ?(all = false) () =
    let open JSON in
    let query_string = if all then [("all", "true")] else [] in
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

  let get_pkhs_peers ?(all = false) () =
    let open JSON in
    let query_string = if all then [("all", "true")] else [] in
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

  let bytes_of_slot = Bytes.of_string

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

  let init_prover ?__LOC__ () =
    let* init =
      Cryptobox.init_prover_dal
        ~find_srs_files:Tezos_base.Dal_srs.find_trusted_setup_files
        ~fetch_trusted_setup:false
        ()
    in
    match init with
    | Error e ->
        Test.fail
          ?__LOC__
          "init_prover failed: %a@."
          Tezos_error_monad.Error_monad.pp_print_trace
          e
    | Ok () -> unit

  let get_commitment_and_shards_with_proofs ?precomputation cryptobox ~slot =
    let res =
      Tezos_crypto_dal.Cryptobox.Internal_for_tests
      .get_commitment_and_shards_with_proofs
        ?precomputation
        cryptobox
        ~slot
    in
    match res with
    | Error err ->
        Test.fail
          "Unexpected error:@.%a@."
          Tezos_crypto_dal.Cryptobox.pp_error
          err
    | Ok v -> v

  let publish_commitment ?dont_wait ?counter ?force ?source ?fee ?gas_limit
      ?error ~index ~commitment ~proof client =
    Operation.Manager.(
      inject
        ?dont_wait
        ?error
        ?force
        [
          make ?source ?fee ?counter ?gas_limit
          @@ dal_publish_commitment ~index ~commitment ~proof;
        ]
        client)

  let store_slot dal_node_or_endpoint ~slot_index slot =
    let call = function
      | Either.Left node -> Dal_RPC.Local.call node
      | Either.Right endpoint -> Dal_RPC.Remote.call endpoint
    in
    call dal_node_or_endpoint @@ Dal_RPC.post_slot ~slot_index slot

  let store_slot_uri dal_node_endpoint ~slot_index slot =
    store_slot (Either.Right dal_node_endpoint) ~slot_index slot

  (* We override store slot so that it uses a DAL node in this file. *)
  let store_slot dal_node ~slot_index slot =
    match Dal_node.runner dal_node with
    | None -> store_slot ~slot_index (Either.Left dal_node) slot
    | Some runner ->
        let endpoint =
          Endpoint.make
            ~host:runner.Runner.address
            ~scheme:"http"
            ~port:(Dal_node.rpc_port dal_node)
            ()
        in
        store_slot ~slot_index (Either.Right endpoint) slot

  let publish_and_store_slot ?dont_wait ?counter ?force ?fee ?gas_limit client
      dal_node source ~index content =
    (* We override store slot so that it uses a DAL node in this file. *)
    let* commitment_string, proof =
      store_slot ~slot_index:index dal_node content
    in
    let commitment = Commitment.of_string commitment_string in
    let proof = Commitment.proof_of_string proof in
    let* _ =
      publish_commitment
        ?dont_wait
        ?counter
        ?force
        ~source
        ?fee
        ?gas_limit
        ~index
        ~commitment
        ~proof
        client
    in
    return commitment_string

  let wait_for_gossipsub_worker_event ~name dal_node lambda =
    Dal_node.wait_for dal_node (sf "dal_gs_%s.v0" name) lambda

  let check_expected expected found =
    if expected <> found then None else Some ()

  let check_disconnection_event dal_node ~peer_id =
    wait_for_gossipsub_worker_event
      ~name:"disconnection"
      dal_node
      (fun peer_event ->
        check_expected peer_id JSON.(peer_event |-> "peer_id" |> as_string))

  let check_new_connection_event ~main_node ?other_peer_id ~other_node
      ~is_trusted () =
    let ( let*?? ) a b = Option.bind a b in
    let* peer_id =
      wait_for_gossipsub_worker_event
        ~name:"new_connection"
        main_node
        (fun event ->
          let*?? peer_id =
            JSON.(event |-> "peer" |-> "peer_id" |> as_string_opt)
          in
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
    let* () = Dal_node.run ~event_level:`Debug dal_node2 in
    Log.info
      "Node %s started. Waiting for connection with node %s"
      (Dal_node.name dal_node2)
      (Dal_node.name dal_node1) ;
    conn_ev_in_node1
end

module Check = struct
  open Dal_RPC

  let profiles_typ : profile Check.typ =
    let pp_controller_profile ppf = function
      | Attester pkh -> Format.fprintf ppf "Attester %s" pkh
      | Operator slot_index -> Format.fprintf ppf "Operator %d" slot_index
      | Observer slot_index -> Format.fprintf ppf "Observer %d" slot_index
    in
    let equal_controller_profile c1 c2 =
      match (c1, c2) with
      | Attester pkh1, Attester pkh2 -> String.equal pkh1 pkh2
      | Operator slot_index1, Operator slot_index2 ->
          Int.equal slot_index1 slot_index2
      | Observer slot_index1, Observer slot_index2 ->
          Int.equal slot_index1 slot_index2
      | _, _ -> false
    in
    let pp ppf = function
      | Bootstrap -> Format.fprintf ppf "Bootstrap"
      | Controller controller_profiles ->
          Format.fprintf
            ppf
            "Controller [%a]"
            (Format.pp_print_list pp_controller_profile)
            controller_profiles
    in
    let equal p1 p2 =
      match (p1, p2) with
      | Bootstrap, Bootstrap -> true
      | Controller c1, Controller c2 ->
          let c1 = List.sort compare c1 in
          let c2 = List.sort compare c2 in
          List.equal equal_controller_profile c1 c2
      | _, _ -> false
    in
    Check.equalable pp equal

  let topic_typ : topic Check.typ =
    Check.convert
      (fun {topic_slot_index; topic_pkh} -> (topic_slot_index, topic_pkh))
      (Check.tuple2 Check.int Check.string)

  let topics_peers_typ : (topic * string list) list Check.typ =
    Check.list (Check.tuple2 topic_typ (Check.list Check.string))

  let slot_id_status_typ =
    Check.equalable Dal_RPC.pp_slot_id_status Stdlib.( = )

  let slot_header_typ : slot_header Check.typ =
    Check.convert slot_header_to_json_u Check.json_u

  let slot_headers_typ : slot_header list Check.typ = Check.list slot_header_typ
end

module RPC = Dal_RPC
