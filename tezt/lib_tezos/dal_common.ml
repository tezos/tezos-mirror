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
end

module Committee = struct
  type member = {attester : string; first_shard_index : int; power : int}

  type t = member list

  let typ =
    let open Check in
    list
    @@ convert
         (fun {attester; first_shard_index; power} ->
           (attester, first_shard_index, power))
         (tuple3 string int int)

  let at_level node ~level =
    let* json =
      Node.RPC.call node @@ RPC.get_chain_block_context_dal_shards ~level ()
    in
    return
    @@ List.map
         (fun json ->
           let pkh = JSON.(json |=> 0 |> as_string) in
           let first_shard_index = JSON.(json |=> 1 |=> 0 |> as_int) in
           let power = JSON.(json |=> 1 |=> 1 |> as_int) in
           {attester = pkh; first_shard_index; power})
         (JSON.as_list json)
end

module RPC_legacy = struct
  type default_uri_provider = (Dal_node.t, Endpoint.t) Either.t

  type local_uri_provider = Dal_node.t

  type remote_uri_provider = Endpoint.t

  let make ?data ?query_string = RPC_core.make ?data ?query_string

  (** [encode_bytes_for_json raw] encodes arbitrary byte sequence as hex string for JSON *)
  let encode_bytes_to_hex_string raw =
    "\"" ^ match Hex.of_string raw with `Hex s -> s ^ "\""

  let decode_hex_string_to_bytes s = Hex.to_string (`Hex s)

  let get_bytes_from_json_string_node json =
    JSON.as_string json |> decode_hex_string_to_bytes

  let slot_pages slot_header =
    make GET ["slot"; "pages"; slot_header] (fun pages ->
        pages |> JSON.as_list |> List.map get_bytes_from_json_string_node)

  let shard ~slot_header ~shard_id =
    make GET ["shard"; slot_header; string_of_int shard_id] @@ fun json ->
    json |> JSON.encode

  let shards ~slot_header shard_ids =
    let data : RPC_core.data =
      Data (`A (List.map (fun i -> `Float (float_of_int i)) shard_ids))
    in
    make ~data POST ["shards"; slot_header] (fun json ->
        JSON.(json |> as_list |> List.map encode))
end

module Dal_RPC = struct
  include RPC_legacy

  type commitment = string

  type operator_profile = Attester of string | Producer of int

  type operator_profiles = operator_profile list

  type profiles = Bootstrap | Operator of operator_profiles

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

  let slot_headers_of_json json =
    JSON.as_list json |> List.map slot_header_of_json

  let as_empty_object_or_fail t =
    match JSON.as_object t with
    | [] -> ()
    | _ -> JSON.error t "Not an empty object"

  let post_commitment slot =
    let slot =
      JSON.parse
        ~origin:"Dal_common.RPC.post_commitments"
        (encode_bytes_to_hex_string slot)
    in
    let data : RPC_core.data = Data (JSON.unannotate slot) in
    make ~data POST ["commitments"] JSON.as_string

  (* Converts a possibly invalid UTF-8 string into a JSON object using
     Data-encoding's unistring representation. *)
  let unistring_to_json s =
    let l =
      String.to_seq s
      |> Seq.map (fun c -> `Float (float_of_int @@ Char.code c))
      |> List.of_seq
    in
    `O [("invalid_utf8_string", `A l)]

  let post_slot slot =
    let data : RPC_core.data = Data (unistring_to_json slot) in
    make
      ~data
      POST
      ["slot"]
      JSON.(
        fun json ->
          ( json |-> "commitment" |> as_string,
            json |-> "commitment_proof" |> as_string ))

  let patch_commitment commitment ~slot_level ~slot_index =
    let data : RPC_core.data =
      Data
        (`O
          [
            ("slot_level", `Float (float_of_int slot_level));
            ("slot_index", `Float (float_of_int slot_index));
          ])
    in
    make ~data PATCH ["commitments"; commitment] as_empty_object_or_fail

  let get_commitment_slot commitment =
    make GET ["commitments"; commitment; "slot"] get_bytes_from_json_string_node

  let put_commitment_shards ?(with_proof = true) commitment =
    let data : RPC_core.data = Data (`O [("with_proof", `Bool with_proof)]) in
    make ~data PUT ["commitments"; commitment; "shards"] as_empty_object_or_fail

  type commitment_proof = string

  let get_commitment_proof commitment =
    make GET ["commitments"; commitment; "proof"] JSON.as_string

  let get_level_index_commitment ~slot_level ~slot_index =
    make
      GET
      [
        "levels";
        string_of_int slot_level;
        "slot_indices";
        string_of_int slot_index;
        "commitment";
      ]
      JSON.as_string

  let json_of_operator_profile = function
    | Attester pkh ->
        `O [("kind", `String "attester"); ("public_key_hash", `String pkh)]
    | Producer slot_index ->
        `O
          [
            ("kind", `String "producer");
            ("slot_index", `Float (float_of_int slot_index));
          ]

  let operator_profile_of_json json =
    let open JSON in
    match json |-> "kind" |> as_string with
    | "attester" -> Attester (json |-> "public_key_hash" |> as_string)
    | "producer" -> Producer (json |-> "slot_index" |> as_int)
    | _ -> failwith "invalid case"

  let profiles_of_json json =
    let open JSON in
    match json |-> "kind" |> as_string with
    | "bootstrap" -> Bootstrap
    | "operator" ->
        let operator_profiles =
          List.map
            operator_profile_of_json
            (json |-> "operator_profiles" |> as_list)
        in
        Operator operator_profiles
    | _ -> failwith "invalid case"

  let patch_profiles profiles =
    let data : RPC_core.data =
      Data (`A (List.map json_of_operator_profile profiles))
    in
    make ~data PATCH ["profiles"] as_empty_object_or_fail

  let get_profiles () = make GET ["profiles"] profiles_of_json

  let mk_query_arg ~to_string field v_opt =
    Option.fold ~none:[] ~some:(fun v -> [(field, to_string v)]) v_opt

  let get_commitment_headers ?slot_level ?slot_index commitment =
    let query_string =
      mk_query_arg ~to_string:string_of_int "slot_level" slot_level
      @ mk_query_arg ~to_string:string_of_int "slot_index" slot_index
    in
    make
      ~query_string
      GET
      ["commitments"; commitment; "headers"]
      slot_headers_of_json

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

  let get_published_level_headers ?status published_level =
    let query_string = mk_query_arg ~to_string:(fun s -> s) "status" status in
    make
      ~query_string
      GET
      ["levels"; string_of_int published_level; "headers"]
      slot_headers_of_json

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

  type topic = {topic_slot_index : int; topic_pkh : string}

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
        node rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
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
        endpoint rpc =
      RPC_core.call_raw
        ?rpc_hooks
        ?log_request
        ?log_response_status
        ?log_response_body
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

module Helpers = struct
  let endpoint dal_node =
    Printf.sprintf
      "http://%s:%d"
      (Dal_node.rpc_host dal_node)
      (Dal_node.rpc_port dal_node)

  let pad n message =
    let padding = String.make n '\000' in
    message ^ padding

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
    Cryptobox.Internal_for_tests.init_prover_dal () ;
    match Cryptobox.make parameters with
    | Ok cryptobox -> cryptobox
    | Error (`Fail msg) -> on_error msg

  let publish_commitment ?counter ?force ?source ?fee ?error ~index ~commitment
      ~proof client =
    (* We scale the fees to match the actual gas cost of publishing a slot header.
       Doing this here allows to keep the diff small as gas cost for
       publishing slot header is adjusted. *)
    let fee = Option.map (fun x -> x * 13) fee in
    Operation.Manager.(
      inject
        ?error
        ?force
        [
          make ?source ?fee ?counter
          @@ dal_publish_commitment ~index ~commitment ~proof;
        ]
        client)

  let store_slot dal_node_or_endpoint ?with_proof slot =
    let call = function
      | Either.Left node -> Dal_RPC.Local.call node
      | Either.Right endpoint -> Dal_RPC.Remote.call endpoint
    in
    (* Use the POST /slot RPC except if shard proof computation is
       explicitly deactivated with with_proof:false *)
    match with_proof with
    | None | Some true -> call dal_node_or_endpoint @@ Dal_RPC.post_slot slot
    | Some false ->
        let* commitment =
          call dal_node_or_endpoint @@ Dal_RPC.post_commitment slot
        in
        let* () =
          Dal_RPC.(
            call dal_node_or_endpoint
            @@ put_commitment_shards ~with_proof:false commitment)
        in
        let* proof =
          Dal_RPC.(call dal_node_or_endpoint @@ get_commitment_proof commitment)
        in
        return (commitment, proof)
end

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
      if padding_length > 0 then Helpers.pad padding_length message else message
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

module Check = struct
  open Dal_RPC

  let profiles_typ : profiles Check.typ =
    let pp_operator_profile ppf = function
      | Attester pkh -> Format.fprintf ppf "Attester %s" pkh
      | Producer slot_index -> Format.fprintf ppf "Producer %d" slot_index
    in
    let equal_operator_profile op1 op2 =
      match (op1, op2) with
      | Attester pkh1, Attester pkh2 -> String.equal pkh1 pkh2
      | Producer slot_index1, Producer slot_index2 ->
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
end

module RPC = Dal_RPC
