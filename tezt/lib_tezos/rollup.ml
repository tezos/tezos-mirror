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

module Dal = struct
  module Cryptobox = Tezos_crypto_dal.Cryptobox

  module Parameters = struct
    type t = {
      feature_enabled : bool;
      cryptobox : Cryptobox.parameters;
      number_of_slots : int;
      attestation_lag : int;
      attestation_threshold : int;
      blocks_per_epoch : int;
    }

    let parameter_file protocol =
      let args = [(["dal_parametric"; "feature_enable"], `Bool true)] in
      Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args

    let from_client client =
      let* json =
        RPC.Client.call client @@ RPC.get_chain_block_context_constants ()
        |> Lwt.map (fun json -> JSON.(json |-> "dal_parametric"))
      in
      let number_of_shards = JSON.(json |-> "number_of_shards" |> as_int) in
      let redundancy_factor = JSON.(json |-> "redundancy_factor" |> as_int) in
      let slot_size = JSON.(json |-> "slot_size" |> as_int) in
      let page_size = JSON.(json |-> "page_size" |> as_int) in
      let number_of_slots = JSON.(json |-> "number_of_slots" |> as_int) in
      let attestation_lag = JSON.(json |-> "attestation_lag" |> as_int) in
      let attestation_threshold =
        JSON.(json |-> "attestation_threshold" |> as_int)
      in
      let blocks_per_epoch = JSON.(json |-> "blocks_per_epoch" |> as_int) in
      let feature_enabled = JSON.(json |-> "feature_enable" |> as_bool) in
      return
        {
          feature_enabled;
          cryptobox =
            Cryptobox.Verifier.
              {number_of_shards; redundancy_factor; slot_size; page_size};
          number_of_slots;
          attestation_lag;
          attestation_threshold;
          blocks_per_epoch;
        }
  end

  let endpoint dal_node =
    Printf.sprintf
      "http://%s:%d"
      (Dal_node.rpc_host dal_node)
      (Dal_node.rpc_port dal_node)

  module Committee = struct
    type member = {attestor : string; first_shard_index : int; power : int}

    type t = member list

    let typ =
      let open Check in
      list
      @@ convert
           (fun {attestor; first_shard_index; power} ->
             (attestor, first_shard_index, power))
           (tuple3 string int int)

    let at_level node ~level =
      let* json =
        RPC.(call node @@ get_chain_block_context_dal_shards ~level ())
      in
      return
      @@ List.map
           (fun json ->
             let pkh = JSON.(json |=> 0 |> as_string) in
             let first_shard_index = JSON.(json |=> 1 |=> 0 |> as_int) in
             let power = JSON.(json |=> 1 |=> 1 |> as_int) in
             {attestor = pkh; first_shard_index; power})
           (JSON.as_list json)
  end

  let pad n message =
    let padding = String.make n '\000' in
    message ^ padding

  type slot = string

  let make_slot ?(padding = true) ~slot_size slot =
    if String.contains slot '\000' then
      Test.fail "make_slot: The content of a slot cannot contain `\000`" ;
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

  module RPC_legacy = struct
    let make ?data ?query_string =
      RPC.make
        ?data
        ?query_string
        ~get_host:Dal_node.rpc_host
        ~get_port:Dal_node.rpc_port
        ~get_scheme:(Fun.const "http")

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

  module RPC = struct
    include RPC_legacy

    type commitment = string

    type profile = Attestor of string

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
          ~origin:"Rollup.RPC.post_commitments"
          (encode_bytes_to_hex_string slot)
      in
      let data : RPC_core.data = Data (JSON.unannotate slot) in
      make ~data POST ["commitments"] JSON.as_string

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
      make
        GET
        ["commitments"; commitment; "slot"]
        get_bytes_from_json_string_node

    let put_commitment_shards ?(with_proof = false) commitment =
      let data : RPC_core.data = Data (`O [("with_proof", `Bool with_proof)]) in
      make
        ~data
        PUT
        ["commitments"; commitment; "shards"]
        as_empty_object_or_fail

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

    let json_of_profile = function
      | Attestor pkh ->
          `O [("kind", `String "attestor"); ("public_key_hash", `String pkh)]

    let profiles_of_json json =
      let json_field_value json ~field = JSON.(get field json |> as_string) in
      JSON.as_list json
      |> List.map (fun obj ->
             match json_field_value ~field:"kind" obj with
             | "attestor" ->
                 Attestor (json_field_value ~field:"public_key_hash" obj)
             | _ -> failwith "invalid case")

    let patch_profiles profiles =
      let data = Client.Data (`A (List.map json_of_profile profiles)) in
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

    let get_attestable_slots ~attestor ~attested_level =
      make
        GET
        [
          "profiles";
          attestor.Account.public_key_hash;
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
  end

  let make
      ?(on_error =
        fun msg -> Test.fail "Rollup.Dal.make: Unexpected error: %s" msg)
      parameters =
    let initialisation_parameters =
      Cryptobox.Internal_for_tests.parameters_initialisation parameters
    in
    Cryptobox.Internal_for_tests.load_parameters initialisation_parameters ;
    match Cryptobox.make parameters with
    | Ok cryptobox -> cryptobox
    | Error (`Fail msg) -> on_error msg

  module Commitment = struct
    let dummy_commitment
        ?(on_error =
          function
          | `Slot_wrong_size str ->
              Test.fail "Rollup.Dal.dummy_commitment failed: %s" str
          | `Invalid_degree_strictly_less_than_expected _ as commit_error ->
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
  end

  module Check = struct
    open RPC

    type profiles = RPC.profile list

    let profile_typ : profile Check.typ =
      Check.equalable
        (fun ppf (Attestor pkh) -> Format.fprintf ppf "(Attestor %s) " pkh)
        (fun (RPC.Attestor att1) (RPC.Attestor att2) -> String.equal att1 att2)

    let profiles_typ : profiles Check.typ =
      let open Check in
      let sort = List.sort compare in
      convert sort (list profile_typ)
  end
end
