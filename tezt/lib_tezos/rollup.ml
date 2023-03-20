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

open Runnable.Syntax

module Tx_rollup = struct
  type range = Empty of int | Interval of int * int

  type commitments_hashes = {message_hash : string; commitment_hash : string}

  let pp_range fmt = function
    | Empty x -> Format.fprintf fmt "next: %d" x
    | Interval (t, h) -> Format.fprintf fmt "oldest: %d newest: %d" t h

  let range_of_json json =
    if JSON.(json |-> "newest" |> is_null) then
      Empty JSON.(json |-> "next" |> as_int)
    else
      let tail = JSON.(json |-> "oldest" |> as_int) in
      let head = JSON.(json |-> "newest" |> as_int) in
      Interval (tail, head)

  let commitment_newest_hash_of_json json =
    let open JSON in
    if is_null json then None
    else
      Some
        {
          message_hash = json |-> "last_message_hash" |> as_string;
          commitment_hash = json |-> "commitment_hash" |> as_string;
        }

  type state = {
    finalized_commitments : range;
    unfinalized_commitments : range;
    uncommitted_inboxes : range;
    tezos_head_level : int option;
    commitment_newest_hash : string option;
    burn_per_byte : int;
    inbox_ema : int;
    last_removed_commitment_hashes : commitments_hashes option;
  }

  type inbox = {inbox_length : int; cumulated_size : int; merkle_root : string}

  type messages = {
    count : int;
    root : string;
    last_message_result_hash : string;
  }

  type commitment = {
    level : int;
    messages : messages;
    predecessor : string option;
    inbox_merkle_root : string;
  }

  type submitted_commitment = {
    commitment : commitment;
    commitment_hash : string;
    committer : string;
    submitted_at : int;
    finalized_at : int option;
  }

  type operation_content_payload = {
    qty : Int64.t;
    destination : string;
    ticket : string;
  }

  type l2_transfer = [`Transfer of operation_content_payload]

  type l2_withdraw = [`Withdraw of operation_content_payload]

  type operation_content = [l2_transfer | l2_withdraw]

  let operation_content_encoding : operation_content Data_encoding.t =
    let open Data_encoding in
    let payload_encoding =
      obj3
        (req "qty" int64)
        (req "destination" string)
        (req "ticket_hash" string)
    in
    union
      [
        case
          ~title:"transfer"
          (Tag 0)
          payload_encoding
          (function
            | `Transfer {qty; destination; ticket} ->
                Some (qty, destination, ticket)
            | _ -> None)
          (fun (qty, destination, ticket) ->
            `Transfer {qty; destination; ticket});
        case
          ~title:"withdraw"
          (Tag 1)
          payload_encoding
          (function
            | `Withdraw {qty; destination; ticket} ->
                Some (qty, destination, ticket)
            | _ -> None)
          (fun (qty, destination, ticket) ->
            `Withdraw {qty; destination; ticket});
      ]

  type operation = {
    signer : string;
    counter : int64 option;
    contents : operation_content list;
  }

  type deposit_content = {
    sender : string;
    destination : string;
    ticket_hash : string;
    amount : int64;
  }

  type deposit = [`Deposit of deposit_content]

  type batch = [`Batch of Hex.t]

  type message = [deposit | batch]

  let make_batch batch = `Batch (Hex.of_string batch)

  let make_deposit ~sender ~destination ~ticket_hash ~amount =
    `Deposit {sender; destination; ticket_hash; amount}

  let json_of_batch (`Hex message) = `O [("batch", `String message)]

  let json_of_deposit {sender; destination; ticket_hash; amount} =
    `O
      [
        ( "deposit",
          `O
            [
              ("sender", `String sender);
              ("destination", `String destination);
              ("ticket_hash", `String ticket_hash);
              ("amount", `String (Int64.to_string amount));
            ] );
      ]

  let json_of_message = function
    | `Batch batch -> json_of_batch batch
    | `Deposit deposit -> json_of_deposit deposit

  type withdraw = {claimer : string; ticket_hash : string; amount : int64}

  let json_of_withdraw {claimer; ticket_hash; amount} =
    `O
      [
        ("claimer", `String claimer);
        ("ticket_hash", `String ticket_hash);
        ("amount", `String (Int64.to_string amount));
      ]

  type ticket_dispatch_info = {
    contents : string;
    ty : string;
    ticketer : string;
    amount : int64;
    claimer : string;
  }

  let get_json_of_ticket_dispatch_info {contents; ty; ticketer; amount; claimer}
      client =
    let* contents_json = Client.convert_data_to_json ~data:contents client in
    let* ty_json = Client.convert_data_to_json ~data:ty client in
    return
      (`O
        [
          ("contents", contents_json);
          ("ty", ty_json);
          ("ticketer", `String ticketer);
          ("amount", `String (Int64.to_string amount));
          ("claimer", `String claimer);
        ])

  let get_state ?hooks ~rollup client =
    let parse json =
      let finalized_commitments =
        JSON.(json |-> "finalized_commitments" |> range_of_json)
      in
      let unfinalized_commitments =
        JSON.(json |-> "unfinalized_commitments" |> range_of_json)
      in
      let uncommitted_inboxes =
        JSON.(json |-> "uncommitted_inboxes" |> range_of_json)
      in
      let tezos_head_level = JSON.(json |-> "tezos_head_level" |> as_int_opt) in
      let commitment_newest_hash =
        JSON.(json |-> "commitment_newest_hash" |> as_string_opt)
      in
      let burn_per_byte = JSON.(json |-> "burn_per_byte" |> as_int) in
      let inbox_ema = JSON.(json |-> "inbox_ema" |> as_int) in
      let last_removed_commitment_hashes =
        JSON.(
          json |-> "last_removed_commitment_hashes"
          |> commitment_newest_hash_of_json)
      in
      {
        finalized_commitments;
        unfinalized_commitments;
        uncommitted_inboxes;
        tezos_head_level;
        commitment_newest_hash;
        burn_per_byte;
        inbox_ema;
        last_removed_commitment_hashes;
      }
    in
    RPC.Tx_rollup.get_state ?hooks ~rollup client |> Runnable.map parse

  let get_inbox ?hooks ~rollup ~level client =
    let parse json =
      if JSON.is_null json then None
      else
        let inbox_length = JSON.(json |-> "inbox_length" |> as_int) in
        let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
        let merkle_root = JSON.(json |-> "merkle_root" |> as_string) in
        Some {inbox_length; cumulated_size; merkle_root}
    in
    RPC.Tx_rollup.get_inbox ?hooks ~rollup ~level client |> Runnable.map parse

  let get_commitment ?hooks ?block ~rollup ~level client =
    let parse json =
      if JSON.is_null json then None
      else
        let commitment_json = JSON.(json |-> "commitment") in
        let level = JSON.(commitment_json |-> "level" |> as_int) in
        let messages_json = JSON.(commitment_json |-> "messages") in
        let count = JSON.(messages_json |-> "count" |> as_int) in
        let root = JSON.(messages_json |-> "root" |> as_string) in
        let last_message_result_hash =
          JSON.(messages_json |-> "last_message_result_hash" |> as_string)
        in
        let messages = {count; root; last_message_result_hash} in
        let predecessor =
          JSON.(commitment_json |-> "predecessor" |> as_string_opt)
        in
        let inbox_merkle_root =
          JSON.(commitment_json |-> "inbox_merkle_root" |> as_string)
        in
        let commitment = {level; messages; predecessor; inbox_merkle_root} in
        let commitment_hash = JSON.(json |-> "commitment_hash" |> as_string) in
        let committer = JSON.(json |-> "committer" |> as_string) in
        let submitted_at = JSON.(json |-> "submitted_at" |> as_int) in
        let finalized_at = JSON.(json |-> "finalized_at" |> as_int_opt) in
        Some
          {commitment; commitment_hash; committer; submitted_at; finalized_at}
    in
    RPC.Tx_rollup.get_commitment ?hooks ?block ~rollup ~level client
    |> Runnable.map parse

  let get_pending_bonded_commitments ?hooks ?block ~rollup ~pkh client =
    RPC.Tx_rollup.get_pending_bonded_commitments
      ?hooks
      ?block
      ~rollup
      ~pkh
      client

  let message_hash ?hooks ~message client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let data : RPC_core.data =
      Data (`O [("message", json_of_message message)])
    in
    RPC.Tx_rollup.Forge.Inbox.message_hash ?hooks ~data client
    |> Runnable.map parse

  let inbox_merkle_tree_hash ?hooks ~message_hashes client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let make_message (`Hash message) : JSON.u = `String message in
    let data : RPC_core.data =
      Data (`O [("message_hashes", `A (List.map make_message message_hashes))])
    in
    RPC.Tx_rollup.Forge.Inbox.merkle_tree_hash ?hooks ~data client
    |> Runnable.map parse

  let inbox_merkle_tree_path ?hooks ~message_hashes ~position client =
    let parse json = JSON.(json |-> "path") in
    let make_message (`Hash message) : JSON.u = `String message in
    let data : RPC_core.data =
      Data
        (`O
          [
            ("message_hashes", `A (List.map make_message message_hashes));
            ("position", `Float (float_of_int position));
          ])
    in
    RPC.Tx_rollup.Forge.Inbox.merkle_tree_path ?hooks ~data client
    |> Runnable.map parse

  let commitment_merkle_tree_hash ?hooks ~message_result_hashes client =
    let parse json = `Hash JSON.(json |-> "hash" |> as_string) in
    let make_message (`Hash message) : JSON.u = `String message in
    let data : RPC_core.data =
      Data
        (`O
          [
            ( "message_result_hashes",
              `A (List.map make_message message_result_hashes) );
          ])
    in
    let runnable =
      RPC.Tx_rollup.Forge.Commitment.merkle_tree_hash ?hooks ~data client
    in
    Runnable.map parse runnable

  let commitment_merkle_tree_path ?hooks ~message_result_hashes ~position client
      =
    let parse json = JSON.(json |-> "path") in
    let make_message (`Hash message) : JSON.u = `String message in
    let data : RPC_core.data =
      Data
        (`O
          [
            ( "message_result_hashes",
              `A (List.map make_message message_result_hashes) );
            ("position", `Float (float_of_int position));
          ])
    in
    let runnable =
      RPC.Tx_rollup.Forge.Commitment.merkle_tree_path ?hooks ~data client
    in
    Runnable.map parse runnable

  let withdraw_list_hash ?hooks ~withdrawals client =
    let parse json = JSON.(json |-> "hash" |> as_string) in
    let data : RPC_core.data =
      Data (`O [("withdraw_list", `A (List.map json_of_withdraw withdrawals))])
    in
    RPC.Tx_rollup.Forge.Withdraw.withdraw_list_hash ?hooks ~data client
    |> Runnable.map parse

  let message_result_hash ?hooks ~context_hash ~withdraw_list_hash client =
    let parse json = JSON.(json |-> "hash" |> as_string) in
    let data : RPC_core.data =
      Data
        (`O
          [
            ("context_hash", `String context_hash);
            ("withdraw_list_hash", `String withdraw_list_hash);
          ])
    in
    RPC.Tx_rollup.Forge.Commitment.message_result_hash ?hooks ~data client
    |> Runnable.map parse

  let compute_inbox_from_messages ?hooks messages client =
    let* message_hashes =
      Lwt_list.map_p
        (fun message ->
          let*! message_hash = message_hash ?hooks ~message client in
          return message_hash)
        messages
    in
    let*! (`Hash merkle_root) =
      inbox_merkle_tree_hash ?hooks ~message_hashes client
    in
    return
      {
        inbox_length = List.length messages;
        cumulated_size =
          List.map
            (function
              | `Batch (`Hex message) ->
                  (* In the Hex represented as a string, a byte is
                     encoded using two characters. *)
                  String.length message / 2
              | `Deposit _ ->
                  let sender_pkh_size = 65 in
                  let destination_bls_pkh_size = 36 in
                  let ticket_hash_size = 32 in
                  let amount_size = 8 in
                  sender_pkh_size + destination_bls_pkh_size + ticket_hash_size
                  + amount_size)
            messages
          |> List.fold_left ( + ) 0;
        merkle_root;
      }

  module Check = struct
    let range : range Check.typ = Check.equalable pp_range ( = )

    let state : state Check.typ =
      let open Check in
      convert
        (fun {
               last_removed_commitment_hashes;
               finalized_commitments;
               unfinalized_commitments;
               uncommitted_inboxes;
               commitment_newest_hash;
               tezos_head_level;
               burn_per_byte;
               inbox_ema;
             } ->
          ( finalized_commitments,
            unfinalized_commitments,
            uncommitted_inboxes,
            tezos_head_level,
            commitment_newest_hash,
            burn_per_byte,
            inbox_ema,
            Option.map
              (function r -> (r.message_hash, r.commitment_hash))
              last_removed_commitment_hashes ))
        (tuple8
           range
           range
           range
           (option int)
           (option string)
           int
           int
           (option (tuple2 string string)))

    let inbox : inbox Check.typ =
      let open Check in
      convert
        (fun {inbox_length; cumulated_size; merkle_root} ->
          (inbox_length, cumulated_size, merkle_root))
        (tuple3 int int string)

    let commitment : submitted_commitment Check.typ =
      let open Check in
      convert
        (fun {
               commitment =
                 {
                   level;
                   messages = {count; root; last_message_result_hash};
                   predecessor;
                   inbox_merkle_root;
                 };
               commitment_hash;
               committer;
               submitted_at;
               finalized_at;
             } ->
          ( ( level,
              (count, root, last_message_result_hash),
              predecessor,
              inbox_merkle_root ),
            commitment_hash,
            committer,
            submitted_at,
            finalized_at ))
        (tuple5
           (tuple4 int (tuple3 int string string) (option string) string)
           string
           string
           int
           (option int))

    let commitments_hashes : commitments_hashes Check.typ =
      let open Check in
      convert
        (fun r -> (r.message_hash, r.commitment_hash))
        (tuple2 string string)
  end

  module Parameters = struct
    type t = {finality_period : int; withdraw_period : int}

    let default = {finality_period = 60_000; withdraw_period = 60_000}

    let parameter_file ?(parameters = default) protocol =
      let args =
        [(["tx_rollup_enable"], `Bool true)]
        @ [(["tx_rollup_finality_period"], `Int parameters.finality_period)]
        @ [(["tx_rollup_withdraw_period"], `Int parameters.withdraw_period)]
      in
      Protocol.write_parameter_file ~base:(Either.right (protocol, None)) args
  end
end

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

    let cryptobox_config_to_json (t : Cryptobox.Config.t) =
      let parameters =
        match t.use_mock_srs_for_testing with
        | Some parameters ->
            `O
              [
                ("slot_size", `Float (float_of_int parameters.slot_size));
                ("page_size", `Float (float_of_int parameters.page_size));
                ( "redundancy_factor",
                  `Float (float_of_int parameters.redundancy_factor) );
                ( "number_of_shards",
                  `Float (float_of_int parameters.number_of_shards) );
              ]
        | None -> `Null
      in
      JSON.annotate
        ~origin:"dal_initialisation"
        (`O
          [
            ("activated", `Bool t.activated);
            ("use_mock_srs_for_testing", parameters);
          ])
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

    let patch_profile profile =
      let data = Client.Data (json_of_profile profile) in
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

module Dac = struct
  module RPC = struct
    let make ?data ?query_string =
      RPC.make
        ?data
        ?query_string
        ~get_host:Dac_node.rpc_host
        ~get_port:Dac_node.rpc_port
        ~get_scheme:(Fun.const "http")

    (** [encode_bytes_for_json raw] encodes arbitrary byte sequence as hex string for JSON *)
    let encode_bytes_to_hex_string raw =
      "\"" ^ match Hex.of_string raw with `Hex s -> s ^ "\""

    let decode_hex_string_to_bytes s = Hex.to_string (`Hex s)

    let get_bytes_from_json_string_node json =
      JSON.as_string json |> decode_hex_string_to_bytes

    let dac_store_preimage ~payload ~pagination_scheme =
      let preimage =
        JSON.parse
          ~origin:"dal_node_dac_store_preimage_rpc"
          (Format.sprintf
             {|{"payload":%s,"pagination_scheme":"%s"}|}
             (encode_bytes_to_hex_string payload)
             pagination_scheme)
      in
      let data : RPC_core.data = Data (JSON.unannotate preimage) in
      make ~data POST ["store_preimage"] @@ fun json ->
      JSON.
        ( json |-> "root_hash" |> as_string,
          json |-> "external_message" |> get_bytes_from_json_string_node )

    let dac_verify_signature external_msg =
      let query_string =
        [
          ( "external_message",
            match Hex.of_string external_msg with `Hex s -> s );
        ]
      in
      make ~query_string GET ["verify_signature"] JSON.as_bool

    let dac_retrieve_preimage page_hash =
      make GET ["preimage"; page_hash] JSON.as_string

    let dac_store_dac_member_signature ~hex_root_hash ~dac_member_pkh ~signature
        =
      let (`Hex root_hash) = hex_root_hash in
      let payload =
        `O
          [
            ("root_hash", `String root_hash);
            ("signer_pkh", `String dac_member_pkh);
            ( "signature",
              `String (Tezos_crypto.Aggregate_signature.to_b58check signature)
            );
          ]
      in
      let data : RPC_core.data = Data payload in
      make ~data PUT ["dac_member_signature"] @@ fun _resp -> ()

    let coordinator_store_preimage ~payload =
      let preimage =
        JSON.parse
          ~origin:"Rollup.DAC.RPC.coordinator_post_preimage"
          (encode_bytes_to_hex_string payload)
      in
      let data : RPC_core.data = Data (JSON.unannotate preimage) in
      make ~data POST ["preimage"] JSON.as_string

    let get_certificate ~hex_root_hash =
      let (`Hex page_hash) = hex_root_hash in
      make GET ["certificates"; page_hash] @@ fun json ->
      JSON.
        ( json |-> "witnesses" |> as_int,
          json |-> "aggregate_signature" |> as_string,
          json |-> "root_hash" |> as_string )
  end
end
