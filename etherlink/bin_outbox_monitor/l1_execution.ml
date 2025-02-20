(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let fetch_l1_block_ops l1_node_endpoint block =
  let open Lwt_result_syntax in
  let uri =
    Uri.with_path l1_node_endpoint
    @@ Format.sprintf
         "%s/chains/main/blocks/%ld/operations/3"
         (Uri.path l1_node_endpoint)
         block
  in
  let uri = Uri.with_query uri [("metdata", ["always"])] in
  let* response =
    Tezos_rpc_http_client_unix.RPC_client_unix.generic_media_type_call
      ~accept:[Media_type.json]
      `GET
      uri
  in
  match response with
  | `Json (`Ok resp) -> return resp
  | `Binary _ ->
      failwith "fetch_l1_block_ops: Expected JSON answer instead of binary"
  | `Other (ct, _) ->
      failwith
        "fetch_l1_block_ops: Expected JSON answer instead of %a"
        (Format.pp_print_option Format.pp_print_string)
        (Option.map snd ct)
  | `Json
      (( `Unauthorized resp
       | `Gone resp
       | `Error resp
       | `Not_found resp
       | `Forbidden resp
       | `Conflict resp ) as err) ->
      failwith
        "fetch_l1_block_ops: RPC failed [%s] with %a"
        (match err with
        | `Unauthorized _ -> "unauthorized"
        | `Gone _ -> "gone"
        | `Error _ -> "error"
        | `Not_found _ -> "not found"
        | `Forbidden _ -> "forbidden"
        | `Conflict _ -> "conflict")
        (Format.pp_print_option Data_encoding.Json.pp)
        resp

type outbox_message_index = {outbox_level : int32; message_index : int}

let extract_outbox_proof_info (hex : Hex.t) =
  let proof =
    Hex.to_bytes_exn hex
    |> Data_encoding.Binary.of_bytes_exn
         Tezos_raw_protocol_alpha.Alpha_context.Sc_rollup.Wasm_2_0_0PVM
         .Protocol_implementation
         .output_proof_encoding
  in
  let info =
    Tezos_raw_protocol_alpha.Alpha_context.Sc_rollup.Wasm_2_0_0PVM
    .Protocol_implementation
    .output_info_of_output_proof
      proof
  in
  {
    outbox_level =
      Tezos_raw_protocol_alpha.Alpha_context.Raw_level.to_int32
        info.outbox_level;
    message_index = Z.to_int info.message_index;
  }

type status = Applied | Not_applied

type sc_rollup_execute = {
  rollup : Octez_smart_rollup.Address.t;
  output_proof : Hex.t;
  status : status;
}

type 'a op_content = Execute of 'a | Other

(*
   Example of response for operations/3 that we want to parse:

[
    {
        "protocol": "PsQuebecnLByd3JwTiGadoG4nGWi3HYiLXUjkibeFV8dCFeVMUg",
        "chain_id": "NetXnHfVqm9iesp",
        "hash": "ooBD25vCmYLKs8GRpnxENeenCv2ZGymkM5U6Ha7QkjdYppXCrbG",
        "branch": "BLaKm5fV4sNgMrLb4CpQHeQNAgwNQgkxbfSL1eckfRJeVihTGE8",
        "contents": [
            {
                "kind": "smart_rollup_execute_outbox_message",
                "source": "tz1P2Jn2fQktnA4HHMkLRNy2ebuscUx7EUbn",
                "fee": "2245",
                "counter": "31461681",
                "gas_limit": "6834",
                "storage_limit": "72",
                "rollup": "sr18wx6ezkeRjt1SZSeZ2UQzQN3Uc3YLMLqg",
                "cemented_commitment": "src12kjEALqoQzNydpnw8ZQAsXFHCw1kYPiF8n2aL9QetHrqbYHjE2",
                "output_proof": "...",
                "metadata": {
                    "balance_updates": [...],
                    "operation_result": {
                        "status": "applied",
                        "balance_updates": [...],
                        "ticket_updates": [...],
                        "consumed_milligas": "5556203",
                        "paid_storage_size_diff": "72"
                    },
                    "internal_operation_results": [...]
                }
            }
        ],
        "signature": "sig..."
    }
]
*)

let never_encoding _ = Stdlib.failwith "Never encoding"

let decoder inj = Json_encoding.conv never_encoding inj

let status_decoder =
  let open Json_encoding in
  decoder (function "applied" -> Applied | _ -> Not_applied) string

let hex_decoder =
  let open Json_encoding in
  decoder (fun s -> `Hex s) string

let rollup_address =
  Data_encoding.Json.convert Octez_smart_rollup.Address.encoding

let op_hash = Data_encoding.Json.convert Operation_hash.encoding

let sc_rollup_execute_decoder =
  let open Json_encoding in
  decoder (fun (rollup, output_proof, status) -> {rollup; output_proof; status})
  @@ obj3
       (req "rollup" rollup_address)
       (req "output_proof" hex_decoder)
       (req
          "metadata"
          (obj1 (req "operation_result" (obj1 (req "status" status_decoder)))))

let content_decoder =
  let open Json_encoding in
  union
    [
      case
        sc_rollup_execute_decoder
        (function Execute e -> Some e | _ -> None)
        (fun e -> Execute e);
      case unit (function Other -> Some () | _ -> None) (fun () -> Other);
    ]

let op_decoder =
  let open Json_encoding in
  obj2 (req "hash" op_hash) (req "contents" (list content_decoder))

let ops_decoder = Json_encoding.list op_decoder

let get_executed_outbox_messages l1_node_endpoint rollup_address block =
  let open Lwt_result_syntax in
  let+ operations_json = fetch_l1_block_ops l1_node_endpoint block in
  let operations =
    Json_encoding.destruct ~ignore_extra_fields:true ops_decoder operations_json
  in
  List.fold_left
    (fun acc (op_hash, contents) ->
      List.fold_left
        (fun acc -> function
          | Other -> acc
          | Execute {status = Not_applied; _} -> acc
          | Execute {rollup; _}
            when Octez_smart_rollup.Address.(rollup <> rollup_address) ->
              acc
          | Execute {rollup = _; output_proof; status = Applied} ->
              let info = extract_outbox_proof_info output_proof in
              (info, op_hash) :: acc)
        acc
        contents)
    []
    operations
  |> List.rev

let make_ctxt endpoint =
  let open Tezos_rpc_http_client_unix.RPC_client_unix in
  new http_ctxt {default_config with endpoint} [Media_type.json]

let mark_executed_outbox_messages db ~l1_node_endpoint ~rollup_address ~block =
  let open Lwt_result_syntax in
  let* executed =
    get_executed_outbox_messages l1_node_endpoint rollup_address block
  in
  when_ (executed <> []) @@ fun () ->
  let* header =
    Tezos_shell_services.Shell_services.Blocks.Header.shell_header
      (make_ctxt l1_node_endpoint)
      ~block:(`Level block)
      ()
  in
  List.iter_es
    (fun ({outbox_level; message_index}, operation_hash) ->
      Db.Executions.store
        db
        ~outbox_level
        ~message_index
        ~operation_hash
        ~l1_block:block
        ~timestamp:header.timestamp)
    executed
