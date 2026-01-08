(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>              *)
(* SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Event = struct
  include Internal_event.Simple

  let section = ["outbox_monitor"; "l1_execution"]

  let executed_withdrawal =
    declare_6
      ~section
      ~name:"executed_withdrawal"
      ~msg:
        "Withdrawal {withdrawal_id} of {amount} {token} was executed in \
         operation {operation_hash} of block {l1_block} at {timestamp}"
      ~level:Notice
      ("withdrawal_id", Db.quantity_hum_encoding)
      ("amount", Db.quantity_hum_encoding)
      ("token", Db.withdrawal_kind_encoding)
      ("operation_hash", Operation_hash.encoding)
      ("l1_block", Data_encoding.int32)
      ("timestamp", Time.Protocol.encoding)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_quantity
      ~pp3:Db.pp_withdrawal_kind
      ~pp4:Operation_hash.pp
      ~pp6:Time.Protocol.pp_hum

  let overdue_count =
    declare_1
      ~section
      ~name:"overdue_count"
      ~msg:"There are {count} withdrawals that are overdue for execution on L1"
      ~level:Warning
      ("count", Data_encoding.int31)

  let overdue_withdrawal =
    declare_7
      ~section
      ~name:"overdue_withdrawal"
      ~msg:
        "Withdrawal {withdrawal_id} of {amount} {token} of transaction \
         {transactionHash}({transactionIndex}) in outbox level {outbox_level} \
         is overdue for {due_for_blocks} blocks"
      ~level:Error
      ("withdrawal_id", Db.quantity_hum_encoding)
      ("amount", Db.quantity_hum_encoding)
      ("token", Db.withdrawal_kind_encoding)
      ("transactionHash", Ethereum_types.hash_encoding)
      ("transactionIndex", Db.quantity_hum_encoding)
      ("outbox_level", Data_encoding.int32)
      ("due_for_blocks", Data_encoding.int32)
      ~pp1:Ethereum_types.pp_quantity
      ~pp2:Ethereum_types.pp_quantity
      ~pp3:Db.pp_withdrawal_kind
      ~pp4:Ethereum_types.pp_hash
      ~pp5:Ethereum_types.pp_quantity
end

let make_ctxt endpoint =
  new Retrier_context.ctxt ~timeout:30. endpoint [Media_type.json]

let call_rpc_json (l1_node_rpc : Retrier_context.t) ?(query = []) path =
  let open Lwt_result_syntax in
  let uri =
    Uri.with_path l1_node_rpc#endpoint
    @@ String.concat "/" [Uri.path l1_node_rpc#endpoint; path]
  in
  let uri = Uri.with_query uri query in
  let* response = l1_node_rpc#generic_media_type_call `GET uri in
  match response with
  | `Json (`Ok resp) -> return resp
  | `Binary _ ->
      failwith "%a: Expected JSON answer instead of binary" Uri.pp uri
  | `Other (ct, _) ->
      failwith
        "%a: Expected JSON answer instead of %a"
        Uri.pp
        uri
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
        "%a: RPC failed [%s] with %a"
        Uri.pp
        uri
        (match err with
        | `Unauthorized _ -> "unauthorized"
        | `Gone _ -> "gone"
        | `Error _ -> "error"
        | `Not_found _ -> "not found"
        | `Forbidden _ -> "forbidden"
        | `Conflict _ -> "conflict")
        (Format.pp_print_option Data_encoding.Json.pp)
        resp

let fetch_l1_block_ops l1_node_rpc block =
  call_rpc_json
    l1_node_rpc
    (Format.sprintf "chains/main/blocks/%ld/operations/3" block)
    ~query:[("metdata", ["always"])]

let fetch_constants l1_node_rpc =
  call_rpc_json l1_node_rpc "chains/main/blocks/head/context/constants"

let get_challenge_window l1_node_rpc =
  let open Lwt_result_syntax in
  let+ constants = fetch_constants l1_node_rpc in
  Ezjsonm.find constants ["smart_rollup_challenge_window_in_blocks"]
  |> Ezjsonm.get_int

let get_challenge_window =
  let challenge_window = ref None in
  fun l1_node_rpc ->
    let open Lwt_result_syntax in
    match !challenge_window with
    | Some w -> return w
    | None ->
        let+ w = get_challenge_window l1_node_rpc in
        challenge_window := Some w ;
        w

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

let get_executed_outbox_messages l1_node_rpc rollup_address block =
  let open Lwt_result_syntax in
  let+ operations_json = fetch_l1_block_ops l1_node_rpc block in
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

let mark_executed_outbox_messages db ~(l1_node_rpc : Retrier_context.t)
    ~rollup_address ~block =
  let open Lwt_result_syntax in
  let* executed =
    get_executed_outbox_messages l1_node_rpc rollup_address block
  in
  when_ (executed <> []) @@ fun () ->
  let* header =
    Tezos_shell_services.Shell_services.Blocks.Header.shell_header
      l1_node_rpc
      ~block:(`Level block)
      ()
  in
  let* () =
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
  in
  let* executed_withdrawals =
    Db.Executions.withdrawals_executed_in_l1_block db ~l1_block:block
  in
  let*! () =
    List.iter_s
      (fun ( (w : Db.withdrawal_log),
             (_outbox_index : Db.outbox_index),
             (execution : Db.execution_short_info) )
         ->
        Event.(emit executed_withdrawal)
          ( w.withdrawal.withdrawal_id,
            w.withdrawal.amount,
            w.withdrawal.kind,
            execution.operation_hash,
            execution.l1_block,
            execution.timestamp ))
      executed_withdrawals
  in
  return_unit

let check_overdue db ~l1_node_rpc =
  let open Lwt_result_syntax in
  let* challenge_window = get_challenge_window l1_node_rpc in
  let* lcc = Db.Pointers.LCC.get db in
  let executable_level = Int32.sub lcc (Int32.of_int challenge_window) in
  let* overdues = Db.Withdrawals.get_overdue db ~challenge_window in
  let*! () =
    if overdues <> [] then Event.(emit overdue_count) (List.length overdues)
    else Lwt.return_unit
  in
  let*! () =
    List.iter_s
      (fun ((w : Db.withdrawal_log), outbox_level) ->
        let due_for_blocks = Int32.sub executable_level outbox_level in
        (* TODO: alert with overdue threshold. *)
        Event.(emit overdue_withdrawal)
          ( w.withdrawal.withdrawal_id,
            w.withdrawal.amount,
            w.withdrawal.kind,
            w.transactionHash,
            w.transactionIndex,
            outbox_level,
            due_for_blocks ))
      overdues
  in
  return_unit
