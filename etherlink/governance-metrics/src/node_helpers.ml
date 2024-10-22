(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Data_encoding
open Tezos_rpc

let failwith_unused_encoding ~__FUNCTION__ =
  Stdlib.failwith @@ __FUNCTION__ ^ ": Unused encoding"

type operation_result = {status : string}

let operation_result_encoding =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (status, ()) -> {status})
    (merge_objs (obj1 (req "status" string)) unit)

type contract_metadata = {operation_result : operation_result}

let contract_metadata_encoding =
  conv
    (fun {operation_result} -> (operation_result, ()))
    (fun (operation_result, ()) -> {operation_result})
    (merge_objs (obj1 (req "operation_result" operation_result_encoding)) unit)

type sequencer_proposal_args = {
  sequencer_pk : string;
  pool_address : string; (* repr: bytes *)
}

type sequencer_proposal_args_opt = {
  sequencer_pk : string option;
  pool_address : string option; (* repr: bytes *)
}

type sequencer_entrypoints_parameters =
  | Trigger_committee_upgrade of string (* repr: address *)
  | Vote of string
  | Upvote_proposal of sequencer_proposal_args
  | New_proposal of sequencer_proposal_args

type kernel_entrypoint_parameters =
  | Trigger_kernel_upgrade of string (* repr: address *)
  | Vote of string
  | Upvote_proposal of string (* repr: bytes *)
  | New_proposal of string (* repr: bytes *)

type entrypoint_parameters =
  | Sequencer of sequencer_entrypoints_parameters
  | Kernel of kernel_entrypoint_parameters
  | Security_kernel of kernel_entrypoint_parameters

let sequencer_proposal_encoding =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (sequencer_pk, pool_address) -> {sequencer_pk; pool_address})
    (obj2 (opt "string" string) (opt "bytes" string))

let entrypoint_parameters_encoding ~contracts =
  let open Configuration in
  union
    [
      case
        ~title:"string_entrypoint_parameters"
        Json_only
        (merge_objs (obj1 (req "string" string)) unit)
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun (value, ()) ~entrypoint ~destination ->
          match (entrypoint, destination) with
          | "vote", dest when dest = contracts.kernel_governance ->
              Some (Kernel (Vote value))
          | "vote", dest when dest = contracts.sequencer_governance ->
              Some (Sequencer (Vote value))
          | "vote", dest when dest = contracts.security_kernel_governance ->
              Some (Security_kernel (Vote value))
          | "trigger_kernel_upgrade", dest
            when dest = contracts.kernel_governance ->
              Some (Kernel (Trigger_kernel_upgrade value))
          | "trigger_committee_upgrade", dest
            when dest = contracts.sequencer_governance ->
              Some (Sequencer (Trigger_committee_upgrade value))
          | "trigger_kernel_upgrade", dest
            when dest = contracts.security_kernel_governance ->
              Some (Security_kernel (Trigger_kernel_upgrade value))
          | _ -> None);
      case
        ~title:"bytes_entrypoint_parameters"
        Json_only
        (merge_objs (obj1 (req "bytes" string)) unit)
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun (value, ()) ~entrypoint ~destination ->
          match (entrypoint, destination) with
          | "upvote_proposal", dest when dest = contracts.kernel_governance ->
              Some (Kernel (Upvote_proposal value))
          | "new_proposal", dest when dest = contracts.kernel_governance ->
              Some (Kernel (New_proposal value))
          | "upvote_proposal", dest
            when dest = contracts.security_kernel_governance ->
              Some (Security_kernel (Upvote_proposal value))
          | "new_proposal", dest
            when dest = contracts.security_kernel_governance ->
              Some (Security_kernel (New_proposal value))
          | _ -> None);
      case
        ~title:"sequencer_proposal_entrypoint_parameters"
        Json_only
        (merge_objs (obj1 (req "args" (list sequencer_proposal_encoding))) unit)
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun (proposal_arguments, ()) ~entrypoint ~destination ->
          match (proposal_arguments, entrypoint, destination) with
          | [{sequencer_pk; _}; {pool_address; _}], "upvote_proposal", dest
            when dest = contracts.sequencer_governance -> (
              match (sequencer_pk, pool_address) with
              | Some sequencer_pk, Some pool_address ->
                  Some
                    (Sequencer (Upvote_proposal {sequencer_pk; pool_address}))
              | _ -> None)
          | [{sequencer_pk; _}; {pool_address; _}], "new_proposal", dest
            when dest = contracts.sequencer_governance -> (
              match (sequencer_pk, pool_address) with
              | Some sequencer_pk, Some pool_address ->
                  Some (Sequencer (New_proposal {sequencer_pk; pool_address}))
              | _ -> None)
          | _ -> None);
      case
        ~title:"ignored_entrypoint_parameters"
        Json_only
        unit
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun _ ~entrypoint:_ ~destination:_ -> None);
    ]

type contract_parameters = {value : entrypoint_parameters}

let contract_parameters_encoding ~contracts =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun ((entrypoint, get_value), ()) ~destination ->
      Option.map (fun value -> {value}) (get_value ~entrypoint ~destination))
    (merge_objs
       (obj2
          (req "entrypoint" string)
          (req "value" (entrypoint_parameters_encoding ~contracts)))
       unit)

type contract_transaction = {source : string; parameters : contract_parameters}

let contract_transaction_encoding ~contracts =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun ((source, destination, get_parameters, metadata), ()) ->
      if metadata.operation_result.status = "applied" then
        Option.map
          (fun parameters -> Some {source; parameters})
          (get_parameters ~destination)
      else None)
    (merge_objs
       (obj4
          (req "source" string)
          (req "destination" string)
          (req "parameters" (contract_parameters_encoding ~contracts))
          (req "metadata" contract_metadata_encoding))
       unit)

let contract_transaction_opt_encoding ~contracts =
  union
    [
      case
        ~title:"contract_transaction"
        Json_only
        (merge_objs
           (obj1 (req "kind" (constant "transaction")))
           (contract_transaction_encoding ~contracts))
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun ((), contract_transaction_tuple) -> contract_transaction_tuple);
      case
        ~title:"ignored_operation"
        Json_only
        unit
        (fun _ -> failwith_unused_encoding ~__FUNCTION__)
        (fun _ -> None);
    ]

let operation_encoding ~contracts =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (contents, ()) -> List.filter_some @@ List.filter_some contents)
    (merge_objs
       (obj1
          (req "contents" (list (contract_transaction_opt_encoding ~contracts))))
       unit)

let operations_encoding ~contracts =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (operations, ()) -> List.concat_map List.concat operations)
    (merge_objs
       (obj1 (req "operations" (list (list (operation_encoding ~contracts)))))
       unit)

let micheline_encoding =
  let open Tezos_micheline.Micheline in
  let open Tezos_micheline.Micheline_encoding in
  let open Tezos_micheline.Micheline_parser in
  conv
    (fun node -> strip_locations node)
    (fun canon -> inject_locations (fun _ -> location_zero) canon)
    (canonical_encoding ~variant:"generic" string)

let data_micheline_encoding = obj1 (req "data" micheline_encoding)

type header = {level : int}

let header_encoding =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (level, ()) -> {level})
    (merge_objs (obj1 (req "level" int31)) unit)

type view_input = {
  contract : string;
  view : string;
  chain_id : string;
  level : int;
}

let micheline_view_input_encoding =
  conv
    (fun input ->
      ( input.contract,
        input.view,
        Micheline_helpers.unit,
        input.chain_id,
        (),
        Int.to_string @@ input.level ))
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (obj6
       (req "contract" string)
       (req "view" string)
       (req "input" micheline_encoding)
       (req "chain_id" string)
       (req "unparsing_mode" (constant "Readable"))
       (req "level" string))

let call_service ~base ?(media_types = Media_type.all_media_types) rpc b c input
    =
  Tezos_rpc_http_client_unix.RPC_client_unix.call_service
    media_types
    ~base
    rpc
    b
    c
    input

let make_streamed_call ~base rpc =
  let open Lwt_result_syntax in
  let stream, push = Lwt_stream.create () in
  let on_chunk v = push (Some v) and on_close () = push None in
  let* _spill_all =
    Tezos_rpc_http_client_unix.RPC_client_unix.call_streamed_service
      [Media_type.json]
      ~base
      rpc
      ~on_chunk
      ~on_close
      ()
      ()
      ()
  in
  return stream

module Path = struct
  open Path

  let main = root / "chains" / "main"

  let chain_id = main / "chain_id"

  let head = main / "blocks" / "head"

  let micheline_view = head / "helpers" / "scripts" / "run_script_view"

  let storage ~contract = head / "context" / "contracts" / contract / "storage"

  let monitor_heads = root / "monitor" / "heads" / "main"

  let block_by_level ~level = main / "blocks" / level
end

module Services = struct
  let chain_id =
    Service.get_service
      ~description:"L1 chain ID"
      ~query:Query.empty
      ~output:Data_encoding.string
      Path.chain_id

  let micheline_view =
    Service.post_service
      ~description:"Micheline view"
      ~query:Query.empty
      ~input:micheline_view_input_encoding
      ~output:data_micheline_encoding
      Path.micheline_view

  let storage ~contract =
    Service.get_service
      ~description:"Storage of a smart contract"
      ~query:Query.empty
      ~output:micheline_encoding
      Path.(storage ~contract)

  let monitor_head =
    Service.get_service
      ~description:"Monitor streamed L1 blocks"
      ~query:Tezos_rpc.Query.empty
      ~output:header_encoding
      Path.monitor_heads

  let governance_operations ~contracts =
    let output = operations_encoding ~contracts in
    fun ~level ->
      Service.get_service
        ~description:"Potential operations from block by hash"
        ~query:Query.empty
        ~output
        Path.(block_by_level ~level)
end

module RPC = struct
  exception Monitoring_heads_timed_out

  let chain_id base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base Services.chain_id () () () in
    match answer with
    | Ok chain_id -> return chain_id
    | Error trace -> fail trace

  let micheline_view ~chain_id ~contract ~view ~level ~decode base =
    let open Lwt_result_syntax in
    let view_input = {chain_id; contract; view; level} in
    let*! answer =
      call_service ~base Services.micheline_view () () view_input
    in
    match answer with
    | Ok answer ->
        let*? res = decode answer in
        return res
    | Error trace -> fail trace

  let storage ~contract ~decode base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base Services.(storage ~contract) () () () in
    match answer with
    | Ok contract ->
        let*? res = decode contract in
        return res
    | Error trace -> fail trace

  let monitor_heads ~process base =
    let open Lwt_result_syntax in
    let timeout_monitor () =
      let*! () = Lwt_unix.sleep 30. in
      Lwt.fail Monitoring_heads_timed_out
    in
    let* stream = make_streamed_call ~base Services.monitor_head in
    let rec read_stream stream =
      let*! chunk = Lwt.pick [Lwt_stream.get stream; timeout_monitor ()] in
      match chunk with
      | None -> Lwt_result.return ()
      | Some chunk ->
          let* () = process chunk in
          read_stream stream
    in
    read_stream stream

  let governance_operations ~config =
    let base = config.Configuration.endpoint in
    let service =
      Services.(governance_operations ~contracts:config.contracts)
    in
    fun ~level -> call_service ~base (service ~level) () () ()
end
