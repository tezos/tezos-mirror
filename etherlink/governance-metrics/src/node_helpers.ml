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

let micheline_encoding =
  let open Tezos_micheline.Micheline in
  let open Tezos_micheline.Micheline_encoding in
  let open Tezos_micheline.Micheline_parser in
  conv
    (fun node -> strip_locations node)
    (fun canon -> inject_locations (fun _ -> location_zero) canon)
    (canonical_encoding ~variant:"generic" string)

let data_micheline_encoding = obj1 (req "data" micheline_encoding)

let level_encoding =
  conv
    (fun _ -> failwith_unused_encoding ~__FUNCTION__)
    (fun (level, ()) -> level)
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

module Path = struct
  open Path

  let main = root / "chains" / "main"

  let chain_id = main / "chain_id"

  let head = main / "blocks" / "head"

  let current_l1_level = head / "helpers" / "current_level"

  let micheline_view = head / "helpers" / "scripts" / "run_script_view"

  let storage ~contract = head / "context" / "contracts" / contract / "storage"
end

module Services = struct
  let chain_id =
    Service.get_service
      ~description:"L1 chain ID"
      ~query:Query.empty
      ~output:Data_encoding.string
      Path.chain_id

  let current_l1_level =
    Service.get_service
      ~description:"Current L1 level"
      ~query:Query.empty
      ~output:level_encoding
      Path.current_l1_level

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
end

module RPC = struct
  let chain_id base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base Services.chain_id () () () in
    match answer with
    | Ok chain_id -> return chain_id
    | Error trace -> fail trace

  let current_l1_level base =
    let open Lwt_result_syntax in
    let*! answer = call_service ~base Services.current_l1_level () () () in
    match answer with
    | Ok current_l1_level -> return current_l1_level
    | Error trace -> fail trace

  let micheline_view ~chain_id ~contract ~view ~decode base =
    let open Lwt_result_syntax in
    let* level = current_l1_level base in
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
end
