(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

open Injector_server

let add_pending_transaction :
    ( [`POST],
      unit,
      unit,
      unit,
      Injector_server_operation.t,
      Inj_operation.hash )
    Tezos_rpc.Service.t =
  Tezos_rpc.Service.post_service
    ~description:"Add a pending operation to the injector queue"
    ~query:Tezos_rpc.Query.empty
    ~input:Injector_server_operation.encoding
    ~output:Inj_operation.Hash.encoding
    Tezos_rpc.Path.(root / "add_pending_transaction")

type op_query = {op_hash : string}

let injector_op_query : op_query Tezos_rpc.Query.t =
  let open Tezos_rpc.Query in
  query (fun op_hash -> {op_hash})
  |+ field "op_hash" Tezos_rpc.Arg.string "" (fun t -> t.op_hash)
  |> seal

(* Simplified version of [Injector.status] *)
type status =
  | Pending
  | Injected of {injected_oph : Operation_hash.t; injected_op_index : int}
  | Included of {
      included_oph : Operation_hash.t;
      included_op_index : int;
      block : Block_hash.t;
      level : int32;
    }

let status_encoding : status Data_encoding.encoding =
  Data_encoding.(
    union
      ~tag_size:`Uint8
      [
        case
          ~title:"Pending"
          (Tag 0)
          (obj1 (req "pending" unit))
          (function Pending -> Some () | _ -> None)
          (fun _s -> Pending);
        case
          ~title:"Injected"
          (Tag 1)
          (obj2 (req "injected_oph" string) (req "injected_op_index" int32))
          (function
            | Injected {injected_oph; injected_op_index} ->
                Some
                  ( Operation_hash.to_b58check injected_oph,
                    Int32.of_int injected_op_index )
            | _ -> None)
          (fun (oph, op_index) ->
            Injected
              {
                injected_oph = Operation_hash.of_b58check_exn oph;
                injected_op_index = Int32.to_int op_index;
              });
        case
          ~title:"Included"
          (Tag 2)
          (obj4
             (req "included_oph" string)
             (req "included_op_index" int32)
             (req "block" string)
             (req "level" int32))
          (function
            | Included {included_oph; included_op_index; block; level} ->
                Some
                  ( Operation_hash.to_b58check included_oph,
                    Int32.of_int included_op_index,
                    Block_hash.to_b58check block,
                    level )
            | _ -> None)
          (fun (oph, op_index, block, level) ->
            Included
              {
                included_oph = Operation_hash.of_b58check_exn oph;
                included_op_index = Int32.to_int op_index;
                block = Block_hash.of_b58check_exn block;
                level;
              });
      ])

let operation_status :
    ([`GET], unit, unit, op_query, unit, status option) Tezos_rpc.Service.t =
  Tezos_rpc.Service.get_service
    ~description:"Query the status of an injector operation"
    ~query:injector_op_query
    ~output:(Data_encoding.option status_encoding)
    Tezos_rpc.Path.(root / "operation_status")

let inject : ([`GET], unit, unit, unit, unit, unit) Tezos_rpc.Service.t =
  Tezos_rpc.Service.get_service
    ~description:"Inject operations"
    ~query:Tezos_rpc.Query.empty
    ~output:Data_encoding.unit
    Tezos_rpc.Path.(root / "inject")
