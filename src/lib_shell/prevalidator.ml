(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2022 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

include Prevalidator_internal_common
open Prevalidator_worker_state

module ChainProto_registry = Map.Make (struct
  type t = Chain_id.t * Protocol_hash.t

  let compare (c1, p1) (c2, p2) =
    let pc = Protocol_hash.compare p1 p2 in
    if pc = 0 then Chain_id.compare c1 c2 else pc
end)

let chain_proto_registry : t ChainProto_registry.t ref =
  ref ChainProto_registry.empty

let create limits (module Filter : Shell_plugin.FILTER) chain_db =
  let open Lwt_result_syntax in
  let chain_store = Distributed_db.chain_store chain_db in
  let chain_id = Store.Chain.chain_id chain_store in
  match
    ChainProto_registry.find (chain_id, Filter.Proto.hash) !chain_proto_registry
  with
  | None ->
      let prevalidator =
        Prevalidator_internal.make limits chain_db chain_id (module Filter)
      in
      let (module Prevalidator : T) = prevalidator in
      chain_proto_registry :=
        ChainProto_registry.add
          Prevalidator.name
          prevalidator
          !chain_proto_registry ;
      return prevalidator
  | Some p -> return p

let shutdown (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  chain_proto_registry :=
    ChainProto_registry.remove Prevalidator.name !chain_proto_registry ;
  Prevalidator.Worker.shutdown w

let flush (t : t) event head live_blocks live_operations =
  let open Lwt_result_syntax in
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let*! r =
    Prevalidator.Worker.Queue.push_request_and_wait
      w
      (Request.Flush (head, event, live_blocks, live_operations))
  in
  match r with
  | Ok r -> Lwt.return_ok r
  | Error (Closed None) -> fail [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> fail errs
  | Error (Any exn) -> fail [Exn exn]
  | Error (Request_error error_trace) -> fail error_trace

let notify_operations (t : t) peer mempool =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  let open Lwt_result_syntax in
  let*! (_was_pushed : bool) =
    Prevalidator.Worker.Queue.push_request w (Request.Notify (peer, mempool))
  in
  Lwt.return_unit

let inject_operation (t : t) ~force op =
  let module Prevalidator : T = (val t) in
  let open Lwt_result_syntax in
  let w = Lazy.force Prevalidator.worker in
  let*! r =
    Prevalidator.Worker.Queue.push_request_and_wait w (Inject {op; force})
  in
  match r with
  | Ok r -> Lwt.return_ok r
  | Error (Closed None) -> fail [Worker_types.Terminated]
  | Error (Closed (Some errs)) -> fail errs
  | Error (Any exn) -> fail [Exn exn]
  | Error (Request_error error_trace) -> fail error_trace

let status (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.status w

let running_workers () =
  ChainProto_registry.fold
    (fun (id, proto) t acc -> (id, proto, t) :: acc)
    !chain_proto_registry
    []

let pending_requests (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests w

let current_request (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.current_request w

let information (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.information w

let pipeline_length (t : t) =
  let module Prevalidator : T = (val t) in
  let w = Lazy.force Prevalidator.worker in
  Prevalidator.Worker.Queue.pending_requests_length w

let empty_rpc_directory : unit Tezos_rpc.Directory.t =
  Tezos_rpc.Directory.gen_register
    Tezos_rpc.Directory.empty
    (Block_services.Empty.S.Mempool.pending_operations Tezos_rpc.Path.open_root)
    (fun _pv params () ->
      let pending_operations =
        {
          Block_services.Empty.Mempool.applied = [];
          refused = Operation_hash.Map.empty;
          outdated = Operation_hash.Map.empty;
          branch_refused = Operation_hash.Map.empty;
          branch_delayed = Operation_hash.Map.empty;
          unprocessed = Operation_hash.Map.empty;
        }
      in
      Block_services.Empty.Mempool.pending_operations_version_dispatcher
        ~version:params#version
        pending_operations)

let rpc_directory : t option Tezos_rpc.Directory.t =
  Tezos_rpc.Directory.register_dynamic_directory
    Tezos_rpc.Directory.empty
    (Block_services.mempool_path Tezos_rpc.Path.open_root)
    (function
      | None ->
          Lwt.return
            (Tezos_rpc.Directory.map
               (fun _ -> Lwt.return_unit)
               empty_rpc_directory)
      | Some t ->
          let module Prevalidator : T = (val t : T) in
          let w = Lazy.force Prevalidator.worker in
          let pv = Prevalidator.Worker.state w in
          let pv_rpc_dir = Lazy.force (Prevalidator.get_rpc_directory pv) in
          Lwt.return
            (Tezos_rpc.Directory.map (fun _ -> Lwt.return pv) pv_rpc_dir))
