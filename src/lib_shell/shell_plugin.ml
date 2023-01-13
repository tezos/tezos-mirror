(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Nomadic Development. <contact@tezcore.com>             *)
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

module type FILTER = sig
  module Proto : Registered_protocol.T

  module Mempool : sig
    type config

    val config_encoding : config Data_encoding.t

    val default_config : config

    type state

    val init :
      Tezos_protocol_environment.Context.t ->
      head:Tezos_base.Block_header.shell_header ->
      state tzresult Lwt.t

    val flush :
      state -> head:Tezos_base.Block_header.shell_header -> state tzresult Lwt.t

    val remove : filter_state:state -> Operation_hash.t -> state

    val pre_filter :
      config ->
      filter_state:state ->
      Proto.operation ->
      [ `Passed_prefilter of Prevalidator_pending_operations.priority
      | Prevalidator_classification.error_classification ]
      Lwt.t

    val add_operation_and_enforce_mempool_bound :
      ?replace:Operation_hash.t ->
      config ->
      state ->
      Operation_hash.t * Proto.operation ->
      ( state
        * [ `No_replace
          | `Replace of
            Operation_hash.t * Prevalidator_classification.error_classification
          ],
        Prevalidator_classification.error_classification )
      result
      Lwt.t

    val conflict_handler : config -> Proto.Mempool.conflict_handler
  end
end

module type RPC = sig
  module Proto : Registered_protocol.T

  val rpc_services :
    Tezos_protocol_environment.rpc_context Tezos_rpc.Directory.directory
end

module No_filter (Proto : Registered_protocol.T) :
  FILTER with module Proto = Proto and type Mempool.state = unit = struct
  module Proto = Proto

  module Mempool = struct
    type config = unit

    let config_encoding = Data_encoding.empty

    let default_config = ()

    type state = unit

    let init _ ~head:_ = Lwt_result_syntax.return_unit

    let remove ~filter_state _ = filter_state

    let flush _ ~head:_ = Lwt_result_syntax.return_unit

    let pre_filter _ ~filter_state:_ _ =
      Lwt.return @@ `Passed_prefilter (`Low [])

    let add_operation_and_enforce_mempool_bound ?replace:_ _ filter_state _ =
      Lwt_result.return (filter_state, `No_replace)

    let conflict_handler _ ~existing_operation ~new_operation =
      if Proto.compare_operations existing_operation new_operation < 0 then
        `Replace
      else `Keep
  end
end

module type METRICS = sig
  val hash : Protocol_hash.t

  val update_metrics :
    protocol_metadata:bytes ->
    Fitness.t ->
    (cycle:float -> consumed_gas:float -> round:float -> unit) ->
    unit Lwt.t
end

module Undefined_metrics_plugin (Proto : sig
  val hash : Protocol_hash.t
end) =
struct
  let hash = Proto.hash

  let update_metrics ~protocol_metadata:_ _ _ = Lwt.return_unit
end

let rpc_table : (module RPC) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let metrics_table : (module METRICS) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let register_rpc (module Rpc : RPC) =
  assert (not (Protocol_hash.Table.mem rpc_table Rpc.Proto.hash)) ;
  Protocol_hash.Table.add rpc_table Rpc.Proto.hash (module Rpc)

let register_metrics (module Metrics : METRICS) =
  Protocol_hash.Table.replace metrics_table Metrics.hash (module Metrics)

let find_rpc = Protocol_hash.Table.find rpc_table

let find_metrics = Protocol_hash.Table.find metrics_table

let safe_find_metrics hash =
  match find_metrics hash with
  | Some proto_metrics -> Lwt.return proto_metrics
  | None ->
      let module Metrics = Undefined_metrics_plugin (struct
        let hash = hash
      end) in
      Lwt.return (module Metrics : METRICS)

let filter_table : (module FILTER) Protocol_hash.Table.t =
  Protocol_hash.Table.create 5

let add_to_filter_table proto_hash filter =
  assert (not (Protocol_hash.Table.mem filter_table proto_hash)) ;
  Protocol_hash.Table.add filter_table proto_hash filter

let register_filter (module Filter : FILTER) =
  add_to_filter_table Filter.Proto.hash (module Filter)

let find_filter = Protocol_hash.Table.find filter_table
