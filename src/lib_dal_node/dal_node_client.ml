(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech <contact@trili.tech>                        *)
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

open Tezos_dal_node_services

module Profiler = (val Profiler.wrap Dal_profiler.dal_profiler)

class type cctxt = object
  inherit Tezos_rpc.Context.generic
end

class unix_cctxt ~rpc_config : cctxt =
  object
    inherit
      Tezos_rpc_http_client_unix.RPC_client_unix.http_ctxt
        rpc_config
        (Tezos_rpc_http.Media_type.Command_line.of_command_line
           rpc_config.media_type)
  end

let make_unix_cctxt endpoint =
  let rpc_config =
    {Tezos_rpc_http_client_unix.RPC_client_unix.default_config with endpoint}
  in
  let cctxt = new unix_cctxt ~rpc_config in
  new Octez_telemetry.Rpc_context.with_telemetry
    ~service_name:"DAL_node_client"
    cctxt

let call (cctxt : #cctxt) = cctxt#call_service

let get_slot_pages cctxt (slot_id : Types.slot_id) =
  call
    cctxt
    Services.get_slot_pages
    (((), slot_id.slot_level), slot_id.slot_index)
    ()
    ()

let get_slot_page_proof cctxt (slot_id : Types.slot_id) page_index =
  call
    cctxt
    Services.get_slot_page_proof
    ((((), slot_id.slot_level), slot_id.slot_index), page_index)
    ()
    ()

let post_slot cctxt ?slot_index slot =
  let query =
    object
      method padding = '\000'

      method slot_index = slot_index
    end
  in
  (call
     cctxt
     Services.post_slot
     ()
     query
     slot [@profiler.aggregate_s {verbosity = Notice} "post_slot"])

let get_slot_status cctxt (slot_id : Types.slot_id) =
  call
    cctxt
    Services.get_slot_status
    (((), slot_id.slot_level), slot_id.slot_index)
    ()
    ()
