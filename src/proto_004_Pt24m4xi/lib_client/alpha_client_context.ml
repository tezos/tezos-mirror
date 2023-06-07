(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

module Lifted_protocol = Tezos_protocol_004_Pt24m4xi_lifted.Lifted_protocol
module Alpha_block_services =
  Block_services.Make (Lifted_protocol) (Lifted_protocol)

(** Client RPC context *)

class type rpc_context =
  object
    inherit Tezos_rpc.Context.generic

    inherit
      [Shell_services.chain * Shell_services.block] Environment.RPC_context
                                                    .simple
  end

class wrap_rpc_context (t : Tezos_rpc.Context.generic) : rpc_context =
  object
    method base : Uri.t = t#base

    method generic_media_type_call = t#generic_media_type_call

    method call_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          'p ->
          'q ->
          'i ->
          'o tzresult Lwt.t =
      t#call_service

    method call_streamed_service
        : 'm 'p 'q 'i 'o.
          (([< Resto.meth] as 'm), unit, 'p, 'q, 'i, 'o) Tezos_rpc.Service.t ->
          on_chunk:('o -> unit) ->
          on_close:(unit -> unit) ->
          'p ->
          'q ->
          'i ->
          (unit -> unit) tzresult Lwt.t =
      t#call_streamed_service

    inherit
      [Shell_services.chain, Shell_services.block] Environment.proto_rpc_context
        (t :> Tezos_rpc.Context.t)
        Shell_services.Blocks.path
  end

class type full =
  object
    inherit Client_context.full

    inherit
      [Shell_services.chain * Shell_services.block] Environment.RPC_context
                                                    .simple

    inherit
      [Shell_services.chain, Shell_services.block] Environment.proto_rpc_context
  end

class wrap_full (t : Client_context.full) : full =
  object
    inherit Client_context.proxy_context t

    inherit
      [Shell_services.chain, Shell_services.block] Environment.proto_rpc_context
        (t :> Tezos_rpc.Context.t)
        Shell_services.Blocks.path
  end

let register_error_kind category ~id ~title ~description ?pp encoding from_error
    to_error =
  let id = "client." ^ Protocol.name ^ "." ^ id in
  register_error_kind
    category
    ~id
    ~title
    ~description
    ?pp
    encoding
    from_error
    to_error

let () =
  let open Data_encoding.Registration in
  let open Data_encoding in
  let stamp_proto id ids = String.concat "." (Protocol.name :: id :: ids) in
  register
  @@ def (stamp_proto "parameters" []) Protocol.Parameters_repr.encoding ;
  register @@ def (stamp_proto "tez" []) Protocol.Alpha_context.Tez.encoding ;
  register @@ def (stamp_proto "roll" []) Protocol.Alpha_context.Roll.encoding ;
  register ~pp:Protocol.Alpha_context.Fitness.pp
  @@ def (stamp_proto "fitness" []) Protocol.Alpha_context.Fitness.encoding ;
  register ~pp:Protocol.Alpha_context.Timestamp.pp
  @@ def (stamp_proto "timestamp" []) Protocol.Alpha_context.Timestamp.encoding ;
  register ~pp:Protocol.Alpha_context.Raw_level.pp
  @@ def (stamp_proto "raw_level" []) Protocol.Alpha_context.Raw_level.encoding ;
  register
  @@ def
       (stamp_proto "vote" ["ballot"])
       Protocol.Alpha_context.Vote.ballot_encoding ;
  register
  @@ def
       (stamp_proto "vote" ["ballots"])
       Protocol.Alpha_context.Vote.ballots_encoding ;
  register
  @@ def
       (stamp_proto "vote" ["listings"])
       Protocol.Alpha_context.Vote.listings_encoding ;
  register
  @@ def (stamp_proto "seed" []) Protocol.Alpha_context.Seed.seed_encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp
  @@ def (stamp_proto "gas" []) Protocol.Alpha_context.Gas.encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp_cost
  @@ def (stamp_proto "gas" ["cost"]) Protocol.Alpha_context.Gas.cost_encoding ;
  register
  @@ def (stamp_proto "script" []) Protocol.Alpha_context.Script.encoding ;
  register
  @@ def
       (stamp_proto "script" ["expr"])
       Protocol.Alpha_context.Script.expr_encoding ;
  register
  @@ def
       (stamp_proto "script" ["prim"])
       Protocol.Alpha_context.Script.prim_encoding ;
  register
  @@ def
       (stamp_proto "script" ["lazy_expr"])
       Protocol.Alpha_context.Script.lazy_expr_encoding ;
  register
  @@ def
       (stamp_proto "script" ["loc"])
       Protocol.Alpha_context.Script.location_encoding ;
  register ~pp:Protocol.Alpha_context.Contract.pp
  @@ def (stamp_proto "contract" []) Protocol.Alpha_context.Contract.encoding ;
  register
  @@ def
       (stamp_proto "contract" ["big_map_diff"])
       Protocol.Alpha_context.Contract.big_map_diff_encoding ;
  register
  @@ def
       (stamp_proto "delegate" ["frozen_balance"])
       Protocol.Alpha_context.Delegate.frozen_balance_encoding ;
  register
  @@ def
       (stamp_proto "delegate" ["balance_updates"])
       Protocol.Alpha_context.Delegate.balance_updates_encoding ;
  register
  @@ def
       (stamp_proto "delegate" ["frozen_balance_by_cycles"])
       Protocol.Alpha_context.Delegate.frozen_balance_by_cycle_encoding ;
  register ~pp:Protocol.Alpha_context.Level.pp_full
  @@ def (stamp_proto "level" []) Protocol.Alpha_context.Level.encoding ;
  register
  @@ def (stamp_proto "operation" []) Protocol.Alpha_context.Operation.encoding ;
  register
  @@ def
       (stamp_proto "operation" ["contents"])
       Protocol.Alpha_context.Operation.contents_encoding ;
  register
  @@ def
       (stamp_proto "operation" ["contents_list"])
       Protocol.Alpha_context.Operation.contents_list_encoding ;
  register
  @@ def
       (stamp_proto "operation" ["protocol_data"])
       Protocol.Alpha_context.Operation.protocol_data_encoding ;
  register
  @@ def
       (stamp_proto "operation" ["raw"])
       Protocol.Alpha_context.Operation.raw_encoding ;
  register
  @@ def
       (stamp_proto "operation" ["internal"])
       Protocol.Alpha_context.Operation.internal_operation_encoding ;
  register
  @@ def
       (stamp_proto "operation" ["unsigned"])
       Protocol.Alpha_context.Operation.unsigned_encoding ;
  register ~pp:Protocol.Alpha_context.Period.pp
  @@ def (stamp_proto "period" []) Protocol.Alpha_context.Period.encoding ;
  register ~pp:Protocol.Alpha_context.Cycle.pp
  @@ def (stamp_proto "cycle" []) Protocol.Alpha_context.Cycle.encoding ;
  register
  @@ def (stamp_proto "constants" []) Protocol.Alpha_context.Constants.encoding ;
  register
  @@ def
       (stamp_proto "constants" ["fixed"])
       Protocol.Alpha_context.Constants.fixed_encoding ;
  register
  @@ def
       (stamp_proto "constants" ["parametric"])
       Protocol.Alpha_context.Constants.parametric_encoding ;
  register @@ def (stamp_proto "nonce" []) Protocol.Alpha_context.Nonce.encoding ;
  register
  @@ def
       (stamp_proto "block_header" [])
       Protocol.Alpha_context.Block_header.encoding ;
  register
  @@ def
       (stamp_proto "block_header" ["unsigned"])
       Protocol.Alpha_context.Block_header.unsigned_encoding ;
  register
  @@ def
       (stamp_proto "block_header" ["raw"])
       Protocol.Alpha_context.Block_header.raw_encoding ;
  register
  @@ def
       (stamp_proto "block_header" ["contents"])
       Protocol.Alpha_context.Block_header.contents_encoding ;
  register
  @@ def
       (stamp_proto "block_header" ["shell_header"])
       Protocol.Alpha_context.Block_header.shell_header_encoding ;
  register
  @@ def
       (stamp_proto "block_header" ["protocol_data"])
       Protocol.Alpha_context.Block_header.protocol_data_encoding ;
  register ~pp:Protocol.Alpha_context.Voting_period.pp
  @@ def
       (stamp_proto "voting_period" [])
       Protocol.Alpha_context.Voting_period.encoding ;
  register
  @@ def
       (stamp_proto "voting_period" ["kind"])
       Protocol.Alpha_context.Voting_period.kind_encoding ;
  register
  @@ Data_encoding.def
       (stamp_proto "errors" [])
       ~description:
         "The full list of RPC errors would be too long to include.It is\n\
          available through the RPC `/errors` (GET)."
       error_encoding
