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

module Lifted_protocol = Tezos_protocol_005_PsBabyM1_lifted.Lifted_protocol
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
  let def id ids ?title ?description encoding =
    Data_encoding.def
      (String.concat "." (Protocol.name :: id :: ids))
      ?title
      ?description
      encoding
  in
  register @@ def "parameters" [] Protocol.Parameters_repr.encoding ;
  register @@ def "tez" [] Protocol.Alpha_context.Tez.encoding ;
  register @@ def "roll" [] Protocol.Alpha_context.Roll.encoding ;
  register ~pp:Protocol.Alpha_context.Fitness.pp
  @@ def "fitness" [] Protocol.Alpha_context.Fitness.encoding ;
  register ~pp:Protocol.Alpha_context.Timestamp.pp
  @@ def "timestamp" [] Protocol.Alpha_context.Timestamp.encoding ;
  register ~pp:Protocol.Alpha_context.Raw_level.pp
  @@ def "raw_level" [] Protocol.Alpha_context.Raw_level.encoding ;
  register @@ def "vote" ["ballot"] Protocol.Alpha_context.Vote.ballot_encoding ;
  register
  @@ def "vote" ["ballots"] Protocol.Alpha_context.Vote.ballots_encoding ;
  register
  @@ def "vote" ["listings"] Protocol.Alpha_context.Vote.listings_encoding ;
  register @@ def "seed" [] Protocol.Alpha_context.Seed.seed_encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp
  @@ def "gas" [] Protocol.Alpha_context.Gas.encoding ;
  register ~pp:Protocol.Alpha_context.Gas.pp_cost
  @@ def "gas" ["cost"] Protocol.Alpha_context.Gas.cost_encoding ;
  register @@ def "script" [] Protocol.Alpha_context.Script.encoding ;
  register @@ def "script" ["expr"] Protocol.Alpha_context.Script.expr_encoding ;
  register @@ def "script" ["prim"] Protocol.Alpha_context.Script.prim_encoding ;
  register
  @@ def "script" ["lazy_expr"] Protocol.Alpha_context.Script.lazy_expr_encoding ;
  register
  @@ def "script" ["loc"] Protocol.Alpha_context.Script.location_encoding ;
  register ~pp:Protocol.Alpha_context.Contract.pp
  @@ def "contract" [] Protocol.Alpha_context.Contract.encoding ;
  register Protocol.Alpha_context.Contract.big_map_diff_encoding ;
  register
  @@ def
       "delegate"
       ["frozen_balance"]
       Protocol.Alpha_context.Delegate.frozen_balance_encoding ;
  register
  @@ def
       "delegate"
       ["balance_updates"]
       Protocol.Alpha_context.Delegate.balance_updates_encoding ;
  register
  @@ def
       "delegate"
       ["frozen_balance_by_cycles"]
       Protocol.Alpha_context.Delegate.frozen_balance_by_cycle_encoding ;
  register ~pp:Protocol.Alpha_context.Level.pp_full
  @@ def "level" [] Protocol.Alpha_context.Level.encoding ;
  register @@ def "operation" [] Protocol.Alpha_context.Operation.encoding ;
  register
  @@ def
       "operation"
       ["contents"]
       Protocol.Alpha_context.Operation.contents_encoding ;
  register
  @@ def
       "operation"
       ["contents_list"]
       Protocol.Alpha_context.Operation.contents_list_encoding ;
  register
  @@ def
       "operation"
       ["protocol_data"]
       Protocol.Alpha_context.Operation.protocol_data_encoding ;
  register
  @@ def "operation" ["raw"] Protocol.Alpha_context.Operation.raw_encoding ;
  register
  @@ def
       "operation"
       ["internal"]
       Protocol.Alpha_context.Operation.internal_operation_encoding ;
  register
  @@ def
       "operation"
       ["unsigned"]
       Protocol.Alpha_context.Operation.unsigned_encoding ;
  register ~pp:Protocol.Alpha_context.Period.pp
  @@ def "period" [] Protocol.Alpha_context.Period.encoding ;
  register ~pp:Protocol.Alpha_context.Cycle.pp
  @@ def "cycle" [] Protocol.Alpha_context.Cycle.encoding ;
  register @@ def "constants" [] Protocol.Alpha_context.Constants.encoding ;
  register
  @@ def "constants" ["fixed"] Protocol.Alpha_context.Constants.fixed_encoding ;
  register
  @@ def
       "constants"
       ["parametric"]
       Protocol.Alpha_context.Constants.parametric_encoding ;
  register @@ def "nonce" [] Protocol.Alpha_context.Nonce.encoding ;
  register @@ def "block_header" [] Protocol.Alpha_context.Block_header.encoding ;
  register
  @@ def
       "block_header"
       ["unsigned"]
       Protocol.Alpha_context.Block_header.unsigned_encoding ;
  register
  @@ def "block_header" ["raw"] Protocol.Alpha_context.Block_header.raw_encoding ;
  register
  @@ def
       "block_header"
       ["contents"]
       Protocol.Alpha_context.Block_header.contents_encoding ;
  register
  @@ def
       "block_header"
       ["shell_header"]
       Protocol.Alpha_context.Block_header.shell_header_encoding ;
  register
  @@ def
       "block_header"
       ["protocol_data"]
       Protocol.Alpha_context.Block_header.protocol_data_encoding ;
  register ~pp:Protocol.Alpha_context.Voting_period.pp
  @@ def "voting_period" [] Protocol.Alpha_context.Voting_period.encoding ;
  register
  @@ def
       "voting_period"
       ["kind"]
       Protocol.Alpha_context.Voting_period.kind_encoding ;
  register
  @@ def
       "errors"
       []
       ~description:
         "The full list of RPC errors would be too long to include.It is\n\
          available through the RPC `/errors` (GET)."
       error_encoding
