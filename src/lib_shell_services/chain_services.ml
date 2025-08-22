(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018-2021 Nomadic Labs. <contact@nomadic-labs.com>          *)
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

open Data_encoding

type chain = [`Main | `Test | `Hash of Chain_id.t]

let chain_arg = Block_services.chain_arg

let to_string = Block_services.chain_to_string

let parse_chain = Block_services.parse_chain

type invalid_block = {hash : Block_hash.t; level : Int32.t; errors : error list}

type prefix = Block_services.chain_prefix

type protocol_info = {
  protocol : Protocol_hash.t;
  proto_level : int;
  activation_block : Block_hash.t * int32;
}

let path = Block_services.chain_path

let block_descriptor_encoding =
  obj2 (req "block_hash" Block_hash.encoding) (req "level" int32)

let invalid_block_encoding =
  conv
    (fun {hash; level; errors} -> (hash, level, errors))
    (fun (hash, level, errors) -> {hash; level; errors})
    (obj3
       (req "block" Block_hash.encoding)
       (req "level" int32)
       (req "errors" Tezos_rpc.Error.encoding))

let bootstrap_encoding =
  obj2
    (req "bootstrapped" Encoding.bool)
    (req "sync_state" Chain_validator_worker_state.sync_status_encoding)

let protocol_info_encoding =
  conv
    (fun {protocol; proto_level; activation_block} ->
      (protocol, proto_level, activation_block))
    (fun (protocol, proto_level, activation_block) ->
      {protocol; proto_level; activation_block})
  @@ obj3
       (req "protocol" Protocol_hash.encoding)
       (req
          "proto_level"
          int31
          ~description:
            "Level of protocol in the sequence of protocol activations.")
       (req
          "activation_block"
          block_descriptor_encoding
          ~description:
            "The activation block for a protocol is the migration block, i.e. \
             the last level of the previous protocol.")

type active_peers_info = {
  peer_id : P2p_peer.Id.t;
  block_hash : Block_hash.t;
  block_level : Int32.t;
}

let active_peers_info_encoding =
  conv
    (fun {peer_id; block_hash; block_level} ->
      (peer_id, block_hash, block_level))
    (fun (peer_id, block_hash, block_level) ->
      {peer_id; block_hash; block_level})
  @@ obj3
       (req "peer_id" P2p_peer.Id.encoding)
       (req "block_hash" Block_hash.encoding)
       (req "block_level" int32)

let active_peers_heads_encoding =
  obj1 (req "active_peers_heads" (list active_peers_info_encoding))

type delegators_contribution = {
  own_delegated : int64;
  delegators_contributions : (string * int64) list;
  former_delegators_unstake_requests : int64;
  overstaked : int64;
  total_delegated_including_overdelegated : int64;
  total_delegated_after_limits : int64;
  overdelegated : int64;
}

let delegators_contribution_encoding =
  conv
    (fun {
           own_delegated;
           delegators_contributions;
           former_delegators_unstake_requests;
           overstaked;
           total_delegated_including_overdelegated;
           total_delegated_after_limits;
           overdelegated;
         }
       ->
      ( own_delegated,
        delegators_contributions,
        former_delegators_unstake_requests,
        overstaked,
        total_delegated_including_overdelegated,
        total_delegated_after_limits,
        overdelegated ))
    (fun ( own_delegated,
           delegators_contributions,
           former_delegators_unstake_requests,
           overstaked,
           total_delegated_including_overdelegated,
           total_delegated_after_limits,
           overdelegated )
       ->
      {
        own_delegated;
        delegators_contributions;
        former_delegators_unstake_requests;
        overstaked;
        total_delegated_including_overdelegated;
        total_delegated_after_limits;
        overdelegated;
      })
  @@ obj7
       (req "own_delegated" int64)
       (req
          "external_delegators"
          (list
             (obj2
                (req "delegator_contract_hash" string)
                (req "contribution" int64))))
       (req "former_delegators_unstake_requests" int64)
       (req "overstaked" int64)
       (req "total_delegated_including_overdelegated" int64)
       (req "total_delegated_after_limits" int64)
       (req "overdelegated" int64)

module Delegators_contribution_errors = struct
  type error +=
    | Cycle_too_far_in_future
    | Cycle_too_far_in_past
    | Protocol_not_supported of {protocol_hash : Protocol_hash.t}

  let () =
    let open Data_encoding in
    register_error_kind
      `Temporary
      ~id:"delegators_contribution.cycle_too_far_in_future"
      ~title:"Cycle too far in future"
      ~description:
        "Requested cycle is too far in the future: its baking rights have not \
         been determined yet."
      unit
      (function Cycle_too_far_in_future -> Some () | _ -> None)
      (function () -> Cycle_too_far_in_future) ;
    register_error_kind
      `Temporary
      ~id:"delegators_contribution.cycle_too_far_in_past"
      ~title:"Cycle too far in past"
      ~description:
        "The data needed for the computation is too far in the past: the node \
         no longer has the data (block or context) required to compute the \
         delegators' contribution. Either you are in rolling mode and didn't \
         keep enough cycles, or you recently imported a fresh snapshot, which \
         is missing the relevant contexts"
      unit
      (function Cycle_too_far_in_past -> Some () | _ -> None)
      (function () -> Cycle_too_far_in_past) ;
    register_error_kind
      `Temporary
      ~id:"delegators_contribution.protocol_not_supported"
      ~title:"Protocol not supported by delegators_contribution"
      ~description:
        "This RPC call involves a protocol that does not support \
         delegators_contribution."
      ~pp:(fun fmt protocol_hash ->
        Format.fprintf
          fmt
          "This RPC call involves protocol %a which does not support \
           delegators_contribution."
          Protocol_hash.pp
          protocol_hash)
      (obj1 (req "protocol_hash" Protocol_hash.encoding))
      (function
        | Protocol_not_supported {protocol_hash} -> Some protocol_hash
        | _ -> None)
      (function protocol_hash -> Protocol_not_supported {protocol_hash})
end

module S = struct
  let path : prefix Tezos_rpc.Path.context = Tezos_rpc.Path.open_root

  let chain_id =
    Tezos_rpc.Service.get_service
      ~description:"The chain unique identifier."
      ~query:Tezos_rpc.Query.empty
      ~output:Chain_id.encoding
      Tezos_rpc.Path.(path / "chain_id")

  let is_bootstrapped =
    Tezos_rpc.Service.get_service
      ~description:"The bootstrap status of a chain"
      ~query:Tezos_rpc.Query.empty
      ~output:bootstrap_encoding
      Tezos_rpc.Path.(path / "is_bootstrapped")

  let bootstrapped_flag_encoding =
    let open Data_encoding in
    obj1 (req "bootstrapped" bool)

  let force_bootstrapped =
    Tezos_rpc.Service.patch_service
      ~description:"Forcefully set the bootstrapped flag of the node"
      ~query:Tezos_rpc.Query.empty
      ~input:bootstrapped_flag_encoding
      ~output:unit
      path

  let active_peers_heads =
    Tezos_rpc.Service.get_service
      ~description:"The heads of all active peers"
      ~query:Tezos_rpc.Query.empty
      ~output:active_peers_heads_encoding
      Tezos_rpc.Path.(path / "active_peers_heads")

  let delegators_contribution =
    Tezos_rpc.Service.get_service
      ~description:
        "A breakdown of all the contributions to the delegation portion of the \
         baking power of the given delegate for the given cycle."
      ~query:Tezos_rpc.Query.empty
      ~output:delegators_contribution_encoding
      Tezos_rpc.Path.(
        path / "delegators_contribution" /: Tezos_rpc.Arg.int32
        /: Signature.Public_key_hash.rpc_arg)

  module Levels = struct
    let path = Tezos_rpc.Path.(path / "levels")

    let checkpoint =
      Tezos_rpc.Service.get_service
        ~description:"The current checkpoint for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "checkpoint")

    let savepoint =
      Tezos_rpc.Service.get_service
        ~description:"The current savepoint for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "savepoint")

    let caboose =
      Tezos_rpc.Service.get_service
        ~description:"The current caboose for this chain."
        ~query:Tezos_rpc.Query.empty
        ~output:block_descriptor_encoding
        Tezos_rpc.Path.(path / "caboose")
  end

  module Blocks = struct
    let list_query =
      let open Tezos_rpc.Query in
      query (fun length heads min_date ->
          object
            method length = length

            method heads = heads

            method min_date = min_date
          end)
      |+ opt_field
           "length"
           ~descr:
             "The requested number of predecessors to return (per request; see \
              next argument)."
           Tezos_rpc.Arg.uint
           (fun x -> x#length)
      |+ multi_field
           "head"
           ~descr:
             "An empty argument requests blocks starting with the current \
              head. A non empty list allows to request one or more specific \
              fragments of the chain."
           Block_hash.rpc_arg
           (fun x -> x#heads)
      |+ opt_field
           "min_date"
           ~descr:
             "When `min_date` is provided, blocks with a timestamp before \
              `min_date` are filtered out. However, if the `length` parameter \
              is also provided, then up to that number of predecessors will be \
              returned regardless of their date."
           Time.Protocol.rpc_arg
           (fun x -> x#min_date)
      |> seal

    let path = Tezos_rpc.Path.(path / "blocks")

    let list =
      let open Data_encoding in
      Tezos_rpc.Service.get_service
        ~description:
          "Lists block hashes from '<chain>', up to the last checkpoint, \
           sorted with decreasing fitness. Without arguments it returns the \
           head of the chain. Optional arguments allow to return the list of \
           predecessors of a given block or of a set of blocks."
        ~query:list_query
        ~output:(list (list Block_hash.encoding))
        path
  end

  module Invalid_blocks = struct
    let path = Tezos_rpc.Path.(path / "invalid_blocks")

    let list =
      Tezos_rpc.Service.get_service
        ~description:
          "Lists blocks that have been declared invalid along with the errors \
           that led to them being declared invalid."
        ~query:Tezos_rpc.Query.empty
        ~output:(list invalid_block_encoding)
        path

    let get =
      Tezos_rpc.Service.get_service
        ~description:"The errors that appears during the block (in)validation."
        ~query:Tezos_rpc.Query.empty
        ~output:invalid_block_encoding
        Tezos_rpc.Path.(path /: Block_hash.rpc_arg)

    let delete =
      Tezos_rpc.Service.delete_service
        ~description:"Remove an invalid block for the tezos storage"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.empty
        Tezos_rpc.Path.(path /: Block_hash.rpc_arg)
  end

  module Protocols = struct
    let path = Tezos_rpc.Path.(path / "protocols")

    let list =
      Tezos_rpc.Service.get_service
        ~description:"Lists protocols of the chain."
        ~query:Tezos_rpc.Query.empty
        ~output:(list protocol_info_encoding)
        path

    let get =
      Tezos_rpc.Service.get_service
        ~description:"Information about a protocol of the chain."
        ~query:Tezos_rpc.Query.empty
        ~output:protocol_info_encoding
        Tezos_rpc.Path.(path /: Protocol_hash.rpc_arg)
  end
end

let make_call0 s ctxt chain q p =
  let s = Tezos_rpc.Service.prefix path s in
  Tezos_rpc.Context.make_call1 s ctxt chain q p

let make_call1 s ctxt chain a q p =
  let s = Tezos_rpc.Service.prefix path s in
  Tezos_rpc.Context.make_call2 s ctxt chain a q p

let chain_id ctxt =
  let f = make_call0 S.chain_id ctxt in
  fun ?(chain = `Main) () ->
    match chain with `Hash h -> Lwt.return_ok h | _ -> f chain () ()

module Levels = struct
  let checkpoint ctxt ?(chain = `Main) () =
    make_call0 S.Levels.checkpoint ctxt chain () ()

  let savepoint ctxt ?(chain = `Main) () =
    make_call0 S.Levels.savepoint ctxt chain () ()

  let caboose ctxt ?(chain = `Main) () =
    make_call0 S.Levels.caboose ctxt chain () ()
end

module Blocks = struct
  let list ctxt =
    let f = make_call0 S.Blocks.list ctxt in
    fun ?(chain = `Main) ?(heads = []) ?length ?min_date () ->
      f
        chain
        (object
           method heads = heads

           method length = length

           method min_date = min_date
        end)
        ()

  include Block_services.Empty

  type protocols = Block_services.protocols = {
    current_protocol : Protocol_hash.t;
    next_protocol : Protocol_hash.t;
  }

  let protocols = Block_services.protocols
end

module Mempool = Block_services.Empty.Mempool

module Invalid_blocks = struct
  let list ctxt =
    let f = make_call0 S.Invalid_blocks.list ctxt in
    fun ?(chain = `Main) () -> f chain () ()

  let get ctxt =
    let f = make_call1 S.Invalid_blocks.get ctxt in
    fun ?(chain = `Main) block -> f chain block () ()

  let delete ctxt =
    let f = make_call1 S.Invalid_blocks.delete ctxt in
    fun ?(chain = `Main) block -> f chain block () ()
end

module Protocols = struct
  let list ctxt =
    let f = make_call0 S.Protocols.list ctxt in
    fun ?(chain = `Main) () -> f chain () ()

  let get ctxt =
    let f = make_call1 S.Protocols.get ctxt in
    fun ?(chain = `Main) proto -> f chain proto () ()
end
