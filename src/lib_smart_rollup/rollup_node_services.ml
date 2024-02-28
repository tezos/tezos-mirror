(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
(* Copyright (c) 2023 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
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

(** This file defines the services of the rollup node that do not depend on the
    protocol. A top-level directory is built for them in the rollup node
    library.

    Protocol specific services are for RPCs under [/global/block/] and are
    defined in the protocol specific libraries in
    [src/proto_*/lib_sc_rollup_layer2/sc_rollup_services.ml].
*)

(** We distinguish RPC endpoints served by the rollup node into [global] and
    [local]. The difference between the two lies in whether the responses given
    by different rollup nodes in the same state (see below for an exact
    definition) must be the same (in the case of global endpoints) or can differ
    (in the case of local endpoints).

    More formally, two rollup nodes are in the same quiescent state if they are
    subscribed to the same rollup address, and have processed the same set of
    heads from the layer1. We only consider quiescent states, that is those
    where rollup nodes are not actively processing a head received from layer1.

    Examples of global endpoints are [current_tezos_head] and
    [last_stored_commitment], as the responses returned by these endpoints is
    expected to be consistent across rollup nodes in the same state.

    An example of local endpoint is [last_published_commitment], as two rollup
    nodes in the same state may either publish or not publish a commitment,
    according to whether its inbox level is below the inbox level of the last
    cemented commitment at the time they tried to publish the commitment.  See
    below for a more detailed explanation.
*)

type message_status =
  | Unknown
  | Pending_batch
  | Pending_injection of L1_operation.t
  | Injected of {op : L1_operation.t; oph : Operation_hash.t; op_index : int}
  | Included of {
      op : L1_operation.t;
      oph : Operation_hash.t;
      op_index : int;
      l1_block : Block_hash.t;
      l1_level : int32;
      finalized : bool;
      cemented : bool;
    }
  | Committed of {
      op : L1_operation.t;
      oph : Operation_hash.t;
      op_index : int;
      l1_block : Block_hash.t;
      l1_level : int32;
      finalized : bool;
      cemented : bool;
      commitment : Commitment.t;
      commitment_hash : Commitment.Hash.t;
      first_published_at_level : int32;
      published_at_level : int32;
    }

type gc_info = {
  last_gc_level : int32;
  first_available_level : int32;
  last_context_split_level : int32 option;
}

type sync_result =
  | Synchronized
  | Synchronizing of { processed_level : int32; l1_head_level : int32 }

module Encodings = struct
  open Data_encoding

  let commitment_with_hash =
    obj2
      (req "commitment" Commitment.encoding)
      (req "hash" Commitment.Hash.encoding)

  let commitment_with_hash_and_level_infos =
    obj4
      (req "commitment" Commitment.encoding)
      (req "hash" Commitment.Hash.encoding)
      (opt "first_published_at_level" int32)
      (opt "published_at_level" int32)

  let queued_message =
    obj2 (req "id" L2_message.Id.encoding) (req "message" L2_message.encoding)

  let batcher_queue = list queued_message

  let message_status =
    union
      [
        case
          (Tag 0)
          ~title:"unknown"
          ~description:"The message is not known by the batcher."
          (obj1 (req "status" (constant "unknown")))
          (function Unknown -> Some () | _ -> None)
          (fun () -> Unknown);
        case
          (Tag 1)
          ~title:"pending_batch"
          ~description:"The message is in the batcher queue."
          (obj1 (req "status" (constant "pending_batch")))
          (function Pending_batch -> Some () | _ -> None)
          (fun () -> Pending_batch);
        case
          (Tag 2)
          ~title:"pending_injection"
          ~description:"The message is batched but not injected yet."
          (obj2
             (req "status" (constant "pending_injection"))
             (req "operation" L1_operation.encoding))
          (function Pending_injection op -> Some ((), op) | _ -> None)
          (fun ((), op) -> Pending_injection op);
        case
          (Tag 3)
          ~title:"injected"
          ~description:
            "The message is injected as part of an L1 operation but it is not \
             included in a block."
          (obj3
             (req "status" (constant "injected"))
             (req "operation" L1_operation.encoding)
             (req
                "layer1"
                (obj2
                   (req "operation_hash" Operation_hash.encoding)
                   (req "operation_index" int31))))
          (function
            | Injected {op; oph; op_index} -> Some ((), op, (oph, op_index))
            | _ -> None)
          (fun ((), op, (oph, op_index)) -> Injected {op; oph; op_index});
        case
          (Tag 4)
          ~title:"included"
          ~description:"The message is included in an inbox in an L1 block."
          (obj5
             (req "status" (constant "included"))
             (req "operation" L1_operation.encoding)
             (req
                "layer1"
                (obj4
                   (req "operation_hash" Operation_hash.encoding)
                   (req "operation_index" int31)
                   (req "block_hash" Block_hash.encoding)
                   (req "level" int32)))
             (req "finalized" bool)
             (req "cemented" bool))
          (function
            | Included
                {op; oph; op_index; l1_block; l1_level; finalized; cemented} ->
                Some
                  ( (),
                    op,
                    (oph, op_index, l1_block, l1_level),
                    finalized,
                    cemented )
            | _ -> None)
          (fun ((), op, (oph, op_index, l1_block, l1_level), finalized, cemented)
               ->
            Included
              {op; oph; op_index; l1_block; l1_level; finalized; cemented});
        case
          (Tag 5)
          ~title:"committed"
          ~description:"The message is included in a committed inbox on L1."
          (obj9
             (req "status" (constant "committed"))
             (req "operation" L1_operation.encoding)
             (req
                "layer1"
                (obj4
                   (req "operation_hash" Operation_hash.encoding)
                   (req "operation_index" int31)
                   (req "block_hash" Block_hash.encoding)
                   (req "level" int32)))
             (req "finalized" bool)
             (req "cemented" bool)
             (req "commitment" Commitment.encoding)
             (req "hash" Commitment.Hash.encoding)
             (req "first_published_at_level" int32)
             (req "published_at_level" int32))
          (function
            | Committed
                {
                  op;
                  oph;
                  op_index;
                  l1_block;
                  l1_level;
                  finalized;
                  cemented;
                  commitment;
                  commitment_hash;
                  first_published_at_level;
                  published_at_level;
                } ->
                Some
                  ( (),
                    op,
                    (oph, op_index, l1_block, l1_level),
                    finalized,
                    cemented,
                    commitment,
                    commitment_hash,
                    first_published_at_level,
                    published_at_level )
            | _ -> None)
          (fun ( (),
                 op,
                 (oph, op_index, l1_block, l1_level),
                 finalized,
                 cemented,
                 commitment,
                 commitment_hash,
                 first_published_at_level,
                 published_at_level ) ->
            Committed
              {
                op;
                oph;
                op_index;
                l1_block;
                l1_level;
                finalized;
                cemented;
                commitment;
                commitment_hash;
                first_published_at_level;
                published_at_level;
              });
      ]

  let message_status_output =
    merge_objs (obj1 (opt "content" (string' Hex))) message_status

  let gc_info : gc_info Data_encoding.t =
    conv
      (fun {last_gc_level; first_available_level; last_context_split_level} ->
        (last_gc_level, first_available_level, last_context_split_level))
      (fun (last_gc_level, first_available_level, last_context_split_level) ->
        {last_gc_level; first_available_level; last_context_split_level})
    @@ obj3
         (req "last_gc_level" int32)
         (req "first_available_level" int32)
         (opt "last_context_split_level" int32)

  let synchronization_result =
    union
      [
        case
          ~title:"synchronized"
          (Tag 0)
          (constant "synchronized")
          (function Synchronized -> Some () | _ -> None)
          (fun () -> Synchronized);
        case
          ~title:"synchronizing"
          (Tag 1)
          (obj1
             (req
                "synchronizing"
                (obj2
                   (req "processed_level" int32)
                   (req "l1_head_level" int32))))
          (function
            | Synchronizing {processed_level; l1_head_level} ->
                Some (processed_level, l1_head_level)
            | _ -> None)
          (fun (processed_level, l1_head_level) ->
            Synchronizing {processed_tezos_level; l1_head_level});
      ]
end

module Arg = struct
  type block_id =
    [`Head | `Hash of Block_hash.t | `Level of Int32.t | `Finalized | `Cemented]

  let construct_block_id = function
    | `Head -> "head"
    | `Hash h -> Block_hash.to_b58check h
    | `Level l -> Int32.to_string l
    | `Finalized -> "finalized"
    | `Cemented -> "cemented"

  let destruct_block_id h =
    match h with
    | "head" -> Ok `Head
    | "finalized" -> Ok `Finalized
    | "cemented" -> Ok `Cemented
    | _ -> (
        match Int32.of_string_opt h with
        | Some l -> Ok (`Level l)
        | None -> (
            match Block_hash.of_b58check_opt h with
            | Some b -> Ok (`Hash b)
            | None -> Error "Cannot parse block id"))

  let block_id : block_id Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"An L1 block identifier."
      ~name:"block_id"
      ~construct:construct_block_id
      ~destruct:destruct_block_id
      ()

  let l2_message_id : L2_message.id Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"A L2 message id."
      ~name:"l2_message_id"
      ~construct:L2_message.Id.to_b58check
      ~destruct:(fun s ->
        L2_message.Id.of_b58check_opt s
        |> Option.to_result ~none:"Invalid L2 message id")
      ()

  let commitment_hash : Commitment.Hash.t Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"A commitment hash."
      ~name:"commitment_hash"
      ~construct:Commitment.Hash.to_b58check
      ~destruct:(fun s ->
        Commitment.Hash.of_b58check_opt s
        |> Option.to_result ~none:"Invalid commitment hash")
      ()
end

module Query = struct
  type proto_query = {protocol : Protocol_hash.t option}

  let proto_query : proto_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun protocol -> {protocol})
    |+ opt_field "protocol" Protocol_hash.rpc_arg (fun p -> p.protocol)
    |> seal
end

module type PREFIX = sig
  type prefix

  val prefix : (unit, prefix) Tezos_rpc.Path.t
end

module Make_services (P : PREFIX) = struct
  include P

  let path : prefix Tezos_rpc.Path.context = Tezos_rpc.Path.open_root

  let make_call s =
    Tezos_rpc.Context.make_call (Tezos_rpc.Service.prefix prefix s)

  let make_call1 s =
    Tezos_rpc.Context.make_call1 (Tezos_rpc.Service.prefix prefix s)

  let make_call2 s =
    Tezos_rpc.Context.make_call2 (Tezos_rpc.Service.prefix prefix s)
end

module Global = struct
  open Tezos_rpc.Path

  include Make_services (struct
    type prefix = unit

    let prefix = open_root / "global"
  end)

  let sc_rollup_address =
    Tezos_rpc.Service.get_service
      ~description:"Smart rollup address"
      ~query:Tezos_rpc.Query.empty
      ~output:Address.encoding
      (path / "smart_rollup_address")

  let current_tezos_head =
    Tezos_rpc.Service.get_service
      ~description:"Tezos head known to the smart rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:(Data_encoding.option Block_hash.encoding)
      (path / "tezos_head")

  let current_tezos_level =
    Tezos_rpc.Service.get_service
      ~description:"Tezos level known to the smart rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:(Data_encoding.option Data_encoding.int32)
      (path / "tezos_level")

  let last_stored_commitment =
    Tezos_rpc.Service.get_service
      ~description:"Last commitment computed by the node"
      ~query:Tezos_rpc.Query.empty
      ~output:(Data_encoding.option Encodings.commitment_with_hash)
      (path / "last_stored_commitment")

  let global_block_watcher =
    Tezos_rpc.Service.get_service
      ~description:"Monitor and streaming the L2 blocks"
      ~query:Tezos_rpc.Query.empty
      ~output:Sc_rollup_block.encoding
      (path / "monitor_blocks")
end

module Local = struct
  open Tezos_rpc.Path

  include Make_services (struct
    type prefix = unit

    let prefix = open_root / "local"
  end)

  (* commitments are published only if their inbox level is above the last
     cemented commitment level inbox level. Because this information is
     fetched from the head of the tezos node to which the rollup node is
     connected, it is possible that two rollup nodes that have processed
     the same set of heads, but whose corresponding layer1 node has
     different information about the last cemented commitment, will
     decide to publish and not to publish a commitment, respectively.
     As a consequence, the results returned by the endpoint below
     in the rollup node will be different.
  *)
  let last_published_commitment =
    Tezos_rpc.Service.get_service
      ~description:"Last commitment published by the node"
      ~query:Tezos_rpc.Query.empty
      ~output:
        (Data_encoding.option Encodings.commitment_with_hash_and_level_infos)
      (path / "last_published_commitment")

  let commitment =
    Tezos_rpc.Service.get_service
      ~description:"Commitment computed and published by the node"
      ~query:Tezos_rpc.Query.empty
      ~output:
        (Data_encoding.option Encodings.commitment_with_hash_and_level_infos)
      (path / "commitments" /: Arg.commitment_hash)

  let gc_info =
    Tezos_rpc.Service.get_service
      ~description:"Information about garbage collection"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.gc_info
      (path / "gc_info")

  let injection =
    Tezos_rpc.Service.post_service
      ~description:"Inject messages in the batcher's queue"
      ~query:Tezos_rpc.Query.empty
      ~input:
        Data_encoding.(
          def
            "messages"
            ~description:"Messages to inject"
            (list L2_message.content_encoding))
      ~output:
        Data_encoding.(
          def
            "message_ids"
            ~description:"Ids of injected L2 messages"
            (list L2_message.Id.encoding))
      (path / "batcher" / "injection")

  let batcher_queue =
    Tezos_rpc.Service.get_service
      ~description:"List messages present in the batcher's queue"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.batcher_queue
      (path / "batcher" / "queue")

  let batcher_message =
    Tezos_rpc.Service.get_service
      ~description:"Retrieve an L2 message and its status"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.message_status_output
      (path / "batcher" / "queue" /: Arg.l2_message_id)

  let synchronized =
    Tezos_rpc.Service.get_service
      ~description:
        "Wait for the node to have synchronized its L2 chain with the L1 \
         chain, streaming its progress."
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.synchronization_result
      (path / "synchronized")
end

module Root = struct
  open Tezos_rpc.Path

  include Make_services (struct
    type prefix = unit

    let prefix = root
  end)

  let openapi =
    Tezos_rpc.Service.get_service
      ~description:"OpenAPI specification of RPCs for rollup node"
      ~query:Query.proto_query
      ~output:Data_encoding.json
      (path / "openapi")
end
