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
  first_available_level : int32;
  last_gc_started_at : int32 option;
  last_context_split_level : int32 option;
  last_successful_gc_target : int32 option;
}

type sync_result =
  | Synchronized
  | Synchronizing of {
      processed_level : int32;
      l1_head_level : int32;
      percentage_done : float;
    }

type l1_health = {
  connection : [`Connected | `Disconnected | `Reconnecting];
  blocks_late : int32;
  last_seen_head : (Block_hash.t * int32 * Time.Protocol.t) option;
}

type health = {
  healthy : bool;
  degraded : bool;
  l1 : l1_health;
  active_workers : (string * [`Running | `Crashed of exn]) list;
}

type version = {
  version : string;
  store_version : string;
  context_version : string;
}

type injector_operation = {
  op : L1_operation.t;
  errors : int;
  last_error : tztrace option;
}

type committed_status =
  | Uncommitted of {
      commitment_level : int32;
          (** Level at which the following commitment will be produced. *)
      estimated_commitment_time : Time.System.t;
          (** Estimated time at which the commitment will be published. *)
      estimated_cementation_time : Time.System.t;
          (** Estimated time at which the commitment will be cemented. *)
    }
  | Committed_ of {
      commitment_hash : Commitment.Hash.t;  (** Hash of following commitment. *)
      commitment_level : int32;  (** Level of following commitment. *)
      first_published_level : int32;
          (** Level at which commitment was first published. *)
      published_level : int32 option;
          (** Level at which commitment was published by operator (may be None
              if e.g. rollup node does not publish). *)
      estimated_cementation_time : Time.System.t;
          (** Estimated time at which the commitment will be cemented. *)
    }
  | Cemented of {
      commitment_hash : Commitment.Hash.t;  (** Hash of following commitment. *)
      commitment_level : int32;  (** Level of following commitment. *)
      first_published_level : int32;
          (** Level at which commitment was first published. *)
      published_level : int32 option;
          (** Level at which commitment was published by operator (may be None
              if e.g. rollup node does not publish). *)
      blocks_since_cemented : int32;
          (** Number of blocks since commitment was cemented  *)
    }

module Encodings = struct
  open Data_encoding

  let version =
    conv
      (fun {version; store_version; context_version} ->
        (version, store_version, context_version))
      (fun (version, store_version, context_version) ->
        {version; store_version; context_version})
    @@ obj3
         (req "version" string)
         (req "store_version" string)
         (req "context_version" string)

  let l1_health =
    conv
      (fun {connection; blocks_late; last_seen_head} ->
        (connection, blocks_late, last_seen_head))
      (fun (connection, blocks_late, last_seen_head) ->
        {connection; blocks_late; last_seen_head})
    @@ obj3
         (req
            "connection"
            (string_enum
               [
                 ("connected", `Connected);
                 ("reconnecting", `Reconnecting);
                 ("disconnected", `Disconnected);
               ]))
         (req "blocks_late" int32)
         (opt
            "last_seen_head"
            (obj3
               (req "hash" Block_hash.encoding)
               (req "level" int32)
               (req "timestamp" Time.Protocol.encoding)))

  let exn =
    def "exception" ~description:"Exception"
    @@ conv Printexc.to_string (fun s -> Failure s) string

  let health =
    conv
      (fun {healthy; degraded; l1; active_workers} ->
        (healthy, degraded, l1, active_workers))
      (fun (healthy, degraded, l1, active_workers) ->
        {healthy; degraded; l1; active_workers})
    @@ obj4
         (req "healthy" bool)
         (req "degraded" bool)
         (req "l1" l1_health)
         (req
            "active_workers"
            (list
               (tup2
                  string
                  (union
                     [
                       case
                         (Tag 0)
                         ~title:"running"
                         (constant "running")
                         (function `Running -> Some () | _ -> None)
                         (fun () -> `Running);
                       case
                         (Tag 1)
                         ~title:"crashed"
                         (obj1 (req "crashed" exn))
                         (function `Crashed e -> Some e | _ -> None)
                         (fun e -> `Crashed e);
                     ]))))

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

  let lcc =
    obj2 (req "commitment_hash" Commitment.Hash.encoding) (req "level" int32)

  let outbox =
    obj2
      (req "outbox_level" int32)
      (req
         "messages"
         (list
            (obj2
               (req "message_index" int31)
               (opt "message" Outbox_message.summary_encoding))))

  let outbox_msg_status : [`Executable | `Lost | `Pending] t =
    string_enum
      [("executable", `Executable); ("pending", `Pending); ("lost", `Lost)]

  let queued_message = L2_message.encoding

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
                 published_at_level )
             ->
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
      (fun {
             first_available_level;
             last_gc_started_at;
             last_context_split_level;
             last_successful_gc_target;
           }
         ->
        ( first_available_level,
          last_gc_started_at,
          last_context_split_level,
          last_successful_gc_target ))
      (fun ( first_available_level,
             last_gc_started_at,
             last_context_split_level,
             last_successful_gc_target )
         ->
        {
          first_available_level;
          last_gc_started_at;
          last_context_split_level;
          last_successful_gc_target;
        })
    @@ obj4
         (req
            "first_available_level"
            int32
            ~description:
              "First level where data is guaranteed to be available in the \
               rollup node.")
         (opt
            "last_gc_level"
            int32
            ~description:"The level at which the last GC was triggered.")
         (opt
            "last_context_split_level"
            int32
            ~description:
              "The level at which the context was split for the last time. \
               Commits before this level are in other chunks.")
         (opt
            "last_successful_gc_target"
            int32
            ~description:
              "The level which was the target of the last successful GC.")

  let ocaml_gc_stat_encoding =
    let open Gc in
    conv
      (fun {
             minor_words;
             promoted_words;
             major_words;
             minor_collections;
             major_collections;
             forced_major_collections;
             heap_words;
             heap_chunks;
             live_words;
             live_blocks;
             free_words;
             free_blocks;
             largest_free;
             fragments;
             compactions;
             top_heap_words;
             stack_size;
           }
         ->
        ( ( minor_words,
            promoted_words,
            major_words,
            minor_collections,
            major_collections,
            forced_major_collections ),
          ( (heap_words, heap_chunks, live_words, live_blocks, free_words),
            ( free_blocks,
              largest_free,
              fragments,
              compactions,
              top_heap_words,
              stack_size ) ) ))
      (fun ( ( minor_words,
               promoted_words,
               major_words,
               minor_collections,
               major_collections,
               forced_major_collections ),
             ( (heap_words, heap_chunks, live_words, live_blocks, free_words),
               ( free_blocks,
                 largest_free,
                 fragments,
                 compactions,
                 top_heap_words,
                 stack_size ) ) )
         ->
        {
          minor_words;
          promoted_words;
          major_words;
          minor_collections;
          major_collections;
          forced_major_collections;
          heap_words;
          heap_chunks;
          live_words;
          live_blocks;
          free_words;
          free_blocks;
          largest_free;
          fragments;
          compactions;
          top_heap_words;
          stack_size;
        })
      (merge_objs
         (obj6
            (req "minor_words" float)
            (req "promoted_words" float)
            (req "major_words" float)
            (req "minor_collections" int31)
            (req "major_collections" int31)
            (req "forced_major_collections" int31))
         (merge_objs
            (obj5
               (req "heap_words" int31)
               (req "heap_chunks" int31)
               (req "live_words" int31)
               (req "live_blocks" int31)
               (req "free_words" int31))
            (obj6
               (req "free_blocks" int31)
               (req "largest_free" int31)
               (req "fragments" int31)
               (req "compactions" int31)
               (req "top_heap_words" int31)
               (req "stack_size" int31))))

  let mem_stat_encoding =
    let open Memory in
    union
      ~tag_size:`Uint8
      [
        case
          (Tag 0)
          (conv
             (fun {page_size; size; resident; shared; text; lib; data; dt} ->
               (page_size, size, resident, shared, text, lib, data, dt))
             (fun (page_size, size, resident, shared, text, lib, data, dt) ->
               {page_size; size; resident; shared; text; lib; data; dt})
             (obj8
                (req "page_size" int31)
                (req "size" int64)
                (req "resident" int64)
                (req "shared" int64)
                (req "text" int64)
                (req "lib" int64)
                (req "data" int64)
                (req "dt" int64)))
          ~title:"Linux_proc_statm"
          (function Statm x -> Some x | _ -> None)
          (function res -> Statm res);
        case
          (Tag 1)
          (conv
             (fun {page_size; mem; resident} -> (page_size, mem, resident))
             (fun (page_size, mem, resident) -> {page_size; mem; resident})
             (obj3
                (req "page_size" int31)
                (req "mem" float)
                (req "resident" int64)))
          ~title:"Darwin_ps"
          (function Ps x -> Some x | _ -> None)
          (function res -> Ps res);
      ]

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
                (obj3
                   (req "processed_level" int32)
                   (req "l1_head_level" int32)
                   (req "percentage_done" float))))
          (function
            | Synchronizing {processed_level; l1_head_level; percentage_done} ->
                Some (processed_level, l1_head_level, percentage_done)
            | _ -> None)
          (fun (processed_level, l1_head_level, percentage_done) ->
            Synchronizing {processed_level; l1_head_level; percentage_done});
      ]

  let injector_queues_total =
    obj2
      (req
         "injectors"
         (list
            (obj2
               (req "tags" (list Operation_kind.encoding))
               (req "queue_size" int31))))
      (req "total" int31)

  let injector_operation =
    conv
      (fun {op; errors; last_error} -> (op, (errors, last_error)))
      (fun (op, (errors, last_error)) -> {op; errors; last_error})
    @@ merge_objs
         L1_operation.encoding
         (obj1
            (dft
               "errors"
               (obj2
                  (req "count" int31)
                  (opt "last" Tezos_rpc.Service.error_encoding))
               (0, None)))

  let injector_queues =
    list
      (obj2
         (req "tags" (list Operation_kind.encoding))
         (req "queue" (list injector_operation)))

  let committed_status =
    union
      [
        case
          (Tag 0)
          ~title:"uncommitted"
          ~description:"L2 Block content is not yet committed on L1"
          (obj1
             (req
                "uncommitted"
                (obj3
                   (req
                      "commitment_level"
                      int32
                      ~description:
                        "Level at which the following commitment will be \
                         produced.")
                   (req
                      "estimated_commitment_time"
                      Time.System.encoding
                      ~description:
                        "Estimated time at which the commitment will be \
                         published.")
                   (req
                      "estimated_cementation_time"
                      Time.System.encoding
                      ~description:
                        "Estimated time at which the commitment will be \
                         cemented."))))
          (function
            | Uncommitted
                {
                  commitment_level = l;
                  estimated_commitment_time = pt;
                  estimated_cementation_time = ct;
                } ->
                Some (l, pt, ct)
            | _ -> None)
          (fun (l, pt, ct) ->
            Uncommitted
              {
                commitment_level = l;
                estimated_commitment_time = pt;
                estimated_cementation_time = ct;
              });
        case
          (Tag 1)
          ~title:"committed"
          ~description:
            "L2 Block content is already committed on L1 but not yet cemented"
          (obj1
             (req
                "committed"
                (obj5
                   (req
                      "commitment_hash"
                      Commitment.Hash.encoding
                      ~description:"Hash of following commitment.")
                   (req
                      "commitment_level"
                      int32
                      ~description:"Level of following commitment.")
                   (req
                      "first_published_level"
                      int32
                      ~description:
                        "Level at which commitment was first published.")
                   (req
                      "published_level"
                      (option int32)
                      ~description:
                        "Level at which commitment was published by operator \
                         (may be null if e.g. rollup node does not publish).")
                   (req
                      "estimated_cementation_time"
                      Time.System.encoding
                      ~description:
                        "Estimated time at which the commitment will be \
                         cemented."))))
          (function
            | Committed_
                {
                  commitment_hash = h;
                  commitment_level = l;
                  first_published_level = f;
                  published_level = p;
                  estimated_cementation_time = ct;
                } ->
                Some (h, l, f, p, ct)
            | _ -> None)
          (fun (h, l, f, p, ct) ->
            Committed_
              {
                commitment_hash = h;
                commitment_level = l;
                first_published_level = f;
                published_level = p;
                estimated_cementation_time = ct;
              });
        case
          (Tag 2)
          ~title:"cemented"
          ~description:"L2 Block content is already cemented"
          (obj1
             (req
                "cemented"
                (obj5
                   (req
                      "commitment_hash"
                      Commitment.Hash.encoding
                      ~description:"Hash of following commitment.")
                   (req
                      "commitment_level"
                      int32
                      ~description:"Level of following commitment.")
                   (req
                      "first_published_level"
                      int32
                      ~description:
                        "Level at which commitment was first published.")
                   (req
                      "published_level"
                      (option int32)
                      ~description:
                        "Level at which commitment was published by operator \
                         (may be null if e.g. rollup node does not publish).")
                   (req
                      "blocks_since_cemented"
                      int32
                      ~description:
                        "Number of blocks since commitment was cemented."))))
          (function
            | Cemented
                {
                  commitment_hash = h;
                  commitment_level = l;
                  first_published_level = f;
                  published_level = p;
                  blocks_since_cemented = b;
                } ->
                Some (h, l, f, p, b)
            | _ -> None)
          (fun (h, l, f, p, b) ->
            Cemented
              {
                commitment_hash = h;
                commitment_level = l;
                first_published_level = f;
                published_level = p;
                blocks_since_cemented = b;
              });
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

  let operation_kind : Operation_kind.t Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"A kind of operation for the injector."
      ~name:"operation_kind"
      ~construct:Operation_kind.to_string
      ~destruct:(fun s ->
        Operation_kind.of_string s
        |> Option.to_result ~none:"Invalid operation kind")
      ()

  let z =
    Resto.Arg.make
      ~name:"z"
      ~destruct:(fun s -> Ok (Z.of_string s))
      ~construct:Z.to_string
      ()
end

module Query = struct
  type proto_query = {protocol : Protocol_hash.t option}

  let proto_query : proto_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun protocol -> {protocol})
    |+ opt_field "protocol" Protocol_hash.rpc_arg (fun p -> p.protocol)
    |> seal

  let operation_tag_query : Operation_kind.t option Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun tag -> tag)
    |+ opt_field "tag" Arg.operation_kind (fun k -> k)
    |> seal

  let order_below_query =
    let open Tezos_rpc.Query in
    query (fun order drop_no_order ->
        object
          method order = order

          method drop_no_order = drop_no_order
        end)
    |+ opt_field "order_below" Arg.z (fun q -> q#order)
    |+ field "drop_no_order" Tezos_rpc.Arg.bool false (fun q -> q#drop_no_order)
    |> seal

  let operation_tag_and_order_below_query =
    let open Tezos_rpc.Query in
    query (fun order operation_tag drop_no_order ->
        object
          method order = order

          method operation_tag = operation_tag

          method drop_no_order = drop_no_order
        end)
    |+ opt_field "order_below" Arg.z (fun q -> q#order)
    |+ opt_field "tag" Arg.operation_kind (fun q -> q#operation_tag)
    |+ field "drop_no_order" Tezos_rpc.Arg.bool false (fun q -> q#drop_no_order)
    |> seal

  let order_and_drop_duplicate_query =
    let open Tezos_rpc.Query in
    query (fun order drop_duplicate ->
        object
          method order = order

          method drop_duplicate = drop_duplicate
        end)
    |+ opt_field "order" Arg.z (fun q -> q#order)
    |+ field "drop_duplicate" Tezos_rpc.Arg.bool false (fun q ->
           q#drop_duplicate)
    |> seal

  let outbox_level_query =
    let open Tezos_rpc.Query in
    query (fun l -> l)
    |+ opt_field "outbox_level" Tezos_rpc.Arg.int32 (fun k -> k)
    |> seal

  let outbox_query : bool Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun b -> b)
    |+ field "outbox" Tezos_rpc.Arg.bool false (fun b -> b)
    |> seal

  type key_query = {key : string}

  let key_query : key_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun key -> {key})
    |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
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

  let last_cemented_commitment =
    Tezos_rpc.Service.get_service
      ~description:"Last commitment computed by the node"
      ~query:Tezos_rpc.Query.empty
      ~output:(Data_encoding.option Encodings.commitment_with_hash)
      (path / "last_cemented_commitment")

  let global_block_watcher =
    Tezos_rpc.Service.get_service
      ~description:"Monitor and streaming the L2 blocks"
      ~query:Tezos_rpc.Query.empty
      ~output:Sc_rollup_block.encoding
      (path / "monitor_blocks")

  let finalized_block_watcher =
    Tezos_rpc.Service.get_service
      ~description:"Monitor and streaming the L2 finalized blocks"
      ~query:Tezos_rpc.Query.empty
      ~output:Sc_rollup_block.encoding
      (path / "monitor_finalized_blocks")
end

module Block = struct
  open Tezos_rpc.Path

  include Make_services (struct
    type prefix = unit * Arg.block_id

    let prefix = open_root / "global" / "block" /: Arg.block_id
  end)

  let block =
    Tezos_rpc.Service.get_service
      ~description:
        "Layer-2 block of the layer-2 chain with respect to a Layer 1 block \
         identifier"
      ~query:Query.outbox_query
      ~output:Sc_rollup_block.full_encoding
      path

  let hash =
    Tezos_rpc.Service.get_service
      ~description:"Tezos block hash of block known to the smart rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:Block_hash.encoding
      (path / "hash")

  let level =
    Tezos_rpc.Service.get_service
      ~description:"Level of Tezos block known to the smart rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.int32
      (path / "level")

  let inbox =
    Tezos_rpc.Service.get_service
      ~description:"Rollup inbox for block"
      ~query:Tezos_rpc.Query.empty
      ~output:Inbox.encoding
      (path / "inbox")

  let ticks =
    Tezos_rpc.Service.get_service
      ~description:"Number of ticks for specified level"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.z
      (path / "ticks")

  let total_ticks =
    Tezos_rpc.Service.get_service
      ~description:"Total number of ticks at specified block"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.n
      (path / "total_ticks")

  let num_messages =
    Tezos_rpc.Service.get_service
      ~description:"Number of messages for specified block"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.z
      (path / "num_messages")

  let state_hash =
    Tezos_rpc.Service.get_service
      ~description:"State hash for this block"
      ~query:Tezos_rpc.Query.empty
      ~output:State_hash.encoding
      (path / "state_hash")

  let state_current_level =
    Tezos_rpc.Service.get_service
      ~description:"Retrieve the current level of a PVM"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.(option int32)
      (path / "state_current_level")

  let state_value =
    Tezos_rpc.Service.get_service
      ~description:"Retrieve value from key is PVM state of specified block"
      ~query:Query.key_query
      ~output:Data_encoding.bytes
      (path / "state")

  let committed_status =
    Tezos_rpc.Service.get_service
      ~description:
        "Commitment status of the rollup state which will include content of \
         this block"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.committed_status
      (path / "committed_status")

  module Helpers = struct
    type nonrec prefix = prefix

    let prefix = prefix

    let path = path / "helpers"
  end
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

  let outbox_pending =
    Tezos_rpc.Service.get_service
      ~description:
        "Pending outbox messages with their status (executable, pending, or \
         lost)"
      ~query:Query.outbox_level_query
      ~output:
        Data_encoding.(
          list
            (merge_objs
               Encodings.outbox
               (obj1 (req "status" Encodings.outbox_msg_status))))
      (path / "outbox" / "pending")

  let outbox_pending_executable =
    Tezos_rpc.Service.get_service
      ~description:"Pending outbox messages which can be executed"
      ~query:Query.outbox_level_query
      ~output:(Data_encoding.list Encodings.outbox)
      (path / "outbox" / "pending" / "executable")

  let outbox_pending_unexecutable =
    Tezos_rpc.Service.get_service
      ~description:"Pending outbox messages which cannot yet be executed"
      ~query:Query.outbox_level_query
      ~output:(Data_encoding.list Encodings.outbox)
      (path / "outbox" / "pending" / "unexecutable")

  let gc_info =
    Tezos_rpc.Service.get_service
      ~description:"Information about garbage collection"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.gc_info
      (path / "gc_info")

  let injection =
    Tezos_rpc.Service.post_service
      ~description:"Inject messages in the batcher's queue"
      ~query:Query.order_and_drop_duplicate_query
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

  let dal_batcher_injection =
    Tezos_rpc.Service.post_service
      ~description:
        "Inject the given messages in the DAL queue, even in case of duplicates"
      ~query:Tezos_rpc.Query.empty
      ~input:
        Data_encoding.(
          def
            "messages"
            ~description:"Messages to inject"
            (list (string' Plain)))
      ~output:Data_encoding.unit
      (path / "dal" / "batcher" / "injection")

  let injector_operation_status =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve the status of the injected operation using its injector ID."
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.message_status
      (path / "injector" / "operation"
     /: Tezos_crypto.Hashed.Injector_operations_hash.rpc_arg / "status")

  let dal_injected_operations_statuses =
    Tezos_rpc.Service.get_service
      ~description:
        "Retrieve the statuses of all known operations injected via DAL."
      ~query:Tezos_rpc.Query.empty
      ~output:
        Data_encoding.(
          list
            (obj2
               (req "id" Tezos_crypto.Hashed.Injector_operations_hash.encoding)
               (req "status" Encodings.message_status)))
      (path / "dal" / "injected" / "operations" / "statuses")

  let forget_dal_injection_id =
    Tezos_rpc.Service.post_service
      ~description:"Forget information about the injection whose id is given"
      ~query:Tezos_rpc.Query.empty
      ~input:Data_encoding.unit
      ~output:Data_encoding.unit
      (path / "dal" / "injection"
     /: Tezos_crypto.Hashed.Injector_operations_hash.rpc_arg / "forget")

  let dal_slot_indices =
    let input_encoding = Data_encoding.(obj1 (req "indices" (list uint8))) in
    Tezos_rpc.Service.post_service
      ~description:
        "Provide the (new) list of slot indices to use to the rollup node's \
         DAL injector"
      ~query:Tezos_rpc.Query.empty
      ~input:
        Data_encoding.(
          def "slot_indices" ~description:"Slot indices to set" input_encoding)
      ~output:Data_encoding.unit
      (path / "dal" / "slot" / "indices")

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

  let ping =
    Tezos_rpc.Service.get_service
      ~description:
        "Returns an empty response if the rollup node can answer requests"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.unit
      (path / "ping")

  let health =
    Tezos_rpc.Service.get_service
      ~description:"Returns health status information for the rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.health
      (path / "health")

  let version =
    Tezos_rpc.Service.get_service
      ~description:"Returns the version information of the rollup node"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.version
      (path / "version")

  let ocaml_gc =
    Tezos_rpc.Service.get_service
      ~description:"Gets stats from the OCaml Garbage Collector"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.ocaml_gc_stat_encoding
      (path / "stats" / "ocaml_gc")

  let memory =
    Tezos_rpc.Service.get_service
      ~description:"Gets memory usage stats"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.mem_stat_encoding
      (path / "stats" / "memory")

  let openapi =
    Tezos_rpc.Service.get_service
      ~description:"OpenAPI specification of RPCs for rollup node"
      ~query:Query.proto_query
      ~output:Data_encoding.json
      (path / "openapi")
end

module Admin = struct
  open Tezos_rpc.Path

  include Make_services (struct
    type prefix = unit

    let prefix = open_root / "admin"
  end)

  let injector_queues_total =
    Tezos_rpc.Service.get_service
      ~description:"Get total operations queued in injectors"
      ~query:Tezos_rpc.Query.empty
      ~output:Encodings.injector_queues_total
      (path / "injector" / "queues" / "total")

  let injector_queues =
    Tezos_rpc.Service.get_service
      ~description:"Get operation queues of injectors"
      ~query:Query.operation_tag_query
      ~output:Encodings.injector_queues
      (path / "injector" / "queues")

  let clear_injector_queues =
    Tezos_rpc.Service.delete_service
      ~description:"Clear operation queues of injectors"
      ~query:Query.operation_tag_and_order_below_query
      ~output:Data_encoding.unit
      (path / "injector" / "queues")

  let cancel_gc =
    Tezos_rpc.Service.get_service
      ~description:"Cancel any ongoing GC"
      ~query:Tezos_rpc.Query.empty
      ~output:Data_encoding.bool
      (path / "cancel_gc")

  let clear_batcher_queues =
    Tezos_rpc.Service.delete_service
      ~description:"Clear operation queues of injectors"
      ~query:Query.order_below_query
      ~output:Data_encoding.unit
      (path / "batcher" / "queue")
end
