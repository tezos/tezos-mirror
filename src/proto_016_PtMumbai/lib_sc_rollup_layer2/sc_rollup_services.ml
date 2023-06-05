(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2023 TriliTech <contact@trili.tech>                         *)
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

open Protocol.Alpha_context

(* We distinguish RPC endpoints served by the rollup node into `global` and
   `local`. The difference between the two lies in whether the responses
   given by different rollup nodes in the same state (see below for an
   exact definition) must be the same (in the case of global endpoints)
   or can differ (in the case of local endpoints).

   More formally,  two rollup nodes are in the same quiescent state if they are
   subscribed to the same rollup address, and have processed the same set of
   heads from the layer1. We only consider quiescent states, that is those
   where rollup nodes are not actively processing a head received from layer1.

   Examples of global endpoints are `current_inbox` and
   `last_stored_commitment`, as the responses returned by these endpoints
   is expected to be consistent across rollup nodes in the same state.

   An example of local endpoint is `last_published_commitments`, as two rollup
   nodes in the same state may either publish or not publish a commitment,
   according to whether its inbox level is below the inbox level of the
   last cemented commitment at the time they tried to publish the commitment.
   See below for a more detailed explanation.
*)

type eval_result = {
  state_hash : Sc_rollup.State_hash.t;
  status : string;
  output : Sc_rollup.output list;
  inbox_level : Raw_level.t;
  num_ticks : Z.t;
  insights : bytes option list;
      (** The simulation can ask to look at values on the state after
          the simulation. *)
}

type insight_request =
  | Pvm_state_key of string list
  | Durable_storage_key of string list

type simulate_input = {
  messages : string list;
  reveal_pages : string list option;
  insight_requests : insight_request list;
}

type commitment_info = {
  commitment : Sc_rollup.Commitment.t;
  commitment_hash : Sc_rollup.Commitment.Hash.t;
  first_published_at_level : Raw_level.t;
  published_at_level : Raw_level.t;
}

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
      commitment : Sc_rollup.Commitment.t;
      commitment_hash : Sc_rollup.Commitment.Hash.t;
      first_published_at_level : Raw_level.t;
      published_at_level : Raw_level.t;
    }

module Encodings = struct
  open Data_encoding

  let commitment_with_hash =
    obj2
      (req "commitment" Sc_rollup.Commitment.encoding)
      (req "hash" Sc_rollup.Commitment.Hash.encoding)

  let commitment_with_hash_and_level_infos =
    obj4
      (req "commitment" Sc_rollup.Commitment.encoding)
      (req "hash" Sc_rollup.Commitment.Hash.encoding)
      (opt "first_published_at_level" Raw_level.encoding)
      (opt "published_at_level" Raw_level.encoding)

  let hex_string = conv Bytes.of_string Bytes.to_string bytes

  let eval_result =
    conv
      (fun {state_hash; status; output; inbox_level; num_ticks; insights} ->
        (state_hash, status, output, inbox_level, num_ticks, insights))
      (fun (state_hash, status, output, inbox_level, num_ticks, insights) ->
        {state_hash; status; output; inbox_level; num_ticks; insights})
    @@ obj6
         (req
            "state_hash"
            Sc_rollup.State_hash.encoding
            ~description:
              "Hash of the state after execution of the PVM on the input \
               messages")
         (req "status" string ~description:"Status of the PVM after evaluation")
         (req
            "output"
            (list Sc_rollup.output_encoding)
            ~description:"Output produced by evaluation of the messages")
         (req
            "inbox_level"
            Raw_level.encoding
            ~description:"Level of the inbox that would contain these messages")
         (req
            "num_ticks"
            z
            ~description:"Ticks taken by the PVM for evaluating the messages")
         (dft
            "insights"
            (list (option bytes))
            []
            ~description:"PVM state values requested after the simulation")

  let insight_request =
    union
      [
        case
          (Tag 0)
          ~title:"pvm_state"
          ~description:"Path in the PVM state"
          (obj2 (req "kind" (constant "pvm_state")) (req "key" (list string)))
          (function Pvm_state_key key -> Some ((), key) | _ -> None)
          (fun ((), key) -> Pvm_state_key key);
        case
          (Tag 1)
          ~title:"durable_storage"
          ~description:"Path in the PVM durable storage"
          (obj2
             (req "kind" (constant "durable_storage"))
             (req "key" (list string)))
          (function Durable_storage_key key -> Some ((), key) | _ -> None)
          (fun ((), key) -> Durable_storage_key key);
      ]

  let simulate_input =
    conv
      (fun {messages; reveal_pages; insight_requests} ->
        (messages, reveal_pages, insight_requests))
      (fun (messages, reveal_pages, insight_requests) ->
        {messages; reveal_pages; insight_requests})
    @@ obj3
         (req
            "messages"
            (list hex_string)
            ~description:"Serialized messages for simulation.")
         (opt
            "reveal_pages"
            (list hex_string)
            ~description:"Pages (at most 4kB) to be used for revelation ticks")
         (dft
            "insight_requests"
            (list insight_request)
            []
            ~description:"Paths in the PVM to inspect after the simulation")

  let queued_message =
    obj2
      (req "hash" L2_message.Hash.encoding)
      (req "message" L2_message.encoding)

  let batcher_queue = list queued_message

  let commitment_info =
    conv
      (fun {
             commitment;
             commitment_hash;
             first_published_at_level;
             published_at_level;
           } ->
        ( commitment,
          commitment_hash,
          first_published_at_level,
          published_at_level ))
      (fun ( commitment,
             commitment_hash,
             first_published_at_level,
             published_at_level ) ->
        {
          commitment;
          commitment_hash;
          first_published_at_level;
          published_at_level;
        })
    @@ obj4
         (req "commitment" Sc_rollup.Commitment.encoding)
         (req "hash" Sc_rollup.Commitment.Hash.encoding)
         (req "first_published_at_level" Raw_level.encoding)
         (req "published_at_level" Raw_level.encoding)

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
             (req "commitment" Sc_rollup.Commitment.encoding)
             (req "hash" Sc_rollup.Commitment.Hash.encoding)
             (req "first_published_at_level" Raw_level.encoding)
             (req "published_at_level" Raw_level.encoding))
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
    merge_objs (obj1 (opt "content" hex_string)) message_status
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

  let l2_message_hash : L2_message.hash Tezos_rpc.Arg.t =
    Tezos_rpc.Arg.make
      ~descr:"A L2 message hash."
      ~name:"l2_message_hash"
      ~construct:L2_message.Hash.to_b58check
      ~destruct:(fun s ->
        L2_message.Hash.of_b58check_opt s
        |> Option.to_result ~none:"Invalid L2 message hash")
      ()
end

module Query = struct
  let outbox_proof_query =
    let open Tezos_rpc.Query in
    let open Sc_rollup in
    let invalid_message e =
      raise
        (Invalid
           (Format.asprintf
              "Invalid message (%a)"
              Environment.Error_monad.pp_trace
              e))
    in
    query (fun outbox_level message_index serialized_outbox_message ->
        let req name f = function
          | None ->
              raise
                (Invalid (Format.sprintf "Query parameter %s is required" name))
          | Some arg -> f arg
        in
        let outbox_level =
          req "outbox_level" Raw_level.of_int32_exn outbox_level
        in
        let message_index = req "message_index" Z.of_int64 message_index in
        let message =
          req
            "serialized_outbox_message"
            (fun s -> Outbox.Message.(unsafe_of_string s |> deserialize))
            serialized_outbox_message
        in
        match message with
        | Error e -> invalid_message e
        | Ok message -> {outbox_level; message_index; message})
    |+ opt_field "outbox_level" Tezos_rpc.Arg.int32 (fun o ->
           Some (Raw_level.to_int32 o.outbox_level))
    |+ opt_field "message_index" Tezos_rpc.Arg.int64 (fun o ->
           Some (Z.to_int64 o.message_index))
    |+ opt_field "serialized_outbox_message" Tezos_rpc.Arg.string (fun o ->
           match Outbox.Message.serialize o.message with
           | Ok message -> Some (Outbox.Message.unsafe_to_string message)
           | Error e -> invalid_message e)
    |> seal

  type key_query = {key : string}

  let key_query : key_query Tezos_rpc.Query.t =
    let open Tezos_rpc.Query in
    query (fun key -> {key})
    |+ field "key" Tezos_rpc.Arg.string "" (fun t -> t.key)
    |> seal

  let outbox_level_query =
    let open Tezos_rpc.Query in
    query (fun outbox_level ->
        let req name f = function
          | None ->
              raise
                (Invalid (Format.sprintf "Query parameter %s is required" name))
          | Some arg -> f arg
        in
        req "outbox_level" Raw_level.of_int32_exn outbox_level)
    |+ opt_field "outbox_level" Tezos_rpc.Arg.int32 (fun o ->
           Some (Raw_level.to_int32 o))
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

type simulate_query = {fuel : int64 option}

let simulate_query : simulate_query Tezos_rpc.Query.t =
  let open Tezos_rpc.Query in
  query (fun fuel -> {fuel})
  |+ opt_field "fuel" Tezos_rpc.Arg.int64 (fun t -> t.fuel)
  |> seal

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
      ~output:Sc_rollup.Address.encoding
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

  module Helpers = struct
    include Make_services (struct
      type prefix = unit

      let prefix = open_root / "helpers"
    end)

    let outbox_proof =
      Tezos_rpc.Service.get_service
        ~description:"Generate serialized output proof for some outbox message"
        ~query:Query.outbox_proof_query
        ~output:
          Data_encoding.(
            obj2
              (req "commitment" Sc_rollup.Commitment.Hash.encoding)
              (req "proof" Encodings.hex_string))
        (path / "proofs" / "outbox")
  end

  module Block = struct
    include Make_services (struct
      type prefix = unit * Arg.block_id

      let prefix = prefix / "block" /: Arg.block_id
    end)

    let block =
      Tezos_rpc.Service.get_service
        ~description:
          "Layer-2 block of the layer-2 chain with respect to a Layer 1 block \
           identifier"
        ~query:Tezos_rpc.Query.empty
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
        ~output:Sc_rollup.Inbox.encoding
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
        ~output:Sc_rollup.Tick.encoding
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
        ~output:Sc_rollup.State_hash.encoding
        (path / "state_hash")

    let state_current_level =
      Tezos_rpc.Service.get_service
        ~description:"Retrieve the current level of a PVM"
        ~query:Tezos_rpc.Query.empty
        ~output:(Data_encoding.option Raw_level.encoding)
        (path / "state_current_level")

    let state_value =
      Tezos_rpc.Service.get_service
        ~description:"Retrieve value from key is PVM state of specified block"
        ~query:Query.key_query
        ~output:Data_encoding.bytes
        (path / "state")

    let durable_state_value (pvm_kind : Sc_rollup.Kind.t) =
      Tezos_rpc.Service.get_service
        ~description:
          "Retrieve value by key from PVM durable storage. PVM state is taken \
           with respect to the specified block level. Value returned in hex \
           format."
        ~query:Query.key_query
        ~output:Data_encoding.(option bytes)
        (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "value")

    let durable_state_length
        (pvm_kind : Protocol.Alpha_context.Sc_rollup.Kind.t) =
      Tezos_rpc.Service.get_service
        ~description:
          "Retrieve number of bytes in raw representation of value by key from \
           PVM durable storage. PVM state is taken with respect to the \
           specified block level."
        ~query:Query.key_query
        ~output:Data_encoding.(option int64)
        (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "length")

    let durable_state_subkeys (pvm_kind : Sc_rollup.Kind.t) =
      Tezos_rpc.Service.get_service
        ~description:
          "Retrieve subkeys of the specified key from PVM durable storage. PVM \
           state is taken with respect to the specified block level."
        ~query:Query.key_query
        ~output:Data_encoding.(list string)
        (path / "durable" / Sc_rollup.Kind.to_string pvm_kind / "subkeys")

    let status =
      Tezos_rpc.Service.get_service
        ~description:"PVM status at block"
        ~query:Tezos_rpc.Query.empty
        ~output:Data_encoding.string
        (path / "status")

    let outbox =
      Tezos_rpc.Service.get_service
        ~description:"Outbox at block for a given outbox level"
        ~query:Query.outbox_level_query
        ~output:Data_encoding.(list Sc_rollup.output_encoding)
        (path / "outbox")

    let simulate =
      Tezos_rpc.Service.post_service
        ~description:"Simulate messages evaluation by the PVM"
        ~query:Tezos_rpc.Query.empty
        ~input:Encodings.simulate_input
        ~output:Encodings.eval_result
        (path / "simulate")

    let dal_slots =
      Tezos_rpc.Service.get_service
        ~description:"Availability slots for a given block"
        ~query:Tezos_rpc.Query.empty
        ~output:(Data_encoding.list Dal.Slot.Header.encoding)
        (path / "dal" / "slot_headers")

    let dal_confirmed_slot_pages =
      Tezos_rpc.Service.get_service
        ~description:
          "Data availability confirmed & downloaded slot pages for a given \
           block hash"
        ~query:Tezos_rpc.Query.empty
        ~output:
          (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3873
               Estimate size of binary encoding and add a check_size to the
             encoding. *)
          Data_encoding.(
            list
            @@ obj2
                 (req "index" Dal.Slot_index.encoding)
                 (req "contents" (list Dal.Page.content_encoding)))
        (path / "dal" / "confirmed_slot_pages")

    type dal_slot_page_query = {index : Dal.Slot_index.t; page : int}

    let dal_slot_page_query =
      let open Tezos_rpc.Query in
      let req name f = function
        | None ->
            raise
              (Invalid (Format.sprintf "Query parameter %s is required" name))
        | Some arg -> f arg
      in
      let invalid_parameter i =
        raise (Invalid (Format.asprintf "Invalid parameter (%d)" i))
      in
      query (fun raw_index raw_page ->
          let index = req "index" Dal.Slot_index.of_int raw_index in
          let page = req "page" (fun p -> p) raw_page in
          match index with
          | None -> invalid_parameter @@ Option.value ~default:0 raw_index
          | Some index ->
              if page < 0 then invalid_parameter page else {index; page})
      |+ opt_field "index" Tezos_rpc.Arg.int (fun q ->
             Some (Dal.Slot_index.to_int q.index))
      |+ opt_field "slot_page" Tezos_rpc.Arg.int (fun q -> Some q.page)
      |> seal

    let dal_slot_page =
      Tezos_rpc.Service.get_service
        ~description:
          "Data availability downloaded slot pages for a given block hash"
        ~query:dal_slot_page_query
        ~output:
          Data_encoding.(
            obj2
              (req "result" string)
              (opt "contents" Dal.Page.content_encoding))
        (path / "dal" / "slot_page")

    module Outbox = struct
      let level_param =
        let destruct s =
          match Int32.of_string_opt s with
          | None -> Error "Invalid level"
          | Some l -> (
              match Raw_level.of_int32 l with
              | Error _ -> Error "Invalid level"
              | Ok l -> Ok l)
        in
        let construct = Format.asprintf "%a" Raw_level.pp in
        Tezos_rpc.Arg.make ~name:"level" ~construct ~destruct ()

      include Make_services (struct
        type nonrec prefix = prefix * Raw_level.t

        let prefix = prefix / "outbox" /: level_param
      end)

      let messages =
        Tezos_rpc.Service.get_service
          ~description:"Outbox at block for a given outbox level"
          ~query:Tezos_rpc.Query.empty
          ~output:Data_encoding.(list Sc_rollup.output_encoding)
          (path / "messages")
    end
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
            "message_hashes"
            ~description:"Hashes of injected L2 messages"
            (list L2_message.Hash.encoding))
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
      (path / "batcher" / "queue" /: Arg.l2_message_hash)
end
