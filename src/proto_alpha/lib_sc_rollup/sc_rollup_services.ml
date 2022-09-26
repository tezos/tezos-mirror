(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_rpc
open Protocol
open Alpha_context

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

module Encodings = struct
  open Data_encoding

  let commitment_with_hash_and_level =
    obj3
      (req "commitment" Sc_rollup.Commitment.encoding)
      (req "hash" Sc_rollup.Commitment.Hash.encoding)
      (opt "published_at_level" Raw_level.encoding)

  let hex_string = conv Bytes.of_string Bytes.to_string bytes
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

  let block_id : block_id RPC_arg.t =
    RPC_arg.make
      ~descr:"An L1 block identifier."
      ~name:"block_id"
      ~construct:construct_block_id
      ~destruct:destruct_block_id
      ()
end

module type PREFIX = sig
  type prefix

  val prefix : (unit, prefix) RPC_path.t
end

module Make_services (P : PREFIX) = struct
  include P

  let path : prefix RPC_path.context = RPC_path.open_root

  let make_call s = RPC_context.make_call (RPC_service.prefix prefix s)

  let make_call1 s = RPC_context.make_call1 (RPC_service.prefix prefix s)

  let make_call2 s = RPC_context.make_call2 (RPC_service.prefix prefix s)
end

module Global = struct
  open RPC_path

  include Make_services (struct
    type prefix = unit

    let prefix = open_root / "global"
  end)

  let sc_rollup_address =
    RPC_service.get_service
      ~description:"Smart-contract rollup address"
      ~query:RPC_query.empty
      ~output:Sc_rollup.Address.encoding
      (path / "sc_rollup_address")

  let current_tezos_head =
    RPC_service.get_service
      ~description:"Tezos head known to the smart-contract rollup node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Block_hash.encoding)
      (path / "tezos_head")

  let current_tezos_level =
    RPC_service.get_service
      ~description:"Tezos level known to the smart-contract rollup node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Data_encoding.int32)
      (path / "tezos_level")

  let last_stored_commitment =
    RPC_service.get_service
      ~description:"Last commitment computed by the node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Encodings.commitment_with_hash_and_level)
      (path / "last_stored_commitment")

  let outbox_proof_query =
    let open RPC_query in
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
    |+ opt_field "outbox_level" RPC_arg.int32 (fun o ->
           Some (Raw_level.to_int32 o.outbox_level))
    |+ opt_field "message_index" RPC_arg.int64 (fun o ->
           Some (Z.to_int64 o.message_index))
    |+ opt_field "serialized_outbox_message" RPC_arg.string (fun o ->
           match Outbox.Message.serialize o.message with
           | Ok message -> Some (Outbox.Message.unsafe_to_string message)
           | Error e -> invalid_message e)
    |> seal

  let outbox_proof =
    RPC_service.get_service
      ~description:"Generate serialized output proof for some outbox message"
      ~query:outbox_proof_query
      ~output:
        Data_encoding.(
          obj2
            (req "commitment" Sc_rollup.Commitment.Hash.encoding)
            (req "proof" Encodings.hex_string))
      (path / "proofs" / "outbox")

  module Block = struct
    include Make_services (struct
      type prefix = unit * Arg.block_id

      let prefix = prefix / "block" /: Arg.block_id
    end)

    let hash =
      RPC_service.get_service
        ~description:
          "Tezos block hash of block known to the smart-contract rollup node"
        ~query:RPC_query.empty
        ~output:Block_hash.encoding
        (path / "hash")

    let level =
      RPC_service.get_service
        ~description:
          "Level of Tezos block known to the smart-contract rollup node"
        ~query:RPC_query.empty
        ~output:Data_encoding.int32
        (path / "level")

    let inbox =
      RPC_service.get_service
        ~description:"Rollup inbox for block"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Inbox.encoding
        (path / "inbox")

    let ticks =
      RPC_service.get_service
        ~description:"Number of ticks for specified level"
        ~query:RPC_query.empty
        ~output:Data_encoding.z
        (path / "ticks")

    let total_ticks =
      RPC_service.get_service
        ~description:"Total number of ticks at specified block"
        ~query:RPC_query.empty
        ~output:Sc_rollup.Tick.encoding
        (path / "total_ticks")

    let num_messages =
      RPC_service.get_service
        ~description:"Number of messages for specified block"
        ~query:RPC_query.empty
        ~output:Data_encoding.z
        (path / "num_messages")

    let state_hash =
      RPC_service.get_service
        ~description:"State hash for this block"
        ~query:RPC_query.empty
        ~output:Sc_rollup.State_hash.encoding
        (path / "state_hash")

    type state_value_query = {key : string}

    let state_value_query : state_value_query RPC_query.t =
      let open RPC_query in
      query (fun key -> {key})
      |+ field "key" RPC_arg.string "" (fun t -> t.key)
      |> seal

    let state_value =
      RPC_service.get_service
        ~description:"Retrieve value from key is PVM state of specified block"
        ~query:state_value_query
        ~output:Data_encoding.bytes
        (path / "state")

    let status =
      RPC_service.get_service
        ~description:"PVM status at block"
        ~query:RPC_query.empty
        ~output:Data_encoding.string
        (path / "status")

    let outbox =
      RPC_service.get_service
        ~description:"Outbox at block"
        ~query:RPC_query.empty
        ~output:Data_encoding.(list Sc_rollup.output_encoding)
        (path / "outbox")

    let dal_slot_subscriptions =
      RPC_service.get_service
        ~description:"Data availability layer slot subscriptions at block"
        ~query:RPC_query.empty
        ~output:Data_encoding.(option @@ list Dal.Slot_index.encoding)
        (path / "dal" / "slot_subscriptions")

    let dal_slots =
      RPC_service.get_service
        ~description:"Availability slots for a given block"
        ~query:RPC_query.empty
        ~output:(Data_encoding.list Dal.Slot.Header.encoding)
        (path / "dal" / "slot_headers")

    let dal_slot_pages =
      RPC_service.get_service
        ~description:
          "Data availability downloaded slot pages for a given block hash"
        ~query:RPC_query.empty
        ~output:
          (* DAL/FIXME: https://gitlab.com/tezos/tezos/-/issues/3873
               Estimate size of binary encoding and add a check_size to the
             encoding. *)
          Data_encoding.(
            list
            @@ obj2
                 (req "index" Dal.Slot_index.encoding)
                 (req "contents" (list @@ option Dal.Page.content_encoding)))
        (path / "dal" / "slot_pages")

    type dal_slot_page_query = {index : Dal.Slot_index.t; page : int}

    let dal_slot_page_query =
      let open RPC_query in
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
      |+ opt_field "index" RPC_arg.int (fun q ->
             Some (Dal.Slot_index.to_int q.index))
      |+ opt_field "slot_page" RPC_arg.int (fun q -> Some q.page)
      |> seal

    let dal_slot_page =
      RPC_service.get_service
        ~description:
          "Data availability downloaded slot pages for a given block hash"
        ~query:dal_slot_page_query
        ~output:
          Data_encoding.(
            obj2
              (req "result" string)
              (opt "contents" Dal.Page.content_encoding))
        (path / "dal" / "slot_page")
  end
end

module Local = struct
  open RPC_path

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
    RPC_service.get_service
      ~description:"Last commitment published by the node"
      ~query:RPC_query.empty
      ~output:(Data_encoding.option Encodings.commitment_with_hash_and_level)
      (path / "last_published_commitment")
end
