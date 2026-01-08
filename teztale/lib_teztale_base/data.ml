(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2020 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

module Delegate_operations = struct
  type reception = {
    source : string;
    reception_time : Time.System.t;
    errors : error list option;
  }

  let reception_encoding =
    let open Data_encoding in
    conv
      (fun {source; reception_time; errors} -> (source, reception_time, errors))
      (fun (source, reception_time, errors) -> {source; reception_time; errors})
      (obj3
         (req "source" string)
         (req "reception_time" Time.System.encoding)
         (dft "errors" Tezos_rpc.Error.opt_encoding None))

  type operation = {
    hash : Operation_hash.t;
    kind : Consensus_ops.operation_kind;
    round : Int32.t option;
    mempool_inclusion : reception list;
    block_inclusion : Block_hash.t list;
  }

  let legacy_operation_encoding =
    let open Data_encoding in
    conv
      (fun _ -> assert false)
      (fun (kind, round, reception_time, errors, block_inclusion) ->
        let mempool_inclusion =
          match reception_time with
          | None -> []
          | Some reception_time ->
              [{source = "archiver"; reception_time; errors}]
        in
        {
          hash = Operation_hash.zero;
          kind;
          round;
          mempool_inclusion;
          block_inclusion;
        })
      (obj5
         (dft
            "kind"
            Consensus_ops.operation_kind_encoding
            Consensus_ops.Attestation)
         (opt "round" int32)
         (req "reception_time" (option Time.System.encoding))
         (dft "errors" Tezos_rpc.Error.opt_encoding None)
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  let operation_encoding =
    let open Data_encoding in
    conv
      (fun {hash; kind; round; mempool_inclusion; block_inclusion} ->
        (hash, kind, round, mempool_inclusion, block_inclusion))
      (fun (hash, kind, round, mempool_inclusion, block_inclusion) ->
        {hash; kind; round; mempool_inclusion; block_inclusion})
      (obj5
         (dft "hash" Operation_hash.encoding Operation_hash.zero)
         (dft
            "kind"
            Consensus_ops.operation_kind_encoding
            Consensus_ops.Attestation)
         (opt "round" int32)
         (dft "received_in_mempools" (list reception_encoding) [])
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  let operation_encoding =
    let open Data_encoding in
    splitted
      ~json:
        (union
           [
             case
               ~title:"current"
               Json_only
               operation_encoding
               Option.some
               (fun x -> x);
             case
               ~title:"legacy"
               Json_only
               legacy_operation_encoding
               (fun _ -> None)
               (fun x -> x);
           ])
      ~binary:operation_encoding

  type t = {
    delegate : Tezos_crypto.Signature.public_key_hash;
    first_slot : int;
    attesting_power : int;
    operations : operation list;
  }

  let legacy_encoding =
    let open Data_encoding in
    conv
      (fun _ -> assert false)
      (fun (delegate, reception_time, errors, block_inclusion) ->
        match (reception_time, block_inclusion) with
        | None, [] ->
            {delegate; first_slot = 0; attesting_power = 0; operations = []}
        | _, _ ->
            let mempool_inclusion =
              match reception_time with
              | None -> []
              | Some reception_time ->
                  [{source = "archiver"; reception_time; errors}]
            in
            {
              delegate;
              first_slot = 0;
              attesting_power = 0;
              operations =
                [
                  {
                    hash = Operation_hash.zero;
                    kind = Attestation;
                    mempool_inclusion;
                    round = None;
                    block_inclusion;
                  };
                ];
            })
      (obj4
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (opt "reception_time" Time.System.encoding)
         (opt "errors" (list error_encoding))
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  let encoding =
    let open Data_encoding in
    conv
      (fun {delegate; first_slot; attesting_power; operations} ->
        (delegate, first_slot, attesting_power, operations))
      (fun (delegate, first_slot, attesting_power, operations) ->
        {delegate; first_slot; attesting_power; operations})
      (obj4
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (dft "first_slot" int16 0)
         (dft "endorsing_power" int16 0)
         (dft "operations" (list operation_encoding) []))

  let encoding =
    let open Data_encoding in
    splitted
      ~json:
        (union
           [
             case ~title:"current" Json_only encoding Option.some (fun x -> x);
             case
               ~title:"legacy"
               Json_only
               legacy_encoding
               (fun _ -> None)
               (fun x -> x);
           ])
      ~binary:encoding
end

module Block = struct
  type reception = {
    source : string;
    application_time : Time.System.t option;
    validation_time : Time.System.t option;
  }

  type t = {
    hash : Block_hash.t;
    predecessor : Block_hash.t option;
    delegate : Tezos_crypto.Signature.public_key_hash;
    round : Int32.t;
    timestamp : Time.Protocol.t;
    reception_times : reception list;
    nonce : unit option;
  }

  let reception_encoding =
    let open Data_encoding in
    conv
      (fun {source; application_time; validation_time} ->
        (source, application_time, validation_time))
      (fun (source, application_time, validation_time) ->
        {source; application_time; validation_time})
      (obj3
         (req "source" string)
         (opt "application" Time.System.encoding)
         (opt "validation" Time.System.encoding))

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             hash;
             predecessor;
             delegate;
             round;
             reception_times;
             timestamp;
             nonce;
           }
         ->
        (hash, predecessor, delegate, round, reception_times, timestamp, nonce))
      (fun ( hash,
             predecessor,
             delegate,
             round,
             reception_times,
             timestamp,
             nonce )
         ->
        {hash; predecessor; delegate; round; reception_times; timestamp; nonce})
      (obj7
         (req "hash" Block_hash.encoding)
         (opt "predecessor" Block_hash.encoding)
         (dft
            "delegate"
            Tezos_crypto.Signature.Public_key_hash.encoding
            Tezos_crypto.Signature.Public_key_hash.zero)
         (dft "round" int32 0l)
         (dft "reception_times" (list reception_encoding) [])
         (dft "timestamp" Time.Protocol.encoding Time.Protocol.epoch)
         (opt "nonce" unit))
end

type cycle_info = {cycle : int32; cycle_position : int32; cycle_size : int32}

let cycle_info_encoding =
  let open Data_encoding in
  conv
    (fun {cycle; cycle_position; cycle_size} ->
      (cycle, cycle_position, cycle_size))
    (fun (cycle, cycle_position, cycle_size) ->
      {cycle; cycle_position; cycle_size})
    (obj3
       (req "cycle" int32)
       (req "cycle_position" int32)
       (req "cycle_size" int32))

type baking_right = {
  delegate : Tezos_crypto.Signature.public_key_hash;
  round : int32;
}

let baking_right_encoding =
  let open Data_encoding in
  conv
    (fun {delegate; round} -> (delegate, round))
    (fun (delegate, round) -> {delegate; round})
    (obj2
       (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
       (req "round" int32))

type missing_blocks = {baking_right : baking_right; sources : string list}

let missing_blocks_encoding =
  let open Data_encoding in
  conv
    (fun {baking_right; sources} -> (baking_right, sources))
    (fun (baking_right, sources) -> {baking_right; sources})
    (obj2
       (req "baking_right" baking_right_encoding)
       (req "sources" (list string)))

type t = {
  cycle_info : cycle_info option;
  blocks : Block.t list;
  delegate_operations : Delegate_operations.t list;
  missing_blocks : missing_blocks list;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {cycle_info; blocks; delegate_operations; missing_blocks} ->
      (cycle_info, blocks, delegate_operations, missing_blocks))
    (fun (cycle_info, blocks, delegate_operations, missing_blocks) ->
      {cycle_info; blocks; delegate_operations; missing_blocks})
    (obj4
       (opt "cycle_info" cycle_info_encoding)
       (dft "blocks" (list Block.encoding) [])
       (* TODO: change name? *)
       (dft "endorsements" (list Delegate_operations.encoding) [])
       (dft "missing_blocks" (list missing_blocks_encoding) []))

let empty =
  {
    cycle_info = None;
    blocks = [];
    delegate_operations = [];
    missing_blocks = [];
  }

type batch_item = {level : int32; data : t}

type batch = batch_item list

let batch_item_encoding =
  let open Data_encoding in
  conv
    (fun {level; data} -> (level, data))
    (fun (level, data) -> {level; data})
    (obj2 (req "level" int32) (req "data" encoding))

let batch_encoding = Data_encoding.list batch_item_encoding

let level_timestamp_encoding =
  let open Data_encoding in
  conv
    (fun x -> x)
    (fun x -> x)
    (obj2 (req "level" int32) (req "timestamp" int32))

let surrounding_levels_encoding =
  let open Data_encoding in
  conv
    (fun x -> x)
    (fun x -> x)
    (obj2
       (opt "lower" level_timestamp_encoding)
       (opt "upper" level_timestamp_encoding))

type head = {level : int32}

let head_encoding =
  let open Data_encoding in
  conv (fun {level} -> level) (fun level -> {level}) (obj1 (req "level" int32))

module Archiver = struct
  (* Collected by the archiver and sent to the server *)
  type raw_block_data =
    Block.t
    * cycle_info option
    * (Consensus_ops.block_op list * Consensus_ops.block_op list)
    * baking_right list

  let raw_block_data_encoding =
    let open Data_encoding in
    conv
      (fun (block_info, cycle_info, (att, preatt), baking_rights) ->
        (block_info, (cycle_info, att, preatt, baking_rights)))
      (fun (block_info, (cycle_info, att, preatt, baking_rights)) ->
        (block_info, cycle_info, (att, preatt), baking_rights))
      (merge_objs
         Block.encoding
         (obj4
            (opt "cycle_info" cycle_info_encoding)
            (req "endorsements" (list Consensus_ops.block_op_encoding))
            (dft "preendorsements" (list Consensus_ops.block_op_encoding) [])
            (dft "baking_rights" (list baking_right_encoding) [])))
end
