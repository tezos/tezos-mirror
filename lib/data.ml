(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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
            Consensus_ops.Endorsement)
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
            Consensus_ops.Endorsement)
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
    endorsing_power : int;
    operations : operation list;
  }

  let legacy_encoding =
    let open Data_encoding in
    conv
      (fun _ -> assert false)
      (fun (delegate, reception_time, errors, block_inclusion) ->
        match (reception_time, block_inclusion) with
        | None, [] ->
            {delegate; first_slot = 0; endorsing_power = 0; operations = []}
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
              endorsing_power = 0;
              operations =
                [
                  {
                    hash = Operation_hash.zero;
                    kind = Endorsement;
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
      (fun {delegate; first_slot; endorsing_power; operations} ->
        (delegate, first_slot, endorsing_power, operations))
      (fun (delegate, first_slot, endorsing_power, operations) ->
        {delegate; first_slot; endorsing_power; operations})
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
           } ->
        (hash, predecessor, delegate, round, reception_times, timestamp, nonce))
      (fun ( hash,
             predecessor,
             delegate,
             round,
             reception_times,
             timestamp,
             nonce ) ->
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

type t = {
  blocks : Block.t list;
  delegate_operations : Delegate_operations.t list;
  unaccurate : bool;
}

let encoding =
  let open Data_encoding in
  conv
    (fun {blocks; delegate_operations; unaccurate} ->
      (blocks, delegate_operations, unaccurate))
    (fun (blocks, delegate_operations, unaccurate) ->
      {blocks; delegate_operations; unaccurate})
    (obj3
       (dft "blocks" (list Block.encoding) [])
       (* TODO: change name? *)
       (dft "endorsements" (list Delegate_operations.encoding) [])
       (dft "unaccurate" bool false))

let empty = {blocks = []; delegate_operations = []; unaccurate = true}

let block_data_encoding =
  let open Data_encoding in
  merge_objs
    Block.encoding
    (obj2
       (req "endorsements" (list Consensus_ops.block_op_encoding))
       (dft "preendorsements" (list Consensus_ops.block_op_encoding) []))

module Anomaly = struct
  (* only anomalies related to endorsements are considered for now *)
  type problem = Missed | Forgotten | Sequestered | Incorrect

  type t = {
    level : Int32.t;
    round : Int32.t option;
    kind : Consensus_ops.operation_kind;
    delegate : Tezos_crypto.Signature.Public_key_hash.t;
    problem : problem;
  }

  let problem_encoding =
    Data_encoding.string_enum
      [
        ("missed", Missed);
        ("forgotten", Forgotten);
        ("sequestered", Sequestered);
        ("incorrect", Incorrect);
      ]

  let encoding =
    let open Data_encoding in
    conv
      (fun {level; round; kind; delegate; problem} ->
        (level, round, kind, delegate, problem))
      (fun (level, round, kind, delegate, problem) ->
        {level; round; kind; delegate; problem})
      (obj5
         (req "level" int32)
         (opt "round" int32)
         (dft
            "kind"
            Consensus_ops.operation_kind_encoding
            Consensus_ops.Endorsement)
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (req "problem" problem_encoding))
end
