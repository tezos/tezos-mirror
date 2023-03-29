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
        {kind; round; mempool_inclusion; block_inclusion})
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
      (fun {kind; round; mempool_inclusion; block_inclusion} ->
        (kind, round, mempool_inclusion, block_inclusion))
      (fun (kind, round, mempool_inclusion, block_inclusion) ->
        {kind; round; mempool_inclusion; block_inclusion})
      (obj4
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
    delegate_alias : string option;
    endorsing_power : int;
    operations : operation list;
  }

  let legacy_encoding =
    let open Data_encoding in
    conv
      (fun _ -> assert false)
      (fun (delegate, delegate_alias, reception_time, errors, block_inclusion) ->
        match (reception_time, block_inclusion) with
        | None, [] ->
            {delegate; delegate_alias; endorsing_power = 0; operations = []}
        | _, _ ->
            let mempool_inclusion =
              match reception_time with
              | None -> []
              | Some reception_time ->
                  [{source = "archiver"; reception_time; errors}]
            in
            {
              delegate;
              delegate_alias;
              endorsing_power = 0;
              operations =
                [
                  {
                    kind = Endorsement;
                    mempool_inclusion;
                    round = None;
                    block_inclusion;
                  };
                ];
            })
      (obj5
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (opt "reception_time" Time.System.encoding)
         (opt "errors" (list error_encoding))
         (dft "included_in_blocks" (list Block_hash.encoding) []))

  let encoding =
    let open Data_encoding in
    conv
      (fun {delegate; delegate_alias; endorsing_power; operations} ->
        (delegate, delegate_alias, endorsing_power, operations))
      (fun (delegate, delegate_alias, endorsing_power, operations) ->
        {delegate; delegate_alias; endorsing_power; operations})
      (obj4
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
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
  type t = {
    hash : Block_hash.t;
    delegate : Tezos_crypto.Signature.public_key_hash;
    delegate_alias : string option;
    round : Int32.t;
    timestamp : Time.Protocol.t;
    reception_times : (string * Time.System.t) list;
    nonce : unit option;
  }

  let encoding =
    let open Data_encoding in
    conv
      (fun {
             hash;
             delegate;
             delegate_alias;
             round;
             reception_times;
             timestamp;
             nonce;
           } ->
        ( hash,
          delegate,
          delegate_alias,
          round,
          reception_times,
          timestamp,
          nonce ))
      (fun ( hash,
             delegate,
             delegate_alias,
             round,
             reception_times,
             timestamp,
             nonce ) ->
        {
          hash;
          delegate;
          delegate_alias;
          round;
          reception_times;
          timestamp;
          nonce;
        })
      (obj7
         (req "hash" Block_hash.encoding)
         (dft
            "delegate"
            Tezos_crypto.Signature.Public_key_hash.encoding
            Tezos_crypto.Signature.Public_key_hash.zero)
         (opt "delegate_alias" string)
         (dft "round" int32 0l)
         (dft
            "reception_times"
            (list
               (obj2
                  (req "source" string)
                  (req "timestamp" Time.System.encoding)))
            [])
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
    delegate_alias : string option;
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
      (fun {level; round; kind; delegate; delegate_alias; problem} ->
        (level, round, kind, delegate, delegate_alias, problem))
      (fun (level, round, kind, delegate, delegate_alias, problem) ->
        {level; round; kind; delegate; delegate_alias; problem})
      (obj6
         (req "level" int32)
         (opt "round" int32)
         (dft
            "kind"
            Consensus_ops.operation_kind_encoding
            Consensus_ops.Endorsement)
         (req "delegate" Tezos_crypto.Signature.Public_key_hash.encoding)
         (opt "delegate_alias" string)
         (req "problem" problem_encoding))
end
