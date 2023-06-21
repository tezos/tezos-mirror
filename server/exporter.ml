(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

open Tezos_lwt_result_stdlib.Lwtreslib.Bare.Monad.Lwt_result_syntax

let parse_block_row ((hash, predecessor, delegate), (round, timestamp)) acc =
  Tezos_crypto.Hashed.Block_hash.Map.add
    hash
    Teztale_lib.Data.Block.
      {
        hash;
        predecessor;
        delegate;
        round;
        reception_times = [];
        timestamp;
        nonce = None;
      }
    acc

let parse_block_reception_row (hash, validation_time, application_time, source)
    acc =
  Tezos_crypto.Hashed.Block_hash.Map.update
    hash
    (function
      | Some
          Teztale_lib.Data.Block.
            {
              hash;
              predecessor;
              delegate;
              round;
              reception_times;
              timestamp;
              nonce;
            } ->
          Some
            Teztale_lib.Data.Block.
              {
                hash;
                predecessor;
                delegate;
                round;
                reception_times =
                  {source; validation_time; application_time} :: reception_times;
                timestamp;
                nonce;
              }
      | None -> None)
    acc

let select_blocks db_pool level =
  let block_request =
    Caqti_request.Infix.(
      Caqti_type.int32
      ->* Caqti_type.(
            tup2
              (tup3
                 Sql_requests.Type.block_hash
                 (option Sql_requests.Type.block_hash)
                 Sql_requests.Type.public_key_hash)
              (tup2 int32 Sql_requests.Type.time_protocol)))
      "SELECT b.hash, p.hash, d.address, b.round, b.timestamp FROM blocks b \
       JOIN delegates d ON d.id = b.baker LEFT JOIN blocks p ON p.id = \
       b.predecessor WHERE b.level = ?"
  in
  let* blocks =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold
          block_request
          parse_block_row
          level
          Tezos_crypto.Hashed.Block_hash.Map.empty)
      db_pool
  in
  let reception_request =
    Caqti_request.Infix.(
      Caqti_type.int32
      ->* Caqti_type.(
            tup4
              Sql_requests.Type.block_hash
              (option ptime)
              (option ptime)
              string))
      "SELECT b.hash, r.validation_timestamp, r.application_timestamp, n.name \
       FROM blocks b JOIN blocks_reception r ON r.block = b.id JOIN nodes n ON \
       n.id = r.source WHERE b.level = ?"
  in
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.fold reception_request parse_block_reception_row level blocks)
    db_pool

module Op_key = struct
  type t = {
    kind : Teztale_lib.Consensus_ops.operation_kind;
    round : Int32.t option;
  }

  let compare op1 op2 =
    let c = Option.compare Int32.compare op1.round op2.round in
    if c = 0 then
      match (op1.kind, op2.kind) with
      | Preendorsement, Endorsement -> -1
      | Endorsement, Preendorsement -> 1
      | _ -> 0
    else c
end

module Pkh_ops = Map.Make (Op_key)

type op_info = {
  included : Tezos_crypto.Hashed.Block_hash.t list;
  received : Teztale_lib.Data.Delegate_operations.reception list;
}

(* NB: It can happen that there is an EQC at round r, but a block at
   round r+1 is still proposed. In this case, the anomaly is rather
   that the block is proposed (either the proposer has not seen an EQC
   in time, or it is malicious), than that there are missing consensus
   ops at round r+1. In other words, the "max round" should be r, not
   r+1. *)
let _max_round (module Db : Caqti_lwt.CONNECTION) level =
  let q_blocks = "SELECT max(round) FROM blocks WHERE level = ?" in
  let r_blocks =
    Caqti_request.Infix.(Caqti_type.int ->! Caqti_type.int) q_blocks
  in
  let* m1 = Db.find r_blocks level in
  let q_ops = "SELECT max(round) FROM operations WHERE level = ?" in
  let r_ops =
    Caqti_request.Infix.(Caqti_type.int ->! Caqti_type.(option int)) q_ops
  in
  let* m2 = Db.find r_ops level in
  return (max (Some m1) m2)

let kind_of_bool = function
  | false -> Teztale_lib.Consensus_ops.Preendorsement
  | true -> Teztale_lib.Consensus_ops.Endorsement

let select_ops db_pool level =
  (* We make 3 queries:
     - one to detect "missing" ops (not included, not received)
     - one to detect included ops
     - one to detect received ops
     We then combine the results.
  *)
  let q_rights =
    Caqti_request.Infix.(
      Caqti_type.int32
      ->* Caqti_type.(tup2 Sql_requests.Type.public_key_hash int))
      "SELECT d.address, e.endorsing_power FROM endorsing_rights e JOIN \
       delegates d ON e.delegate = d.id WHERE e.level = ?"
  in
  let q_included =
    Caqti_request.Infix.(
      Caqti_type.int32
      ->* Caqti_type.(
            tup4
              Sql_requests.Type.public_key_hash
              bool
              (option int32)
              Sql_requests.Type.block_hash))
      "SELECT d.address, o.endorsement, o.round, b.hash FROM operations o JOIN \
       operations_inclusion i ON i.operation = o.id JOIN delegates d ON \
       o.endorser = d.id JOIN blocks b ON i.block = b.id WHERE o.level = ?"
  in
  let q_received =
    Caqti_request.Infix.(
      Caqti_type.int32
      ->* Caqti_type.(
            tup2
              (tup2 Sql_requests.Type.public_key_hash ptime)
              (tup4 Sql_requests.Type.errors string bool (option int32))))
      "SELECT d.address, r.timestamp, r.errors, n.name, o.endorsement, o.round \
       FROM operations o JOIN operations_reception r ON r.operation = o.id \
       JOIN delegates d ON o.endorser = d.id JOIN nodes n ON n.id = r.source \
       WHERE o.level = ?"
  in
  let module Ops = Tezos_crypto.Signature.Public_key_hash.Map in
  let cb_rights (delegate, power) info =
    Ops.add delegate (power, Pkh_ops.empty) info
  in
  let cb_included (delegate, endorsement, round, block_hash) info =
    let kind = kind_of_bool endorsement in
    Ops.update
      delegate
      (function
        | Some (power, ops) ->
            let op_key = Op_key.{kind; round} in
            let op =
              match Pkh_ops.find_opt op_key ops with
              | Some op_info ->
                  {op_info with included = block_hash :: op_info.included}
              | None -> {included = [block_hash]; received = []}
            in
            let ops' = Pkh_ops.add op_key op ops in
            Some (power, ops')
        | None -> None)
      info
  in
  let cb_received
      ((delegate, reception_time), (errors, source, endorsement, round)) info =
    let kind = kind_of_bool endorsement in
    let received_info =
      Teztale_lib.Data.Delegate_operations.{source; reception_time; errors}
    in
    Ops.update
      delegate
      (function
        | Some (power, ops) ->
            let op_key = Op_key.{kind; round} in
            let op =
              match Pkh_ops.find_opt op_key ops with
              | Some op_info ->
                  {op_info with received = received_info :: op_info.received}
              | None -> {included = []; received = [received_info]}
            in
            let ops' = Pkh_ops.add op_key op ops in
            Some (power, ops')
        | None -> None)
      info
  in
  let* out =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold q_rights cb_rights level Ops.empty)
      db_pool
  in
  let* out =
    Caqti_lwt.Pool.use
      (fun (module Db : Caqti_lwt.CONNECTION) ->
        Db.fold q_included cb_included level out)
      db_pool
  in
  Caqti_lwt.Pool.use
    (fun (module Db : Caqti_lwt.CONNECTION) ->
      Db.fold q_received cb_received level out)
    db_pool

let translate_ops info =
  let translate pkh_ops =
    Pkh_ops.fold
      (fun Op_key.{kind; round} op_info acc ->
        Teztale_lib.Data.Delegate_operations.
          {
            kind;
            round;
            mempool_inclusion = op_info.received;
            block_inclusion = op_info.included;
          }
        :: acc)
      pkh_ops
      []
  in
  Tezos_crypto.Signature.Public_key_hash.Map.fold
    (fun pkh (power, pkh_ops) acc ->
      Teztale_lib.Data.Delegate_operations.
        {
          delegate = pkh;
          endorsing_power = power;
          operations = translate pkh_ops;
        }
      :: acc)
    info
    []

(* NB: We're not yet extracting [Incorrect] operations. we easily
     could, they are quite noisy. At least in some cases, the "consensus
     operations for old/future round/level" errors should be seen as a
     "per block anomaly" rather than a "per delegate anomaly". *)
let anomalies level ops =
  let extract_anomalies delegate pkh_ops =
    let open Teztale_lib.Data.Anomaly in
    Pkh_ops.fold
      (fun Op_key.{kind; round} {received; included} acc ->
        let problem =
          match (received, included) with
          | [], [] -> Some Missed
          | [], _ -> Some Sequestered
          | _, [] -> Some Forgotten
          | _ -> None
        in
        match problem with
        | None -> acc
        | Some problem -> {level; kind; round; delegate; problem} :: acc)
      pkh_ops
      []
  in
  Tezos_crypto.Signature.Public_key_hash.Map.fold
    (fun pkh (_power, pkh_ops) acc -> extract_anomalies pkh pkh_ops @ acc)
    ops
    []

let data_at_level db_pool level =
  let blocks_e = select_blocks db_pool level in
  let* delegate_operations = select_ops db_pool level in
  let* blocks = blocks_e in
  let delegate_operations = translate_ops delegate_operations in
  let blocks =
    Tezos_crypto.Hashed.Block_hash.Map.fold (fun _ x acc -> x :: acc) blocks []
  in
  let unaccurate = false in
  return Teztale_lib.Data.{blocks; delegate_operations; unaccurate}

let anomalies_at_level db_pool level =
  let* ops = select_ops db_pool level in
  return (anomalies level ops)
