(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type kind = Preattestation | Attestation | Aggregate

(** Crafts a consensus operation.

    By default, a (pre)attestation is for the first slot, whereas an
    attestations aggregate is for all the level's attesters that are
    using a BLS consensus key.

    Moreover, the operation points to the given [attested_block]: in
    other words, it has that block's level, round, payload hash, and
    its branch is the predecessor of that block.

    Optional arguments allow to override these default parameters. *)
let craft_consensus_operation ?attesting_slot ?level ?round ?block_payload_hash
    ?branch ~attested_block kind =
  match kind with
  | Preattestation ->
      Op.preattestation
        ?attesting_slot
        ?level
        ?round
        ?block_payload_hash
        ?branch
        attested_block
  | Attestation ->
      Op.attestation
        ?attesting_slot
        ?level
        ?round
        ?block_payload_hash
        ?branch
        attested_block
  | Aggregate ->
      Op.attestations_aggregate
        ?level
        ?round
        ?block_payload_hash
        ?branch
        attested_block

let test_consensus_operation ?attesting_slot ?level ?round ?block_payload_hash
    ?branch ~attested_block ?(predecessor = attested_block) ?error ~loc kind
    mode =
  let open Lwt_result_syntax in
  let* operation =
    craft_consensus_operation
      ?attesting_slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      ~attested_block
      kind
  in
  Op.check_validation_and_application ~loc ?error ~predecessor mode operation

let test_consensus_operation_all_modes_different_outcomes ?attesting_slot ?level
    ?round ?block_payload_hash ?branch ~attested_block
    ?(predecessor = attested_block) ~loc ?application_error ?construction_error
    ?mempool_error kind =
  let open Lwt_result_syntax in
  let* operation =
    craft_consensus_operation
      ?attesting_slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      ~attested_block
      kind
  in
  Op.check_validation_and_application_all_modes_different_outcomes
    ~loc
    ?application_error
    ?construction_error
    ?mempool_error
    ~predecessor
    operation

let test_consensus_operation_all_modes ?attesting_slot ?level ?round
    ?block_payload_hash ?branch ~attested_block ?(predecessor = attested_block)
    ?error ~loc kind =
  let open Lwt_result_syntax in
  let* operation =
    craft_consensus_operation
      ?attesting_slot
      ?level
      ?round
      ?block_payload_hash
      ?branch
      ~attested_block
      kind
  in
  Op.check_validation_and_application_all_modes
    ~loc
    ?error
    ~predecessor
    operation

let test_consensus_op_for_next ~genesis ~kind ~next =
  let open Lwt_result_syntax in
  let consensus_op ~attested_block ~attesting_slot =
    match kind with
    | `Preattestation -> Op.preattestation ~attesting_slot attested_block
    | `Attestation -> Op.attestation ~attesting_slot attested_block
  in
  let* b1 = Block.bake genesis in
  let* b2 =
    match next with
    | `Level -> Block.bake b1
    | `Round -> Block.bake ~policy:(By_round 1) genesis
  in
  let* inc = Incremental.begin_construction ~mempool_mode:true b1 in
  let* attesting_slot = Op.get_attesting_slot ~attested_block:b1 in
  let* operation = consensus_op ~attested_block:b1 ~attesting_slot in
  let* inc = Incremental.add_operation inc operation in
  let* attesting_slot =
    Op.get_different_attesting_slot
      ~consensus_pkh_to_avoid:attesting_slot.consensus_pkh
      ~attested_block:b2
  in
  let* operation = consensus_op ~attested_block:b2 ~attesting_slot in
  let* (_ : Incremental.t) = Incremental.add_operation inc operation in
  return_unit
