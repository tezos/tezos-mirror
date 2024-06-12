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

open Protocol
open Alpha_context

type mode = Application | Construction | Mempool

let show_mode = function
  | Application -> "Application"
  | Construction -> "Construction"
  | Mempool -> "Mempool"

type kind = Preattestation | Attestation

(** Craft an attestation or preattestation, and bake a block
    containing it (in application or construction modes) or inject it
    into a mempool. When [error] is [None], check that it succeeds,
    otherwise check that it fails as specified by [error].

    By default, the (pre)attestation is for the first slot and is
    signed by the delegate that owns this slot. Moreover, the operation
    points to the given [attested_block]: in other words, it has that
    block's level, round, payload hash, and its branch is the
    predecessor of that block. Optional arguments allow to override
    these default parameters.

    The [predecessor] is used as the predecessor of the baked block or
    the head of the mempool. When it is not provided, we use the
    [attested_block] for this. *)
let test_consensus_operation ?delegate ?slot ?level ?round ?block_payload_hash
    ?branch ~attested_block ?(predecessor = attested_block) ?error ~loc kind
    mode =
  let open Lwt_result_syntax in
  let* operation =
    match kind with
    | Preattestation ->
        Op.preattestation
          ?delegate
          ?slot
          ?level
          ?round
          ?block_payload_hash
          ?branch
          attested_block
    | Attestation ->
        Op.attestation
          ?delegate
          ?slot
          ?level
          ?round
          ?block_payload_hash
          ?branch
          attested_block
  in
  let check_error res =
    match error with
    | Some error -> Assert.proto_error ~loc res error
    | None ->
        let*? _ = res in
        return_unit
  in
  match mode with
  | Application ->
      let*! result =
        Block.bake ~baking_mode:Application ~operation predecessor
      in
      check_error result
  | Construction ->
      let*! result = Block.bake ~baking_mode:Baking ~operation predecessor in
      check_error result
  | Mempool ->
      let*! res =
        let* inc =
          Incremental.begin_construction ~mempool_mode:true predecessor
        in
        let* inc = Incremental.add_operation inc operation in
        (* Finalization doesn't do much in mempool mode, but some RPCs
           still call it, so we check that it doesn't fail unexpectedly. *)
        Incremental.finalize_block inc
      in
      check_error res

let test_consensus_operation_all_modes_different_outcomes ?delegate ?slot ?level
    ?round ?block_payload_hash ?branch ~attested_block ?predecessor ~loc
    ?application_error ?construction_error ?mempool_error kind =
  List.iter_es
    (fun (mode, error) ->
      test_consensus_operation
        ?delegate
        ?slot
        ?level
        ?round
        ?block_payload_hash
        ?branch
        ~attested_block
        ?predecessor
        ?error
        ~loc:(Format.sprintf "%s (%s mode)" loc (show_mode mode))
        kind
        mode)
    [
      (Application, application_error);
      (Construction, construction_error);
      (Mempool, mempool_error);
    ]

let test_consensus_operation_all_modes ?delegate ?slot ?level ?round
    ?block_payload_hash ?branch ~attested_block ?predecessor ?error ~loc kind =
  test_consensus_operation_all_modes_different_outcomes
    ?delegate
    ?slot
    ?level
    ?round
    ?block_payload_hash
    ?branch
    ~attested_block
    ?predecessor
    ~loc
    ?application_error:error
    ?construction_error:error
    ?mempool_error:error
    kind

let delegate_of_first_slot b =
  let open Lwt_result_syntax in
  let module V = Plugin.RPC.Validators in
  let+ attesters = Context.get_attesters b in
  match attesters with
  | {V.consensus_key; slots = s :: _; _} :: _ -> (consensus_key, s)
  | _ -> assert false

let delegate_of_slot ?(different_slot = false) slot b =
  let open Lwt_result_syntax in
  let module V = Plugin.RPC.Validators in
  let+ attesters = Context.get_attesters b in
  List.find_map
    (function
      | {V.consensus_key; slots = s :: _; _}
        when if different_slot then not (Slot.equal s slot)
             else Slot.equal s slot ->
          Some consensus_key
      | _ -> None)
    attesters
  |> function
  | None -> assert false
  | Some d -> d

let test_consensus_op_for_next ~genesis ~kind ~next =
  let open Lwt_result_syntax in
  let dorsement ~attested_block ~delegate =
    match kind with
    | `Preattestation -> Op.preattestation ~delegate attested_block
    | `Attestation -> Op.attestation ~delegate attested_block
  in
  let* b1 = Block.bake genesis in
  let* b2 =
    match next with
    | `Level -> Block.bake b1
    | `Round -> Block.bake ~policy:(By_round 1) genesis
  in
  let* inc = Incremental.begin_construction ~mempool_mode:true b1 in
  let* delegate, slot = delegate_of_first_slot (B b1) in
  let* operation = dorsement ~attested_block:b1 ~delegate in
  let* inc = Incremental.add_operation inc operation in
  let* delegate = delegate_of_slot ~different_slot:true slot (B b2) in
  let* operation = dorsement ~attested_block:b2 ~delegate in
  let* (_ : Incremental.t) = Incremental.add_operation inc operation in
  return_unit
