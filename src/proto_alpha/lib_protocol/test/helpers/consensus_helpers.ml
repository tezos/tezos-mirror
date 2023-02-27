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

type kind = Preendorsement | Endorsement

(** Craft an endorsement or preendorsement, and bake a block
    containing it (in application or construction modes) or inject it
    into a mempool. When [error] is [None], check that it succeeds,
    otherwise check that it fails as specified by [error].

    By default, the (pre)endorsement is for the first slot and is
    signed by the delegate that owns this slot. Moreover, the operation
    points to the given [endorsed_block]: in other words, it has that
    block's level, round, payload hash, and its branch is the
    predecessor of that block. Optional arguments allow to override
    these default parameters.

    The [predecessor] is used as the predecessor of the baked block or
    the head of the mempool. When it is not provided, we use the
    [endorsed_block] for this. *)
let test_consensus_operation ?delegate ?slot ?level ?round ?block_payload_hash
    ?branch ~endorsed_block ?(predecessor = endorsed_block) ?error ~loc kind
    mode =
  let open Lwt_result_syntax in
  let* operation =
    match kind with
    | Preendorsement ->
        Op.preendorsement
          ?delegate
          ?slot
          ?level
          ?round
          ?block_payload_hash
          ?branch
          endorsed_block
    | Endorsement ->
        Op.endorsement
          ?delegate
          ?slot
          ?level
          ?round
          ?block_payload_hash
          ?branch
          endorsed_block
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
      Block.bake ~baking_mode:Application ~operation predecessor >>= check_error
  | Construction ->
      Block.bake ~baking_mode:Baking ~operation predecessor >>= check_error
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
    ?round ?block_payload_hash ?branch ~endorsed_block ?predecessor ~loc
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
        ~endorsed_block
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
    ?block_payload_hash ?branch ~endorsed_block ?predecessor ?error ~loc kind =
  test_consensus_operation_all_modes_different_outcomes
    ?delegate
    ?slot
    ?level
    ?round
    ?block_payload_hash
    ?branch
    ~endorsed_block
    ?predecessor
    ~loc
    ?application_error:error
    ?construction_error:error
    ?mempool_error:error
    kind

let delegate_of_first_slot b =
  let module V = Plugin.RPC.Validators in
  Context.get_endorsers b >|=? function
  | {V.consensus_key; slots = s :: _; _} :: _ -> (consensus_key, s)
  | _ -> assert false

let delegate_of_slot ?(different_slot = false) slot b =
  let module V = Plugin.RPC.Validators in
  Context.get_endorsers b >|=? fun endorsers ->
  List.find_map
    (function
      | {V.consensus_key; slots = s :: _; _}
        when if different_slot then not (Slot.equal s slot)
             else Slot.equal s slot ->
          Some consensus_key
      | _ -> None)
    endorsers
  |> function
  | None -> assert false
  | Some d -> d

let test_consensus_op_for_next ~genesis ~kind ~next =
  let dorsement ~endorsed_block ~delegate =
    match kind with
    | `Preendorsement -> Op.preendorsement ~delegate endorsed_block
    | `Endorsement -> Op.endorsement ~delegate endorsed_block
  in
  Block.bake genesis >>=? fun b1 ->
  (match next with
  | `Level -> Block.bake b1
  | `Round -> Block.bake ~policy:(By_round 1) genesis)
  >>=? fun b2 ->
  Incremental.begin_construction ~mempool_mode:true b1 >>=? fun inc ->
  delegate_of_first_slot (B b1) >>=? fun (delegate, slot) ->
  dorsement ~endorsed_block:b1 ~delegate >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  delegate_of_slot ~different_slot:true slot (B b2) >>=? fun delegate ->
  dorsement ~endorsed_block:b2 ~delegate >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun (_ : Incremental.t) ->
  return_unit
