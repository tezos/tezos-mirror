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

type kind = Preendorsement | Endorsement

(** Craft an endorsement or preendorsement, and bake a block
    containing it (in application or construction modes) or inject it
    into a mempool. Check that it fails as specified by [error].

    By default, the (pre)endorsement is for the first slot and is
    signed by the delegate that owns this slot. Moreover, the operation
    points to the given [endorsed_block]: in other words, it has that
    block's level, round, payload hash, and its branch is the
    predecessor of that block. Optional arguments allow to override
    these default parameters.

    The [endorsed_block] is also used as the predecessor of the baked
    block, or as the head of the mempool. *)
let test_consensus_operation ?slot ?level ?round ?block_payload_hash
    ~endorsed_block ~error ~loc kind mode =
  let open Lwt_result_syntax in
  let* operation =
    match kind with
    | Preendorsement ->
        Op.preendorsement ?slot ?level ?round ?block_payload_hash endorsed_block
    | Endorsement ->
        Op.endorsement ?slot ?level ?round ?block_payload_hash endorsed_block
  in
  let assert_error res = Assert.proto_error ~loc res error in
  match mode with
  | Application ->
      Block.bake ~baking_mode:Application ~operation endorsed_block
      >>= assert_error
  | Construction ->
      Block.bake ~baking_mode:Baking ~operation endorsed_block >>= assert_error
  | Mempool ->
      let*! res =
        let* inc =
          Incremental.begin_construction ~mempool_mode:true endorsed_block
        in
        Incremental.validate_operation inc operation
      in
      assert_error res

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
  Incremental.add_operation inc operation >>= fun res ->
  let error_title =
    match next with
    | `Level -> "Consensus operation for future level"
    | `Round -> "Consensus operation for future round"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title
