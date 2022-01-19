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

let test_consensus_operation ?construction_mode ?level ?block_payload_hash ?slot
    ?round ~endorsed_block ~error_title ~is_preendorsement ~context ~loc () =
  (if is_preendorsement then
   Op.preendorsement
     ~endorsed_block
     ?block_payload_hash
     ?level
     ?slot
     ?round
     context
     ()
   >|=? fun op -> Operation.pack op
  else
    Op.endorsement
      ~endorsed_block
      ?block_payload_hash
      ?level
      ?slot
      ?round
      context
      ()
    >|=? fun op -> Operation.pack op)
  >>=? fun op ->
  match construction_mode with
  | None ->
      (* meaning Application mode *)
      Block.bake ~operations:[op] endorsed_block >>= fun res ->
      Assert.proto_error_with_info ~loc res error_title
  | Some (pred, protocol_data) ->
      (* meaning partial construction or full construction mode, depending on
         [protocol_data] *)
      Block.get_construction_vstate ~protocol_data pred >>=? fun vstate ->
      apply_operation vstate op >|= Environment.wrap_tzresult >>= fun res ->
      Assert.proto_error_with_info ~loc res error_title

let delegate_of_first_slot b =
  let module V = Plugin.RPC.Validators in
  Context.get_endorsers b >|=? function
  | {V.delegate; slots = s :: _ as slots; _} :: _ -> ((delegate, slots), s)
  | _ -> assert false

let delegate_of_slot slot b =
  let module V = Plugin.RPC.Validators in
  Context.get_endorsers b >|=? fun endorsers ->
  List.find_map
    (function
      | {V.delegate; slots = s :: _ as slots; _} when Slot.equal s slot ->
          Some (delegate, slots)
      | _ -> None)
    endorsers
  |> function
  | None -> assert false
  | Some d -> d

let test_consensus_op_for_next ~genesis ~kind ~next =
  let dorsement ~endorsed_block ~delegate b =
    match kind with
    | `Preendorsement ->
        Op.preendorsement ~endorsed_block ~delegate b () >|=? Operation.pack
    | `Endorsement ->
        Op.endorsement ~endorsed_block ~delegate b () >|=? Operation.pack
  in
  Block.bake genesis >>=? fun b1 ->
  (match next with
  | `Level -> Block.bake b1
  | `Round -> Block.bake ~policy:(By_round 1) genesis)
  >>=? fun b2 ->
  Incremental.begin_construction ~mempool_mode:true b1 >>=? fun inc ->
  delegate_of_first_slot (B b1) >>=? fun (delegate, slot) ->
  dorsement ~endorsed_block:b1 ~delegate (B genesis) >>=? fun operation ->
  Incremental.add_operation inc operation >>=? fun inc ->
  delegate_of_slot slot (B b2) >>=? fun delegate ->
  dorsement ~endorsed_block:b2 ~delegate (B b1) >>=? fun operation ->
  Incremental.add_operation inc operation >>= fun res ->
  let error_title =
    match next with
    | `Level -> "Consensus operation for future level"
    | `Round -> "Consensus operation for future round"
  in
  Assert.proto_error_with_info ~loc:__LOC__ res error_title
