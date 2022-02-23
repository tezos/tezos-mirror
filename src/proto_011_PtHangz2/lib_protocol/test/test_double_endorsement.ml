(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

(** Testing
    -------
    Component:    Protocol (double endorsement)
    Invocation:   dune exec src/proto_011_PtHangz2/lib_protocol/test/main.exe -- test "^double endorsement$"
    Subject:      Double endorsement evidence operation may happen when an
                  endorser endorsed two different blocks on the same level.
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                  Utility functions                           *)
(****************************************************************)

let get_hd_hd = function x :: y :: _ -> (x, y) | _ -> assert false

let get_first_different_baker baker bakers =
  WithExceptions.Option.get ~loc:__LOC__
  @@ List.find
       (fun baker' -> Signature.Public_key_hash.( <> ) baker baker')
       bakers

let get_first_different_bakers ctxt =
  Context.get_bakers ctxt >|=? function
  | [] -> assert false
  | baker_1 :: other_bakers ->
      (baker_1, get_first_different_baker baker_1 other_bakers)

let get_first_different_endorsers ctxt =
  Context.get_endorsers ctxt >|=? fun endorsers -> get_hd_hd endorsers

let block_fork b =
  get_first_different_bakers (B b) >>=? fun (baker_1, baker_2) ->
  Block.bake ~policy:(By_account baker_1) b >>=? fun blk_a ->
  Block.bake ~policy:(By_account baker_2) b >|=? fun blk_b -> (blk_a, blk_b)

(****************************************************************)
(*                        Tests                                 *)
(****************************************************************)

(** Simple scenario where two endorsements are made from the same
    delegate and exposed by a double_endorsement operation. Also verify
    that punishment is operated. *)
let test_valid_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (delegate, slots) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->
  Op.endorsement_with_slot ~delegate:(delegate, slots) (B blk_a) ()
  >>=? fun endorsement_with_slot_a ->
  Block.bake ~operations:[Operation.pack endorsement_with_slot_a] blk_a
  >>=? fun blk_a ->
  (* Block.bake ~operations:[endorsement_b] blk_b >>=? fun _ -> *)
  Op.double_endorsement
    (B blk_a)
    endorsement_a
    endorsement_b
    ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd slots))
  |> fun operation ->
  (* Bake with someone different than the bad endorser *)
  Context.get_bakers (B blk_a) >>=? fun bakers ->
  get_first_different_baker delegate bakers |> fun baker ->
  Block.bake ~policy:(By_account baker) ~operation blk_a >>=? fun blk ->
  (* Check that the frozen deposit, the fees and rewards are removed *)
  List.iter_es
    (fun kind ->
      let contract = Alpha_context.Contract.implicit_contract delegate in
      Assert.balance_is ~loc:__LOC__ (B blk) contract ~kind Tez.zero)
    [Deposit; Fees; Rewards]

(****************************************************************)
(*  The following test scenarios are supposed to raise errors.  *)
(****************************************************************)

(** Check that an invalid double endorsement operation that exposes a
    valid endorsement fails. *)
let test_invalid_double_endorsement () =
  Context.init 10 >>=? fun (b, _) ->
  Block.bake b >>=? fun b ->
  Op.endorsement (B b) () >>=? fun endorsement ->
  Op.endorsement_with_slot (B b) () >>=? fun endorsement_with_slot ->
  Block.bake ~operation:(Operation.pack endorsement_with_slot) b >>=? fun b ->
  Op.double_endorsement (B b) endorsement endorsement ~slot:0
  |> fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Invalid_double_endorsement_evidence -> true
      | _ -> false)

(** Check that a double endorsement added at the same time as a double
    endorsement operation fails. *)
let test_too_early_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (delegate, slots) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->
  Op.double_endorsement
    (B b)
    endorsement_a
    endorsement_b
    ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd slots))
  |> fun operation ->
  Block.bake ~operation b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Too_early_double_endorsement_evidence _ -> true
      | _ -> false)

(** Check that after [preserved_cycles + 1], it is not possible
    to create a double_endorsement anymore. *)
let test_too_late_double_endorsement_evidence () =
  Context.init 2 >>=? fun (b, _) ->
  Context.get_constants (B b)
  >>=? fun Constants.{parametric = {preserved_cycles; _}; _} ->
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (delegate, slots) ->
  Op.endorsement ~delegate (B blk_a) () >>=? fun endorsement_a ->
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->
  List.fold_left_es
    (fun blk _ -> Block.bake_until_cycle_end blk)
    blk_a
    (1 -- (preserved_cycles + 1))
  >>=? fun blk ->
  Op.double_endorsement
    (B blk)
    endorsement_a
    endorsement_b
    ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd slots))
  |> fun operation ->
  Block.bake ~operation blk >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Apply.Outdated_double_endorsement_evidence _ -> true
      | _ -> false)

(** Check that an invalid double endorsement evidence that exposes two
    endorsements made by two different endorsers fails. *)
let test_different_delegates () =
  Context.init 2 >>=? fun (b, _) ->
  Block.bake b >>=? fun b ->
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (endorser_a, _a_slots) ->
  get_first_different_endorsers (B blk_b)
  >>=? fun (endorser_b1c, endorser_b2c) ->
  let (endorser_b, b_slots) =
    if Signature.Public_key_hash.( = ) endorser_a endorser_b1c.delegate then
      (endorser_b2c.delegate, endorser_b2c.slots)
    else (endorser_b1c.delegate, endorser_b1c.slots)
  in
  Op.endorsement ~delegate:endorser_a (B blk_a) () >>=? fun e_a ->
  Op.endorsement ~delegate:endorser_b (B blk_b) () >>=? fun e_b ->
  Op.endorsement_with_slot ~delegate:(endorser_b, b_slots) (B blk_b) ()
  >>=? fun e_ws_b ->
  Block.bake ~operation:(Operation.pack e_ws_b) blk_b >>=? fun _ ->
  Op.double_endorsement
    (B blk_b)
    e_a
    e_b
    ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd b_slots))
  |> fun operation ->
  Block.bake ~operation blk_b >>= fun res ->
  Assert.proto_error ~loc:__LOC__ res (function
      | Baking.Unexpected_endorsement -> true
      | _ -> false)

(** Check that a double endorsement evidence that exposes a ill-formed
    endorsement fails. *)
let test_wrong_delegate () =
  Context.init ~endorsers_per_block:1 2 >>=? fun (b, contracts) ->
  List.map_es (Context.Contract.manager (B b)) contracts >>=? fun accounts ->
  let (account_1, account_2) = get_hd_hd accounts in
  let pkh1 = account_1.Account.pkh in
  let pkh2 = account_2.Account.pkh in
  block_fork b >>=? fun (blk_a, blk_b) ->
  Context.get_endorser (B blk_a) >>=? fun (endorser_a, a_slots) ->
  Op.endorsement ~delegate:endorser_a (B blk_a) () >>=? fun endorsement_a ->
  Context.get_endorser (B blk_b) >>=? fun (endorser_b, _b_slots) ->
  let delegate =
    if Signature.Public_key_hash.equal pkh1 endorser_b then pkh2 else pkh1
  in
  Op.endorsement ~delegate (B blk_b) () >>=? fun endorsement_b ->
  Op.double_endorsement
    (B blk_b)
    endorsement_a
    endorsement_b
    ~slot:(WithExceptions.Option.get ~loc:__LOC__ (List.hd a_slots))
  |> fun operation ->
  Block.bake ~operation blk_b >>= fun e ->
  Assert.proto_error ~loc:__LOC__ e (function
      | Baking.Unexpected_endorsement -> true
      | _ -> false)

let tests =
  [
    Tztest.tztest
      "valid double endorsement evidence"
      `Quick
      test_valid_double_endorsement_evidence;
    Tztest.tztest
      "invalid double endorsement evidence"
      `Quick
      test_invalid_double_endorsement;
    Tztest.tztest
      "too early double endorsement evidence"
      `Quick
      test_too_early_double_endorsement_evidence;
    Tztest.tztest
      "too late double endorsement evidence"
      `Quick
      test_too_late_double_endorsement_evidence;
    Tztest.tztest "different delegates" `Quick test_different_delegates;
    Tztest.tztest "wrong delegate" `Quick test_wrong_delegate;
  ]
