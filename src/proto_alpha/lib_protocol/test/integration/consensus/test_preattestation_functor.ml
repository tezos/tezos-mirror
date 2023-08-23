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
    Component:  Protocol (preattestation) in Full_construction & Application modes
    Subject:    preattestation inclusion in a block
*)

open Protocol
open Alpha_context

(****************************************************************)
(*                    Utility functions                         *)
(****************************************************************)
module type MODE = sig
  val name : string

  val baking_mode : Block.baking_mode
end

module BakeWithMode (Mode : MODE) : sig
  val tests : unit Alcotest_lwt.test_case trace
end = struct
  let name = Mode.name

  let bake = Block.bake ~baking_mode:Mode.baking_mode

  let aux_simple_preattestation_inclusion ?(payload_round = Some Round.zero)
      ?(locked_round = Some Round.zero) ?(block_round = 1)
      ?(preattestation_round = Round.zero)
      ?(preattested_block = fun _predpred _pred curr -> curr)
      ?(mk_ops = fun op -> [op])
      ?(get_delegate_and_slot =
        fun _predpred _pred _curr -> return (None, None))
      ?(post_process = Ok (fun _ -> return_unit)) ~loc () =
    let open Lwt_result_syntax in
    let* genesis, _contracts = Context.init_n ~consensus_threshold:1 5 () in
    let* b1 = bake genesis in
    let* attestation = Op.attestation b1 in
    let* b2 = bake b1 ~operations:[attestation] in
    let attested_block = preattested_block genesis b1 b2 in
    let* delegate, slot = get_delegate_and_slot genesis b1 b2 in
    let* p =
      Op.preattestation
        ?delegate
        ?slot
        ~round:preattestation_round
        attested_block
    in
    let operations = attestation :: (mk_ops @@ p) in
    let*! res =
      bake
        ~payload_round
        ~locked_round
        ~policy:(By_round block_round)
        ~operations
        b1
    in
    match (res, post_process) with
    | Ok ok, Ok success_fun -> success_fun ok
    | Error _, Error error -> Assert.proto_error ~loc res error
    | Ok _, Error _ -> Assert.error ~loc res (fun _ -> false)
    | Error _, Ok _ -> Assert.error ~loc res (fun _ -> false)

  (****************************************************************)
  (*                      Tests                                   *)
  (****************************************************************)

  (** OK: bake a block "_b2_1" at round 1, containing a PQC and a locked
    round of round 0 *)
  let include_preattestation_in_block_with_locked_round () =
    aux_simple_preattestation_inclusion ~loc:__LOC__ ()

  (** KO: The same preattestation injected twice in the PQC *)
  let duplicate_preattestation_in_pqc () =
    aux_simple_preattestation_inclusion (* inject the op twice *)
      ~mk_ops:(fun op -> [op; op])
      ~loc:__LOC__
      ~post_process:
        (Error
           (function
           | Validate_errors.Consensus.Conflicting_consensus_operation {kind; _}
             when kind = Validate_errors.Consensus.Preattestation ->
               true
           | _ -> false))
      ()

  (** KO: locked round declared in the block is not smaller than
    that block's round *)
  let locked_round_not_before_block_round () =
    aux_simple_preattestation_inclusion
    (* default locked_round = 0 < block_round = 1 for this aux function *)
      ~block_round:0
      ~loc:__LOC__
      ~post_process:
        (Error
           (function
           | Fitness_repr.Locked_round_not_less_than_round _ -> true
           | _ -> false))
      ()

  (** KO: because we announce a locked_round, but we don't provide the
    preattestation quorum certificate in the operations *)
  let with_locked_round_in_block_but_without_any_pqc () =
    (* This test only fails in Application mode. If full_construction mode, the
       given locked_round is not used / checked. Moreover, the test succeed in
       this case.
    *)
    let post_process =
      if Mode.baking_mode == Block.Application then
        Error (function Fitness_repr.Wrong_fitness -> true | _ -> false)
      else Ok (fun _ -> return_unit)
    in
    aux_simple_preattestation_inclusion
    (* with declared locked_round but without a PQC in the ops *)
      ~mk_ops:(fun _p -> [])
      ~loc:__LOC__
      ~post_process
      ()

  (** KO: The preattested block is the pred one, not the current one *)
  let preattestation_has_wrong_level () =
    aux_simple_preattestation_inclusion
    (* preattestation should be for _curr block to be valid *)
      ~preattested_block:(fun _predpred pred _curr -> pred)
      ~loc:__LOC__
      ~post_process:
        (Error
           (function
           | Validate_errors.Consensus.Consensus_operation_for_old_level
               {kind; _}
             when kind = Validate_errors.Consensus.Preattestation ->
               true
           | _ -> false))
      ()

  (** OK: explicit the correct attester and preattesting slot in the test *)
  let preattestation_in_block_with_good_slot () =
    let open Lwt_result_syntax in
    aux_simple_preattestation_inclusion
      ~get_delegate_and_slot:(fun _predpred _pred curr ->
        let module V = Plugin.RPC.Validators in
        let* validators = Context.get_attesters (B curr) in
        match validators with
        | {V.delegate; slots = s :: _; _} :: _ -> return (Some delegate, Some s)
        | _ -> assert false
        (* there is at least one attester with a slot *))
      ~loc:__LOC__
      ()

  (** KO: the used slot for injecting the attestation is not the canonical one *)
  let preattestation_in_block_with_wrong_slot () =
    let open Lwt_result_syntax in
    aux_simple_preattestation_inclusion
      ~get_delegate_and_slot:(fun _predpred _pred curr ->
        let module V = Plugin.RPC.Validators in
        let* validators = Context.get_attesters (B curr) in
        match validators with
        | {V.delegate; V.slots = _ :: non_canonical_slot :: _; _} :: _ ->
            return (Some delegate, Some non_canonical_slot)
        | _ -> assert false
        (* there is at least one attester with a slot *))
      ~loc:__LOC__
      ~post_process:
        (Error
           (function
           | Validate_errors.Consensus.Wrong_slot_used_for_consensus_operation
               {kind; _}
             when kind = Validate_errors.Consensus.Preattestation ->
               true
           | _ -> false))
      ()

  (** KO: the delegate tries to injects with a canonical slot of another delegate *)
  let preattestation_in_block_with_wrong_signature () =
    let open Lwt_result_syntax in
    aux_simple_preattestation_inclusion
      ~get_delegate_and_slot:(fun _predpred _pred curr ->
        let module V = Plugin.RPC.Validators in
        let* validators = Context.get_attesters (B curr) in
        match validators with
        | {V.delegate; _} :: {V.slots = s :: _; _} :: _ ->
            (* the canonical slot s is not owned by the delegate "delegate" !*)
            return (Some delegate, Some s)
        | _ -> assert false
        (* there is at least one attester with a slot *))
      ~loc:__LOC__
      ~post_process:
        (Error
           (function Operation_repr.Invalid_signature -> true | _ -> false))
      ()

  (** KO: cannot have a locked_round higher than attached PQC's round *)
  let locked_round_is_higher_than_pqc_round () =
    (* This test only fails in Application mode. If full_construction mode, the
       given locked_round is not used / checked. Moreover, the test succeed in
       this case.
    *)
    let post_process =
      if Mode.baking_mode == Application then
        Error
          (function
          | Validate_errors.Consensus.Consensus_operation_for_old_round
              {kind; _}
            when kind = Validate_errors.Consensus.Preattestation ->
              true
          | _ -> false)
      else Ok (fun _ -> return_unit)
    in
    aux_simple_preattestation_inclusion
      ~preattestation_round:Round.zero
      ~locked_round:(Some (Round.succ Round.zero))
      ~block_round:2
      ~loc:__LOC__
      ~post_process
      ()

  let my_tztest title test =
    Tztest.tztest (Format.sprintf "%s: %s" name title) test

  let tests =
    [
      my_tztest
        "ok: include_preattestation_in_block_with_locked_round"
        `Quick
        include_preattestation_in_block_with_locked_round;
      my_tztest
        "ko: duplicate_preattestation_in_pqc"
        `Quick
        duplicate_preattestation_in_pqc;
      my_tztest
        "ko:locked_round_not_before_block_round"
        `Quick
        locked_round_not_before_block_round;
      my_tztest
        "ko: with_locked_round_in_block_but_without_any_pqc"
        `Quick
        with_locked_round_in_block_but_without_any_pqc;
      my_tztest
        "ko: preattestation_has_wrong_level"
        `Quick
        preattestation_has_wrong_level;
      my_tztest
        "ok: preattestation_in_block_with_good_slot"
        `Quick
        preattestation_in_block_with_good_slot;
      my_tztest
        "ko: preattestation_in_block_with_wrong_slot"
        `Quick
        preattestation_in_block_with_wrong_slot;
      my_tztest
        "ko: preattestation_in_block_with_wrong_signature"
        `Quick
        preattestation_in_block_with_wrong_signature;
      my_tztest
        "ko: locked_round_is_higher_than_pqc_round"
        `Quick
        locked_round_is_higher_than_pqc_round;
    ]
end
