(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Misbehaviour_repr = struct
  open Protocol.Misbehaviour_repr

  let pp fmt {level; round; kind} =
    Format.fprintf
      fmt
      "misbehaviour: %s at level %a round %a"
      (match kind with
      | Double_baking -> "double baking"
      | Double_attesting -> "double attesting"
      | Double_preattesting -> "double preattesting")
      Protocol.Raw_level_repr.pp
      level
      Protocol.Round_repr.pp
      round

  include Compare.Make (struct
    type t = Protocol.Misbehaviour_repr.t

    let compare = Protocol.Misbehaviour_repr.compare
  end)

  let from_duplicate_operation (type a)
      (duplicate_op :
        a Protocol.Alpha_context.Kind.consensus Protocol.Alpha_context.operation)
      =
    let ( ({slot = _; level; round; block_payload_hash = _} :
            Protocol.Alpha_context.consensus_content),
          kind ) =
      match duplicate_op.protocol_data.contents with
      | Single (Preattestation consensus_content) ->
          (consensus_content, Double_preattesting)
      | Single (Attestation {consensus_content; _}) ->
          (consensus_content, Double_attesting)
    in
    let level =
      Protocol.Alpha_context.Raw_level.Internal_for_tests.to_repr level
    in
    let round = Protocol.Alpha_context.Round.Internal_for_tests.to_repr round in
    {level; round; kind}

  let check_from_duplicate_operation ~loc misbehaviour duplicate_op =
    Assert.equal
      ~loc
      equal
      "misbehaviours are not equal"
      pp
      misbehaviour
      (from_duplicate_operation duplicate_op)

  let from_duplicate_block (b : Block.t) =
    let open Result_wrap_syntax in
    let open Result_syntax in
    let*@ level = Protocol.Raw_level_repr.of_int32 b.header.shell.level in
    let*@ round = Protocol.Fitness_repr.round_from_raw b.header.shell.fitness in
    return {kind = Double_baking; level; round}
end

module Denunciations_repr = struct
  open Protocol.Denunciations_repr

  let pp_item fmt {operation_hash = _; rewarded; misbehaviour} =
    Format.fprintf
      fmt
      "rewarded: %a; %a"
      Signature.Public_key_hash.pp
      rewarded
      Misbehaviour_repr.pp
      misbehaviour

  let compare_item_except_hash
      {operation_hash = _; rewarded = r1; misbehaviour = m1}
      {operation_hash = _; rewarded = r2; misbehaviour = m2} =
    Compare.or_else (Protocol.Misbehaviour_repr.compare m1 m2) @@ fun () ->
    Signature.Public_key_hash.compare r1 r2
end

module Full_denunciation = struct
  open Protocol.Denunciations_repr

  type t = Signature.Public_key_hash.t * item

  let pp fmt (culprit, item) =
    Format.fprintf
      fmt
      "culprit: %a; %a"
      Signature.Public_key_hash.pp
      culprit
      Denunciations_repr.pp_item
      item

  let compare_except_hash (culprit1, item1) (culprit2, item2) =
    Compare.or_else (Signature.Public_key_hash.compare culprit1 culprit2)
    @@ fun () -> Denunciations_repr.compare_item_except_hash item1 item2

  let check_same_lists_any_order ~loc list1 list2 =
    Assert.equal_list_any_order
      ~loc
      ~compare:compare_except_hash
      "denunciation lists are not the same (not taking order into account)"
      pp
      list1
      list2
end
