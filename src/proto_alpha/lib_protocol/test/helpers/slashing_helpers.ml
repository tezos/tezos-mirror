(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

module Misbehaviour_repr = struct
  let pp fmt {Protocol.Misbehaviour_repr.level; round; kind} =
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
          (consensus_content, Protocol.Misbehaviour_repr.Double_preattesting)
      | Single (Attestation {consensus_content; _}) ->
          (consensus_content, Protocol.Misbehaviour_repr.Double_attesting)
    in
    let level =
      Protocol.Alpha_context.Raw_level.Internal_for_tests.to_repr level
    in
    let round = Protocol.Alpha_context.Round.Internal_for_tests.to_repr round in
    {Protocol.Misbehaviour_repr.level; round; kind}

  let check_from_duplicate_operation ~loc misbehaviour duplicate_op =
    Assert.equal
      ~loc
      equal
      "misbehaviours are not equal"
      pp
      misbehaviour
      (from_duplicate_operation duplicate_op)
end
