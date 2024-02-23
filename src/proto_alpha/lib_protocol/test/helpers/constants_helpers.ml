(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = Protocol.Alpha_context.Constants.Parametric.t

(* Warning: not a Set *)
module Set = struct
  let issuance_weights (c : t) issuance_weights = {c with issuance_weights}

  module Issuance_weights = struct
    let base_total_issued_per_minute (c : t) base_total_issued_per_minute =
      issuance_weights c {c.issuance_weights with base_total_issued_per_minute}
  end

  let adaptive_issuance (c : t) adaptive_issuance = {c with adaptive_issuance}

  module Adaptive_issuance = struct
    let activation_vote_enable (c : t) activation_vote_enable =
      adaptive_issuance c {c.adaptive_issuance with activation_vote_enable}

    let autostaking_enable (c : t) autostaking_enable =
      adaptive_issuance c {c.adaptive_issuance with autostaking_enable}

    let force_activation (c : t) force_activation =
      adaptive_issuance c {c.adaptive_issuance with force_activation}

    let ns_enable (c : t) ns_enable =
      adaptive_issuance c {c.adaptive_issuance with ns_enable}
  end
end
