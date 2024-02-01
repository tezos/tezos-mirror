(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let for_double_baking ctxt =
  Constants_storage.percentage_of_frozen_deposits_slashed_per_double_baking ctxt

let for_double_attestation ctxt =
  Constants_storage.percentage_of_frozen_deposits_slashed_per_double_attestation
    ctxt
