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

let get ctxt ~(kind : Misbehaviour_repr.kind) ~level denunciations =
  ignore level ;
  ignore denunciations ;
  match kind with
  | Double_baking -> for_double_baking ctxt
  | Double_attesting | Double_preattesting -> for_double_attestation ctxt
