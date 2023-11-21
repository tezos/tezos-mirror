(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type t = {
  own_frozen : Tez_repr.t;
  staked_frozen : Tez_repr.t;
  delegated : Tez_repr.t;
}

let make ~own_frozen ~staked_frozen ~delegated =
  {own_frozen; staked_frozen; delegated}

let zero =
  make
    ~own_frozen:Tez_repr.zero
    ~staked_frozen:Tez_repr.zero
    ~delegated:Tez_repr.zero

let encoding =
  let open Data_encoding in
  conv
    (fun {own_frozen; staked_frozen; delegated} ->
      (own_frozen, staked_frozen, delegated))
    (fun (own_frozen, staked_frozen, delegated) ->
      {own_frozen; staked_frozen; delegated})
    (obj3
       (req "own_frozen" Tez_repr.encoding)
       (req "staked_frozen" Tez_repr.encoding)
       (req "delegated" Tez_repr.encoding))

let voting_weight {own_frozen; staked_frozen; delegated} =
  let open Result_syntax in
  let* frozen = Tez_repr.(own_frozen +? staked_frozen) in
  let+ all = Tez_repr.(frozen +? delegated) in
  Tez_repr.to_mutez all

let apply_slashing ~percentage {own_frozen; staked_frozen; delegated} =
  let remaining_percentage = Int_percentage.neg percentage in
  let own_frozen =
    Tez_repr.mul_percentage ~rounding:`Down own_frozen remaining_percentage
  in
  let staked_frozen =
    Tez_repr.mul_percentage ~rounding:`Down staked_frozen remaining_percentage
  in
  {own_frozen; staked_frozen; delegated}
