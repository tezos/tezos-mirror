(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type misbehaviour_cycle = Current | Previous

let misbehaviour_cycle_encoding =
  let open Data_encoding in
  conv_with_guard
    (function Current -> 0 | Previous -> 1)
    (function
      | 0 -> Ok Current
      | 1 -> Ok Previous
      | _ -> Error "Invalid misbehaviour cycle")
    int8

type item = {
  rewarded : Signature.public_key_hash;
  misbehaviour : Misbehaviour.t;
  misbehaviour_cycle : misbehaviour_cycle;
}

let item_encoding =
  let open Data_encoding in
  conv
    (fun {rewarded; misbehaviour; misbehaviour_cycle} ->
      (rewarded, misbehaviour, misbehaviour_cycle))
    (fun (rewarded, misbehaviour, misbehaviour_cycle) ->
      {rewarded; misbehaviour; misbehaviour_cycle})
    (obj3
       (req "rewarded" Signature.Public_key_hash.encoding)
       (req "misbehaviour" Misbehaviour.encoding)
       (req "misbehaviour_cycle" misbehaviour_cycle_encoding))

type t = item list

let encoding = Data_encoding.list item_encoding

let add rewarded misbehaviour misbehaviour_cycle list =
  list @ [{rewarded; misbehaviour; misbehaviour_cycle}]
