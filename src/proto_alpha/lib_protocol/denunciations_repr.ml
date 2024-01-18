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
  operation_hash : Operation_hash.t;
  rewarded : Signature.public_key_hash;
  misbehaviour : Misbehaviour_repr.t;
  misbehaviour_cycle : misbehaviour_cycle;
}

let item_encoding =
  let open Data_encoding in
  conv
    (fun {operation_hash; rewarded; misbehaviour; misbehaviour_cycle} ->
      (operation_hash, rewarded, misbehaviour, misbehaviour_cycle))
    (fun (operation_hash, rewarded, misbehaviour, misbehaviour_cycle) ->
      {operation_hash; rewarded; misbehaviour; misbehaviour_cycle})
    (obj4
       (req "operation_hash" Operation_hash.encoding)
       (req "rewarded" Signature.Public_key_hash.encoding)
       (req "misbehaviour" Misbehaviour_repr.encoding)
       (req "misbehaviour_cycle" misbehaviour_cycle_encoding))

type t = item list

let encoding = Data_encoding.list item_encoding

let add operation_hash rewarded misbehaviour misbehaviour_cycle list =
  list @ [{operation_hash; rewarded; misbehaviour; misbehaviour_cycle}]
