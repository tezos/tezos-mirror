(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

type item = {
  operation_hash : Operation_hash.t;
  rewarded : Signature.public_key_hash;
  misbehaviour : Misbehaviour_repr.t;
}

let item_encoding =
  let open Data_encoding in
  conv
    (fun {operation_hash; rewarded; misbehaviour} ->
      (operation_hash, rewarded, misbehaviour))
    (fun (operation_hash, rewarded, misbehaviour) ->
      {operation_hash; rewarded; misbehaviour})
    (obj3
       (req "operation_hash" Operation_hash.encoding)
       (req "rewarded" Signature.Public_key_hash.encoding)
       (req "misbehaviour" Misbehaviour_repr.encoding))

type t = item list

let encoding = Data_encoding.list item_encoding

let add operation_hash rewarded misbehaviour list =
  list @ [{operation_hash; rewarded; misbehaviour}]
