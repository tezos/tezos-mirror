(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs <contact@nomadic-labs.com>                *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Ethereum_types

let keccak256 (Hex s) =
  let bytes = Hex.to_bytes_exn (`Hex s) in
  Tezos_crypto.Hacl.Hash.Keccak_256.digest bytes

let encode_i32_le i =
  let bytes = Bytes.make 4 '\000' in
  Bytes.set_int32_le bytes 0 i ;
  bytes

let encoding_with_optional_last_param encoding second_param_encoding
    default_second_param =
  let open Data_encoding in
  let encoding = if is_tup encoding then encoding else tup1 encoding in
  union
    [
      case
        ~title:"with_second_param"
        (Tag 0)
        (merge_tups encoding (tup1 second_param_encoding))
        (fun (t, second_param) -> Some (t, second_param))
        (fun (t, second_param) -> (t, second_param));
      case
        ~title:"without_second_param"
        (Tag 1)
        encoding
        (fun (t, _) -> Some t)
        (fun t -> (t, default_second_param));
    ]

(* A variation of List.fold_left where the function f returns an option.
   If f returns None, the fold stops and returns None.
   If f returns Some, the fold continues with the updated accumulator.
   This is a lazy fold_left to avoid iterating over the entire list unnecessarily. *)
let fold_left_option f acc l =
  let rec aux f acc l =
    match (acc, l) with
    | None, _ -> None
    | _, [] -> acc
    | Some acc, hd :: tl -> aux f (f acc hd) tl
  in
  aux f (Some acc) l
