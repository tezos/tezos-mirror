(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

open Tezos_micheline.Micheline

let location_zero = Tezos_micheline.Micheline_parser.location_zero

let unit = Prim (location_zero, "Unit", [], [])

let is_unit = function Prim (_, "Unit", [], _) -> true | _ -> false

let decode_nat = function
  | Int (_, n) -> Ok n
  | _ -> error_with "DecodeNatError"

let decode_string = function
  | String (_, s) -> Ok s
  | _ -> error_with "DecodeStringError"

let decode_bytes = function
  | Bytes (_, b) -> Ok b
  | _ -> error_with "DecodeBytesError"

let decode_pair decode =
  let open Result_syntax in
  function
  | Prim (_, "Pair", arg, _) ->
      let* arg = decode arg in
      Ok arg
  | _ -> error_with "DecodePairError"

let decode_option decode_some =
  let open Result_syntax in
  function
  | Prim (_, "Some", [arg], _) ->
      let* arg = decode_some arg in
      Ok (Some arg)
  | Prim (_, "None", [], _) -> Ok None
  | _ -> error_with "DecodeOptionError"

let decode_set decode_elt = function
  | Seq (_, all_elt) -> List.map_e decode_elt all_elt
  | _ -> error_with "DecodeSetError"

let decode_map ~decode_key ~decode_elt =
  let open Result_syntax in
  decode_set (function
    | Prim (_, "Elt", [key; value], _) ->
        let* key = decode_key key in
        let* value = decode_elt value in
        Ok (key, value)
    | _ -> error_with "DecodeMapError")
