(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

open Rope

type rlp = Types.rlp = S of Rope.t | L of rlp list

let rec display_rope (tree : rlp) =
  let comma = of_string ", " in
  let quotation = of_string "\"" in
  let quote r = concat empty [quotation; r; quotation] in
  match tree with
  | S content -> quote content
  | L lst ->
      concat
        empty
        [of_string "["; concat comma (List.map display_rope lst); of_string "]"]

let display (tree : rlp) = to_string (display_rope tree)

let immediate (i : int) : rope = of_char (Char.chr i)

let rec encode_int (i : int) : rope =
  if i = 0 then empty
  else concat2 (encode_int (i / 256)) (immediate (i mod 256))

let rec encode_z (i : Z.t) acc : rope =
  if i = Z.zero then acc
  else
    let modulo = Z.of_int 256 in
    encode_z
      (Z.div i modulo)
      (concat2 (immediate Z.(to_int (Z.rem i modulo))) acc)

let int (i : int) : rlp = S (encode_int i)

let z (i : Z.t) : rlp = S (encode_z i empty)

let rec encode = function
  | S rope -> encode_rope rope
  | L lst -> encode_list lst

and encode_rope (rope : rope) : rope =
  let len = length rope in
  if len = 1 && Char.code (get rope 0) < 128 then rope
  else if len < 56 then concat2 (immediate (0x80 + len)) rope
  else
    let encodedLen = encode_int len in
    let lenlen = length encodedLen in
    concat empty [immediate (183 + lenlen); encodedLen; rope]

and encode_list (lst : rlp list) : rope =
  let body = concat empty (List.map encode lst) in
  let bodyLen = length body in
  if bodyLen < 56 then concat2 (immediate (192 + bodyLen)) body
  else
    let bodyLen = encode_int bodyLen in
    let bodyLenLen = length bodyLen in
    concat empty [immediate (247 + bodyLenLen); bodyLen; body]

exception InvalidRlp

let rec decode_int (r : rope) : int =
  if is_empty r then 0
  else
    (256 * decode_int (sub r 0 (length r - 1)))
    + Char.code (get r (length r - 1))

let rec decode_obj (rope : rope) : rlp * rope =
  let len = length rope in
  if len = 0 then raise InvalidRlp
  else
    let firstChar = Char.code (get rope 0) in
    if firstChar < 128 then (S (sub rope 0 1), sub rope 1 (len - 1))
    else if firstChar < 184 then
      let bodyLen = firstChar - 128 in
      let ret = sub rope 1 bodyLen in
      if bodyLen = 1 && Char.code (get ret 0) < 128 then raise InvalidRlp
      else (S ret, sub rope (1 + bodyLen) (len - 1 - bodyLen))
    else if firstChar < 192 then
      let bodyLenLen = firstChar - 183 in
      let bodyLen = decode_int (sub rope 1 bodyLenLen) in
      if bodyLen < 56 then raise InvalidRlp
      else
        ( S (sub rope (1 + bodyLenLen) bodyLen),
          sub rope (1 + bodyLenLen + bodyLen) (len - 1 - bodyLenLen - bodyLen)
        )
    else if firstChar < 248 then
      let bodyLen = firstChar - 192 in
      ( L (decode_list (sub rope 1 bodyLen)),
        sub rope (1 + bodyLen) (len - 1 - bodyLen) )
    else
      let bodyLenLen = firstChar - 247 in
      let bodyLen = decode_int (sub rope 1 bodyLenLen) in
      if bodyLen < 56 then raise InvalidRlp
      else
        ( L (decode_list (sub rope (1 + bodyLenLen) bodyLen)),
          sub rope (1 + bodyLenLen + bodyLen) (len - 1 - bodyLenLen - bodyLen)
        )

and decode_list (rope : rope) : rlp list = decode_list_inner [] rope

and decode_list_inner revAcc rope : rlp list =
  if is_empty rope then List.rev revAcc
  else
    let hd, rest = decode_obj rope in
    decode_list_inner (hd :: revAcc) rest

let decode (rope : rope) : rlp =
  try
    let ret, rest = decode_obj rope in
    if is_empty rest then ret else raise InvalidRlp
  with Invalid_argument _ -> raise InvalidRlp
