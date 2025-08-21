(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020 Metastate AG <hello@metastate.dev>                     *)
(* Copyright (c) 2018-2021 Tarides <contact@tarides.com>                     *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

module Option = struct
  let map f = function None -> None | Some x -> Some (f x)

  let bind x f = match x with None -> None | Some x -> f x
end

let base = 58

let zbase = Z.of_int base

module Alphabet = struct
  type t = {encode : string; decode : string}

  let make alphabet =
    if String.length alphabet <> base then
      invalid_arg "Base58: invalid alphabet (length)" ;
    let str = Bytes.make 256 '\255' in
    for i = 0 to String.length alphabet - 1 do
      let char = int_of_char alphabet.[i] in
      if Bytes.get str char <> '\255' then
        Format.kasprintf
          invalid_arg
          "Base58: invalid alphabet (dup '%c' %d %d)"
          (char_of_int char)
          (int_of_char @@ Bytes.get str char)
          i ;
      Bytes.set str char (char_of_int i)
    done ;
    {encode = alphabet; decode = Bytes.to_string str}

  let v = make "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  let encode = v.encode

  let decode = v.decode
end

let count_trailing_char s c =
  let len = String.length s in
  let rec loop i =
    if i < 0 then len else if s.[i] <> c then len - i - 1 else loop (i - 1)
  in
  loop (len - 1)

let count_leading_char s c =
  let len = String.length s in
  let rec loop i =
    if i = len then len else if s.[i] <> c then i else loop (i + 1)
  in
  loop 0

let of_char x =
  let pos = Alphabet.decode.[int_of_char x] in
  match pos with '\255' -> None | _ -> Some (int_of_char pos)

let to_char x = Alphabet.encode.[x]

let raw_encode s =
  let len = String.length s in
  let s = String.init len (fun i -> s.[len - i - 1]) in
  let zero = Alphabet.encode.[0] in
  let zeros = count_trailing_char s '\000' in
  let res_len = ((len * 8) + 4) / 5 in
  let res = Bytes.make res_len '\000' in
  let s = Z.of_bits s in
  let rec loop s i =
    if s = Z.zero then i
    else
      let s, r = Z.div_rem s zbase in
      Bytes.set res i (to_char (Z.to_int r)) ;
      loop s (i - 1)
  in
  let i = loop s (res_len - 1) in
  let ress = Bytes.sub_string res (i + 1) (res_len - i - 1) in
  String.make zeros zero ^ ress

let string_fold f acc s =
  let acc = ref acc in
  String.iter (fun c -> acc := f !acc c) s ;
  !acc

let raw_decode s =
  string_fold
    (fun a c ->
      match (a, of_char c) with
      | Some a, Some i -> Some Z.(add (of_int i) (mul a zbase))
      | _ -> None)
    (Some Z.zero)
    s
  |> Option.map (fun res ->
         let res = Z.to_bits res in
         let res_tzeros = count_trailing_char res '\000' in
         let len = String.length res - res_tzeros in
         let zeros = count_leading_char s Alphabet.encode.[0] in
         String.make zeros '\000' ^ String.init len (fun i -> res.[len - i - 1]))

let checksum s =
  let open Digestif.SHA256 in
  let hash = digest_string (to_raw_string (digest_string s)) in
  String.sub (to_raw_string hash) 0 4

(* Append a 4-bytes cryptographic checksum before encoding string s *)
let safe_encode s = raw_encode (s ^ checksum s)

let safe_decode s =
  Option.bind (raw_decode s) (fun s ->
      let len = String.length s in
      if len < 4 then None
      else
        (* only if the string is long enough to extract a checksum do we check it *)
        let msg = String.sub s 0 (len - 4) in
        let msg_hash = String.sub s (len - 4) 4 in
        if msg_hash <> checksum msg then None else Some msg)

let remove_prefix ~prefix s =
  let x = String.length prefix in
  let n = String.length s in
  if n >= x && String.sub s 0 x = prefix then Some (String.sub s x (n - x))
  else None

type t = Base58 of string

let pp ppf (Base58 s) = Fmt.string ppf s

let decode ~prefix (Base58 s) =
  Option.bind (safe_decode s) (remove_prefix ~prefix)

let encode ~prefix d = Base58 (safe_encode (prefix ^ d))
