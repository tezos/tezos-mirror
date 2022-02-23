(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Error_monad

type t = string

let name = "Chain_id"

let title = "Network identifier"

let extract bh = Bytes.sub_string (Block_hash.to_bytes bh) 0 4

let hash_bytes ?key l = extract (Block_hash.hash_bytes ?key l)

let hash_string ?key l = extract (Block_hash.hash_string ?key l)

let size = 4

let of_string_opt s = if String.length s <> size then None else Some s

let of_string s =
  match of_string_opt s with
  | None ->
      error_with
        "%s.of_string: wrong string size (%d instead of %d)"
        name
        (String.length s)
        size
  | Some h -> Ok h

let of_string_exn s =
  match of_string_opt s with
  | None ->
      Format.kasprintf
        invalid_arg
        "%s.of_string_exn: wrong string size (%d instead of %d)"
        name
        (String.length s)
        size
  | Some h -> h

let to_string s = s

let of_hex s =
  match Hex.to_string s with
  | None -> error_with "%s.of_hex: invalid hex string (%a)" name Hex.pp s
  | Some s -> of_string s

let of_hex_opt s = Option.bind (Hex.to_string s) of_string_opt

let of_hex_exn s =
  match Hex.to_string s with
  | None ->
      Format.kasprintf
        invalid_arg
        "%s.of_hex_exn: invalid hex string (%a)"
        name
        Hex.pp
        s
  | Some s -> of_string_exn s

let to_hex s = Hex.of_string (to_string s)

let of_bytes_opt b =
  if Bytes.length b <> size then None else Some (Bytes.to_string b)

let of_bytes_exn b =
  match of_bytes_opt b with
  | None ->
      let msg =
        Printf.sprintf
          "%s.of_bytes_exn: wrong string size (%d instead of %d)"
          name
          (Bytes.length b)
          size
      in
      raise (Invalid_argument msg)
  | Some h -> h

let of_bytes s =
  match of_bytes_opt s with
  | Some x -> Ok x
  | None -> error_with "Failed to deserialize a hash (%s)" name

let to_bytes = Bytes.of_string

let path_length = 1

let to_path key l =
  let (`Hex h) = to_hex key in
  h :: l

let of_path path =
  let path = String.concat "" path in
  of_hex_opt (`Hex path)

let of_path_exn path =
  let path = String.concat "" path in
  of_hex_exn (`Hex path)

let prefix_path p =
  let (`Hex p) = Hex.of_string p in
  [p]

let zero = of_hex_exn (`Hex (String.make (size * 2) '0'))

type Base58.data += Data of t

let b58check_encoding =
  Base58.register_encoding
    ~prefix:Base58.Prefix.chain_id
    ~length:size
    ~wrap:(fun s -> Data s)
    ~of_raw:of_string_opt
    ~to_raw:to_string

let raw_encoding =
  let open Data_encoding in
  conv to_bytes of_bytes_exn (Fixed.bytes size)

let of_block_hash bh = hash_bytes [Block_hash.to_bytes bh]

include (Compare.String : Compare.S with type t := t)

let hash = Stdlib.Hashtbl.hash

let seeded_hash = Stdlib.Hashtbl.seeded_hash

include Helpers.Make (struct
  type nonrec t = t

  let title = title

  let name = name

  let b58check_encoding = b58check_encoding

  let raw_encoding = raw_encoding

  let compare = compare

  let equal = equal

  let hash = hash

  let seeded_hash = seeded_hash
end)
