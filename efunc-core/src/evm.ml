(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(*                                                                           *)
(*****************************************************************************)

(* This file is sourced from the efunc library, licensed under the MIT License:
   https://gitlab.com/functori/dev/efunc. *)

open Types

let lpad ?(fill = '\000') b =
  let n = Rope.length b in
  if n = 32 then b else Rope.concat2 (Rope.make (32 - n) fill) b

let rpad ?(fill = '\000') b =
  let r = Rope.length b mod 32 in
  if r = 0 then b else Rope.concat2 b (Rope.make (32 - r) fill)

let rec fixed_size ?(storage = false) : evm_type -> int = function
  | `bool -> if storage then 1 else 32
  | `int n | `uint n -> if storage then n / 8 else 32
  | `address | `fixed _ | `ufixed _ | `fbytes _ | `bytes | `string | `array _
  | `tuple _ ->
      32
  | `farray (t, n) -> n * fixed_size ~storage t

let rec encode_fixed ?(storage = false) (t : fixed_type) (value : evm_value) =
  match (value, t) with
  | `bool false, `bool ->
      if storage then Rope.of_string "\000" else lpad (Rope.of_string "\000")
  | `bool true, `bool ->
      if storage then Rope.of_string "\001" else lpad (Rope.of_string "\001")
  | `address addr, `address -> lpad (Forge.address addr)
  | `int i, (`uint n | `int n) ->
      let fill = if i >= Z.zero then '\000' else '\255' in
      lpad ~fill (Forge.z (n / 8) i)
  | `bytes b, `fbytes _n -> rpad (Forge.bytes b)
  | `string s, `fbytes _n -> rpad (Rope.of_string s)
  | `int i, `ufixed (m, n) ->
      encode_fixed ~storage (`uint m) (`int (Z.mul i (Z.pow (Z.of_int 10) n)))
  | `int i, `fixed (m, n) ->
      encode_fixed ~storage (`int m) (`int (Z.mul i (Z.pow (Z.of_int 10) n)))
  | _ -> failwith "unexpected solidity type"

let encode_length ?storage n =
  encode_fixed ?storage (`uint 256) (`int (Z.of_int n))

let rec is_fixed : evm_type -> bool = function
  | #fixed_type -> true
  | `farray (t, _) -> is_fixed t
  | `tuple l -> List.for_all is_fixed l
  | _ -> false

let rec encode_tuple ?storage lt lv =
  let l =
    match lt with
    | `s t -> List.map (fun v -> (t, v)) lv
    | `l lt -> List.combine lt lv
  in
  let offset = List.fold_left (fun s (t, _) -> s + fixed_size ?storage t) 0 l in
  let _, fv, dv =
    List.fold_left
      (fun (offset, fv, dv) (t, v) ->
        if is_fixed t then (offset, encode_value ?storage t v :: fv, dv)
        else
          let dyn = encode_value ?storage t v in
          let n_offset = offset + Rope.length dyn in
          (n_offset, encode_length ?storage offset :: fv, dyn :: dv))
      (offset, [], [])
      l
  in
  Rope.concat Rope.empty @@ List.rev_append fv (List.rev dv)

and encode_value ?storage (t : evm_type) (value : evm_value) =
  match (value, t) with
  | _, (#fixed_type as t) -> encode_fixed ?storage t value
  | `bytes b, `bytes ->
      Rope.concat2
        (encode_length ?storage @@ (String.length (b :> string) / 2))
        (rpad @@ Forge.bytes b)
  | `string s, (`bytes | `string) ->
      Rope.concat2
        (encode_length ?storage @@ String.length s)
        (rpad @@ Rope.of_string s)
  | `array l, `farray (t, _n) -> encode_tuple ?storage (`s t) l
  | `array l, `array t ->
      Rope.concat2
        (encode_length ?storage (List.length l))
        (encode_tuple ?storage (`s t) l)
  | `array l, `tuple t -> encode_tuple ?storage (`l t) l
  | _ -> failwith "unexpected solidity type"

let rec type_to_str : evm_type -> string = function
  | `int n -> "int" ^ string_of_int n
  | `uint n -> "uint" ^ string_of_int n
  | `bool -> "bool"
  | `address -> "address"
  | `fixed (m, n) -> Format.sprintf "fixed%dx%d" m n
  | `ufixed (m, n) -> Format.sprintf "ufixed%dx%d" m n
  | `fbytes n -> "bytes" ^ string_of_int n
  | `bytes -> "bytes"
  | `string -> "string"
  | `array t -> type_to_str t ^ "[]"
  | `farray (t, n) -> Format.sprintf "%s[%d]" (type_to_str t) n
  | `tuple l ->
      Format.sprintf "(%s)" @@ String.concat "," @@ List.map type_to_str l

let rec type_of_str s : evm_type =
  let rec aux ?(from = 0) s f =
    match String.rindex_opt s '[' with
    | None -> f s
    | Some i when i < from -> f s
    | Some i when String.sub s 0 i = "bytes" -> f s
    | Some i ->
        let j = String.rindex s ']' in
        if j = i + 1 then `array (aux (String.sub s 0 i) f)
        else
          `farray
            ( aux (String.sub s 0 i) f,
              int_of_string (String.sub s (i + 1) (j - i - 1)) )
  in
  if String.get s 0 = '(' then
    aux ~from:(String.rindex s ')') s @@ fun s ->
    let l = String.split_on_char ',' (String.sub s 1 (String.length s - 2)) in
    `tuple (List.map type_of_str l)
  else
    match String.sub s 0 3 with
    | "int" ->
        aux s @@ fun s ->
        `int (int_of_string @@ String.sub s 3 (String.length s - 3))
    | "uin" ->
        aux s @@ fun s ->
        `uint (int_of_string @@ String.sub s 4 (String.length s - 4))
    | "boo" -> aux s @@ fun _ -> `bool
    | "add" -> aux s @@ fun _ -> `address
    | "fix" ->
        aux s @@ fun s ->
        let i = String.rindex s 'x' in
        `fixed
          ( int_of_string @@ String.sub s 5 (i - 5),
            int_of_string @@ String.sub s (i + 1) (String.length s - i - 1) )
    | "ufi" ->
        aux s @@ fun s ->
        let i = String.rindex s 'x' in
        `ufixed
          ( int_of_string @@ String.sub s 6 (i - 6),
            int_of_string @@ String.sub s (i + 1) (String.length s - i - 1) )
    | "byt" ->
        aux s @@ fun s ->
        if s = "bytes" then `bytes
        else `fbytes (int_of_string @@ String.sub s 5 (String.length s - 5))
    | "str" -> aux s @@ fun _ -> `string
    | s -> failwith ("type not understood: " ^ s)

let method_id ~name l =
  let hash =
    Crypto.keccak @@ Format.sprintf "%s%s" name (type_to_str (`tuple l))
  in
  b @@ Hex.(show @@ of_string @@ String.sub hash 0 4)

let event_id ~name l =
  b
  @@ Hex.(
       show @@ of_string @@ Crypto.keccak
       @@ Format.sprintf "%s%s" name (type_to_str (`tuple l)))

let of_rope r = b @@ Hex.(show @@ of_string @@ Rope.to_string r)

let encode ?storage ~name lt lv =
  let method_id = method_id ~name lt in
  let values = encode_value ?storage (`tuple lt) (`array lv) in
  b @@ (method_id :> string) ^ (of_rope values :> string)

let rec decode_fixed ?(storage = false) (t : fixed_type) (r, offset) : evm_value
    =
  match t with
  | `bool ->
      let offset = if storage then offset else offset + 31 in
      let c = Rope.get r offset in
      if c = '\000' then `bool false
      else if c = '\001' then `bool true
      else
        failwith (Format.sprintf "unexpected solidity: expected bool got %C" c)
  | `address -> `address (a (of_rope (Rope.sub r (offset + 12) 20) :> string))
  | `uint n ->
      let m = n / 8 in
      if storage then
        let s = Rope.sub r offset m in
        let z, _offset = Read.z ~unsigned:true m s in
        `int z
      else `int (fst @@ Read.z m (Rope.sub r (offset + 32 - m) m))
  | `int n ->
      let m = n / 8 in
      if storage then
        let s = Rope.sub r offset m in
        `int (fst @@ Read.z m s)
      else `int (fst @@ Read.z m (Rope.sub r (offset + 32 - m) m))
  | `fbytes n -> `bytes (of_rope @@ Rope.sub r offset n)
  | `ufixed (m, _) | `fixed (m, _) -> decode_fixed ~storage (`int m) (r, offset)

let decode_length (r, offset) =
  Z.to_int @@ fst @@ Read.bez 32 (Rope.sub r offset 32)

let rec decode_tuple ?storage lt (r, offset) =
  let _, values =
    List.fold_left
      (fun (off, acc) t ->
        let acc =
          if is_fixed t then
            let v = decode_value ?storage t (r, off) in
            v :: acc
          else
            let offset2 = decode_length (r, off) + offset in
            let v = decode_value ?storage t (r, offset2) in
            v :: acc
        in
        (off + fixed_size ?storage t, acc))
      (offset, [])
      lt
  in
  `array (List.rev values)

and decode_value ?storage (t : evm_type) (r, offset) =
  match t with
  | #fixed_type as t -> decode_fixed ?storage t (r, offset)
  | `farray (t, n) ->
      decode_tuple ?storage (List.init n (fun _ -> t)) (r, offset)
  | `tuple lt -> decode_tuple ?storage lt (r, offset)
  | `bytes ->
      let size = decode_length (r, offset) in
      `bytes (of_rope (Rope.sub r (offset + 32) size))
  | `string ->
      let size = decode_length (r, offset) in
      `string (Rope.to_string (Rope.sub r (offset + 32) size))
  | `array t ->
      let size = decode_length (r, offset) in
      decode_tuple ?storage (List.init size (fun _ -> t)) (r, offset + 32)

let decode ?storage t (b : b) =
  decode_value
    ?storage
    t
    (Rope.of_string @@ Hex.to_string (`Hex (b :> string)), 0)
