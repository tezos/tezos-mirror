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

open Binary_error_types

let fixed_length e =
  match Encoding.classify e with
  | `Fixed n -> Some n
  | `Dynamic | `Variable -> None

let n_length value =
  let bits = Z.numbits value in
  if bits = 0 then 1 else (bits + 6) / 7

let z_length value = (Z.numbits value + 1 + 6) / 7

let rec length : type x. x Encoding.t -> x -> int =
 fun e value ->
  let open Encoding in
  match e.encoding with
  (* Fixed *)
  | Null -> 0
  | Empty -> 0
  | Constant _ -> 0
  | Bool -> Binary_size.bool
  | Int8 -> Binary_size.int8
  | Uint8 -> Binary_size.uint8
  | Int16 -> Binary_size.int16
  | Uint16 -> Binary_size.uint16
  | Int31 -> Binary_size.int31
  | Int32 -> Binary_size.int32
  | Int64 -> Binary_size.int64
  | N -> n_length value
  | Z -> z_length value
  | RangedInt {minimum; maximum} ->
      Binary_size.integer_to_size @@ Binary_size.range_to_size ~minimum ~maximum
  | Float -> Binary_size.float
  | RangedFloat _ -> Binary_size.float
  | Bytes (`Fixed n) -> n
  | String (`Fixed n) -> n
  | Padded (e, n) -> length e value + n
  | String_enum (_, arr) ->
      Binary_size.integer_to_size @@ Binary_size.enum_size arr
  | Objs {kind = `Fixed n; _} -> n
  | Tups {kind = `Fixed n; _} -> n
  | Union {kind = `Fixed n; _} -> n
  (* Dynamic *)
  | Objs {kind = `Dynamic; left; right} ->
      let v1, v2 = value in
      length left v1 + length right v2
  | Tups {kind = `Dynamic; left; right} ->
      let v1, v2 = value in
      length left v1 + length right v2
  | Mu {kind = `Dynamic; fix; _} -> length (fix e) value
  | Obj (Opt {kind = `Dynamic; encoding = e; _}) -> (
      match value with None -> 1 | Some value -> 1 + length e value)
  (* Variable *)
  | Ignore -> 0
  | Bytes `Variable -> Bytes.length value
  | String `Variable -> String.length value
  | Array {length_limit; elts} -> (
      match length_limit with
      | No_limit -> Array.fold_left (fun acc v -> length elts v + acc) 0 value
      | At_most max_length ->
          if Array.length value > max_length then
            raise (Write_error Array_too_long) ;
          Array.fold_left (fun acc v -> length elts v + acc) 0 value
      | Exactly exact_length -> (
          if Array.length value <> exact_length then
            raise (Write_error Array_too_long) ;
          match fixed_length elts with
          | Some s -> exact_length * s
          | None -> Array.fold_left (fun acc v -> length elts v + acc) 0 value))
  | List {length_limit; elts} -> (
      match length_limit with
      | No_limit -> List.fold_left (fun acc v -> length elts v + acc) 0 value
      | At_most max_length ->
          if List.length value > max_length then
            raise (Write_error List_too_long) ;
          List.fold_left (fun acc v -> length elts v + acc) 0 value
      | Exactly exact_length -> (
          if List.length value <> exact_length then
            raise (Write_error List_too_long) ;
          match fixed_length elts with
          | Some s -> exact_length * s
          | None -> List.fold_left (fun acc v -> length elts v + acc) 0 value))
  | Objs {kind = `Variable; left; right} ->
      let v1, v2 = value in
      length left v1 + length right v2
  | Tups {kind = `Variable; left; right} ->
      let v1, v2 = value in
      length left v1 + length right v2
  | Obj (Opt {kind = `Variable; encoding = e; _}) -> (
      match value with None -> 0 | Some value -> length e value)
  | Mu {kind = `Variable; fix; _} -> length (fix e) value
  (* Variable or Dynamic we don't care for those constructors *)
  | Union {kind = `Dynamic | `Variable; tag_size; match_case; _} ->
      let (Matched (tag, e, value)) = match_case value in
      assert (tag <= Binary_size.max_int tag_size) ;
      Binary_size.tag_size tag_size + length e value
  (* Recursive*)
  | Obj (Req {encoding = e; _}) -> length e value
  | Obj (Dft {encoding = e; _}) -> length e value
  | Tup e -> length e value
  | Conv {encoding = e; proj; _} -> length e (proj value)
  | Describe {encoding = e; _} -> length e value
  | Splitted {encoding = e; _} -> length e value
  | Dynamic_size {kind; encoding = e} ->
      let length = length e value in
      Binary_size.integer_to_size kind + length
  | Check_size {limit; encoding = e} ->
      let length = length e value in
      if length > limit then raise (Write_error Size_limit_exceeded) ;
      length
  | Delayed f -> length (f ()) value

let rec maximum_length : type a. a Encoding.t -> int option =
 fun e ->
  let ( >>? ) = Option.bind in
  let ( >|? ) x f = Option.map f x in
  let open Encoding in
  match e.encoding with
  (* Fixed *)
  | Null -> Some 0
  | Empty -> Some 0
  | Constant _ -> Some 0
  | Bool -> Some Binary_size.bool
  | Int8 -> Some Binary_size.int8
  | Uint8 -> Some Binary_size.uint8
  | Int16 -> Some Binary_size.int16
  | Uint16 -> Some Binary_size.uint16
  | Int31 -> Some Binary_size.int31
  | Int32 -> Some Binary_size.int32
  | Int64 -> Some Binary_size.int64
  | N -> None
  | Z -> None
  | RangedInt {minimum; maximum} ->
      Some
        (Binary_size.integer_to_size
        @@ Binary_size.range_to_size ~minimum ~maximum)
  | Float -> Some Binary_size.float
  | RangedFloat _ -> Some Binary_size.float
  | Bytes (`Fixed n) -> Some n
  | String (`Fixed n) -> Some n
  | Padded (e, n) -> maximum_length e >|? fun s -> s + n
  | String_enum (_, arr) ->
      Some (Binary_size.integer_to_size @@ Binary_size.enum_size arr)
  | Objs {kind = `Fixed n; _} -> Some n
  | Tups {kind = `Fixed n; _} -> Some n
  | Union {kind = `Fixed n; _} -> Some n
  (* Dynamic *)
  | Obj (Opt {kind = `Dynamic; encoding = e; _}) ->
      maximum_length e >|? fun s -> s + Binary_size.uint8
  (* Variable *)
  | Ignore -> Some 0
  | Bytes `Variable -> None
  | String `Variable -> None
  | Array {length_limit; elts = e} -> (
      match length_limit with
      | No_limit -> None
      | At_most max_length -> maximum_length e >|? fun s -> s * max_length
      | Exactly exact_length -> maximum_length e >|? fun s -> s * exact_length)
  | List {length_limit; elts = e} -> (
      match length_limit with
      | No_limit -> None
      | At_most max_length -> maximum_length e >|? fun s -> s * max_length
      | Exactly exact_length -> maximum_length e >|? fun s -> s * exact_length)
  | Obj (Opt {kind = `Variable; encoding = e; _}) -> maximum_length e
  (* Variable or Dynamic we don't care for those constructors *)
  | Union {kind = `Dynamic | `Variable; tag_size; cases; _} ->
      List.fold_left
        (fun acc (Case {encoding = e; _}) ->
          acc >>? fun acc ->
          maximum_length e >|? fun s -> Stdlib.max acc s)
        (Some 0)
        cases
      >|? fun s -> s + Binary_size.tag_size tag_size
  | Objs {kind = `Dynamic | `Variable; left; right} ->
      maximum_length left >>? fun l ->
      maximum_length right >|? fun r -> l + r
  | Tups {kind = `Dynamic | `Variable; left; right} ->
      maximum_length left >>? fun l ->
      maximum_length right >|? fun r -> l + r
  | Mu _ ->
      (* There could be bounded-size uses of Mu but it's unreasonable to expect
         to detect them statically this way. Use `check_size` around the mu to
         translate user-invariants into static encoding invariants *)
      None
  (* Recursive*)
  | Obj (Req {encoding = e; _}) -> maximum_length e
  | Obj (Dft {encoding = e; _}) -> maximum_length e
  | Tup e -> maximum_length e
  | Conv {encoding = e; _} -> maximum_length e
  | Describe {encoding = e; _} -> maximum_length e
  | Splitted {encoding = e; _} -> maximum_length e
  | Dynamic_size {kind; encoding = e} ->
      maximum_length e >|? fun s -> s + Binary_size.integer_to_size kind
  | Check_size {limit; encoding = e} ->
      (* NOTE: it is possible that the statically-provable maximum size exceeds
         the dynamically checked limit. But the difference might be explained by
         subtle invariants that do not appear in the encoding. *)
      Some
        (Option.fold
           (maximum_length e)
           ~some:(fun s -> min s limit)
           ~none:limit)
  | Delayed f -> maximum_length (f ())
