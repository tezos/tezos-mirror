(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

type index_only = Index_only

type value_only = Value_only

type unknown = Unknown

type (_, 'a) t =
  | Value : 'a -> (value_only, 'a) t
  | Hidden_value : 'a -> (unknown, 'a) t
  | Index : int32 -> (index_only, 'a) t
  | Hidden_index : int32 -> (unknown, 'a) t

type error += Index_cannot_be_negative of int32

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"indexable.index_cannot_be_negative"
    ~title:"Index of values cannot be negative"
    ~description:"A negative integer cannot be used as an index for a value."
    ~pp:(fun ppf wrong_id ->
      Format.fprintf
        ppf
        "%ld cannot be used as an index because it is negative."
        wrong_id)
    (obj1 (req "wrong_index" int32))
    (function Index_cannot_be_negative wrong_id -> Some wrong_id | _ -> None)
    (fun wrong_id -> Index_cannot_be_negative wrong_id)

type 'a value = (value_only, 'a) t

type 'a index = (index_only, 'a) t

type 'a either = (unknown, 'a) t

let value : 'a -> 'a value = fun v -> Value v

let from_value : 'a -> 'a either = fun v -> Hidden_value v

let index : int32 -> 'a index tzresult =
 fun i ->
  if Compare.Int32.(0l <= i) then ok (Index i)
  else error (Index_cannot_be_negative i)

let from_index : int32 -> 'a either tzresult =
 fun i ->
  if Compare.Int32.(0l <= i) then ok (Hidden_index i)
  else error (Index_cannot_be_negative i)

let index_exn : int32 -> 'a index =
 fun i ->
  match index i with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "Indexable.index_exn")

let from_index_exn : int32 -> 'a either =
 fun i ->
  match from_index i with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "Indexable.from_index_exn")

let destruct : type state a. (state, a) t -> (a index, a) Either.t = function
  | Hidden_value x | Value x -> Right x
  | Hidden_index x | Index x -> Left (Index x)

let forget : type state a. (state, a) t -> (unknown, a) t = function
  | Hidden_value x | Value x -> Hidden_value x
  | Hidden_index x | Index x -> Hidden_index x

let to_int32 = function Index x -> x

let to_value = function Value x -> x

let is_value_e : error:'trace -> ('state, 'a) t -> ('a, 'trace) result =
 fun ~error v ->
  match destruct v with Left _ -> Result.error error | Right v -> Result.ok v

let compact val_encoding =
  Data_encoding.Compact.(
    conv
      (function Hidden_index x -> Either.Left x | Hidden_value x -> Right x)
      (function Left x -> Hidden_index x | Right x -> Hidden_value x)
    @@ or_int32 ~int32_title:"index" ~alt_title:"value" val_encoding)

let encoding : 'a Data_encoding.t -> 'a either Data_encoding.t =
 fun val_encoding ->
  Data_encoding.Compact.make ~tag_size:`Uint8 @@ compact val_encoding

let pp :
    type state a.
    (Format.formatter -> a -> unit) -> Format.formatter -> (state, a) t -> unit
    =
 fun ppv fmt -> function
  | Hidden_index x | Index x -> Format.(fprintf fmt "#%ld" x)
  | Hidden_value x | Value x -> Format.(fprintf fmt "%a" ppv x)

let in_memory_size :
    type state a.
    (a -> Cache_memory_helpers.sint) ->
    (state, a) t ->
    Cache_memory_helpers.sint =
 fun ims ->
  let open Cache_memory_helpers in
  function
  | Hidden_value x | Value x -> header_size +! word_size +! ims x
  | Hidden_index _ | Index _ -> header_size +! word_size +! int32_size

let size : type state a. (a -> int) -> (state, a) t -> int =
 fun s -> function
  | Hidden_value x | Value x -> 1 + s x
  | Hidden_index _ | Index _ -> (* tag + int32 *) 1 + 4

let compare :
    type state state' a. (a -> a -> int) -> (state, a) t -> (state', a) t -> int
    =
 fun c x y ->
  match (x, y) with
  | (Hidden_index x | Index x), (Hidden_index y | Index y) ->
      Compare.Int32.compare x y
  | (Hidden_value x | Value x), (Hidden_value y | Value y) -> c x y
  | (Hidden_index _ | Index _), (Hidden_value _ | Value _) -> -1
  | (Hidden_value _ | Value _), (Hidden_index _ | Index _) -> 1

let compare_values c : 'a value -> 'a value -> int =
 fun (Value x) (Value y) -> c x y

let compare_indexes : 'a index -> 'a index -> int =
 fun (Index x) (Index y) -> Compare.Int32.compare x y

module type VALUE = sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module Make (V : VALUE) = struct
  type nonrec 'state t = ('state, V.t) t

  type nonrec index = V.t index

  type nonrec value = V.t value

  type nonrec either = V.t either

  let value = value

  let index = index

  let index_exn = index_exn

  let compact = compact V.encoding

  let encoding = encoding V.encoding

  let index_encoding : index Data_encoding.t =
    Data_encoding.(
      conv (fun (Index x) -> x) (fun x -> Index x) Data_encoding.int32)

  let value_encoding : value Data_encoding.t =
    Data_encoding.(conv (fun (Value x) -> x) (fun x -> Value x) V.encoding)

  let pp : Format.formatter -> 'state t -> unit = fun fmt x -> pp V.pp fmt x

  let compare_values = compare_values V.compare

  let compare_indexes = compare_indexes

  let compare : 'state t -> 'state' t -> int = fun x y -> compare V.compare x y
end
