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

type (_, 'a) t =
  | Value : 'a -> ([> `Value], 'a) t
  | Index : int32 -> ([> `Id], 'a) t

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

type value_only = [`Value]

type id_only = [`Id]

type unknown = [value_only | id_only]

type 'a value = (value_only, 'a) t

type 'a index = (id_only, 'a) t

type 'a either = (unknown, 'a) t

let forget_index : 'a index -> 'a either = function Index _ as id -> id

let forget_value : 'a value -> 'a either = function Value _ as v -> v

type index_only = [`Id]

let value : 'a -> 'a value = fun v -> Value v

let from_value : 'a -> 'a either = fun x -> value x |> forget_value

let index : int32 -> 'a index tzresult =
 fun i ->
  if Compare.Int32.(0l <= i) then ok (Index i)
  else error (Index_cannot_be_negative i)

let index_exn : int32 -> 'a index =
 fun i ->
  match index i with
  | Ok x -> x
  | Error _ -> raise (Invalid_argument "Indexable.index_exn")

let from_index : int32 -> 'a either tzresult = fun x -> index x >|? forget_index

let from_index_exn : int32 -> 'a either = fun x -> index_exn x |> forget_index

let prepare_index h : 'a either -> 'a index tzresult Lwt.t = function
  | Value x -> ( h x >|= function Ok x -> index x | Error e -> Error e)
  | Index x -> return (Index x)

let prepare_value h : 'a either -> 'a value tzresult Lwt.t = function
  | Index x -> h x >|=? fun x -> Value x
  | Value x -> return (Value x)

let encoding : 'a Data_encoding.t -> 'a either Data_encoding.t =
 fun val_encoding ->
  Data_encoding.(
    union
      [
        case
          (Tag 0)
          ~title:"Key"
          int32
          (function Index x -> Some x | _ -> None)
          (fun x -> Index x);
        case
          (Tag 1)
          ~title:"Value"
          val_encoding
          (function Value x -> Some x | _ -> None)
          (fun x -> Value x);
      ])

let pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a either -> unit =
 fun ppv fmt -> function
  | Index x -> Format.(fprintf fmt "#%ld" x)
  | Value x -> Format.(fprintf fmt "%a" ppv x)

let in_memory_size ims =
  let open Cache_memory_helpers in
  function
  | Value x -> header_size +! word_size +! ims x
  | Index _ -> header_size +! word_size +! int32_size

let size s = function Value x -> 1 + s x | Index _ -> 1 (* tag *) + 4
(* int32 *)

let compare c x y =
  match (x, y) with
  | (Index x, Index y) -> Compare.Int32.compare x y
  | (Value x, Value y) -> c x y
  | (Index _, Value _) -> -1
  | (Value _, Index _) -> 1

let compare_values c : 'a value -> 'a value -> int =
 fun x y -> match (x, y) with (Value x, Value y) -> c x y

module type VALUE = sig
  type t

  val encoding : t Data_encoding.t

  val compare : t -> t -> int

  val pp : Format.formatter -> t -> unit
end

module Make (V : VALUE) = struct
  type nonrec 'state t = ('state, V.t) t

  type nonrec either = V.t either

  type nonrec index = V.t index

  type nonrec value = V.t value

  let prepare_index = prepare_index

  let prepare_value = prepare_value

  let forget_value = forget_value

  let forget_index = forget_index

  let value = value

  let from_value = from_value

  let index = index

  let index_exn = index_exn

  let from_index = from_index

  let from_index_exn = from_index_exn

  let encoding = encoding V.encoding

  let index_encoding : index Data_encoding.t =
    Data_encoding.(
      conv
        (fun (Index x : index) -> x)
        (fun x : index -> Index x)
        Data_encoding.int32)

  let pp = pp V.pp

  let compare = compare V.compare

  let compare_values = compare_values V.compare
end
