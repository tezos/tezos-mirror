(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(* This datatype aims to represent encodings from
   [Data_encoding.t]. The GADT is used to know what is the underlying
   ocaml datatype. This allows to write the logic of this module in a
   generic manner, as long as the tests. *)

type 'ocaml ty =
  | Int64 : int64 ty
  | Int32 : int32 ty
  | Int31 : int ty
  | Int16 : int ty
  | Uint16 : int ty
  | Int8 : int ty
  | Uint8 : int ty

let encoding : type a. a ty -> a Data_encoding.t = function
  | Int64 -> Data_encoding.int64
  | Int32 -> Data_encoding.int32
  | Int31 -> Data_encoding.int31
  | Int16 -> Data_encoding.int16
  | Uint16 -> Data_encoding.uint16
  | Int8 -> Data_encoding.int8
  | Uint8 -> Data_encoding.uint8

let compare : type a. a ty -> (module Compare.S with type t = a) = function
  | Int64 -> (module Compare.Int64)
  | Int32 -> (module Compare.Int32)
  | Int31 -> (module Compare.Int)
  | Int16 -> (module Compare.Int)
  | Uint16 -> (module Compare.Int)
  | Int8 -> (module Compare.Int)
  | Uint8 -> (module Compare.Int)

let pp_ty : type a. Format.formatter -> a ty -> unit =
 fun fmt ty ->
  match ty with
  | Int64 -> Format.fprintf fmt "int64"
  | Int32 -> Format.fprintf fmt "int32"
  | Int31 -> Format.fprintf fmt "int31"
  | Int16 -> Format.fprintf fmt "int16"
  | Uint16 -> Format.fprintf fmt "uint16"
  | Int8 -> Format.fprintf fmt "int8"
  | Uint8 -> Format.fprintf fmt "uint8"

let pp : type a. a ty -> Format.formatter -> a -> unit = function
  | Int64 -> fun fmt value -> Format.fprintf fmt "%Ld" value
  | Int32 -> fun fmt value -> Format.fprintf fmt "%ld" value
  | Int31 -> Format.pp_print_int
  | Int16 -> Format.pp_print_int
  | Uint16 -> Format.pp_print_int
  | Int8 -> Format.pp_print_int
  | Uint8 -> Format.pp_print_int

let ty_max_value : type a. a ty -> a = function
  | Int64 -> Int64.max_int
  | Int32 -> Int32.max_int
  | Int31 -> (1 lsl 30) - 1
  | Int16 -> (1 lsl 15) - 1
  | Uint16 -> (1 lsl 16) - 1
  | Int8 -> (1 lsl 7) - 1
  | Uint8 -> (1 lsl 8) - 1

let ty_min_value : type a. a ty -> a = function
  | Int64 -> Int64.min_int
  | Int32 -> Int32.min_int
  | Int31 -> -(1 lsl 30)
  | Int16 -> -(1 lsl 15)
  | Uint16 -> 0
  | Int8 -> -(1 lsl 7)
  | Uint8 -> 0

module type BOUNDS = sig
  type ocaml_type

  val min_value : ocaml_type

  val max_value : ocaml_type
end

module type S = sig
  type t

  type ocaml_type

  include BOUNDS with type ocaml_type := ocaml_type

  include Compare.S with type t := t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit

  val to_value : t -> ocaml_type

  val of_value : ocaml_type -> t option
end

(* If the encoding choosen can represent strictly less values than the
   underlying ocaml datatype, some exceptions could be raised at
   encoding time. Those static checks aims to be executed when the
   functor is instantiated to detect those cases sooner. *)
let checks (type ocaml_type) (ty : ocaml_type ty) ( < ) ( > ) ~min_value
    ~max_value =
  let pp = pp ty in
  if max_value > ty_max_value ty then
    invalid_arg
      (Format.asprintf
         "Tezos-base.Bounded(%a): Maximum encodable value: %a. Bound given: %a"
         pp_ty
         ty
         pp
         (ty_max_value ty)
         pp
         max_value) ;
  if min_value < ty_min_value ty then
    invalid_arg
      (Format.asprintf
         "Tezos-base.Bounded(%a): Minimum encodable value: %a. Bound given: %a"
         pp_ty
         ty
         pp
         (ty_max_value ty)
         pp
         max_value)
[@@inline always]

(* A partial encoding that ensures the decoded value is in the specified bounds. *)
let guarded_encoding ty ~to_value ~of_value =
  let open Data_encoding in
  conv_with_guard
    to_value
    (fun x ->
      match of_value x with None -> Error "Out of bounds" | Some x -> Ok x)
    (encoding ty)
[@@inline always]

let of_value ( < ) ( > ) ~min_value ~max_value x =
  if x < min_value then None else if x > max_value then None else Some x
[@@inline always]

(* We introduce one functor by OCaml datatype so that comparison
   functions are statically known and consequently inlined. Using the
   GADT, we could generalise this, but OCaml (without flambda) is
   unable to make the correct optimisations to inline the comparison
   functions. All the business code has been factored out so only the
   declaration is duplicated. *)
module Int64 (B : BOUNDS with type ocaml_type := int64) = struct
  include Compare.Int64
  include B

  let to_value = Fun.id

  let of_value = of_value ( < ) ( > ) ~min_value ~max_value

  let encoding = guarded_encoding Int64 ~to_value ~of_value

  let pp = pp Int64

  let () = checks Int64 ( < ) ( > ) ~min_value ~max_value
end

module Int32 (B : BOUNDS with type ocaml_type := int32) = struct
  include Compare.Int32
  include B

  let to_value = Fun.id

  let of_value = of_value ( < ) ( > ) ~min_value ~max_value

  let encoding = guarded_encoding Int32 ~to_value ~of_value

  let pp = pp Int32

  let () = checks Int32 ( < ) ( > ) ~min_value ~max_value
end

(* A specifialisation of the functor above where the interval is
   restricted to the non negative integer that can be represented on
   4 bytes. *)
module Non_negative_int32 = Int32 (struct
  let min_value = 0l

  let max_value = Stdlib.Int32.max_int
end)

(* The parameter [T] of this functor allows to choose the desired
   encoding without duplicating the interface. *)
module Make31
    (T : sig
      val ty : int ty
    end)
    (B : BOUNDS with type ocaml_type := int) =
struct
  include Compare.Int
  include B

  let to_value = Fun.id

  let of_value = of_value ( < ) ( > ) ~min_value ~max_value

  let encoding = guarded_encoding T.ty ~to_value ~of_value

  let pp = pp T.ty

  let () = checks T.ty ( < ) ( > ) ~min_value ~max_value
end

module Int31 = Make31 (struct
  let ty = Int31
end)

module Int16 = Make31 (struct
  let ty = Int16
end)

module Uint16 = Make31 (struct
  let ty = Uint16
end)

module Int8 = Make31 (struct
  let ty = Int8
end)

module Uint8 = Make31 (struct
  let ty = Uint8
end)

module Internal_for_tests = struct
  type 'ocaml t = 'ocaml ty =
    | Int64 : int64 t
    | Int32 : int32 t
    | Int31 : int t
    | Int16 : int t
    | Uint16 : int t
    | Int8 : int t
    | Uint8 : int t

  let min_value = ty_min_value

  let max_value = ty_max_value

  let compare = compare

  let pp_ty = pp_ty
end
