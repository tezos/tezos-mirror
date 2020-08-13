(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2020 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let char = Crowbar.map [Crowbar.uint8] Char.chr

let int31 : int Crowbar.gen =
  let open Crowbar in
  map [int32] (fun i32 ->
      let i = Int32.to_int i32 in
      guard (-(1 lsl 30) <= i && i <= (1 lsl 30) - 1) ;
      i)

let string = Crowbar.bytes

let short_string =
  let open Crowbar in
  choose
    [
      const "";
      bytes_fixed 1;
      bytes_fixed 2;
      bytes_fixed 3;
      bytes_fixed 4;
      bytes_fixed 5;
    ]

let short_string1 =
  let open Crowbar in
  choose
    [bytes_fixed 1; bytes_fixed 2; bytes_fixed 3; bytes_fixed 4; bytes_fixed 5]

let bytes = Crowbar.map [Crowbar.bytes] Bytes.of_string

let short_bytes = Crowbar.map [short_string] Bytes.of_string

let short_bytes1 = Crowbar.map [short_string1] Bytes.of_string

(* We need to hide the type parameter of `Encoding.t` to avoid the generator
 * combinator `choose` from complaining about different types. We use first
 * level modules (for now) to encode existentials.
 *
 * An alternative is used in https://gitlab.com/gasche/fuzz-data-encoding *)

type ('a, 'b) either = Left of 'a | Right of 'b

type _ ty =
  | Null : unit ty
  | Empty : unit ty
  | Unit : unit ty
  | Constant : string -> unit ty
  | Int8 : int ty
  | UInt8 : int ty
  | Int16 : int ty
  | UInt16 : int ty
  | Int31 : int ty
  | RangedInt : int * int -> int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Float : float ty
  | Bool : bool ty
  | String : string ty
  | Bytes : bytes ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | Dynamic_size : 'a ty -> 'a ty
  | Tup1 : 'a ty -> 'a ty
  | Tup2 : 'a ty * 'b ty -> ('a * 'b) ty
  | Union1 : 'a ty -> 'a ty
  | Union2 : 'a ty * 'b ty -> ('a, 'b) either ty
  | Matching2 : 'a ty * 'b ty -> ('a, 'b) either ty

let rec is_nullable : type a. a ty -> bool = function
  | Null ->
      true
  | Empty ->
      true
  | Unit ->
      true
  | Constant _ ->
      true
  | Int8 ->
      false
  | UInt8 ->
      false
  | Int16 ->
      false
  | UInt16 ->
      false
  | Int31 ->
      false
  | RangedInt _ ->
      false
  | Int32 ->
      false
  | Int64 ->
      false
  | Float ->
      false
  | Bool ->
      false
  | String ->
      true
  | Bytes ->
      true
  | Option _ ->
      true
  | Result (tya, tyb) ->
      is_nullable tya || is_nullable tyb
  | List ty ->
      is_nullable ty
  | Array ty ->
      is_nullable ty
  | Dynamic_size ty ->
      is_nullable ty
  | Tup1 ty ->
      is_nullable ty
  | Tup2 (tya, tyb) ->
      is_nullable tya && is_nullable tyb
  | Union1 _ -> false
  | Union2 _ | Matching2 _ -> false

let rec is_variable : type a. a ty -> bool = function
  | Null ->
      false
  | Empty ->
      false
  | Unit ->
      false
  | Constant _ ->
      false
  | Int8 ->
      false
  | UInt8 ->
      false
  | Int16 ->
      false
  | UInt16 ->
      false
  | Int31 ->
      false
  | RangedInt _ ->
      false
  | Int32 ->
      false
  | Int64 ->
      false
  | Float ->
      false
  | Bool ->
      false
  | String ->
      true
  | Bytes ->
      true
  | Option _ ->
      true
  | Result (tya, tyb) ->
      is_variable tya || is_variable tyb
  | List _ ->
      true
  | Array _ ->
      true
  | Dynamic_size _ ->
      false
  | Tup1 ty ->
      is_variable ty
  | Tup2 (tya, tyb) ->
      is_variable tya && is_variable tyb
  | Union1 tya -> is_variable tya
  | Union2 (tya, tyb) ->
      is_variable tya || is_variable tyb
  | Matching2 (tya, tyb) ->
      is_variable tya || is_variable tyb

let rec fixed_size : type a. a ty -> int option = function
  | Null ->
      Some 0
  | Empty ->
      Some 0
  | Unit ->
      Some 0
  | Constant _ ->
      Some 0
  | Int8 ->
      Some Data_encoding__Binary_size.int8
  | UInt8 ->
      Some Data_encoding__Binary_size.uint8
  | Int16 ->
      Some Data_encoding__Binary_size.int16
  | UInt16 ->
      Some Data_encoding__Binary_size.uint16
  | Int31 ->
      Some Data_encoding__Binary_size.int31
  | RangedInt (low, high) ->
      Some Data_encoding__Binary_size.(integer_to_size @@ range_to_size
      ~minimum:low ~maximum:high)
  | Int32 ->
      Some Data_encoding__Binary_size.int32
  | Int64 ->
      Some Data_encoding__Binary_size.int64
  | Float ->
      Some Data_encoding__Binary_size.float
  | Bool ->
      Some Data_encoding__Binary_size.bool
  | String ->
      None
  | Bytes ->
      None
  | Option _ ->
      None (* actually Some if payload is Null *)
  | Result _ ->
      None (* actually Some if both payloads are same fixed size *)
  | List _ ->
      None
  | Array _ ->
      None
  | Dynamic_size _ ->
      None
  | Tup1 ty ->
      fixed_size ty
  | Tup2 (tya, tyb) ->
      let ( >>? ) = Stdlib.Option.bind in
      fixed_size tya >>? fun a ->
      fixed_size tyb >>? fun b ->
      Some (a + b)
  | Union1 tya ->
      let ( >>? ) = Stdlib.Option.bind in
      fixed_size tya >>? fun a ->
      Some (a + Data_encoding__Binary_size.tag_size `Uint8)
  | Union2 _ | Matching2 _ ->
      None (* actually Some if both sizes are same fixed size *)

let rec is_zeroable : type a. a ty -> bool = function
  | Null ->
      true
  | Empty ->
      true
  | Unit ->
      true
  | Constant _ ->
      true
  | Int8 ->
      false
  | UInt8 ->
      false
  | Int16 ->
      false
  | UInt16 ->
      false
  | Int31 ->
      false
  | RangedInt _ ->
      false
  | Int32 ->
      false
  | Int64 ->
      false
  | Float ->
      false
  | Bool ->
      false
  | String ->
      true
  | Bytes ->
      true
  | Option _ ->
      true
  | Result (tya, tyb) ->
      is_zeroable tya || is_zeroable tyb
  | List _ ->
      true
  | Array _ ->
      true
  | Dynamic_size _ ->
      false
  | Tup1 ty ->
      is_zeroable ty
  | Tup2 (tya, tyb) ->
      is_zeroable tya && is_zeroable tyb
  | Union1 _ -> false
  | Union2 _ | Matching2 _ -> false

(* TODO:
   | RangedFloat : float * float -> float ty
   | Tup[3-10] : ..
*)

let rec pp_ty : type a. a ty Crowbar.printer =
 fun ppf ty ->
  match ty with
  | Null ->
      Crowbar.pp ppf "(null)"
  | Empty ->
      Crowbar.pp ppf "{}"
  | Unit ->
      Crowbar.pp ppf "()"
  | Constant s ->
      Crowbar.pp ppf "(constant:%S)" s
  | Int8 ->
      Crowbar.pp ppf "int8"
  | UInt8 ->
      Crowbar.pp ppf "uint8"
  | Int16 ->
      Crowbar.pp ppf "int16"
  | UInt16 ->
      Crowbar.pp ppf "uint16"
  | Int31 ->
      Crowbar.pp ppf "int31"
  | RangedInt (low, high) ->
      Crowbar.pp ppf "rangedint:[%d;%d]" low high
  | Int32 ->
      Crowbar.pp ppf "int32"
  | Int64 ->
      Crowbar.pp ppf "int64"
  | Float ->
      Crowbar.pp ppf "float"
  | Bool ->
      Crowbar.pp ppf "bool"
  | String ->
      Crowbar.pp ppf "string"
  | Bytes ->
      Crowbar.pp ppf "bytes"
  | Option ty ->
      Crowbar.pp ppf "option(%a)" pp_ty ty
  | Result (tya, tyb) ->
      Crowbar.pp ppf "result(%a,%a)" pp_ty tya pp_ty tyb
  | List ty ->
      Crowbar.pp ppf "list(%a)" pp_ty ty
  | Array ty ->
      Crowbar.pp ppf "array(%a)" pp_ty ty
  | Dynamic_size ty ->
      Crowbar.pp ppf "dynamic_size(%a)" pp_ty ty
  | Tup1 ty ->
      Crowbar.pp ppf "tup1(%a)" pp_ty ty
  | Tup2 (tya, tyb) ->
      Crowbar.pp ppf "tup2(%a,%a)" pp_ty tya pp_ty tyb
  | Union1 ty ->
      Crowbar.pp ppf "union1(%a)" pp_ty ty
  | Union2 (tya, tyb) ->
      Crowbar.pp ppf "union2(%a,%a)" pp_ty tya pp_ty tyb
  | Matching2 (tya, tyb) ->
      Crowbar.pp ppf "union2(%a,%a)" pp_ty tya pp_ty tyb

let dynamic_if_needed : type a. a ty -> a Data_encoding.t -> a Data_encoding.t
    =
 fun ty e ->
  if is_variable ty || is_zeroable ty then Data_encoding.dynamic_size e else e

type any_ty = AnyTy : _ ty -> any_ty

let pp_any_ty : any_ty Crowbar.printer =
 fun ppf any_ty -> match any_ty with AnyTy ty -> pp_ty ppf ty

let any_ty_gen =
  let open Crowbar in
  let g : any_ty Crowbar.gen =
    fix (fun g ->
        choose
          [
            const @@ AnyTy Null;
            const @@ AnyTy Empty;
            const @@ AnyTy Unit;
            map [string] (fun s -> AnyTy (Constant s));
            const @@ AnyTy Int8;
            const @@ AnyTy UInt8;
            const @@ AnyTy Int16;
            const @@ AnyTy UInt16;
            const @@ AnyTy Int31;
            map [int31; int31] (fun a b ->
                if a = b then Crowbar.bad_test () ;
                let low = min a b in
                let high = max a b in
                AnyTy (RangedInt (low, high)));
            const @@ AnyTy Int32;
            const @@ AnyTy Int64;
            const @@ AnyTy Float;
            const @@ AnyTy Bool;
            const @@ AnyTy String;
            const @@ AnyTy Bytes;
            map [g] (fun (AnyTy ty) -> AnyTy (Option ty));
            map [g; g] (fun (AnyTy ty_ok) (AnyTy ty_error) ->
                AnyTy (Result (ty_ok, ty_error)));
            map [g] (fun (AnyTy ty_both) ->
                AnyTy (Result (ty_both, ty_both)));
            map [g] (fun (AnyTy ty) -> AnyTy (List ty));
            map [g] (fun (AnyTy ty) -> AnyTy (Array ty));
            map [g] (fun (AnyTy ty) -> AnyTy (Dynamic_size ty));
            map [g] (fun (AnyTy ty) -> AnyTy (Tup1 ty));
            map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
                AnyTy (Tup2 (ty_a, ty_b)));
            map [g] (fun (AnyTy ty_both) ->
                AnyTy (Tup2 (ty_both, ty_both)));
            map [g] (fun (AnyTy ty_a) -> AnyTy (Union1 ty_a));
            map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
                AnyTy (Union2 (ty_a, ty_b)));
            map [g] (fun (AnyTy ty_both) ->
                AnyTy (Union2 (ty_both, ty_both)));
            map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
                AnyTy (Matching2 (ty_a, ty_b)));
            map [g] (fun (AnyTy ty_both) ->
                AnyTy (Matching2 (ty_both, ty_both)));
          ])
  in
  with_printer pp_any_ty g

module type FULL = sig
  type t

  val ty : t ty

  val eq : t -> t -> bool

  val pp : t Crowbar.printer

  val gen : t Crowbar.gen

  val encoding : t Data_encoding.t
end

type 'a full = (module FULL with type t = 'a)

(* TODO: derive equality from "parent" *)

let full_null : unit full =
  ( module struct
    type t = unit

    let ty = Null

    let eq = ( = )

    let pp ppf () = Crowbar.pp ppf "(null)"

    let gen = Crowbar.const ()

    let encoding = Data_encoding.null
  end )

let full_empty : unit full =
  ( module struct
    type t = unit

    let ty = Empty

    let eq = ( = )

    let pp ppf () = Crowbar.pp ppf "{}"

    let gen = Crowbar.const ()

    let encoding = Data_encoding.empty
  end )

let full_unit : unit full =
  ( module struct
    type t = unit

    let ty = Unit

    let eq = ( = )

    let pp ppf () = Crowbar.pp ppf "()"

    let gen = Crowbar.const ()

    let encoding = Data_encoding.unit
  end )

let full_constant s : unit full =
  ( module struct
    type t = unit

    let ty = Constant s

    let eq = ( = )

    let pp ppf () = Crowbar.pp ppf "constant:%S" s

    let gen = Crowbar.const ()

    let encoding = Data_encoding.constant s
  end )

let full_int8 : int full =
  ( module struct
    type t = int

    let ty = Int8

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = Crowbar.int8

    let encoding = Data_encoding.int8
  end )

let full_uint8 : int full =
  ( module struct
    type t = int

    let ty = UInt8

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = Crowbar.int8

    let encoding = Data_encoding.int8
  end )

let full_int16 : int full =
  ( module struct
    type t = int

    let ty = Int16

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = Crowbar.int16

    let encoding = Data_encoding.int16
  end )

let full_uint16 : int full =
  ( module struct
    type t = int

    let ty = UInt16

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = Crowbar.int16

    let encoding = Data_encoding.int16
  end )

let full_int31 : int full =
  ( module struct
    type t = int

    let ty = Int31

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = int31

    let encoding = Data_encoding.int31
  end )

let full_int32 : int32 full =
  ( module struct
    type t = int32

    let ty = Int32

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%ld" v

    let gen = Crowbar.int32

    let encoding = Data_encoding.int32
  end )

let full_int64 : int64 full =
  ( module struct
    type t = int64

    let ty = Int64

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%Ld" v

    let gen = Crowbar.int64

    let encoding = Data_encoding.int64
  end )

let full_float : float full =
  ( module struct
    type t = float

    let ty = Float

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%g" v

    let gen = Crowbar.float

    let encoding = Data_encoding.float
  end )

let full_rangedint low high : int full =
  ( module struct
    let () = assert (low < high)

    let ty = RangedInt (low, high)

    type t = int

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen =
      let open Crowbar in
      map [range (high - low)] (fun v -> v + low)

    let encoding = Data_encoding.ranged_int low high
  end )

let full_bool : bool full =
  ( module struct
    type t = bool

    let ty = Bool

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%b" v

    let gen = Crowbar.bool

    let encoding = Data_encoding.bool
  end )

let full_string : string full =
  ( module struct
    type t = string

    let ty = String

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%S" v

    let gen = string

    let encoding = Data_encoding.string
  end )

let full_string_with_check_size : string full =
  ( module struct
    type t = string

    let ty = String

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%S" v

    let gen = short_string

    let encoding =
      let open Data_encoding in
      check_size
        (5 (* max lenght of short string *) + Data_encoding__Binary_size.uint30)
        string
  end )

let full_bytes : bytes full =
  ( module struct
    type t = bytes

    let ty = Bytes

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "%S" (Bytes.to_string v)

    let gen = bytes

    let encoding = Data_encoding.bytes
  end )

let full_option : type a. a full -> a option full =
 fun full ->
  let module Full = (val full) in
  if is_nullable Full.ty then Crowbar.bad_test ()
  else
    ( module struct
      type t = Full.t option

      let ty = Option Full.ty

      let eq a b =
        match (a, b) with
        | (None, None) ->
            true
        | (Some a, Some b) ->
            Full.eq a b
        | (Some _, None) | (None, Some _) ->
            false

      let pp ppf = function
        | None ->
            Crowbar.pp ppf "none"
        | Some p ->
            Crowbar.pp ppf "some(%a)" Full.pp p

      let gen = Crowbar.option Full.gen

      let encoding = Data_encoding.(option Full.encoding)
    end )

let full_list : type a. a full -> a list full =
 fun full ->
  let module Full = (val full) in
  ( module struct
    type t = Full.t list

    let ty = List Full.ty

    let eq = ( = )

    let pp ppf v =
      Crowbar.pp
        ppf
        "list(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        v

    let gen = Crowbar.list Full.gen

    let encoding =
      Data_encoding.(list (dynamic_if_needed Full.ty Full.encoding))
  end )

let full_array : type a. a full -> a array full =
 fun full ->
  let module Full = (val full) in
  ( module struct
    type t = Full.t array

    let ty = Array Full.ty

    let eq = ( = )

    let pp ppf v =
      Crowbar.pp
        ppf
        "array(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        (Array.to_list v)

    let gen = Crowbar.(map [list Full.gen] Array.of_list)

    let encoding =
      Data_encoding.(array (dynamic_if_needed Full.ty Full.encoding))
  end )

let full_dynamic_size : type a. a full -> a full =
 fun full ->
  let module Full = (val full) in
  ( module struct
    include Full

    let ty = Dynamic_size ty

    let encoding = Data_encoding.dynamic_size encoding
  end )

let full_tup1 : type a. a full -> a full =
 fun full ->
  let module Full = (val full) in
  ( module struct
    type t = Full.t

    let ty = Tup1 Full.ty

    let eq = ( = )

    let pp ppf v = Crowbar.pp ppf "tup1(%a)" Full.pp v

    let gen = Full.gen

    let encoding = Data_encoding.tup1 Full.encoding
  end )

let full_tup2 : type a b. a full -> b full -> (a * b) full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  ( module struct
    type t = Fulla.t * Fullb.t

    let ty = Tup2 (Fulla.ty, Fullb.ty)

    let eq = ( = )

    let pp ppf (a, b) = Crowbar.pp ppf "tup2(%a,%a)" Fulla.pp a Fullb.pp b

    let gen = Crowbar.map [Fulla.gen; Fullb.gen] (fun a b -> (a, b))

    let encoding =
      Data_encoding.(
        tup2 (dynamic_if_needed Fulla.ty Fulla.encoding) Fullb.encoding)
  end )

let full_result : type a b. a full -> b full -> (a, b) result full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  ( module struct
    type t = (Fulla.t, Fullb.t) result

    let ty = Result (Fulla.ty, Fullb.ty)

    let eq = ( = )

    let gen = Crowbar.result Fulla.gen Fullb.gen

    let encoding = Data_encoding.result Fulla.encoding Fullb.encoding

    let pp ppf = function
      | Ok a ->
          Crowbar.pp ppf "ok(%a)" Fulla.pp a
      | Error b ->
          Crowbar.pp ppf "error(%a)" Fullb.pp b

  end )


let full_union1
: type a. a full -> a full
= fun fulla ->
  let module Fulla = (val fulla) in
  ( module struct
    type t = Fulla.t

    let ty = Union1 Fulla.ty

    let eq = Fulla.eq

    let a_ding =
      let open Data_encoding in
      obj1 (req "OnlyThisOneOnly" Fulla.encoding)

    let encoding =
      let open Data_encoding in
      union
        [
          case
            ~title:"A"
            (Tag 0)
            a_ding
            (function v -> Some v)
            (fun v -> v);
        ]

    let gen = Fulla.gen

    let pp ppf = function
      | v1 ->
          Crowbar.pp ppf "@[<hv 1>(Union1 %a)@]" Fulla.pp v1
  end )

let full_union2
: type a b. a full -> b full -> (a, b) either full
= fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  ( module struct
    type t = (Fulla.t, Fullb.t) either

    let ty = Union2 (Fulla.ty, Fullb.ty)

    let eq x y = match x, y with
    | Left _, Right _ | Right _, Left _ -> false
    | Left x, Left y -> Fulla.eq x y
    | Right x, Right y -> Fullb.eq x y

    let a_ding =
      let open Data_encoding in
      obj1 (req "A" Fulla.encoding)

    let b_ding =
      let open Data_encoding in
      obj1 (req "B" Fullb.encoding)

    let encoding =
      let open Data_encoding in
      union
        [
          case
            ~title:"A"
            (Tag 0)
            a_ding
            (function Left v -> Some v | Right _ -> None)
            (fun v -> Left v);
          case
            ~title:"B"
            (Tag 1)
            b_ding
            (function Left _ -> None | Right v -> Some v)
            (fun v -> Right v);
        ]

    let gen =
       let open Crowbar in
       map [bool; Fulla.gen; Fullb.gen] (fun choice a b -> if choice then Left a else Right b)

    let pp ppf = function
      | Left v1 ->
          Crowbar.pp ppf "@[<hv 1>(A %a)@]" Fulla.pp v1
      | Right v2 ->
          Crowbar.pp ppf "@[<hv 1>(B %a)@]" Fullb.pp v2
  end )

(* TODO: test recursive-sum. How to get a `_ ty`? a `_ full`? *)

let full_matching2
: type a b. a full -> b full -> (a, b) either full
= fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  ( module struct
    type t = (Fulla.t, Fullb.t) either

    let ty = Matching2 (Fulla.ty, Fullb.ty)

    let eq x y = match x, y with
    | Left _, Right _ | Right _, Left _ -> false
    | Left x, Left y -> Fulla.eq x y
    | Right x, Right y -> Fullb.eq x y

    let a_ding =
      let open Data_encoding in
      obj1 (req "A" Fulla.encoding)

    let b_ding =
      let open Data_encoding in
      obj1 (req "B" Fullb.encoding)

    let encoding =
      let open Data_encoding in
      matching
        (function Left v -> matched 0 a_ding v | Right v -> matched 1 b_ding v)
        [
          case
            ~title:"A"
            (Tag 0)
            a_ding
            (function Left v -> Some v | Right _ -> None)
            (fun v -> Left v);
          case
            ~title:"B"
            (Tag 1)
            b_ding
            (function Left _ -> None | Right v -> Some v)
            (fun v -> Right v);
        ]

    let gen =
       let open Crowbar in
       map [bool; Fulla.gen; Fullb.gen] (fun choice a b -> if choice then Left a else Right b)

    let pp ppf = function
      | Left v1 ->
          Crowbar.pp ppf "@[<hv 1>(A %a)@]" Fulla.pp v1
      | Right v2 ->
          Crowbar.pp ppf "@[<hv 1>(B %a)@]" Fullb.pp v2
  end )

(*
(* TODO: check `has_max_size` rather than `fixed_size` (e.g., `int option` has
 a max size but is not fixed size. *)
(* TODO: fix this, currently "The type t in this module cannot be exported." *)
let full_check_size
: type a. a full -> a full =
 fun full ->
  let module Full = (val full) in
  match fixed_size Full.ty with
  | None -> Crowbar.bad_test ()
  | Some size ->
    ( module struct
       include Fulla
       let encoding = Data_encoding.check_size size encoding
    end )
*)

let rec full_of_ty : type a. a ty -> a full = function
  | Null ->
      full_null
  | Empty ->
      full_empty
  | Unit ->
      full_unit
  | Constant s ->
      full_constant s
  | Int8 ->
      full_int8
  | UInt8 ->
      full_uint8
  | Int16 ->
      full_int16
  | UInt16 ->
      full_uint16
  | Int31 ->
      full_int31
  | RangedInt (low, high) ->
      full_rangedint low high
  | Int32 ->
      full_int32
  | Int64 ->
      full_int64
  | Float ->
      full_float
  | Bool ->
      full_bool
  | String ->
      full_string
  | Bytes ->
      full_bytes
  | Option ty ->
      full_option (full_of_ty ty)
  | Result (tya, tyb) ->
      full_result (full_of_ty tya) (full_of_ty tyb)
  | List ty ->
      full_list (full_of_ty ty)
  | Array ty ->
      full_array (full_of_ty ty)
  | Dynamic_size ty ->
      full_dynamic_size (full_of_ty ty)
  | Tup1 ty ->
      full_tup1 (full_of_ty ty)
  | Tup2 (tya, tyb) ->
      full_tup2 (full_of_ty tya) (full_of_ty tyb)
  | Union1 ty ->
      full_union1 (full_of_ty ty)
  | Union2 (tya, tyb) ->
      full_union2 (full_of_ty tya) (full_of_ty tyb)
  | Matching2 (tya, tyb) ->
      full_matching2 (full_of_ty tya) (full_of_ty tyb)

type full_and_v = FullAndV : 'a full * 'a -> full_and_v

let gen : full_and_v Crowbar.gen =
  let open Crowbar in
  dynamic_bind any_ty_gen (function AnyTy ty ->
      let full = full_of_ty ty in
      let module Full = (val full) in
      map [Full.gen] (fun v -> FullAndV (full, v)))
