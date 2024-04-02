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

type ('a, 'b) recursion_test_type =
  | C of ('a * 'b * ('a, 'b) recursion_test_type option)

let char = Crowbar.map [Crowbar.uint8] Char.chr

let int31 : int Crowbar.gen =
  let open Crowbar in
  map [int32] (fun i32 ->
      let open Int32 in
      guard (neg (shift_left 1l 30) <= i32 && i32 <= sub (shift_left 1l 30) 1l) ;
      Int32.to_int i32)

let uint30 : int Crowbar.gen =
  let open Crowbar in
  map [int32] (fun i32 ->
      let open Int32 in
      guard (0l <= i32 && i32 <= sub (shift_left 1l 30) 1l) ;
      Int32.to_int i32)

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

type big_length = [`N | `Uint30]

let big_length_gen : big_length Crowbar.gen =
  Crowbar.(map [bool] (function true -> `N | false -> `Uint30))

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
  | UInt30_like_N : int -> int ty
  | Int31_like_Z : int * int -> int ty
  | Int32 : int32 ty
  | Int64 : int64 ty
  | Int16_le : int ty
  | UInt16_le : int ty
  | Int31_le : int ty
  | RangedInt_le : int * int -> int ty
  | Int32_le : int32 ty
  | Int64_le : int64 ty
  | Float : float ty
  | RangedFloat : float * float -> float ty
  | Bool : bool ty
  | StringPlain : string ty
  | FixedStringPlain : int -> string ty
  | BytesPlain : bytes ty
  | FixedBytesPlain : int -> bytes ty
  | StringHex : string ty
  | FixedStringHex : int -> string ty
  | BytesHex : bytes ty
  | FixedBytesHex : int -> bytes ty
  | BigstringHex : Bigstringaf.t ty
  | FixedBigstringHex : int -> Bigstringaf.t ty
  | Option : 'a ty -> 'a option ty
  | Result : 'a ty * 'b ty -> ('a, 'b) result ty
  | List : 'a ty -> 'a list ty
  | FixedList : int * 'a ty -> 'a list ty
  | Array : 'a ty -> 'a array ty
  | FixedArray : int * 'a ty -> 'a array ty
  | Dynamic_size : (big_length * 'a ty) -> 'a ty
  | Tup1 : 'a ty -> 'a ty
  | Tup2 : 'a ty * 'b ty -> ('a * 'b) ty
  | Tup3 : 'a ty * 'b ty * 'c ty -> ('a * 'b * 'c) ty
  | Tup4 : 'a ty * 'b ty * 'c ty * 'd ty -> ('a * 'b * 'c * 'd) ty
  | Union1 : 'a ty -> 'a ty
  | Union2 : 'a ty * 'b ty -> ('a, 'b) either ty
  | Matching2 : 'a ty * 'b ty -> ('a, 'b) either ty
  | Mu_matching : 'a ty -> 'a list ty
  | Mu_bigmatching : 'a ty * 'b ty -> ('a option * 'b) list ty
  | Mu_obj : 'a ty * 'b ty -> ('a, 'b) recursion_test_type ty
  | Check_size : (int * 'a ty) -> 'a ty
  | Dynamic_and_check_size : (int * 'a ty) -> 'a ty
  | StringEnum : int ty
  | Add_padding : 'a ty * int -> 'a ty
  | CompactMake : 'a compactty -> 'a ty
  | CompactMake16 : 'a compactty -> 'a ty

and _ compactty =
  | CmpctUnit : unit compactty
  | CmpctBool : bool compactty
  | CmpctOption : 'a compactty -> 'a option compactty
  | CmpctTup1 : 'a compactty -> 'a compactty
  | CmpctTup2 : 'a compactty * 'b compactty -> ('a * 'b) compactty
  | CmpctTup3 :
      'a compactty * 'b compactty * 'c compactty
      -> ('a * 'b * 'c) compactty
  | CmpctTup4 :
      'a compactty * 'b compactty * 'c compactty * 'd compactty
      -> ('a * 'b * 'c * 'd) compactty
  | CmpctTup7 :
      'a compactty
      * 'b compactty
      * 'c compactty
      * 'd compactty
      * 'e compactty
      * 'f compactty
      * 'g compactty
      -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) compactty
  | CmpctInt32 : int32 compactty
  | CmpctInt64 : int64 compactty
  | CmpctList : 'a ty -> 'a list compactty
  | CmpctUnion1 : 'a compactty -> 'a compactty
  | CmpctUnion2 : 'a compactty * 'b compactty -> ('a, 'b) either compactty
  | CmpctUnion4 :
      'a compactty * 'b compactty * 'c compactty * 'd compactty
      -> (('a, 'b) either, ('c, 'd) either) either compactty
  | CmpctUnion4Spill : 'a compactty * 'b compactty -> ('a, 'b) either compactty
  | CmpctOrInt32 : 'a ty -> (int32, 'a) Either.t compactty
  | CmpctPayload : 'a ty -> 'a compactty

(* TODO:
   | Tup[5-10] : ..
   | Obj
   | Conv
   | Delayed
*)

let rec pp_ty : type a. a ty Crowbar.printer =
 fun ppf ty ->
  match ty with
  | Null -> Crowbar.pp ppf "(null)"
  | Empty -> Crowbar.pp ppf "{}"
  | Unit -> Crowbar.pp ppf "()"
  | Constant s -> Crowbar.pp ppf "(constant:%S)" s
  | Int8 -> Crowbar.pp ppf "int8"
  | UInt8 -> Crowbar.pp ppf "uint8"
  | Int16 -> Crowbar.pp ppf "int16"
  | UInt16 -> Crowbar.pp ppf "uint16"
  | Int31 -> Crowbar.pp ppf "int31"
  | RangedInt (low, high) -> Crowbar.pp ppf "rangedint:[%d;%d]" low high
  | UInt30_like_N high -> Crowbar.pp ppf "uint30asn:[0;%d]" high
  | Int31_like_Z (low, high) -> Crowbar.pp ppf "int31asz:[%d;%d]" low high
  | Int32 -> Crowbar.pp ppf "int32"
  | Int64 -> Crowbar.pp ppf "int64"
  | Int16_le -> Crowbar.pp ppf "int16_le"
  | UInt16_le -> Crowbar.pp ppf "uint16_le"
  | Int31_le -> Crowbar.pp ppf "int31_le"
  | RangedInt_le (low, high) -> Crowbar.pp ppf "rangedint_le:[%d;%d]" low high
  | Int32_le -> Crowbar.pp ppf "int32_le"
  | Int64_le -> Crowbar.pp ppf "int64_le"
  | Float -> Crowbar.pp ppf "float"
  | RangedFloat (low, high) -> Crowbar.pp ppf "rangedfloat:[%g;%g]" low high
  | Bool -> Crowbar.pp ppf "bool"
  | StringPlain -> Crowbar.pp ppf "string"
  | BytesPlain -> Crowbar.pp ppf "bytes"
  | FixedStringPlain n -> Crowbar.pp ppf "fixedstring(%d)" n
  | FixedBytesPlain n -> Crowbar.pp ppf "fixedbytes(%d)" n
  | StringHex -> Crowbar.pp ppf "stringhex"
  | BytesHex -> Crowbar.pp ppf "byteshex"
  | FixedStringHex n -> Crowbar.pp ppf "fixedstringhex(%d)" n
  | FixedBytesHex n -> Crowbar.pp ppf "fixedbyteshex(%d)" n
  | BigstringHex -> Crowbar.pp ppf "bigstringhex"
  | FixedBigstringHex n -> Crowbar.pp ppf "fixedbigstringhex(%d)" n
  | Option ty -> Crowbar.pp ppf "option(%a)" pp_ty ty
  | Result (tya, tyb) -> Crowbar.pp ppf "result(%a,%a)" pp_ty tya pp_ty tyb
  | List ty -> Crowbar.pp ppf "list(%a)" pp_ty ty
  | FixedList (n, ty) -> Crowbar.pp ppf "fixedlist(%d)(%a)" n pp_ty ty
  | Array ty -> Crowbar.pp ppf "array(%a)" pp_ty ty
  | FixedArray (n, ty) -> Crowbar.pp ppf "fixedarray(%d)(%a)" n pp_ty ty
  | Dynamic_size (big_length, ty) ->
      Crowbar.pp
        ppf
        "dynamic_size(%s,%a)"
        (match big_length with `N -> "n" | `Uint30 -> "uint30")
        pp_ty
        ty
  | Tup1 ty -> Crowbar.pp ppf "tup1(%a)" pp_ty ty
  | Tup2 (tya, tyb) -> Crowbar.pp ppf "tup2(%a,%a)" pp_ty tya pp_ty tyb
  | Tup3 (tya, tyb, tyc) ->
      Crowbar.pp ppf "tup3(%a,%a,%a)" pp_ty tya pp_ty tyb pp_ty tyc
  | Tup4 (tya, tyb, tyc, tyd) ->
      Crowbar.pp ppf "tup4(%a,%a,%a,%a)" pp_ty tya pp_ty tyb pp_ty tyc pp_ty tyd
  | Union1 ty -> Crowbar.pp ppf "union1(%a)" pp_ty ty
  | Union2 (tya, tyb) -> Crowbar.pp ppf "union2(%a,%a)" pp_ty tya pp_ty tyb
  | Matching2 (tya, tyb) ->
      Crowbar.pp ppf "matching2(%a,%a)" pp_ty tya pp_ty tyb
  | Mu_matching ty -> Crowbar.pp ppf "mu_matching(%a)" pp_ty ty
  | Mu_bigmatching (tya, tyb) ->
      Crowbar.pp ppf "mu_bigmatching(%a,%a)" pp_ty tya pp_ty tyb
  | Mu_obj (tya, tyb) -> Crowbar.pp ppf "mu_obj(%a,%a)" pp_ty tya pp_ty tyb
  | Check_size (n, ty) -> Crowbar.pp ppf "check_size(%d,%a)" n pp_ty ty
  | Dynamic_and_check_size (n, ty) ->
      Crowbar.pp ppf "dynamic_and_check_size(%d,%a)" n pp_ty ty
  | StringEnum -> Crowbar.pp ppf "string_enum"
  | Add_padding (ty, n) -> Crowbar.pp ppf "add_padding(%a)(%d)" pp_ty ty n
  | CompactMake cty -> Crowbar.pp ppf "compact_make(%a)" pp_cty cty
  | CompactMake16 cty -> Crowbar.pp ppf "compact_make16(%a)" pp_cty cty

and pp_cty : type a. a compactty Crowbar.printer =
 fun ppf cty ->
  match cty with
  | CmpctUnit -> Crowbar.pp ppf "unit"
  | CmpctBool -> Crowbar.pp ppf "bool"
  | CmpctOption cty -> Crowbar.pp ppf "option(%a)" pp_cty cty
  | CmpctTup1 cty -> Crowbar.pp ppf "tup1(%a)" pp_cty cty
  | CmpctTup2 (ctya, ctyb) ->
      Crowbar.pp ppf "tup2(%a,%a)" pp_cty ctya pp_cty ctyb
  | CmpctTup3 (ctya, ctyb, ctyc) ->
      Crowbar.pp ppf "tup2(%a,%a,%a)" pp_cty ctya pp_cty ctyb pp_cty ctyc
  | CmpctTup4 (ctya, ctyb, ctyc, ctyd) ->
      Crowbar.pp
        ppf
        "tup2(%a,%a,%a,%a)"
        pp_cty
        ctya
        pp_cty
        ctyb
        pp_cty
        ctyc
        pp_cty
        ctyd
  | CmpctTup7 (ctya, ctyb, ctyc, ctyd, ctye, ctyf, ctyg) ->
      Crowbar.pp
        ppf
        "tup7(%a,%a,%a,%a,%a,%a,%a)"
        pp_cty
        ctya
        pp_cty
        ctyb
        pp_cty
        ctyc
        pp_cty
        ctyd
        pp_cty
        ctye
        pp_cty
        ctyf
        pp_cty
        ctyg
  | CmpctInt32 -> Crowbar.pp ppf "int32"
  | CmpctInt64 -> Crowbar.pp ppf "int32"
  | CmpctList ty -> Crowbar.pp ppf "list(%a)" pp_ty ty
  | CmpctUnion1 cty -> Crowbar.pp ppf "union1(%a)" pp_cty cty
  | CmpctUnion2 (ctya, ctyb) ->
      Crowbar.pp ppf "union2(%a,%a)" pp_cty ctya pp_cty ctyb
  | CmpctUnion4 (ctya, ctyb, ctyc, ctyd) ->
      Crowbar.pp
        ppf
        "union2(%a,%a,%a,%a)"
        pp_cty
        ctya
        pp_cty
        ctyb
        pp_cty
        ctyc
        pp_cty
        ctyd
  | CmpctUnion4Spill (ctya, ctyb) ->
      Crowbar.pp ppf "union4(%a,%a,void,void)" pp_cty ctya pp_cty ctyb
  | CmpctOrInt32 ty -> Crowbar.pp ppf "orint32(%a)" pp_ty ty
  | CmpctPayload ty -> Crowbar.pp ppf "payload(%a)" pp_ty ty

let dynamic_if_needed : 'a Data_encoding.t -> 'a Data_encoding.t =
 fun e ->
  match Data_encoding.classify e with
  | `Fixed 0 | `Variable -> Data_encoding.dynamic_size e
  | `Fixed _ | `Dynamic -> e

type any_ty = AnyTy : _ ty -> any_ty

let pp_any_ty : any_ty Crowbar.printer =
 fun ppf any_ty -> match any_ty with AnyTy ty -> pp_ty ppf ty

type any_compactty = AnyCTy : _ compactty -> any_compactty

let pp_any_compactty : any_compactty Crowbar.printer =
 fun ppf any_compactty -> match any_compactty with AnyCTy ty -> pp_cty ppf ty

let any_ty_ground_gen =
  let open Crowbar in
  let g : any_ty Crowbar.gen =
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
        map [uint30] (fun a -> AnyTy (UInt30_like_N a));
        map [int31; int31] (fun a b ->
            let low = min a b in
            let high = max a b in
            AnyTy (Int31_like_Z (low, high)));
        const @@ AnyTy Int32;
        const @@ AnyTy Int64;
        const @@ AnyTy Float;
        map [float; float] (fun a b ->
            if Float.is_nan a || Float.is_nan b then Crowbar.bad_test () ;
            if a = b then Crowbar.bad_test () ;
            let low = min a b in
            let high = max a b in
            AnyTy (RangedFloat (low, high)));
        const @@ AnyTy Bool;
        const @@ AnyTy StringPlain;
        const @@ AnyTy BytesPlain;
        const @@ AnyTy StringHex;
        const @@ AnyTy BytesHex;
        map [range ~min:1 10] (fun i -> AnyTy (FixedStringPlain i));
        map [range ~min:1 10] (fun i -> AnyTy (FixedBytesPlain i));
        map [range ~min:1 10] (fun i -> AnyTy (FixedStringHex i));
        map [range ~min:1 10] (fun i -> AnyTy (FixedBytesHex i));
        const @@ AnyTy StringEnum;
      ]
  in
  with_printer pp_any_ty g

let any_ty_fix g =
  let open Crowbar in
  let g : any_ty Crowbar.gen =
    choose
      [
        map [g] (fun (AnyTy ty) -> AnyTy (Option ty));
        map [g; g] (fun (AnyTy ty_ok) (AnyTy ty_error) ->
            AnyTy (Result (ty_ok, ty_error)));
        map [g] (fun (AnyTy ty_both) -> AnyTy (Result (ty_both, ty_both)));
        map [g] (fun (AnyTy ty) -> AnyTy (List ty));
        map [range ~min:1 4; g] (fun n (AnyTy ty) -> AnyTy (FixedList (n, ty)));
        map [g] (fun (AnyTy ty) -> AnyTy (Array ty));
        map [range ~min:1 4; g] (fun n (AnyTy ty) -> AnyTy (FixedArray (n, ty)));
        map [big_length_gen; g] (fun big_length (AnyTy ty) ->
            AnyTy (Dynamic_size (big_length, ty)));
        map [g] (fun (AnyTy ty) -> AnyTy (Tup1 ty));
        map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) -> AnyTy (Tup2 (ty_a, ty_b)));
        map [g] (fun (AnyTy ty_both) -> AnyTy (Tup2 (ty_both, ty_both)));
        map [g; g; g] (fun (AnyTy ty_a) (AnyTy ty_b) (AnyTy ty_c) ->
            AnyTy (Tup3 (ty_a, ty_b, ty_c)));
        map
          [g; g; g; g]
          (fun (AnyTy ty_a) (AnyTy ty_b) (AnyTy ty_c) (AnyTy ty_d) ->
            AnyTy (Tup4 (ty_a, ty_b, ty_c, ty_d)));
        map [g] (fun (AnyTy ty_a) -> AnyTy (Union1 ty_a));
        map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
            AnyTy (Union2 (ty_a, ty_b)));
        map [g] (fun (AnyTy ty_both) -> AnyTy (Union2 (ty_both, ty_both)));
        map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
            AnyTy (Matching2 (ty_a, ty_b)));
        map [g] (fun (AnyTy ty_both) -> AnyTy (Matching2 (ty_both, ty_both)));
        map [g] (fun (AnyTy ty) -> AnyTy (Mu_matching ty));
        map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
            AnyTy (Mu_bigmatching (ty_a, ty_b)));
        map [g; g] (fun (AnyTy ty_a) (AnyTy ty_b) ->
            AnyTy (Mu_obj (ty_a, ty_b)));
        map [uint30; g] (fun n (AnyTy ty) -> AnyTy (Check_size (n, ty)));
        map [uint30; g] (fun n (AnyTy ty) ->
            AnyTy (Dynamic_and_check_size (n, ty)));
        map
          [g; range ~min:1 10]
          (fun (AnyTy ty) n -> AnyTy (Add_padding (ty, n)));
      ]
  in
  with_printer pp_any_ty g

let any_ty_gen =
  let open Crowbar in
  let g : any_ty Crowbar.gen =
    fix (fun g -> choose [any_ty_ground_gen; any_ty_fix g])
  in
  with_printer pp_any_ty g

let any_compactty_gen =
  let open Crowbar in
  let shallow_ty =
    choose
      [
        any_ty_ground_gen;
        any_ty_fix any_ty_ground_gen;
        any_ty_fix (any_ty_fix any_ty_ground_gen);
      ]
  in
  let g : any_compactty Crowbar.gen =
    fix (fun g ->
        choose
          [
            const @@ AnyCTy CmpctUnit;
            const @@ AnyCTy CmpctInt32;
            const @@ AnyCTy CmpctInt64;
            const @@ AnyCTy CmpctBool;
            map [g] (fun (AnyCTy cty) -> AnyCTy (CmpctOption cty));
            map [shallow_ty] (fun (AnyTy ty) -> AnyCTy (CmpctList ty));
            map [g] (fun (AnyCTy cty) -> AnyCTy (CmpctTup1 cty));
            map [g; g] (fun (AnyCTy cty_a) (AnyCTy cty_b) ->
                AnyCTy (CmpctTup2 (cty_a, cty_b)));
            map [g] (fun (AnyCTy cty_both) ->
                AnyCTy (CmpctTup2 (cty_both, cty_both)));
            map [g; g; g] (fun (AnyCTy cty_a) (AnyCTy cty_b) (AnyCTy cty_c) ->
                AnyCTy (CmpctTup3 (cty_a, cty_b, cty_c)));
            map
              [g; g; g; g]
              (fun (AnyCTy cty_a) (AnyCTy cty_b) (AnyCTy cty_c) (AnyCTy cty_d)
              -> AnyCTy (CmpctTup4 (cty_a, cty_b, cty_c, cty_d)));
            map
              [g; g; g; g; g; g; g]
              (fun
                (AnyCTy cty_a)
                (AnyCTy cty_b)
                (AnyCTy cty_c)
                (AnyCTy cty_d)
                (AnyCTy cty_e)
                (AnyCTy cty_f)
                (AnyCTy cty_g)
              ->
                AnyCTy
                  (CmpctTup7 (cty_a, cty_b, cty_c, cty_d, cty_e, cty_f, cty_g)));
            map [g] (fun (AnyCTy cty_a) -> AnyCTy (CmpctUnion1 cty_a));
            map [g; g] (fun (AnyCTy cty_a) (AnyCTy cty_b) ->
                AnyCTy (CmpctUnion2 (cty_a, cty_b)));
            map [g] (fun (AnyCTy cty_both) ->
                AnyCTy (CmpctUnion2 (cty_both, cty_both)));
            map
              [g; g; g; g]
              (fun (AnyCTy cty_a) (AnyCTy cty_b) (AnyCTy cty_c) (AnyCTy cty_d)
              -> AnyCTy (CmpctUnion4 (cty_a, cty_b, cty_c, cty_d)));
            map [g; g] (fun (AnyCTy cty_a) (AnyCTy cty_b) ->
                AnyCTy (CmpctUnion4Spill (cty_a, cty_b)));
            map [shallow_ty] (fun (AnyTy ty) -> AnyCTy (CmpctOrInt32 ty));
            map [shallow_ty] (fun (AnyTy ty) -> AnyCTy (CmpctPayload ty));
          ])
  in
  let g =
    choose
      [
        map [g] (fun (AnyCTy cty) -> AnyTy (CompactMake cty));
        map [g] (fun (AnyCTy cty) -> AnyTy (CompactMake16 cty));
      ]
  in
  let g = choose [g; any_ty_fix g; any_ty_fix (any_ty_fix g)] in
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

let make_unit ty s encoding : unit full =
  (module struct
    type t = unit

    let ty = ty

    let eq _ _ = true

    let pp ppf () = Crowbar.pp ppf "%s" s

    let gen = Crowbar.const ()

    let encoding = encoding
  end)

let full_null : unit full = make_unit Null "null" Data_encoding.null

let full_empty : unit full = make_unit Empty "{}" Data_encoding.empty

let full_unit : unit full = make_unit Unit "()" Data_encoding.unit

let full_constant s : unit full =
  make_unit (Constant s) ("constant:" ^ s) (Data_encoding.constant s)

let make_int ty gen encoding : int full =
  (module struct
    type t = int

    let ty = ty

    let eq = Int.equal

    let pp ppf v = Crowbar.pp ppf "%d" v

    let gen = gen

    let encoding = encoding
  end)

let full_int8 : int full = make_int Int8 Crowbar.int8 Data_encoding.int8

let full_uint8 : int full = make_int UInt8 Crowbar.uint8 Data_encoding.uint8

let full_int16 : int full = make_int Int16 Crowbar.int16 Data_encoding.int16

let full_uint16 : int full = make_int UInt16 Crowbar.uint16 Data_encoding.uint16

let full_int31 : int full = make_int Int31 int31 Data_encoding.int31

let full_rangedint low high : int full =
  assert (low < high) ;
  make_int
    (RangedInt (low, high))
    Crowbar.(
      if low < 0 && high > 0 then
        (* special casing this avoids overflow on 32bit machines *)
        choose [range high; map [range (-low)] (fun v -> -v)]
      else map [range (high - low)] (fun v -> v + low))
    (Data_encoding.ranged_int low high)

let full_uint30_like_n high : int full =
  make_int
    (UInt30_like_N high)
    (Crowbar.range (high + 1))
    (Data_encoding.uint_like_n ~max_value:high ())

let full_int31_like_z low high : int full =
  make_int
    (Int31_like_Z (low, high))
    Crowbar.(
      if low < 0 && high > 0 then
        (* special casing this avoids overflow on 32bit machines *)
        choose [range (high + 1); map [range (-(low + 1))] (fun v -> -v)]
      else map [range (high - low + 1)] (fun v -> v + low))
    (Data_encoding.int_like_z ~min_value:low ~max_value:high ())

let full_int32 : int32 full =
  (module struct
    type t = int32

    let ty = Int32

    let eq = Int32.equal

    let pp ppf v = Crowbar.pp ppf "%ld" v

    let gen = Crowbar.int32

    let encoding = Data_encoding.int32
  end)

let full_int64 : int64 full =
  (module struct
    type t = int64

    let ty = Int64

    let eq = Int64.equal

    let pp ppf v = Crowbar.pp ppf "%Ld" v

    let gen = Crowbar.int64

    let encoding = Data_encoding.int64
  end)

let full_rangedint_le low high : int full =
  assert (low < high) ;
  make_int
    (RangedInt (low, high))
    Crowbar.(
      if low < 0 && high > 0 then
        (* special casing this avoids overflow on 32bit machines *)
        choose [range high; map [range (-low)] (fun v -> -v)]
      else map [range (high - low)] (fun v -> v + low))
    (Data_encoding.Little_endian.ranged_int low high)

let full_int32_le : int32 full =
  (module struct
    type t = int32

    let ty = Int32

    let eq = Int32.equal

    let pp ppf v = Crowbar.pp ppf "%ld" v

    let gen = Crowbar.int32

    let encoding = Data_encoding.Little_endian.int32
  end)

let full_int64_le : int64 full =
  (module struct
    type t = int64

    let ty = Int64

    let eq = Int64.equal

    let pp ppf v = Crowbar.pp ppf "%Ld" v

    let gen = Crowbar.int64

    let encoding = Data_encoding.Little_endian.int64
  end)

let full_int16_le : int full =
  make_int Int16 Crowbar.int16 Data_encoding.Little_endian.int16

let full_uint16_le : int full =
  make_int UInt16 Crowbar.uint16 Data_encoding.Little_endian.uint16

let full_int31_le : int full =
  make_int Int31 int31 Data_encoding.Little_endian.int31

let make_float ty gen encoding : float full =
  (module struct
    type t = float

    let ty = ty

    let eq = Float.equal

    let pp ppf v = Crowbar.pp ppf "%g" v

    let gen = gen

    let encoding = encoding
  end)

let full_float : float full = make_float Float Crowbar.float Data_encoding.float

let full_rangedfloat low high : float full =
  assert (low < high) ;
  make_float
    (RangedFloat (low, high))
    Crowbar.(
      map [float] (fun f ->
          if Float.is_nan f then Crowbar.bad_test () ;
          if f < low || f > high then Crowbar.bad_test () ;
          f))
    (Data_encoding.ranged_float low high)

let full_bool : bool full =
  (module struct
    type t = bool

    let ty = Bool

    let eq = Bool.equal

    let pp ppf v = Crowbar.pp ppf "%b" v

    let gen = Crowbar.bool

    let encoding = Data_encoding.bool
  end)

let make_string ty gen encoding : string full =
  (module struct
    type t = string

    let ty = ty

    let eq = String.equal

    let pp ppf v = Crowbar.pp ppf "%S" v

    let gen = gen

    let encoding = encoding
  end)

let full_string_plain : string full =
  make_string StringPlain string (Data_encoding.string' Plain)

let full_string_hex : string full =
  make_string StringHex string (Data_encoding.string' Hex)

let full_fixed_string_plain n : string full =
  make_string
    (FixedStringPlain n)
    (Crowbar.bytes_fixed n)
    (Data_encoding.Fixed.string' Plain n)

let full_fixed_string_hex n : string full =
  make_string
    (FixedStringHex n)
    (Crowbar.bytes_fixed n)
    (Data_encoding.Fixed.string' Hex n)

let make_bytes ty gen encoding : bytes full =
  (module struct
    type t = bytes

    let ty = ty

    let eq = Bytes.equal

    let pp ppf v = Crowbar.pp ppf "%S" (Bytes.unsafe_to_string v)

    let gen = gen

    let encoding = encoding
  end)

let full_bytes_hex : bytes full =
  make_bytes BytesHex bytes (Data_encoding.bytes' Hex)

let full_bytes_plain : bytes full =
  make_bytes BytesPlain bytes (Data_encoding.bytes' Plain)

let full_fixed_bytes_hex n : bytes full =
  make_bytes
    (FixedBytesHex n)
    Crowbar.(map [bytes_fixed n] Bytes.unsafe_of_string)
    (Data_encoding.Fixed.bytes' Hex n)

let full_fixed_bytes_plain n : bytes full =
  make_bytes
    (FixedBytesPlain n)
    Crowbar.(map [bytes_fixed n] Bytes.unsafe_of_string)
    (Data_encoding.Fixed.bytes' Plain n)

let full_bigstring : Bigstringaf.t full =
  (module struct
    type t = Bigstringaf.t

    let ty = BigstringHex

    let eq a b =
      Bigstringaf.length a = Bigstringaf.length b
      && Bigstringaf.memcmp a 0 b 0 (Bigstringaf.length a) = 0

    let pp ppf v = Crowbar.pp ppf "%S" (Bigstringaf.to_string v)

    let gen =
      Crowbar.map [string] (fun s ->
          Bigstringaf.of_string ~off:0 ~len:(String.length s) s)

    let encoding = Data_encoding.bigstring ()
  end)

let full_fixed_bigstring n : Bigstringaf.t full =
  (module struct
    type t = Bigstringaf.t

    let ty = FixedBigstringHex n

    let eq a b =
      Bigstringaf.length a = n
      && Bigstringaf.length b = n
      && Bigstringaf.memcmp a 0 b 0 n = 0

    let pp ppf v = Crowbar.pp ppf "%S" (Bigstringaf.to_string v)

    let gen =
      Crowbar.map
        [Crowbar.bytes_fixed n]
        (fun s -> Bigstringaf.of_string ~off:0 ~len:(String.length s) s)

    let encoding = Data_encoding.Fixed.bigstring n
  end)

let full_option : type a. a full -> a option full =
 fun full ->
  let module Full = (val full) in
  if Data_encoding__Encoding.is_nullable Full.encoding then Crowbar.bad_test ()
  else
    (module struct
      type t = Full.t option

      let ty = Option Full.ty

      let eq a b =
        match (a, b) with
        | None, None -> true
        | Some a, Some b -> Full.eq a b
        | Some _, None | None, Some _ -> false

      let pp ppf = function
        | None -> Crowbar.pp ppf "none"
        | Some p -> Crowbar.pp ppf "some(%a)" Full.pp p

      let gen = Crowbar.option Full.gen

      let encoding = Data_encoding.option Full.encoding
    end)

let full_list : type a. a full -> a list full =
 fun full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t list

    let ty = List Full.ty

    let eq xs ys = List.compare_lengths xs ys = 0 && List.for_all2 Full.eq xs ys

    let pp ppf v =
      Crowbar.pp
        ppf
        "list(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        v

    let gen = Crowbar.list Full.gen

    let encoding = Data_encoding.(list (dynamic_if_needed Full.encoding))
  end)

let list_n_gen n gen =
  (* NOTE: always called with 1-4 *)
  let open Crowbar in
  match n with
  | 1 -> map [gen] (fun e -> [e])
  | 2 -> map [gen; gen] (fun e e' -> [e; e'])
  | 3 -> map [gen; gen; gen] (fun e e' e'' -> [e; e'; e''])
  | 4 -> map [gen; gen; gen; gen] (fun e e' e'' e''' -> [e; e'; e''; e'''])
  | _ -> assert false

let full_fixed_list : type a. int -> a full -> a list full =
 fun fix full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t list

    let ty = FixedList (fix, Full.ty)

    let eq xs ys = List.compare_lengths xs ys = 0 && List.for_all2 Full.eq xs ys

    let pp ppf v =
      Crowbar.pp
        ppf
        "fixedlist(%d)(%a)"
        fix
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        v

    let gen = list_n_gen fix Full.gen

    let encoding =
      Data_encoding.(Fixed.list fix (dynamic_if_needed Full.encoding))
  end)

let full_array : type a. a full -> a array full =
 fun full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t array

    let ty = Array Full.ty

    let eq xs ys =
      Array.length xs = Array.length ys
      && Array.for_all Fun.id (Array.map2 Full.eq xs ys)

    let pp ppf v =
      Crowbar.pp
        ppf
        "array(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        (Array.to_list v)

    let gen = Crowbar.(map [list Full.gen] Array.of_list)

    let encoding = Data_encoding.(array (dynamic_if_needed Full.encoding))
  end)

let full_fixed_array : type a. int -> a full -> a array full =
 fun fixen full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t array

    let ty = FixedArray (fixen, Full.ty)

    let eq xs ys =
      Array.length xs = Array.length ys
      && Array.for_all Fun.id (Array.map2 Full.eq xs ys)

    let pp ppf v =
      Crowbar.pp
        ppf
        "fixedarray(%d)(%a)"
        fixen
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        (Array.to_list v)

    let gen =
      let open Crowbar in
      map [list_n_gen fixen Full.gen] Array.of_list

    let encoding =
      Data_encoding.(Fixed.array fixen (dynamic_if_needed Full.encoding))
  end)

let full_dynamic_size : type a. big_length -> a full -> a full =
 fun big_length full ->
  let module Full = (val full) in
  (module struct
    include Full

    let ty = Dynamic_size (big_length, ty)

    let encoding =
      Data_encoding.dynamic_size
        ~kind:(big_length :> Data_encoding__Binary_size.length)
        encoding
  end)

let full_tup1 : type a. a full -> a full =
 fun full ->
  let module Full = (val full) in
  (module struct
    include Full

    let ty = Tup1 Full.ty

    let pp ppf v = Crowbar.pp ppf "tup1(%a)" Full.pp v

    let encoding = Data_encoding.tup1 Full.encoding
  end)

let full_tup2 : type a b. a full -> b full -> (a * b) full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = Fulla.t * Fullb.t

    let ty = Tup2 (Fulla.ty, Fullb.ty)

    let eq (a, b) (u, v) = Fulla.eq a u && Fullb.eq b v

    let pp ppf (a, b) = Crowbar.pp ppf "tup2(%a,%a)" Fulla.pp a Fullb.pp b

    let gen = Crowbar.map [Fulla.gen; Fullb.gen] (fun a b -> (a, b))

    let encoding =
      Data_encoding.(tup2 (dynamic_if_needed Fulla.encoding) Fullb.encoding)
  end)

let full_tup3 : type a b c. a full -> b full -> c full -> (a * b * c) full =
 fun fulla fullb fullc ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  let module Fullc = (val fullc) in
  (module struct
    type t = Fulla.t * Fullb.t * Fullc.t

    let ty = Tup3 (Fulla.ty, Fullb.ty, Fullc.ty)

    let eq (a, b, c) (u, v, w) = Fulla.eq a u && Fullb.eq b v && Fullc.eq c w

    let pp ppf (a, b, c) =
      Crowbar.pp ppf "tup3(%a,%a,%a)" Fulla.pp a Fullb.pp b Fullc.pp c

    let gen =
      Crowbar.map [Fulla.gen; Fullb.gen; Fullc.gen] (fun a b c -> (a, b, c))

    let encoding =
      Data_encoding.(
        tup3
          (dynamic_if_needed Fulla.encoding)
          (dynamic_if_needed Fullb.encoding)
          Fullc.encoding)
  end)

let full_tup4 :
    type a b c d. a full -> b full -> c full -> d full -> (a * b * c * d) full =
 fun fulla fullb fullc fulld ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  let module Fullc = (val fullc) in
  let module Fulld = (val fulld) in
  (module struct
    type t = Fulla.t * Fullb.t * Fullc.t * Fulld.t

    let ty = Tup4 (Fulla.ty, Fullb.ty, Fullc.ty, Fulld.ty)

    let eq (a, b, c, d) (u, v, w, z) =
      Fulla.eq a u && Fullb.eq b v && Fullc.eq c w && Fulld.eq d z

    let pp ppf (a, b, c, d) =
      Crowbar.pp
        ppf
        "tup4(%a,%a,%a,%a)"
        Fulla.pp
        a
        Fullb.pp
        b
        Fullc.pp
        c
        Fulld.pp
        d

    let gen =
      Crowbar.map [Fulla.gen; Fullb.gen; Fullc.gen; Fulld.gen] (fun a b c d ->
          (a, b, c, d))

    let encoding =
      Data_encoding.(
        tup4
          (dynamic_if_needed Fulla.encoding)
          (dynamic_if_needed Fullb.encoding)
          (dynamic_if_needed Fullc.encoding)
          Fulld.encoding)
  end)

let full_result : type a b. a full -> b full -> (a, b) result full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t, Fullb.t) result

    let ty = Result (Fulla.ty, Fullb.ty)

    let eq = Stdlib.Result.equal ~ok:Fulla.eq ~error:Fullb.eq

    let gen = Crowbar.result Fulla.gen Fullb.gen

    let encoding = Data_encoding.result Fulla.encoding Fullb.encoding

    let pp ppf = function
      | Ok a -> Crowbar.pp ppf "ok(%a)" Fulla.pp a
      | Error b -> Crowbar.pp ppf "error(%a)" Fullb.pp b
  end)

let full_union1 : type a. a full -> a full =
 fun fulla ->
  let module Fulla = (val fulla) in
  (module struct
    type t = Fulla.t

    let ty = Union1 Fulla.ty

    let eq = Fulla.eq

    let a_ding =
      let open Data_encoding in
      obj1 (req "OnlyThisOneOnly" Fulla.encoding)

    let encoding =
      let open Data_encoding in
      union
        [case ~title:"A" (Tag 0) a_ding (function v -> Some v) (fun v -> v)]

    let gen = Fulla.gen

    let pp ppf = function
      | v1 -> Crowbar.pp ppf "@[<hv 1>(Union1 %a)@]" Fulla.pp v1
  end)

let full_union2 : type a b. a full -> b full -> (a, b) either full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t, Fullb.t) either

    let ty = Union2 (Fulla.ty, Fullb.ty)

    let eq x y =
      match (x, y) with
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
      map [bool; Fulla.gen; Fullb.gen] (fun choice a b ->
          if choice then Left a else Right b)

    let pp ppf = function
      | Left v1 -> Crowbar.pp ppf "@[<hv 1>(A %a)@]" Fulla.pp v1
      | Right v2 -> Crowbar.pp ppf "@[<hv 1>(B %a)@]" Fullb.pp v2
  end)

let full_matching2 : type a b. a full -> b full -> (a, b) either full =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t, Fullb.t) either

    let ty = Matching2 (Fulla.ty, Fullb.ty)

    let eq x y =
      match (x, y) with
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
        (function
          | Left v -> matched 0 a_ding v | Right v -> matched 1 b_ding v)
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
      map [bool; Fulla.gen; Fullb.gen] (fun choice a b ->
          if choice then Left a else Right b)

    let pp ppf = function
      | Left v1 -> Crowbar.pp ppf "@[<hv 1>(A %a)@]" Fulla.pp v1
      | Right v2 -> Crowbar.pp ppf "@[<hv 1>(B %a)@]" Fullb.pp v2
  end)

let fresh_name =
  let r = ref 0 in
  fun () ->
    incr r ;
    "mun" ^ string_of_int !r

let full_mu_matching : type a. a full -> a list full =
 fun fulla ->
  let module Fulla = (val fulla) in
  (module struct
    type t = Fulla.t list

    let ty = Mu_matching Fulla.ty

    let rec eq x y =
      match (x, y) with
      | [], [] -> true
      | x :: xs, y :: ys -> Fulla.eq x y && eq xs ys
      | _ :: _, [] | [], _ :: _ -> false

    let encoding =
      let open Data_encoding in
      mu (fresh_name ()) @@ fun self ->
      matching
        (function
          | [] -> matched 0 (obj1 (req "nil" unit)) ()
          | x :: xs ->
              matched
                2
                (obj2 (req "head" Fulla.encoding) (req "tail" self))
                (x, xs))
        [
          case
            ~title:"nil"
            (Tag 0)
            (obj1 (req "nil" unit))
            (function [] -> Some () | _ :: _ -> None)
            (fun () -> []);
          case
            ~title:"cons"
            (Tag 2)
            (obj2 (req "head" Fulla.encoding) (req "tail" self))
            (function [] -> None | x :: xs -> Some (x, xs))
            (fun (x, xs) -> x :: xs);
        ]

    let gen = Crowbar.list Fulla.gen

    let pp ppf v =
      Crowbar.pp
        ppf
        "list(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Fulla.pp)
        v
  end)

let full_mu_bigmatching : type a b. a full -> b full -> (a option * b) list full
    =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t option * Fullb.t) list

    let ty = Mu_bigmatching (Fulla.ty, Fullb.ty)

    let rec eq x y =
      match (x, y) with
      | [], [] -> true
      | (xa, xb) :: xs, (ya, yb) :: ys ->
          Option.equal Fulla.eq xa ya && Fullb.eq xb yb && eq xs ys
      | _ :: _, [] | [], _ :: _ -> false

    let encoding =
      let open Data_encoding in
      mu (fresh_name ()) @@ fun self ->
      matching
        ~tag_size:`Uint16
        (function
          | [] -> matched 0 (obj1 (req "nil" unit)) ()
          | (ao, b) :: xs ->
              matched
                3
                (obj3
                   (opt "heada" Fulla.encoding)
                   (req "headb" Fullb.encoding)
                   (req "tail" self))
                (ao, b, xs))
        [
          case
            ~title:"nil"
            (Tag 0)
            (obj1 (req "nil" unit))
            (function [] -> Some () | _ :: _ -> None)
            (fun () -> []);
          case
            ~title:"cons"
            (Tag 3)
            (obj3
               (opt "heada" Fulla.encoding)
               (req "headb" Fullb.encoding)
               (req "tail" self))
            (function [] -> None | (xa, xb) :: xs -> Some (xa, xb, xs))
            (fun (xa, xb, xs) -> (xa, xb) :: xs);
        ]

    let gen =
      Crowbar.list
      @@ Crowbar.map [Crowbar.option Fulla.gen; Fullb.gen] (fun a b -> (a, b))

    let pp ppf v =
      Crowbar.pp
        ppf
        "list(%a)"
        Format.(
          pp_print_list
            ~pp_sep:(fun fmt () -> pp_print_char fmt ',')
            (fun ppf (a, b) ->
              Format.fprintf
                ppf
                "(%a,%a)"
                (pp_print_option Fulla.pp)
                a
                Fullb.pp
                b))
        v
  end)

let full_mu_obj : type a b. a full -> b full -> (a, b) recursion_test_type full
    =
 fun fulla fullb ->
  let module Fulla = (val fulla) in
  let module Fullb = (val fullb) in
  (module struct
    type t = (Fulla.t, Fullb.t) recursion_test_type

    let ty = Mu_obj (Fulla.ty, Fullb.ty)

    let rec eq (C (xa, xb, xs)) (C (ya, yb, ys)) =
      Fulla.eq xa ya && Fullb.eq xb yb && Option.equal eq xs ys

    let encoding =
      let open Data_encoding in
      mu (fresh_name ()) @@ fun self ->
      conv
        (fun (C (xa, xb, xs)) -> (xa, xb, xs))
        (fun (xa, xb, xs) -> C (xa, xb, xs))
      @@ obj3
           (req "a" Fulla.encoding)
           (req "b" Fullb.encoding)
           (opt "more" self)

    let gen =
      Crowbar.fix (fun self ->
          Crowbar.map
            [Fulla.gen; Fullb.gen; Crowbar.option self]
            (fun a b xs -> C (a, b, xs)))

    let rec pp ppf (C (xa, xb, ot)) =
      Crowbar.pp
        ppf
        "C(%a,%a,%a)"
        Fulla.pp
        xa
        Fullb.pp
        xb
        (Format.pp_print_option pp)
        ot
  end)

let full_check_size : type a. int -> a full -> a full =
 fun maxsize full ->
  let module Full = (val full) in
  match Data_encoding.Binary.maximum_length Full.encoding with
  | None -> Crowbar.bad_test ()
  | Some size ->
      let size = max maxsize size in
      (module struct
        include Full

        let encoding = Data_encoding.check_size size Full.encoding
      end)

let full_dyn_and_check_size : type a. int -> a full -> a full =
 fun size full ->
  let module Full = (val full) in
  match Data_encoding.Binary.maximum_length Full.encoding with
  | None -> Crowbar.bad_test ()
  | Some inner_size ->
      let size = max size inner_size in
      (module struct
        include Full

        let encoding =
          Data_encoding.dynamic_size
            (Data_encoding.check_size size Full.encoding)
      end)

let full_string_enum : int full =
  make_int
    StringEnum
    (Crowbar.range 8)
    (Data_encoding.string_enum
       [
         ("zero", 0);
         ("never", 123234);
         ("one", 1);
         ("two", 2);
         ("three", 3);
         ("four", 4);
         ("also-never", 1232234);
         ("five", 5);
         ("six", 6);
         ("seven", 7);
       ])

let full_add_padding : type a. a full -> int -> a full =
 fun full n ->
  let module Full = (val full) in
  match Data_encoding.classify Full.encoding with
  | `Variable | `Dynamic -> Crowbar.bad_test ()
  | `Fixed _ ->
      (module struct
        include Full

        let ty = Add_padding (Full.ty, n)

        let encoding = Data_encoding.Fixed.add_padding Full.encoding n

        let pp ppf x = Crowbar.pp ppf "add_padding(%a)(%d)" Full.pp x n
      end)

module type COMPACTFULL = sig
  type t

  val ty : t compactty

  val eq : t -> t -> bool

  val pp : t Crowbar.printer

  val gen : t Crowbar.gen

  val encoding : t Data_encoding.Compact.t
end

type 'a compactfull = (module COMPACTFULL with type t = 'a)

let full_make_compact : type a. a compactfull -> a full =
 fun compactfull ->
  try
    let module CompactFull = (val compactfull) in
    (module struct
      type t = CompactFull.t

      let ty = CompactMake CompactFull.ty

      let eq = CompactFull.eq

      let pp = CompactFull.pp

      let gen = CompactFull.gen

      let encoding =
        Data_encoding.Compact.make ~tag_size:`Uint8 CompactFull.encoding
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let full_make_compact16 : type a. a compactfull -> a full =
 fun compactfull ->
  try
    let module CompactFull = (val compactfull) in
    (module struct
      type t = CompactFull.t

      let ty = CompactMake16 CompactFull.ty

      let eq = CompactFull.eq

      let pp = CompactFull.pp

      let gen = CompactFull.gen

      let encoding =
        Data_encoding.Compact.make ~tag_size:`Uint16 CompactFull.encoding
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_payload : type a. a full -> a compactfull =
 fun full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t

    let ty = CmpctPayload Full.ty

    let eq = Full.eq

    let pp = Full.pp

    let gen = Full.gen

    let encoding = Data_encoding.Compact.payload Full.encoding
  end)

let compactfull_unit : unit compactfull =
  (module struct
    type t = unit

    let ty = CmpctUnit

    let eq () () = true

    let pp ppf () = Crowbar.pp ppf "()"

    let gen = Crowbar.const ()

    let encoding = Data_encoding.Compact.unit
  end)

let compactfull_bool : bool compactfull =
  (module struct
    type t = bool

    let ty = CmpctBool

    let eq = Bool.equal

    let pp ppf v = Crowbar.pp ppf "%b" v

    let gen = Crowbar.bool

    let encoding = Data_encoding.Compact.bool
  end)

let compactfull_option : type a. a compactfull -> a option compactfull =
 fun compactfull ->
  let module CompactFull = (val compactfull) in
  match
    Data_encoding__Encoding.is_nullable
      (Data_encoding.Compact.make CompactFull.encoding)
  with
  | exception Invalid_argument _ -> Crowbar.bad_test ()
  | true -> Crowbar.bad_test ()
  | false ->
      (module struct
        type t = CompactFull.t option

        let ty = CmpctOption CompactFull.ty

        let eq a b =
          match (a, b) with
          | None, None -> true
          | Some a, Some b -> CompactFull.eq a b
          | Some _, None | None, Some _ -> false

        let pp ppf = function
          | None -> Crowbar.pp ppf "none"
          | Some p -> Crowbar.pp ppf "some(%a)" CompactFull.pp p

        let gen = Crowbar.option CompactFull.gen

        let encoding = Data_encoding.Compact.option CompactFull.encoding
      end)

let compactfull_tup1 : type a. a compactfull -> a compactfull =
 fun compactfull ->
  let module CompactFull = (val compactfull) in
  (module struct
    include CompactFull

    let ty = CmpctTup1 CompactFull.ty

    let pp ppf v = Crowbar.pp ppf "tup1(%a)" CompactFull.pp v

    let encoding = Data_encoding.Compact.tup1 CompactFull.encoding
  end)

let compactfull_tup2 :
    type a b. a compactfull -> b compactfull -> (a * b) compactfull =
 fun compactfulla compactfullb ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    (module struct
      type t = CompactFulla.t * CompactFullb.t

      let ty = CmpctTup2 (CompactFulla.ty, CompactFullb.ty)

      let eq (a, b) (u, v) = CompactFulla.eq a u && CompactFullb.eq b v

      let pp ppf (a, b) =
        Crowbar.pp ppf "tup2(%a,%a)" CompactFulla.pp a CompactFullb.pp b

      let gen =
        Crowbar.map [CompactFulla.gen; CompactFullb.gen] (fun a b -> (a, b))

      let encoding =
        Data_encoding.Compact.(tup2 CompactFulla.encoding CompactFullb.encoding)
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_tup3 :
    type a b c.
    a compactfull -> b compactfull -> c compactfull -> (a * b * c) compactfull =
 fun compactfulla compactfullb compactfullc ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    let module CompactFullc = (val compactfullc) in
    (module struct
      type t = CompactFulla.t * CompactFullb.t * CompactFullc.t

      let ty = CmpctTup3 (CompactFulla.ty, CompactFullb.ty, CompactFullc.ty)

      let eq (a, b, c) (u, v, w) =
        CompactFulla.eq a u && CompactFullb.eq b v && CompactFullc.eq c w

      let pp ppf (a, b, c) =
        Crowbar.pp
          ppf
          "tup3(%a,%a,%a)"
          CompactFulla.pp
          a
          CompactFullb.pp
          b
          CompactFullc.pp
          c

      let gen =
        Crowbar.map
          [CompactFulla.gen; CompactFullb.gen; CompactFullc.gen]
          (fun a b c -> (a, b, c))

      let encoding =
        Data_encoding.Compact.(
          tup3 CompactFulla.encoding CompactFullb.encoding CompactFullc.encoding)
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_tup4 :
    type a b c d.
    a compactfull ->
    b compactfull ->
    c compactfull ->
    d compactfull ->
    (a * b * c * d) compactfull =
 fun compactfulla compactfullb compactfullc compactfulld ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    let module CompactFullc = (val compactfullc) in
    let module CompactFulld = (val compactfulld) in
    (module struct
      type t = CompactFulla.t * CompactFullb.t * CompactFullc.t * CompactFulld.t

      let ty =
        CmpctTup4
          (CompactFulla.ty, CompactFullb.ty, CompactFullc.ty, CompactFulld.ty)

      let eq (a, b, c, d) (u, v, w, z) =
        CompactFulla.eq a u && CompactFullb.eq b v && CompactFullc.eq c w
        && CompactFulld.eq d z

      let pp ppf (a, b, c, d) =
        Crowbar.pp
          ppf
          "tup4(%a,%a,%a,%a)"
          CompactFulla.pp
          a
          CompactFullb.pp
          b
          CompactFullc.pp
          c
          CompactFulld.pp
          d

      let gen =
        Crowbar.map
          [
            CompactFulla.gen;
            CompactFullb.gen;
            CompactFullc.gen;
            CompactFulld.gen;
          ]
          (fun a b c d -> (a, b, c, d))

      let encoding =
        Data_encoding.Compact.(
          tup4
            CompactFulla.encoding
            CompactFullb.encoding
            CompactFullc.encoding
            CompactFulld.encoding)
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_tup7 :
    type a b c d e f g.
    a compactfull ->
    b compactfull ->
    c compactfull ->
    d compactfull ->
    e compactfull ->
    f compactfull ->
    g compactfull ->
    (a * b * c * d * e * f * g) compactfull =
 fun compactfulla
     compactfullb
     compactfullc
     compactfulld
     compactfulle
     compactfullf
     compactfullg ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    let module CompactFullc = (val compactfullc) in
    let module CompactFulld = (val compactfulld) in
    let module CompactFulle = (val compactfulle) in
    let module CompactFullf = (val compactfullf) in
    let module CompactFullg = (val compactfullg) in
    (module struct
      type t =
        CompactFulla.t
        * CompactFullb.t
        * CompactFullc.t
        * CompactFulld.t
        * CompactFulle.t
        * CompactFullf.t
        * CompactFullg.t

      let ty =
        CmpctTup7
          ( CompactFulla.ty,
            CompactFullb.ty,
            CompactFullc.ty,
            CompactFulld.ty,
            CompactFulle.ty,
            CompactFullf.ty,
            CompactFullg.ty )

      let eq (a, b, c, d, e, f, g) (u, v, w, z, y, k, p) =
        CompactFulla.eq a u && CompactFullb.eq b v && CompactFullc.eq c w
        && CompactFulld.eq d z && CompactFulle.eq e y && CompactFullf.eq f k
        && CompactFullg.eq g p

      let pp ppf (a, b, c, d, e, f, g) =
        Crowbar.pp
          ppf
          "tup7(%a,%a,%a,%a,%a,%a,%a)"
          CompactFulla.pp
          a
          CompactFullb.pp
          b
          CompactFullc.pp
          c
          CompactFulld.pp
          d
          CompactFulle.pp
          e
          CompactFullf.pp
          f
          CompactFullg.pp
          g

      let gen =
        Crowbar.map
          [
            CompactFulla.gen;
            CompactFullb.gen;
            CompactFullc.gen;
            CompactFulld.gen;
            CompactFulle.gen;
            CompactFullf.gen;
            CompactFullg.gen;
          ]
          (fun a b c d e f g -> (a, b, c, d, e, f, g))

      let encoding =
        Data_encoding.Compact.(
          tup7
            CompactFulla.encoding
            CompactFullb.encoding
            CompactFullc.encoding
            CompactFulld.encoding
            CompactFulle.encoding
            CompactFullf.encoding
            CompactFullg.encoding)
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_int32 : int32 compactfull =
  (module struct
    type t = int32

    let ty = CmpctInt32

    let eq = Int32.equal

    let pp ppf v = Crowbar.pp ppf "%ld" v

    let gen =
      let open Crowbar in
      choose
        [
          const 0x0l;
          const 0x1l;
          const 0xFl;
          const 0xFFl;
          const 0xFFFl;
          const 0xFFFFl;
          const 0xFFFFFl;
          const 0xFFFFFFl;
          const 0xFFFFFFFl;
          const 0xFFFFFFFFl;
          const (Int32.neg 0x1l);
          const (Int32.neg 0xFl);
          const (Int32.neg 0xFFl);
          const (Int32.neg 0xFFFl);
          const (Int32.neg 0xFFFFl);
          const (Int32.neg 0xFFFFFl);
          const (Int32.neg 0xFFFFFFl);
          const (Int32.neg 0xFFFFFFFl);
          const (Int32.neg 0xFFFFFFFFl);
          int32;
        ]

    let encoding = Data_encoding.Compact.int32
  end)

let compactfull_int64 : int64 compactfull =
  (module struct
    type t = int64

    let ty = CmpctInt64

    let eq = Int64.equal

    let pp ppf v = Crowbar.pp ppf "%Ld" v

    let gen =
      let open Crowbar in
      choose
        [
          const 0x0L;
          const 0x1L;
          const 0xFL;
          const 0xFFL;
          const 0xFFFL;
          const 0xFFFFL;
          const 0xFFFFFFL;
          const 0xFFFFFFFFL;
          const 0xFFFFFFFFFFFFL;
          const 0xFFFFFFFFFFFFFFFFL;
          const (Int64.neg 0x1L);
          const (Int64.neg 0xFL);
          const (Int64.neg 0xFFL);
          const (Int64.neg 0xFFFL);
          const (Int64.neg 0xFFFFL);
          const (Int64.neg 0xFFFFFFL);
          const (Int64.neg 0xFFFFFFFFL);
          const (Int64.neg 0xFFFFFFFFFFFFL);
          const (Int64.neg 0xFFFFFFFFFFFFFFFFL);
          int64;
        ]

    let encoding = Data_encoding.Compact.int64
  end)

let compactfull_list : type a. a full -> a list compactfull =
 fun full ->
  let module Full = (val full) in
  (module struct
    type t = Full.t list

    let ty = CmpctList Full.ty

    let eq xs ys = List.compare_lengths xs ys = 0 && List.for_all2 Full.eq xs ys

    let pp ppf v =
      Crowbar.pp
        ppf
        "list(%a)"
        Format.(
          pp_print_list ~pp_sep:(fun fmt () -> pp_print_char fmt ',') Full.pp)
        v

    let gen = Crowbar.list Full.gen

    let encoding =
      Data_encoding.Compact.list ~bits:3 (dynamic_if_needed Full.encoding)
  end)

let compactfull_union1 : type a. a compactfull -> a compactfull =
 fun compactfulla ->
  try
    let module CompactFulla = (val compactfulla) in
    (module struct
      type t = CompactFulla.t

      let ty = CmpctUnion1 CompactFulla.ty

      let eq = CompactFulla.eq

      let encoding =
        let open Data_encoding.Compact in
        union
          [
            case
              ~title:"A"
              CompactFulla.encoding
              (function v -> Some v)
              (fun v -> v);
          ]

      let gen = CompactFulla.gen

      let pp ppf = function
        | v1 -> Crowbar.pp ppf "@[<hv 1>(Union1 %a)@]" CompactFulla.pp v1
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_union2 :
    type a b. a compactfull -> b compactfull -> (a, b) either compactfull =
 fun compactfulla compactfullb ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    (module struct
      type t = (CompactFulla.t, CompactFullb.t) either

      let ty = CmpctUnion2 (CompactFulla.ty, CompactFullb.ty)

      let eq x y =
        match (x, y) with
        | Left _, Right _ | Right _, Left _ -> false
        | Left x, Left y -> CompactFulla.eq x y
        | Right x, Right y -> CompactFullb.eq x y

      let encoding =
        let open Data_encoding.Compact in
        union
          [
            case
              ~title:"A"
              CompactFulla.encoding
              (function Left v -> Some v | Right _ -> None)
              (fun v -> Left v);
            case
              ~title:"B"
              CompactFullb.encoding
              (function Left _ -> None | Right v -> Some v)
              (fun v -> Right v);
          ]

      let gen =
        let open Crowbar in
        map [bool; CompactFulla.gen; CompactFullb.gen] (fun choice a b ->
            if choice then Left a else Right b)

      let pp ppf = function
        | Left v1 -> Crowbar.pp ppf "@[<hv 1>(A %a)@]" CompactFulla.pp v1
        | Right v2 -> Crowbar.pp ppf "@[<hv 1>(B %a)@]" CompactFullb.pp v2
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_union4 :
    type a b c d.
    a compactfull ->
    b compactfull ->
    c compactfull ->
    d compactfull ->
    ((a, b) either, (c, d) either) either compactfull =
 fun compactfulla compactfullb compactfullc compactfulld ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    let module CompactFullc = (val compactfullc) in
    let module CompactFulld = (val compactfulld) in
    (module struct
      type t =
        ( (CompactFulla.t, CompactFullb.t) either,
          (CompactFullc.t, CompactFulld.t) either )
        either

      let ty =
        CmpctUnion4
          (CompactFulla.ty, CompactFullb.ty, CompactFullc.ty, CompactFulld.ty)

      let eq x y =
        match (x, y) with
        | Left _, Right _ | Right _, Left _ -> false
        | Left (Left _), Left (Right _)
        | Left (Right _), Left (Left _)
        | Right (Left _), Right (Right _)
        | Right (Right _), Right (Left _) ->
            false
        | Left (Left x), Left (Left y) -> CompactFulla.eq x y
        | Left (Right x), Left (Right y) -> CompactFullb.eq x y
        | Right (Left x), Right (Left y) -> CompactFullc.eq x y
        | Right (Right x), Right (Right y) -> CompactFulld.eq x y

      let encoding =
        let open Data_encoding.Compact in
        union
          [
            case
              ~title:"A"
              CompactFulla.encoding
              (function Left (Left v) -> Some v | _ -> None)
              (fun v -> Left (Left v));
            case
              ~title:"B"
              CompactFullb.encoding
              (function Left (Right v) -> Some v | _ -> None)
              (fun v -> Left (Right v));
            case
              ~title:"C"
              CompactFullc.encoding
              (function Right (Left v) -> Some v | _ -> None)
              (fun v -> Right (Left v));
            case
              ~title:"D"
              CompactFulld.encoding
              (function Right (Right v) -> Some v | _ -> None)
              (fun v -> Right (Right v));
          ]

      let gen =
        let open Crowbar in
        map
          [
            bool;
            bool;
            CompactFulla.gen;
            CompactFullb.gen;
            CompactFullc.gen;
            CompactFulld.gen;
          ]
          (fun choice choice2 a b c d ->
            match (choice, choice2) with
            | true, true -> Left (Left a)
            | true, false -> Left (Right b)
            | false, true -> Right (Left c)
            | false, false -> Right (Right d))

      let pp ppf = function
        | Left (Left v1) -> Crowbar.pp ppf "@[<hv 1>(A %a)@]" CompactFulla.pp v1
        | Left (Right v2) ->
            Crowbar.pp ppf "@[<hv 1>(B %a)@]" CompactFullb.pp v2
        | Right (Left v3) ->
            Crowbar.pp ppf "@[<hv 1>(C %a)@]" CompactFullc.pp v3
        | Right (Right v4) ->
            Crowbar.pp ppf "@[<hv 1>(D %a)@]" CompactFulld.pp v4
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_union4spill :
    type a b. a compactfull -> b compactfull -> (a, b) either compactfull =
 fun compactfulla compactfullb ->
  try
    let module CompactFulla = (val compactfulla) in
    let module CompactFullb = (val compactfullb) in
    (module struct
      type t = (CompactFulla.t, CompactFullb.t) either

      let ty = CmpctUnion4Spill (CompactFulla.ty, CompactFullb.ty)

      let eq x y =
        match (x, y) with
        | Left _, Right _ | Right _, Left _ -> false
        | Left x, Left y -> CompactFulla.eq x y
        | Right x, Right y -> CompactFullb.eq x y

      let encoding =
        let open Data_encoding.Compact in
        union
          [
            case
              ~title:"A"
              CompactFulla.encoding
              (function Left v -> Some v | _ -> None)
              (fun v -> Left v);
            case
              ~title:"B"
              CompactFullb.encoding
              (function Right v -> Some v | _ -> None)
              (fun v -> Right v);
            void_case ~title:"Cvoid";
            void_case ~title:"Dvoid";
          ]

      let gen =
        let open Crowbar in
        map [bool; CompactFulla.gen; CompactFullb.gen] (fun choice a b ->
            match choice with true -> Left a | false -> Right b)

      let pp ppf = function
        | Left v1 -> Crowbar.pp ppf "@[<hv 1>(A %a)@]" CompactFulla.pp v1
        | Right v2 -> Crowbar.pp ppf "@[<hv 1>(B %a)@]" CompactFullb.pp v2
    end)
  with Invalid_argument _ -> Crowbar.bad_test ()

let compactfull_orint32 : type a. a full -> (int32, a) Either.t compactfull =
 fun full ->
  let module Full = (val full) in
  (module struct
    type t = (int32, Full.t) Either.t

    let ty = CmpctOrInt32 Full.ty

    let eq = Either.equal ~left:Int32.equal ~right:Full.eq

    let pp ppf = function
      | Either.Left i32 -> Crowbar.pp ppf "left(%ld)" i32
      | Either.Right p -> Crowbar.pp ppf "right(%a)" Full.pp p

    let gen =
      let open Crowbar in
      choose [map [int32] Either.left; map [Full.gen] Either.right]

    let encoding =
      Data_encoding.Compact.or_int32
        ~int32_title:"thirty_two"
        ~alt_title:"alt"
        Full.encoding
  end)

let rec full_of_ty : type a. a ty -> a full = function
  | Null -> full_null
  | Empty -> full_empty
  | Unit -> full_unit
  | Constant s -> full_constant s
  | Int8 -> full_int8
  | UInt8 -> full_uint8
  | Int16 -> full_int16
  | UInt16 -> full_uint16
  | Int31 -> full_int31
  | RangedInt (low, high) -> full_rangedint low high
  | UInt30_like_N high -> full_uint30_like_n high
  | Int31_like_Z (low, high) -> full_int31_like_z low high
  | Int32 -> full_int32
  | Int64 -> full_int64
  | Int16_le -> full_int16_le
  | UInt16_le -> full_uint16_le
  | Int31_le -> full_int31_le
  | RangedInt_le (low, high) -> full_rangedint_le low high
  | Int32_le -> full_int32_le
  | Int64_le -> full_int64_le
  | Float -> full_float
  | RangedFloat (low, high) -> full_rangedfloat low high
  | Bool -> full_bool
  | StringPlain -> full_string_plain
  | BytesPlain -> full_bytes_plain
  | FixedStringPlain n -> full_fixed_string_plain n
  | FixedBytesPlain n -> full_fixed_bytes_plain n
  | StringHex -> full_string_hex
  | BytesHex -> full_bytes_hex
  | FixedStringHex n -> full_fixed_string_hex n
  | FixedBytesHex n -> full_fixed_bytes_hex n
  | BigstringHex -> full_bigstring
  | FixedBigstringHex n -> full_fixed_bigstring n
  | Option ty -> full_option (full_of_ty ty)
  | Result (tya, tyb) -> full_result (full_of_ty tya) (full_of_ty tyb)
  | List ty -> full_list (full_of_ty ty)
  | FixedList (n, ty) -> full_fixed_list n (full_of_ty ty)
  | Array ty -> full_array (full_of_ty ty)
  | FixedArray (n, ty) -> full_fixed_array n (full_of_ty ty)
  | Dynamic_size (big_length, ty) ->
      full_dynamic_size big_length (full_of_ty ty)
  | Tup1 ty -> full_tup1 (full_of_ty ty)
  | Tup2 (tya, tyb) -> full_tup2 (full_of_ty tya) (full_of_ty tyb)
  | Tup3 (tya, tyb, tyc) ->
      full_tup3 (full_of_ty tya) (full_of_ty tyb) (full_of_ty tyc)
  | Tup4 (tya, tyb, tyc, tyd) ->
      full_tup4
        (full_of_ty tya)
        (full_of_ty tyb)
        (full_of_ty tyc)
        (full_of_ty tyd)
  | Union1 ty -> full_union1 (full_of_ty ty)
  | Union2 (tya, tyb) -> full_union2 (full_of_ty tya) (full_of_ty tyb)
  | Matching2 (tya, tyb) -> full_matching2 (full_of_ty tya) (full_of_ty tyb)
  | Mu_matching ty -> full_mu_matching (full_of_ty ty)
  | Mu_bigmatching (tya, tyb) ->
      full_mu_bigmatching (full_of_ty tya) (full_of_ty tyb)
  | Mu_obj (tya, tyb) -> full_mu_obj (full_of_ty tya) (full_of_ty tyb)
  | Check_size (n, ty) -> full_check_size n (full_of_ty ty)
  | Dynamic_and_check_size (n, ty) -> full_dyn_and_check_size n (full_of_ty ty)
  | StringEnum -> full_string_enum
  | Add_padding (ty, n) -> full_add_padding (full_of_ty ty) n
  | CompactMake cty -> full_make_compact (compactfull_of_compactty cty)
  | CompactMake16 cty -> full_make_compact16 (compactfull_of_compactty cty)

and compactfull_of_compactty : type a. a compactty -> a compactfull = function
  | CmpctUnit -> compactfull_unit
  | CmpctBool -> compactfull_bool
  | CmpctOption cty -> compactfull_option (compactfull_of_compactty cty)
  | CmpctTup1 cty -> compactfull_tup1 (compactfull_of_compactty cty)
  | CmpctTup2 (ctya, ctyb) ->
      compactfull_tup2
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
  | CmpctTup3 (ctya, ctyb, ctyc) ->
      compactfull_tup3
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
        (compactfull_of_compactty ctyc)
  | CmpctTup4 (ctya, ctyb, ctyc, ctyd) ->
      compactfull_tup4
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
        (compactfull_of_compactty ctyc)
        (compactfull_of_compactty ctyd)
  | CmpctTup7 (ctya, ctyb, ctyc, ctyd, ctye, ctyf, ctyg) ->
      compactfull_tup7
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
        (compactfull_of_compactty ctyc)
        (compactfull_of_compactty ctyd)
        (compactfull_of_compactty ctye)
        (compactfull_of_compactty ctyf)
        (compactfull_of_compactty ctyg)
  | CmpctInt32 -> compactfull_int32
  | CmpctInt64 -> compactfull_int64
  | CmpctList ty -> compactfull_list (full_of_ty ty)
  | CmpctUnion1 cty -> compactfull_union1 (compactfull_of_compactty cty)
  | CmpctUnion2 (ctya, ctyb) ->
      compactfull_union2
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
  | CmpctUnion4 (ctya, ctyb, ctyc, ctyd) ->
      compactfull_union4
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
        (compactfull_of_compactty ctyc)
        (compactfull_of_compactty ctyd)
  | CmpctUnion4Spill (ctya, ctyb) ->
      compactfull_union4spill
        (compactfull_of_compactty ctya)
        (compactfull_of_compactty ctyb)
  | CmpctOrInt32 ty -> compactfull_orint32 (full_of_ty ty)
  | CmpctPayload ty -> compactfull_payload (full_of_ty ty)

type full_and_v = FullAndV : 'a full * 'a -> full_and_v

let gen : full_and_v Crowbar.gen =
  let open Crowbar in
  dynamic_bind any_ty_gen (function AnyTy ty ->
      let full = full_of_ty ty in
      let module Full = (val full) in
      map [Full.gen] (fun v -> FullAndV (full, v)))

let gen_with_compact : full_and_v Crowbar.gen =
  let open Crowbar in
  dynamic_bind any_compactty_gen (function AnyTy ty ->
      let full = full_of_ty ty in
      let module Full = (val full) in
      map [Full.gen] (fun v -> FullAndV (full, v)))

type any_full = AnyFull : 'a full -> any_full

let gen_full : any_full Crowbar.gen =
  let open Crowbar in
  map [any_ty_gen] (fun (AnyTy ty) -> AnyFull (full_of_ty ty))
