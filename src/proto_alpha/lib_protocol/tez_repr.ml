(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2020-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

let id = "tez"

let name = "mutez"

open Compare.Int64 (* invariant: positive *)

type repr = t

type t = Tez_tag of repr [@@ocaml.unboxed]

let wrap t = Tez_tag t [@@ocaml.inline always]

type error +=
  | Addition_overflow of t * t (* `Temporary *)
  | Subtraction_underflow of t * t (* `Temporary *)
  | Multiplication_overflow of t * int64 (* `Temporary *)
  | Negative_multiplicator of t * int64 (* `Temporary *)
  | Invalid_divisor of t * int64

(* `Temporary *)

let zero = Tez_tag 0L

(* all other constant are defined from the value of one micro tez *)
let one_mutez = Tez_tag 1L

let max_mutez = Tez_tag Int64.max_int

let mul_int (Tez_tag tez) i = Tez_tag (Int64.mul tez i)

let one_cent = mul_int one_mutez 10_000L

let fifty_cents = mul_int one_cent 50L

(* 1 tez = 100 cents = 1_000_000 mutez *)
let one = mul_int one_cent 100L

let of_string s =
  let triplets = function
    | hd :: tl ->
        let len = String.length hd in
        Compare.Int.(
          len <= 3 && len > 0 && List.for_all (fun s -> String.length s = 3) tl)
    | [] -> false
  in
  let integers s = triplets (String.split_on_char ',' s) in
  let decimals s =
    let l = String.split_on_char ',' s in
    if Compare.List_length_with.(l > 2) then false else triplets (List.rev l)
  in
  let parse left right =
    let remove_commas s = String.concat "" (String.split_on_char ',' s) in
    let pad_to_six s =
      let len = String.length s in
      String.init 6 (fun i -> if Compare.Int.(i < len) then s.[i] else '0')
    in
    let prepared = remove_commas left ^ pad_to_six (remove_commas right) in
    Option.map wrap (Int64.of_string_opt prepared)
  in
  match String.split_on_char '.' s with
  | [left; right] ->
      if String.contains s ',' then
        if integers left && decimals right then parse left right else None
      else if
        Compare.Int.(String.length right > 0)
        && Compare.Int.(String.length right <= 6)
      then parse left right
      else None
  | [left] ->
      if (not (String.contains s ',')) || integers left then parse left ""
      else None
  | _ -> None

let pp ppf (Tez_tag amount) =
  let mult_int = 1_000_000L in
  let rec left ppf amount =
    let d, r = (Int64.div amount 1000L, Int64.rem amount 1000L) in
    if d > 0L then Format.fprintf ppf "%a%03Ld" left d r
    else Format.fprintf ppf "%Ld" r
  in
  let right ppf amount =
    let triplet ppf v =
      if Compare.Int.(v mod 10 > 0) then Format.fprintf ppf "%03d" v
      else if Compare.Int.(v mod 100 > 0) then Format.fprintf ppf "%02d" (v / 10)
      else Format.fprintf ppf "%d" (v / 100)
    in
    let hi, lo = (amount / 1000, amount mod 1000) in
    if Compare.Int.(lo = 0) then Format.fprintf ppf "%a" triplet hi
    else Format.fprintf ppf "%03d%a" hi triplet lo
  in
  let ints, decs =
    (Int64.div amount mult_int, Int64.(to_int (rem amount mult_int)))
  in
  left ppf ints ;
  if Compare.Int.(decs > 0) then Format.fprintf ppf ".%a" right decs

let to_string t = Format.asprintf "%a" pp t

let ( -? ) tez1 tez2 =
  let open Result_syntax in
  let (Tez_tag t1) = tez1 in
  let (Tez_tag t2) = tez2 in
  if t2 <= t1 then return (Tez_tag (Int64.sub t1 t2))
  else tzfail (Subtraction_underflow (tez1, tez2))

let sub_opt (Tez_tag t1) (Tez_tag t2) =
  if t2 <= t1 then Some (Tez_tag (Int64.sub t1 t2)) else None

let ( +? ) tez1 tez2 =
  let open Result_syntax in
  let (Tez_tag t1) = tez1 in
  let (Tez_tag t2) = tez2 in
  let t = Int64.add t1 t2 in
  if t < t1 then tzfail (Addition_overflow (tez1, tez2)) else return (Tez_tag t)

let ( *? ) tez m =
  let open Result_syntax in
  let (Tez_tag t) = tez in
  if m < 0L then tzfail (Negative_multiplicator (tez, m))
  else if m = 0L then return (Tez_tag 0L)
  else if t > Int64.(div max_int m) then
    tzfail (Multiplication_overflow (tez, m))
  else return (Tez_tag (Int64.mul t m))

let ( /? ) tez d =
  let open Result_syntax in
  let (Tez_tag t) = tez in
  if d <= 0L then tzfail (Invalid_divisor (tez, d))
  else return (Tez_tag (Int64.div t d))

let div2 (Tez_tag t) = Tez_tag (Int64.div t 2L)

let mul_exn t m =
  match t *? Int64.of_int m with Ok v -> v | Error _ -> invalid_arg "mul_exn"

let div_exn t d =
  match t /? Int64.of_int d with Ok v -> v | Error _ -> invalid_arg "div_exn"

let mul_ratio ~rounding tez ~num ~den =
  let open Result_syntax in
  let (Tez_tag t) = tez in
  if num < 0L then tzfail (Negative_multiplicator (tez, num))
  else if den <= 0L then tzfail (Invalid_divisor (tez, den))
  else if num = 0L then return zero
  else
    let numerator = Z.(mul (of_int64 t) (of_int64 num)) in
    let denominator = Z.of_int64 den in
    let z =
      match rounding with
      | `Down -> Z.div numerator denominator
      | `Up -> Z.cdiv numerator denominator
    in
    if Z.fits_int64 z then return (Tez_tag (Z.to_int64 z))
    else tzfail (Multiplication_overflow (tez, num))

let mul_percentage ~rounding =
  let z100 = Z.of_int 100 in
  fun (Tez_tag t) (percentage : Int_percentage.t) ->
    (* Guaranteed to produce no errors by the invariants on {!Int_percentage.t}. *)
    let div' = match rounding with `Down -> Z.div | `Up -> Z.cdiv in
    Tez_tag
      Z.(to_int64 (div' (mul (of_int64 t) (of_int (percentage :> int))) z100))

let of_mutez t = if t < 0L then None else Some (Tez_tag t)

let of_mutez_exn x =
  match of_mutez x with None -> invalid_arg "Tez.of_mutez" | Some v -> v

let to_mutez (Tez_tag t) = t

let encoding =
  let open Data_encoding in
  let decode (Tez_tag t) = Z.of_int64 t in
  let encode = Json.wrap_error (fun i -> Tez_tag (Z.to_int64 i)) in
  Data_encoding.def name (check_size 10 (conv decode encode n))

let balance_update_encoding =
  let open Data_encoding in
  conv
    (function
      | `Credited v -> to_mutez v | `Debited v -> Int64.neg (to_mutez v))
    ( Json.wrap_error @@ fun v ->
      if Compare.Int64.(v < 0L) then `Debited (Tez_tag (Int64.neg v))
      else `Credited (Tez_tag v) )
    int64

let () =
  let open Data_encoding in
  register_error_kind
    `Temporary
    ~id:(id ^ ".addition_overflow")
    ~title:("Overflowing " ^ id ^ " addition")
    ~pp:(fun ppf (opa, opb) ->
      Format.fprintf
        ppf
        "Overflowing addition of %a %s and %a %s"
        pp
        opa
        id
        pp
        opb
        id)
    ~description:("An addition of two " ^ id ^ " amounts overflowed")
    (obj1 (req "amounts" (tup2 encoding encoding)))
    (function Addition_overflow (a, b) -> Some (a, b) | _ -> None)
    (fun (a, b) -> Addition_overflow (a, b)) ;
  register_error_kind
    `Temporary
    ~id:(id ^ ".subtraction_underflow")
    ~title:("Underflowing " ^ id ^ " subtraction")
    ~pp:(fun ppf (opa, opb) ->
      Format.fprintf
        ppf
        "Underflowing subtraction of %a %s and %a %s"
        pp
        opa
        id
        pp
        opb
        id)
    ~description:
      ("A subtraction of two " ^ id
     ^ " amounts underflowed (i.e., would have led to a negative amount)")
    (obj1 (req "amounts" (tup2 encoding encoding)))
    (function Subtraction_underflow (a, b) -> Some (a, b) | _ -> None)
    (fun (a, b) -> Subtraction_underflow (a, b)) ;
  register_error_kind
    `Temporary
    ~id:(id ^ ".multiplication_overflow")
    ~title:("Overflowing " ^ id ^ " multiplication")
    ~pp:(fun ppf (opa, opb) ->
      Format.fprintf
        ppf
        "Overflowing multiplication of %a %s and %Ld"
        pp
        opa
        id
        opb)
    ~description:
      ("A multiplication of a " ^ id ^ " amount by an integer overflowed")
    (obj2 (req "amount" encoding) (req "multiplicator" int64))
    (function Multiplication_overflow (a, b) -> Some (a, b) | _ -> None)
    (fun (a, b) -> Multiplication_overflow (a, b)) ;
  register_error_kind
    `Temporary
    ~id:(id ^ ".negative_multiplicator")
    ~title:("Negative " ^ id ^ " multiplicator")
    ~pp:(fun ppf (opa, opb) ->
      Format.fprintf
        ppf
        "Multiplication of %a %s by negative integer %Ld"
        pp
        opa
        id
        opb)
    ~description:("Multiplication of a " ^ id ^ " amount by a negative integer")
    (obj2 (req "amount" encoding) (req "multiplicator" int64))
    (function Negative_multiplicator (a, b) -> Some (a, b) | _ -> None)
    (fun (a, b) -> Negative_multiplicator (a, b)) ;
  register_error_kind
    `Temporary
    ~id:(id ^ ".invalid_divisor")
    ~title:("Invalid " ^ id ^ " divisor")
    ~pp:(fun ppf (opa, opb) ->
      Format.fprintf
        ppf
        "Division of %a %s by non positive integer %Ld"
        pp
        opa
        id
        opb)
    ~description:
      ("Multiplication of a " ^ id ^ " amount by a non positive integer")
    (obj2 (req "amount" encoding) (req "divisor" int64))
    (function Invalid_divisor (a, b) -> Some (a, b) | _ -> None)
    (fun (a, b) -> Invalid_divisor (a, b))

type tez = t

let compare (Tez_tag x) (Tez_tag y) = compare x y

let ( = ) (Tez_tag x) (Tez_tag y) = x = y

let ( <> ) (Tez_tag x) (Tez_tag y) = x <> y

let ( < ) (Tez_tag x) (Tez_tag y) = x < y

let ( > ) (Tez_tag x) (Tez_tag y) = x > y

let ( <= ) (Tez_tag x) (Tez_tag y) = x <= y

let ( >= ) (Tez_tag x) (Tez_tag y) = x >= y

let equal (Tez_tag x) (Tez_tag y) = equal x y

let max (Tez_tag x) (Tez_tag y) = Tez_tag (max x y)

let min (Tez_tag x) (Tez_tag y) = Tez_tag (min x y)
