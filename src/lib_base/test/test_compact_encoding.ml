(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

(** Testing
    -------
    Component:    Protocol Library
    Invocation:   dune exec \
                  src/lib_base/test/test_compact_encoding.exe
    Subject:      Compact encoding
*)

open Lib_test.Qcheck2_helpers
open Compact_encoding
module Roundtrip = Lib_test.Roundtrip

(* ------ reusable helpers -------------------------------------------------- *)

let power_two =
  let rec aux acc x = if x = 0 then acc else aux (2 * acc) (x - 1) in
  aux 1

let pp_int32 fmt x = Format.fprintf fmt "%ld" x

let pp_int64 fmt x = Format.fprintf fmt "%Ld" x

let compact_int32 : int32 Data_encoding.t = make ~tag_size:`Uint8 int32

let int32_gen =
  let open QCheck2.Gen in
  oneof
    [
      map Int32.of_int (0 -- Stdlib.Int.shift_left 1 8);
      map Int32.of_int (Stdlib.Int.shift_left 1 8 -- Stdlib.Int.shift_left 1 16);
      int32;
    ]

let compact_int64 : int64 Data_encoding.t = make ~tag_size:`Uint8 int64

let int64_gen =
  let open QCheck2.Gen in
  oneof
    [
      map Int64.of_int (0 -- Stdlib.Int.shift_left 1 8);
      map Int64.of_int (Stdlib.Int.shift_left 1 8 -- Stdlib.Int.shift_left 1 16);
      map Int64.of_int (Stdlib.Int.shift_left 1 16 -- Stdlib.Int.shift_left 1 32);
      int64;
    ]

let special_int64s =
  let weirds =
    [
      0L;
      1L;
      2L;
      0x7FL;
      0xFFL;
      0x80L;
      0x7FFFL;
      0x8000L;
      0xFFFFL;
      0x7FFF_FFFFL;
      0x8000_0000L;
      0xFFFF_FFFFL;
    ]
  in
  let nweirds = List.map Int64.neg weirds in
  let minsandmaxs =
    [
      Int64.min_int;
      Int64.max_int;
      Int64.of_int Int.min_int;
      Int64.of_int Int.max_int;
      Int64.of_int32 Int32.min_int;
      Int64.of_int32 Int32.max_int;
    ]
  in
  List.concat [weirds; nweirds; minsandmaxs]

let compact_list : int -> 'a Data_encoding.t -> 'a list Data_encoding.t =
 fun n encoding -> make ~tag_size:`Uint8 (list ~bits:n encoding)

let compactcompact_list :
    int -> 'a Compact_encoding.t -> 'a list Data_encoding.t =
 fun n encoding -> make ~tag_size:`Uint8 (list ~bits:n (make encoding))

(* ------ generators and compact encodings ---------------------------------- *)

let list_gen =
  let open QCheck2.Gen in
  let* n = nat in
  let n = n mod 8 in
  let* l = list_size ((fun m -> m mod power_two n) <$> nat) int64_gen in
  return (n, l)

let list32_gen =
  let open QCheck2.Gen in
  let* n = nat in
  let n = n mod 8 in
  let* l = list_size ((fun m -> m mod power_two n) <$> nat) int32 in
  return (n, l)

type t = {f1 : int32 option; f2 : bool}

let t_gen =
  let open QCheck2.Gen in
  let* choice = bool in
  let* f1 = if choice then Option.some <$> int32 else return None in
  let* f2 = bool in
  return {f1; f2}

let compact_t =
  conv
    (fun {f1; f2} -> (f1, f2))
    (fun (f1, f2) -> {f1; f2})
    (obj2 (opt "f1" int32) (req "f2" bool))

let t_encoding = make compact_t

let t_equal {f1 = f1_a; f2 = f2_a} {f1 = f1_b; f2 = f2_b} =
  Option.equal Int32.equal f1_a f1_b && Bool.equal f2_a f2_b

let pp_t fmt {f1; f2} =
  Format.(
    fprintf
      fmt
      "{f1 = %a; f2 = %a}"
      (pp_print_option pp_int32)
      f1
      pp_print_bool
      f2)

type u = X of int64 | Y of bool | Z

let u_gen =
  let open QCheck2.Gen in
  let* choice = bool in
  if choice then (fun x -> X x) <$> int64
  else
    let* choice = bool in
    if choice then (fun y -> Y y) <$> bool else return Z

let compact_u =
  union
    [
      case "X" (function X x -> Some x | _ -> None) (fun x -> X x) int64;
      case "Y" (function Y y -> Some y | _ -> None) (fun y -> Y y) bool;
      case "Z" (function Z -> Some () | _ -> None) (fun () -> Z) empty;
    ]

let u_encoding = make compact_u

let not_so_compact_u =
  union
    [
      case
        "X"
        (function X x -> Some x | _ -> None)
        (fun x -> X x)
        (payload Data_encoding.int64);
      case "___X_X" (fun _ -> None) refute void;
      case "___X_X" (fun _ -> None) refute void;
      case "___X_X" (fun _ -> None) refute void;
      case "Y" (function Y y -> Some y | _ -> None) (fun y -> Y y) bool;
      case "Z" (function Z -> Some () | _ -> None) (fun () -> Z) empty;
    ]

let other_u_encoding = make not_so_compact_u

let u_equal x y =
  match (x, y) with
  | (X x, X x') -> Int64.equal x x'
  | (Y y, Y y') -> Bool.equal y y'
  | (Z, Z) -> true
  | _ -> false

let pp_u fmt =
  Format.(
    function
    | X x -> fprintf fmt "X %Ld" x
    | Y y -> fprintf fmt "Y %a" pp_print_bool y
    | Z -> fprintf fmt "Z")

type e = (t, u) Either.t

let e_gen =
  let open QCheck2.Gen in
  let* choice = bool in
  if choice then Either.left <$> t_gen else Either.right <$> u_gen

let e_0 =
  let open Data_encoding in
  union
    [
      case ~title:"L" (Tag 0) t_encoding Either.find_left Either.left;
      case ~title:"R" (Tag 1) u_encoding Either.find_right Either.right;
    ]

let e_1 =
  make
  @@ union
       [
         case "L" Either.find_left Either.left compact_t;
         case "R" Either.find_right Either.right compact_u;
       ]

let e_2 =
  make
  @@ union
       [
         case "L" Either.find_left Either.left compact_t;
         case "R" Either.find_right Either.right not_so_compact_u;
       ]

let e_equal = Either.equal ~left:t_equal ~right:u_equal

let pp_e fmt =
  let open Format in
  function
  | Either.Left t ->
      pp_print_string fmt "Left " ;
      pp_t fmt t
  | Either.Right u ->
      pp_print_string fmt "Right " ;
      pp_u fmt u

(* ------ test template ----------------------------------------------------- *)

let test_roundtrip ?(count = 1_000_000) title arb equ pp f =
  let test rdt input =
    let (input, encoding) = f input in
    let output = Roundtrip.make encoding rdt input in
    let success = equ input output in
    if not success then
      QCheck2.Test.fail_reportf
        "%s %s roundtrip error: %a became %a"
        title
        (Roundtrip.target rdt)
        pp
        input
        pp
        output
  in
  QCheck2.Test.make
    ~count
    ~name:(Format.asprintf "roundtrip %s" title)
    arb
    (fun input ->
      test Roundtrip.binary input ;
      test Roundtrip.json input ;
      true)

let () =
  let qcheck_wrap = qcheck_wrap ~rand:(Random.State.make_self_init ()) in
  Alcotest.run
    "Compact_encoding"
    [
      ( "int64roundtrip",
        [
          Alcotest.test_case "all weird int64" `Quick (fun () ->
              List.iter
                (fun i64 ->
                  let output =
                    Roundtrip.make compact_int64 Roundtrip.binary i64
                  in
                  let success = Int64.equal i64 output in
                  if not success then
                    Format.kasprintf
                      failwith
                      "Roundtrip failure for %Ld: got %Ld"
                      i64
                      output)
                special_int64s);
        ] );
      ( "qcheckroundtrip",
        qcheck_wrap
          [
            test_roundtrip "int32" int32_gen Int32.equal pp_int32 (fun input ->
                (input, compact_int32));
            test_roundtrip
              "int64"
              QCheck2.Gen.int64
              Int64.equal
              pp_int64
              (fun input -> (input, compact_int64));
            test_roundtrip
              ~count:10_000
              "list"
              list_gen
              (List.equal Int64.equal)
              Format.(
                pp_print_list
                  ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                  pp_int64)
              (fun (n, input) -> (input, compact_list n Data_encoding.int64));
            test_roundtrip
              ~count:10_000
              "list compactcompact"
              list_gen
              (List.equal Int64.equal)
              Format.(
                pp_print_list
                  ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                  pp_int64)
              (fun (n, input) -> (input, compactcompact_list n int64));
            test_roundtrip
              ~count:10_000
              "list32"
              list32_gen
              (List.equal Int32.equal)
              Format.(
                pp_print_list
                  ~pp_sep:(fun fmt () -> pp_print_string fmt ",")
                  pp_int32)
              (fun (n, input) -> (input, compactcompact_list n int32));
            test_roundtrip "record t" t_gen t_equal pp_t (fun input ->
                (input, t_encoding));
            test_roundtrip "union u" u_gen u_equal pp_u (fun input ->
                (input, u_encoding));
            test_roundtrip
              "union u (not so compact)"
              u_gen
              u_equal
              pp_u
              (fun input -> (input, other_u_encoding));
            test_roundtrip "either t u (0)" e_gen e_equal pp_e (fun input ->
                (input, e_0));
            test_roundtrip "either t u (1)" e_gen e_equal pp_e (fun input ->
                (input, e_1));
            test_roundtrip "either t u (2)" e_gen e_equal pp_e (fun input ->
                (input, e_2));
          ] );
    ]
