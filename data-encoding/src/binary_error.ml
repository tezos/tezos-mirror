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

let invalid_int_encoding =
  let open Encoding in
  obj3 (req "min" int31) (req "v" int31) (req "max" int31)

let invalid_float_encoding =
  let open Encoding in
  obj3 (req "min" float) (req "v" float) (req "max" float)

let read_error_encoding : read_error Encoding.t =
  let open Encoding in
  matching
    (function
      | Not_enough_data -> Matched (0, empty, ())
      | Extra_bytes -> Matched (1, empty, ())
      | No_case_matched -> Matched (2, empty, ())
      | Unexpected_tag tag -> Matched (3, int31, tag)
      | Invalid_int {min; v; max} ->
          Matched (5, invalid_int_encoding, (min, v, max))
      | Invalid_float {min; v; max} ->
          Matched (6, invalid_float_encoding, (min, v, max))
      | Trailing_zero -> Matched (7, empty, ())
      | Size_limit_exceeded -> Matched (8, empty, ())
      | List_too_long -> Matched (9, empty, ())
      | Array_too_long -> Matched (10, empty, ())
      | Exception_raised_in_user_function msg -> Matched (11, string, msg)
      | User_invariant_guard msg -> Matched (12, string, msg))
    [
      case
        (Tag 0)
        ~title:"Not enough data"
        empty
        (function Not_enough_data -> Some () | _ -> None)
        (fun () -> Not_enough_data);
      case
        (Tag 1)
        ~title:"Extra bytes"
        empty
        (function Extra_bytes -> Some () | _ -> None)
        (fun () -> Extra_bytes);
      case
        (Tag 2)
        ~title:"No case matched"
        empty
        (function (No_case_matched : read_error) -> Some () | _ -> None)
        (fun () -> No_case_matched);
      case
        (Tag 3)
        ~title:"Unexpected tag"
        int31
        (function Unexpected_tag i -> Some i | _ -> None)
        (fun i -> Unexpected_tag i);
      case
        (Tag 5)
        ~title:"Invalid int"
        invalid_int_encoding
        (function
          | (Invalid_int {min; v; max} : read_error) -> Some (min, v, max)
          | _ -> None)
        (fun (min, v, max) -> Invalid_int {min; v; max});
      case
        (Tag 6)
        ~title:"Invalid float"
        invalid_float_encoding
        (function
          | (Invalid_float {min; v; max} : read_error) -> Some (min, v, max)
          | _ -> None)
        (fun (min, v, max) -> Invalid_float {min; v; max});
      case
        (Tag 7)
        ~title:"Trailing zero"
        empty
        (function Trailing_zero -> Some () | _ -> None)
        (fun () -> Trailing_zero);
      case
        (Tag 8)
        ~title:"Size limit exceeded"
        empty
        (function (Size_limit_exceeded : read_error) -> Some () | _ -> None)
        (fun () -> Size_limit_exceeded);
      case
        (Tag 9)
        ~title:"List too long"
        empty
        (function (List_too_long : read_error) -> Some () | _ -> None)
        (fun () -> List_too_long);
      case
        (Tag 10)
        ~title:"Array too long"
        empty
        (function (Array_too_long : read_error) -> Some () | _ -> None)
        (fun () -> Array_too_long);
      case
        (Tag 11)
        ~title:"Exception raised in user function"
        string
        (function
          | (Exception_raised_in_user_function s : read_error) -> Some s
          | _ -> None)
        (fun s -> Exception_raised_in_user_function s);
      case
        (Tag 12)
        ~title:"User invariant guard"
        string
        (function User_invariant_guard s -> Some s | _ -> None)
        (fun s -> User_invariant_guard s);
    ]

let pp_read_error ppf = function
  | Not_enough_data -> Format.fprintf ppf "Not enough data"
  | Extra_bytes -> Format.fprintf ppf "Extra bytes"
  | No_case_matched -> Format.fprintf ppf "No case matched"
  | Unexpected_tag tag -> Format.fprintf ppf "Unexpected tag %d" tag
  | Invalid_int {min; v; max} ->
      Format.fprintf ppf "Invalid int (%d <= %d <= %d) " min v max
  | Invalid_float {min; v; max} ->
      Format.fprintf ppf "Invalid float (%f <= %f <= %f) " min v max
  | Trailing_zero -> Format.fprintf ppf "Trailing zero in Z"
  | Size_limit_exceeded -> Format.fprintf ppf "Size limit exceeded"
  | List_too_long -> Format.fprintf ppf "List length limit exceeded"
  | Array_too_long -> Format.fprintf ppf "Array length limit exceeded"
  | Exception_raised_in_user_function s ->
      Format.fprintf ppf "Exception raised in user function: %s" s
  | User_invariant_guard s ->
      Format.fprintf
        ppf
        "User-specified invariant not respected in encoded data: %s"
        s

let write_error_encoding =
  let open Encoding in
  matching
    (function
      | Size_limit_exceeded -> Matched (0, empty, ())
      | No_case_matched -> Matched (1, empty, ())
      | Invalid_int {min; v; max} ->
          Matched (2, invalid_int_encoding, (min, v, max))
      | Invalid_float {min; v; max} ->
          Matched (3, invalid_float_encoding, (min, v, max))
      | Invalid_bytes_length {expected; found} ->
          Matched
            ( 4,
              obj2 (req "expected" int31) (req "found" int31),
              (expected, found) )
      | Invalid_string_length {expected; found} ->
          Matched
            ( 5,
              obj2 (req "expected" int31) (req "found" int31),
              (expected, found) )
      | Invalid_natural -> Matched (6, empty, ())
      | List_invalid_length -> Matched (7, empty, ())
      | Array_invalid_length -> Matched (8, empty, ())
      | (Exception_raised_in_user_function s : write_error) ->
          Matched (9, string, s))
    [
      case
        (Tag 0)
        ~title:"Size limit exceeded"
        empty
        (function Size_limit_exceeded -> Some () | _ -> None)
        (fun () -> Size_limit_exceeded);
      case
        (Tag 1)
        ~title:"No case matched"
        empty
        (function No_case_matched -> Some () | _ -> None)
        (fun () -> No_case_matched);
      case
        (Tag 2)
        ~title:"Invalid int"
        (obj3 (req "min" int31) (req "v" int31) (req "max" int31))
        (function Invalid_int {min; v; max} -> Some (min, v, max) | _ -> None)
        (fun (min, v, max) -> Invalid_int {min; v; max});
      case
        (Tag 3)
        ~title:"Invalid float"
        (obj3 (req "min" float) (req "v" float) (req "max" float))
        (function
          | Invalid_float {min; v; max} -> Some (min, v, max) | _ -> None)
        (fun (min, v, max) -> Invalid_float {min; v; max});
      case
        (Tag 4)
        ~title:"Invalid bytes length"
        (obj2 (req "expected" int31) (req "found" int31))
        (function
          | Invalid_bytes_length {expected; found} -> Some (expected, found)
          | _ -> None)
        (fun (expected, found) -> Invalid_bytes_length {expected; found});
      case
        (Tag 5)
        ~title:"Invalid string length"
        (obj2 (req "expected" int31) (req "found" int31))
        (function
          | Invalid_string_length {expected; found} -> Some (expected, found)
          | _ -> None)
        (fun (expected, found) -> Invalid_bytes_length {expected; found});
      case
        (Tag 6)
        ~title:"Invalid natural"
        empty
        (function Invalid_natural -> Some () | _ -> None)
        (fun () -> Invalid_natural);
      case
        (Tag 7)
        ~title:"List too long or too short"
        empty
        (function List_invalid_length -> Some () | _ -> None)
        (fun () -> List_invalid_length);
      case
        (Tag 8)
        ~title:"Array too long or too short"
        empty
        (function Array_invalid_length -> Some () | _ -> None)
        (fun () -> Array_invalid_length);
      case
        (Tag 9)
        ~title:"Exception raised in user function"
        string
        (function
          | (Exception_raised_in_user_function s : write_error) -> Some s
          | _ -> None)
        (fun s -> Exception_raised_in_user_function s);
    ]

let pp_write_error ppf = function
  | Size_limit_exceeded -> Format.fprintf ppf "Size limit exceeded"
  | No_case_matched -> Format.fprintf ppf "No case matched"
  | Invalid_int {min; v; max} ->
      Format.fprintf ppf "Invalid int (%d <= %d <= %d) " min v max
  | Invalid_float {min; v; max} ->
      Format.fprintf ppf "Invalid float (%f <= %f <= %f) " min v max
  | Invalid_bytes_length {expected; found} ->
      Format.fprintf
        ppf
        "Invalid bytes length (expected: %d ; found %d)"
        expected
        found
  | Invalid_string_length {expected; found} ->
      Format.fprintf
        ppf
        "Invalid string length (expected: %d ; found %d)"
        expected
        found
  | Invalid_natural -> Format.fprintf ppf "Negative natural"
  | List_invalid_length -> Format.fprintf ppf "List is too long or too short"
  | Array_invalid_length -> Format.fprintf ppf "Array is too long or too short"
  | Exception_raised_in_user_function s ->
      Format.fprintf ppf "Exception raised in user function: %s" s
