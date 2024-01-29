(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** Like most other [.mli] files in this directory, this is not intended for
    end-users. Instead, the interface from this file is used internally to
    assemble the end-user-intended module {!Data_encoding}. Refer to that module
    for doucmentation. *)

exception Invariant_guard of string

type read_error =
  | Not_enough_data
  | Extra_bytes
  | No_case_matched
  | Unexpected_tag of int
  | Invalid_int of {min : int; v : int; max : int}
  | Invalid_float of {min : float; v : float; max : float}
  | Trailing_zero
  | Size_limit_exceeded
  | List_too_long
  | Array_too_long
  | Exception_raised_in_user_function of string
  | User_invariant_guard of string

exception Read_error of read_error

type write_error =
  | Size_limit_exceeded
  | No_case_matched
  | Invalid_int of {min : int; v : int; max : int}
  | Invalid_float of {min : float; v : float; max : float}
  | Invalid_bytes_length of {expected : int; found : int}
  | Invalid_string_length of {expected : int; found : int}
  | Invalid_natural
  | List_invalid_length
  | Array_invalid_length
  | Exception_raised_in_user_function of string

exception Write_error of write_error
