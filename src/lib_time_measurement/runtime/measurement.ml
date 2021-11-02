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

(** This module defines the model of a measurement as well as
    its related pretty printers and encoders. *)

(** The name of a measurement. *)
type label = string

(** Some additional data that can be used to discrimine
    measurements. *)
type metadata = string list

(** The key of a measurement is used to discriminate measurements.
    It is intended to be used as a search criteria if measurements
    are stored in a collection. *)
type key = label * metadata

(* TODO: https://gitlab.com/tezos/tezos/-/issues/1844
   Distinguish durations from timestamps. *)

(** The value of a measurement.
    It can actually represent both a duration or a timestamp. *)
type value = float

type t = key * value

(* Constructors *)

let create label metadata value = ((label, metadata), value)

(* Pretty printing *)

let pp_list fmt pp l =
  Format.(
    fprintf
      fmt
      "[%a]"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "; ") pp)
      l)

let pp_metadata fmt metadata = pp_list fmt Format.pp_print_string metadata

let pp_value fmt f = Format.fprintf fmt "%f" f

let pp_measurement fmt ((label, metadata), value) =
  Format.fprintf fmt "(%s, %a) -> %a" label pp_metadata metadata pp_value value

let pp_measurements fmt = pp_list fmt pp_measurement

(* Encodings *)

let key_encoding = Data_encoding.(tup2 string (list string))

let value_encoding = Data_encoding.float

let measurement_encoding = Data_encoding.tup2 key_encoding value_encoding

let measurements_encoding = Data_encoding.list measurement_encoding
