(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2021-2022 Nomadic Labs <contact@nomadic-labs.com>           *)
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

(** Defines the internal Michelson representation for timestamps and basic
    operations that can be performed on it. *)

open Script_int

type repr

(** Representation of timestamps specific to the Michelson interpreter.
    A number of seconds since the epoch.
    [t] is made algebraic in order to distinguish it from the other type
    parameters of [Script_typed_ir.ty]. *)
type t = Timestamp_tag of repr [@@ocaml.unboxed]

(** Convert a number of seconds since the epoch to a timestamp.*)
val of_int64 : int64 -> t

(** Compare timestamps. Returns [1] if the first timestamp is later than the
    second one; [0] if they're equal and [-1] othwerwise. *)
val compare : t -> t -> int

(** Convert a timestamp to RFC3339 notation if possible **)
val to_notation : t -> string option

(** Convert a timestamp to a string representation of the seconds *)
val to_num_str : t -> string

(** Convert to RFC3339 notation if possible, or num if not *)
val to_string : t -> string

val of_string : string -> t option

(** Returns difference between timestamps as integral number of seconds
    in Michelson representation of numbers. *)
val diff : t -> t -> z num

(** Add a number of seconds to the timestamp. *)
val add_delta : t -> z num -> t

(** Subtract a number of seconds from the timestamp. *)
val sub_delta : t -> z num -> t

val to_zint : t -> Z.t

val of_zint : Z.t -> t

(* Timestamps are encoded exactly as Z. *)
val encoding : t Data_encoding.encoding

val now : Alpha_context.t -> t
