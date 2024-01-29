(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright 2014 OCamlPro                                                   *)
(* Copyright (c) 2020 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** An abstract type for native browser objects. *)
type value

(** A view over the browser representation.*)
module Repr : Json_repr.Repr with type value = value

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_encoding : Json_encoding.S with type repr_value = value

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_query : module type of Json_query.Make (Repr)

(** Parse a JSON string using the native browser parser. *)
val parse : string -> value

(** Produce a JSON string using the native browser printer.

    If indent is not present, everything is printed on a single line.
    Otherwise, it is the number (up to 10) of spaces inserted at
    beginning of lines for each indentation level. *)
val stringify : ?indent:int -> value -> string

(** Same as {!parse} with native browser strings. *)
val parse_js_string : Js.js_string Js.t -> value

(** Same as {!stringify} with native browser strings. *)
val js_stringify : ?indent:int -> value -> Js.js_string Js.t
