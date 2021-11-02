(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Encoding for expressions, as their {!canonical} encoding.
    Locations are stored in a side table.
    See {!canonical_encoding} for the [variant] parameter. *)
val table_encoding :
  variant:string ->
  'l Data_encoding.encoding ->
  'p Data_encoding.encoding ->
  ('l, 'p) Micheline.node Data_encoding.encoding

(** Encoding for expressions, as their {!canonical} encoding.
    Locations are erased when serialized, and restored to a provided
    default value when deserialized.
    See {!canonical_encoding} for the [variant] parameter. *)
val erased_encoding :
  variant:string ->
  'l ->
  'p Data_encoding.encoding ->
  ('l, 'p) Micheline.node Data_encoding.encoding

val node_encoding : Micheline_parser.node Data_encoding.encoding

(** Encoding for canonical integer locations. *)
val canonical_location_encoding :
  Micheline.canonical_location Data_encoding.encoding

(** Encoding for expressions in canonical form. The first parameter
    is a name used to produce named definitions in the schemas. Make
    sure to use different names if two expression variants with
    different primitive encodings are used in the same schema. *)
val canonical_encoding :
  variant:string ->
  'l Data_encoding.encoding ->
  'l Micheline.canonical Data_encoding.encoding

(** Old version of {!canonical_encoding} for backward compatibility.
    Do not use in new code. *)
val canonical_encoding_v0 :
  variant:string ->
  'l Data_encoding.encoding ->
  'l Micheline.canonical Data_encoding.encoding

(** Old version of {!canonical_encoding} for backward compatibility.
    Do not use in new code. *)
val canonical_encoding_v1 :
  variant:string ->
  'l Data_encoding.encoding ->
  'l Micheline.canonical Data_encoding.encoding

(** Alias for {!canonical_encoding}. *)
val canonical_encoding_v2 :
  variant:string ->
  'l Data_encoding.encoding ->
  'l Micheline.canonical Data_encoding.encoding
