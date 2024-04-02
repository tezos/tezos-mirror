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

(** A intermediate representation for BSON, a binary encoding for JSON.

    Decoding and encoding is (optionally) done as lazily as possible.
    First, the [view] function is able to unfold only one
    level and not the whole structure. Also, serialized versions are
    cached, so that later serializations of the same object are faster.

    Notes:

    1. Only JSON compatible BSON documents are supported.
       BSON extensions are not supported (int32, int64, timestamp, etc.).
    2. Arrays in BSON are stored inefficiently.
       Prefer another binary format if you manipulate lots of arrays.
    3. We differ from BSON to allow toplevel immediates.
       For this, we produce a document with only one byte indicating
       the kind of immediate followed by the immediate.
       The byte is [0x80 lor (the corresponding BSON field kind)].
    4. We differ from BSON to allow unambiguous toplevel arrays.
       As with (3), the subdocument to be decoded as an array is
       preceded with a 0x84.

    Use the [conforming] flag to deactivates the extension from notes (3)
    and (4). In this case, the toplevel value must be an object. *)
type bson

(** A view over the {!bson} representation.*)
module Repr : Json_repr.Repr with type value = bson

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_encoding : Json_encoding.S with type repr_value = bson

(** Pre-instanciated {!Json_encoding.Make}. *)
module Json_query : module type of Json_query.Make (Repr)

(** Serializes the intermediate BSON representation to actual BSON.

    By default, [conforming] is [false], so that any value can be serialized,
    including immediates (see {!type:bson}).

    By default, [cache] is [true], so a future serialization of the
    same data will be faster. The resulting bytes are stored in the
    value. You may want to turn this off if these values have a long
    lifespan, and that you care more about memory consumption than
    serialization speed.

    Will raise [Invalid_argument "Json_repr.bson_to_bytes"] when
    [conforming] and trying to serialize a toplevel array or immediate. *)
val bson_to_bytes : ?cache:bool -> ?conforming:bool -> bson -> bytes

(** Bson decoding error, with a message, the BSON and an offset. *)
exception Bson_decoding_error of string * bytes * int

(** Creates a lazily unfolded representation for some BSON.
    Because of the mutability of [bytes] and this laziness,
    set the copy parameter to [true] if you are not sure that the
    [bytes] will not be mutated in the future.

    By default, [conforming] is [false], so that any value can be serialized,
    including immediates (see {!type:bson}).

    By default, [cache] is [true], so a future serialization of the
    same data will be faster. The input bytes are stored in the
    value. You may want to turn this off if these values have a long
    lifespan, and that you care more about memory consumption than
    serialization speed.

    By default, [laziness] is [true]. If the data is a serialized
    object, it means that only the field names are read, the field
    values are eluded, and will be deserialized on demand when calling
    [Repr.view]. This implies that {!Bson_decoding_error} may be
    raised later. If set to [false], the whole structure is decoded
    upfront, so any decoding error will happen at this point. This may
    be preferable mostly when reading from untusted sources.

    May raise {!Bson_decoding_error}. *)
val bytes_to_bson :
  ?laziness:bool ->
  ?cache:bool ->
  ?conforming:bool ->
  copy:bool ->
  bytes ->
  bson
