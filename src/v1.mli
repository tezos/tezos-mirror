(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
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

(** Type-safe serialization and deserialization of data structures. *)

(** This page is for the API documentation of data-encoding: the technical
    description of each of the available type and combinator exported by the
    data-encoding library.

    For a high-level view and a tutorial, see {!page-tutorial}. *)

(** {1 Data Encoding} *)

(** {2 Overview}

    This module provides type-safe serialization and deserialization of
    data structures. Backends are provided to both /ad hoc/ binary, JSON
    and BSON.

    This works by writing type descriptors by hand, using the provided
    combinators. These combinators can fine-tune the binary
    representation to be compact and efficient, but also provide
    proper field names and meta information. As a result, an API that uses
    those descriptors can be automatically introspected and documented.

    Here is an example encoding for type [(int * string)].

    [let enc = obj2 (req "code" uint16) (req "message" string)]

    In JSON, this encoding maps values of type [int * string] to JSON
    objects with a field [code] whose value is a number and a field
    [message] whose value is a string.

    In binary, this encoding maps to two raw bytes for the [int]
    followed by the size of the string in bytes, and finally the raw
    contents of the string. This binary format is mostly tagless,
    meaning that serialized data cannot be interpreted without the
    encoding that was used for serialization.

    Regarding binary serialization, encodings are classified as either:
    - fixed size (booleans, integers, numbers)
      data is always the same size for that type ;
    - dynamically sized (arbitrary strings and bytes)
      data is of unknown size and requires an explicit length field ;
    - variable size (special case of strings, bytes, and arrays)
      data makes up the remainder of an object of known size,
      thus its size is given by the context, and does not
      have to be serialized.

    JSON operations are delegated to [json-data-encoding]. *)

(** {2 Module structure}

    This [Data_encoding] module provides multiple submodules:
    - {!Encoding} contains the necessary types and constructors for making the
    type descriptors.
    - {!Json}, {!Bson}, and {!Binary} contain functions to serialize and
    deserialize values.

*)

module Encoding : sig
  (** The type descriptors for values of type ['a]. *)
  type 'a t = 'a Encoding.t

  type 'a encoding = 'a t

  type string_json_repr = Hex | Plain

  (** {3 Ground descriptors} *)

  (** {4 voids} *)

  (** Special value [null] in JSON, nothing in binary. *)
  val null : unit encoding

  (** Empty object (not included in binary, encoded as empty object in JSON). *)
  val empty : unit encoding

  (** Unit value, omitted in binary.
      Serialized as an empty object in JSON, accepts any object when deserializing. *)
  val unit : unit encoding

  (** Constant string (data is not included in the binary data). *)
  val constant : string -> unit encoding

  (** {4 ground numerical types}

      All encodings are big-endians.

      - 8-bit integers (signed or unsigned) are encoded over 1 single byte.
      - 16-bit integers (signed or unsigned) are encoded over 2 bytes.
      - 31-bit integers are always signed and always encoded over 4 bytes.
      - 32-bit integers are always signed and always encoded over 4 bytes.
      - 64-bit integers are always signed and always encoded over 8 bytes.

      A note on 31-bit integers. The internal representation of integers in
      OCaml reserves one bit for GC tagging. The remaining bits encode a signed
      integer. For compatibility with 32-bit machine, we restrict these native
      integers to the 31-bit range. *)

  (** Signed 8 bit integer
      (data is encoded as a byte in binary and an integer in JSON). *)
  val int8 : int encoding

  (** Unsigned 8 bit integer
      (data is encoded as a byte in binary and an integer in JSON). *)
  val uint8 : int encoding

  (** Signed 16 bit integer
      (data is encoded as a short in binary and an integer in JSON). *)
  val int16 : int encoding

  (** Unsigned 16 bit integer
      (data is encoded as a short in binary and an integer in JSON). *)
  val uint16 : int encoding

  (** Signed 31 bit integer, which corresponds to type int on 32-bit OCaml systems
      (data is encoded as a 32 bit int in binary and an integer in JSON). *)
  val int31 : int encoding

  (** Signed 32 bit integer
      (data is encoded as a 32-bit int in binary and an integer in JSON). *)
  val int32 : int32 encoding

  (** Signed 64 bit integer
      (data is encoded as a 64-bit int in binary and a decimal string in JSON). *)
  val int64 : int64 encoding

  (** Integer with bounds in a given range. Both bounds are inclusive.

      @raise Invalid_argument if the bounds are beyond the interval
      [-2^30; 2^30-1]. These bounds are chosen to be compatible with all versions
      of OCaml.
  *)
  val ranged_int : int -> int -> int encoding

  (** Integers with the same ranges, restrictions, and representations as above.
      The endianness is explicitly (rather than implicitly) big. *)
  module Big_endian : sig
    val int16 : int encoding

    val uint16 : int encoding

    val int31 : int encoding

    val int32 : int32 encoding

    val int64 : int64 encoding

    val ranged_int : int -> int -> int encoding
  end

  (** Integers with the same ranges and restrictions as above, but in
      little-endian representation. The JSON representation is unaffected. *)
  module Little_endian : sig
    val int16 : int encoding

    val uint16 : int encoding

    val int31 : int encoding

    val int32 : int32 encoding

    val int64 : int64 encoding

    val ranged_int : int -> int -> int encoding
  end

  (** Big number

      In JSON, data is encoded as a string containing the decimal representation
      of the number.

      In binary, data is encoded as a variable length sequence of
      bytes, with a running unary size bit: the most significant bit of
      each byte tells is this is the last byte in the sequence (0) or if
      there is more to read (1). The second most significant bit of the
      first byte is reserved for the sign (positive if zero). Sizing and
      sign bits ignored, data is then the binary representation of the
      absolute value of the number in little-endian order. *)
  val z : Z.t encoding

  (** Positive big number.

      In JSON, data is encoded as a string containing the decimal representation
      of the number.

      In binary, data is encoded similarly to [z] but the sign bit is omitted.
      In other words:

      Data is encoded as a variable length sequence of bytes, with a running
      unary size bit: the most significant bit of each byte tells is this is the
      last byte in the sequence (0) or if there is more to read (1).
      Sizing bits ignored, data is then the binary representation of the
      number in little-endian order. *)
  val n : Z.t encoding

  (** [uint_like_n ()] is an encoding for [int] which uses the same representation
      as {!n}.

      For compatibility with 32-bit machines, this encoding supports the same
      range of encodings as [int31], but only the positive ones. I.e., it
      supports the inclusive range [0] to [(1 lsl 30) - 1].

      The optional parameter [?max_value] can be used to further restrict the
      range of values. If [max_value] is set and is greater than
      [(1 lsl 30) - 1] then the function raises [Invalid_argument].

      The encoding is partial: attempting to de/serialise values which are
      outside of the supported range will fail. In addition, in binary, a
      maximum size for the serialised representation is computed based on the
      maximum value in the range, and the de/serialisation process fails before
      attempting any conversion if the size is exceeded.

      @raise Invalid_argument if [max_value < 0] or
      [max_value > (1 lsl 30) - 1] *)
  val uint_like_n : ?max_value:int -> unit -> int encoding

  (** [int_like_z ()] is an encoding for [int] which uses the same representation
      as {!z}.

      For compatibility with 32-bit machines, this encoding supports the same
      range of encodings as [int31]. I.e., it supports the inclusive range
      [-(1 lsl 30)] to [(1 lsl 30) - 1].

      The optional parameters [?min_value] and [?max_value] can be used to
      further restrict the
      range of values. If [min_value] is set and less than [-(1 lsl 30)] or if
      [max_value] is set and is greater than [(1 lsl 30) - 1] then the function
      raises [Invalid_argument].

      The encoding is partial: attempting to de/serialise values which are
      outside of the supported range will fail. In addition, in binary, a
      maximum size for the serialised representation is computed based on the
      encoding's range, and the de/serialisation process fails before attempting
      any conversion if the size is exceeded.

      @raise Invalid_argument if [max_value < min_value]

      @raise Invalid_argument if [max_value > (1 lsl 30) - 1]

      @raise Invalid_argument if [min_value < -(1 lsl 30)] *)
  val int_like_z : ?min_value:int -> ?max_value:int -> unit -> int encoding

  (** Encoding of floating point number
      (encoded as a floating point number in JSON and a double in binary). *)
  val float : float encoding

  (** Float with bounds in a given range. Both bounds are inclusive *)
  val ranged_float : float -> float -> float encoding

  (** {4 Other ground type encodings} *)

  (** Encoding of a boolean
      (data is encoded as a byte in binary and a boolean in JSON). *)
  val bool : bool encoding

  (** Encoding of a string
      - In binary, encoded as a byte sequence prefixed by the length
        of the string. The length is represented as specified by the
        [length_kind] parameter (default [`Uint30]).
      - in JSON when [string_json_repr = Plain], encoded as a string
      - in JSON when [string_json_repr = Hex],  encoded via hex. *)
  val string' :
    ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
    string_json_repr ->
    string encoding

  (** Encoding of arbitrary bytes. See [string'] *)
  val bytes' :
    ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
    string_json_repr ->
    Bytes.t encoding

  (** same as [string' Plain] *)
  val string : string encoding

  (** same as [bytes' Hex] *)
  val bytes : Bytes.t encoding

  type bigstring =
    (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

  (** same as [string'] but for bigstring. *)
  val bigstring :
    ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
    ?string_json_repr:string_json_repr ->
    unit ->
    bigstring encoding

  (** {3 Descriptor combinators} *)

  (** Combinator to make an optional value
      (represented as a 1-byte tag followed by the data (or nothing) in binary
       and either the raw value or a null in JSON).

      Note that the JSON representation is only weakly discriminating.
      Specifically, the value [Some None] is represented as the raw value [None]
      and so the two are indistinguishable. For this reason, this combinator
      does not support nesting, nor does it support use within a recursive
      ({!mu}) encoding.

      @raise Invalid_argument if called on an encoding which may be represented
      as [null] in JSON. This includes an encoding of the form [option _],
      [conv _ _ (option _)], [dynamic_size (option _)], etc.

      @raise Invalid_argument if called within the body of a {!mu}.
       *)
  val option : 'a encoding -> 'a option encoding

  (** Combinator to make a {!result} value
      (represented as a 1-byte tag followed by the data of either type in binary,
       and either unwrapped value in JSON (the caller must ensure that both
       encodings do not collide)). *)
  val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding

  (** Array combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of all the element in binary
       prefixed by its size in bytes

      @param [max_length]
      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is variable. *)
  val array : ?max_length:int -> 'a encoding -> 'a array encoding

  (** Array combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of its length (number of elements) and all
        the element in binary

      @param kind ([[`N | `Uint8 | `Uint16 | `Uint30]]) controls the
      representation of the length: {!uint_like_n}, {!uint8}, {!uint16}, or
      {!int31} (but only positive values).

      @param [max_length]
      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is variable. *)
  val array_with_length :
    ?max_length:int ->
    [`N | `Uint8 | `Uint16 | `Uint30] ->
    'a encoding ->
    'a array encoding

  (** List combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of all the element in binary
        prefixed by its size in bytes

      @param [max_length]
      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is variable. *)
  val list : ?max_length:int -> 'a encoding -> 'a list encoding

  (** List combinator.
      - encoded as an array in JSON
      - encoded as the concatenation of its length (number of elements) and all
        the element in binary

      @param kind ([[`N | `Uint8 | `Uint16 | `Uint30]]) controls the
      representation of the length: {!uint_like_n}, {!uint8}, {!uint16}, or
      {!int31} (but only positive values).


      @param [max_length]
      If [max_length] is passed and the encoding of elements has fixed
      size, a {!check_size} is automatically added for earlier rejection.

      @raise Invalid_argument if the inner encoding is variable. *)
  val list_with_length :
    ?max_length:int ->
    [`N | `Uint8 | `Uint16 | `Uint30] ->
    'a encoding ->
    'a list encoding

  (** Provide a transformer from one encoding to a different one.

      Used to simplify nested encodings or to change the generic tuples
      built by {!obj1}, {!tup1} and the like into proper records.

      A schema may optionally be provided as documentation of the new encoding. *)
  val conv :
    ('a -> 'b) ->
    ('b -> 'a) ->
    ?schema:Json_schema.schema ->
    'b encoding ->
    'a encoding

  (** [conv_with_guard] is similar to {!conv} but the function that takes in the value
      from the outside (untrusted) world has a chance to fail.

      Specifically, if the function returns [Error msg] then the decoding is
      interrupted with an error carrying the message [msg]. If the function
      returns [Ok _] then the decoding proceeds normally. *)
  val conv_with_guard :
    ('a -> 'b) ->
    ('b -> ('a, string) result) ->
    ?schema:Json_schema.schema ->
    'b encoding ->
    'a encoding

  (** [with_decoding_guard g e] is similar to [e] but decoding fails if [g]
      returns [Error _] on the decoded value. *)
  val with_decoding_guard :
    ('a -> (unit, string) result) -> 'a encoding -> 'a encoding

  (** Association list.
      An object in JSON, a list of pairs in binary. *)
  val assoc : 'a encoding -> (string * 'a) list encoding

  (** {3 Product descriptors} *)

  (** An enriched encoding to represent a component in a structured
      type, augmenting the encoding with a name and whether it is a
      required or optional. Fields are used to encode OCaml tuples as
      objects in JSON, and as sequences in binary, using combinator
      {!obj1} and the like. *)
  type 'a field

  (** Required field. *)
  val req :
    ?title:string -> ?description:string -> string -> 't encoding -> 't field

  (** Optional field. Omitted entirely in JSON encoding if None.
      Omitted in binary if the only optional field in a [`Variable]
      encoding, otherwise a 1-byte prefix (`0` or `255`) tells if the
      field is present or not. *)
  val opt :
    ?title:string ->
    ?description:string ->
    string ->
    't encoding ->
    't option field

  (** Optional field of variable length.
      Only one can be present in a given object. *)
  val varopt :
    ?title:string ->
    ?description:string ->
    string ->
    't encoding ->
    't option field

  (** Required field with a default value.
      If the default value is passed, the field is omitted in JSON.
      The value is always serialized in binary. *)
  val dft :
    ?title:string ->
    ?description:string ->
    string ->
    't encoding ->
    't ->
    't field

  (** {4 Constructors for objects with N fields} *)

  (** These are serialized to binary by converting each internal
      object to binary and placing them in the order of the original
      object. These are serialized to JSON as a JSON object with the
      field names. An object might only contains one 'variable'
      field, typically the last one. If the encoding of more than one
      field are 'variable', the first ones should be wrapped with
      [dynamic_size].

      @raise Invalid_argument if more than one field is a variable one. *)

  val obj1 : 'f1 field -> 'f1 encoding

  val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding

  val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding

  val obj4 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    ('f1 * 'f2 * 'f3 * 'f4) encoding

  val obj5 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

  val obj6 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    'f6 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

  val obj7 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    'f6 field ->
    'f7 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding

  val obj8 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    'f6 field ->
    'f7 field ->
    'f8 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding

  val obj9 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    'f6 field ->
    'f7 field ->
    'f8 field ->
    'f9 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding

  val obj10 :
    'f1 field ->
    'f2 field ->
    'f3 field ->
    'f4 field ->
    'f5 field ->
    'f6 field ->
    'f7 field ->
    'f8 field ->
    'f9 field ->
    'f10 field ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

  (** Create a larger object from the encodings of two smaller ones.
      @raise Invalid_argument if both arguments are not objects  or if both
      tuples contains a variable field.. *)
  val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding

  (** [With_field_name_duplicate_checks] is a subset of [Encoding] where all the
      constructed objects are checked for duplicates.

      Note that the analysis can include false positives: it might fail on
      encodings which will never serialise a value with duplicate fields.
      Still, these false positives are uncommon and we recommend you use these
      combinators when relevant.

      {[
      let e =
        let open Data_encoding in
        let open Data_encoding.With_field_name_duplicate_checks in
        …
      ]}
      *)
  module With_field_name_duplicate_checks : sig
    val obj1 : 'f1 field -> 'f1 encoding

    val obj2 : 'f1 field -> 'f2 field -> ('f1 * 'f2) encoding

    val obj3 : 'f1 field -> 'f2 field -> 'f3 field -> ('f1 * 'f2 * 'f3) encoding

    val obj4 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      ('f1 * 'f2 * 'f3 * 'f4) encoding

    val obj5 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

    val obj6 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

    val obj7 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding

    val obj8 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding

    val obj9 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      'f9 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding

    val obj10 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      'f9 field ->
      'f10 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

    (** Create a larger object from the encodings of two smaller ones.
      @raise Invalid_argument if both arguments are not objects  or if both
      tuples contains a variable field.. *)
    val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding
  end

  (** {4 Constructors for tuples with N fields} *)

  (** These are serialized to binary by converting each internal
      object to binary and placing them in the order of the original
      object. These are serialized to JSON as JSON arrays/lists.  Like
      objects, a tuple might only contains one 'variable' field,
      typically the last one. If the encoding of more than one field
      are 'variable', the first ones should be wrapped with
      [dynamic_size].

      @raise Invalid_argument if more than one field is a variable one. *)

  val tup1 : 'f1 encoding -> 'f1 encoding

  val tup2 : 'f1 encoding -> 'f2 encoding -> ('f1 * 'f2) encoding

  val tup3 :
    'f1 encoding -> 'f2 encoding -> 'f3 encoding -> ('f1 * 'f2 * 'f3) encoding

  val tup4 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4) encoding

  val tup5 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5) encoding

  val tup6 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    'f6 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) encoding

  val tup7 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    'f6 encoding ->
    'f7 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) encoding

  val tup8 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    'f6 encoding ->
    'f7 encoding ->
    'f8 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) encoding

  val tup9 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    'f6 encoding ->
    'f7 encoding ->
    'f8 encoding ->
    'f9 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) encoding

  val tup10 :
    'f1 encoding ->
    'f2 encoding ->
    'f3 encoding ->
    'f4 encoding ->
    'f5 encoding ->
    'f6 encoding ->
    'f7 encoding ->
    'f8 encoding ->
    'f9 encoding ->
    'f10 encoding ->
    ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) encoding

  (** Create a large tuple encoding from two smaller ones.
      @raise Invalid_argument if both values are not tuples or if both
      tuples contains a variable field. *)
  val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

  (** {3 Sum descriptors} *)

  (** A partial encoding to represent a case in a variant type.  Hides
      the (existentially bound) type of the parameter to the specific
      case, providing its encoder, and converter functions to and from
      the union type. *)
  type 't case

  type case_tag = Tag of int | Json_only

  (** A sum descriptor can be optimized by providing a specific
     [matching_function] which efficiently determines in which case
     some value of type ['a] falls.

     Note that in general you should use a total function (i.e., one defined
     over the whole of the ['a] type) for the [matching_function]. However, in
     the case where you have a good reason to use a partial function, you should
     raise [No_case_matched] in the dead branches. Reasons why you may want to
     do so include:
     - ['a] is an open variant and you will complete the matching function
       later, and
     - there is a code invariant that guarantees that ['a] is not fully
       inhabited.
     *)
  type 'a matching_function = 'a -> match_result

  and match_result

  (** [matched t e u] represents the fact that a value is tagged with [t] and
      carries the payload [u] which can be encoded with [e].

      The optional argument [tag_size] must match the one passed to the
      {!matching} function [matched] is called inside of.

      An example is given in the documentation of {!matching}.

      @raise [Invalid_argument] if [t < 0]

      @raise [Invalid_argument] if [t] does not fit in [tag_size] *)
  val matched :
    ?tag_size:[`Uint8 | `Uint16] -> int -> 'a encoding -> 'a -> match_result

  (** Encodes a variant constructor. Takes the encoding for the specific
      parameters, a recognizer function that will extract the parameters
      in case the expected case of the variant is being serialized, and
      a constructor function for deserialization.

      The tag must be less than the tag size of the union in which you use the case.
      An optional tag gives a name to a case and should be used to maintain
      compatibility.

      An optional name for the case can be provided, which is used in the binary
      documentation.

      @raise [Invalid_argument] if [case_tag] is [Tag t] with [t < 0]
      *)
  val case :
    title:string ->
    ?description:string ->
    case_tag ->
    'a encoding ->
    ('t -> 'a option) ->
    ('a -> 't) ->
    't case

  (** Create a single encoding from a series of cases.

     In JSON, all cases are tried one after the other using the [case list]. The
     caller is responsible for avoiding collisions. If there are collisions
     (i.e., if multiple cases produce the same JSON output) then the encoding
     and decoding processes might not be inverse of each other. In other words,
     [destruct e (construct e v)] may not be equal to [v].

     In binary, a prefix tag is added to discriminate quickly between
     cases. The default is [`Uint8] and you must use a [`Uint16] if
     you are going to have more than 256 cases.

     The matching function is used during binary encoding of a value
     [v] to efficiently determine which of the cases corresponds to
     [v]. The case list is used during decoding to reconstruct a value based on
     the encoded tag. (Decoding is optimised internally: tag look-up has a
     constant cost.)

     The caller is responsible for ensuring that the [matching_function] and the
     [case list] describe the same encoding. If they describe different
     encodings, then the decoding and encoding processes will not be inverses of
     each others. In other words, [of_bytes e (to_bytes e v)] will not be equal
     to [v].

     If you do not wish to be responsible for this, you can use the unoptimised
     {!union} that uses a [case list] only (see below). Beware that in {!union}
     the complexity of the encoding is linear in the number of cases.

     Following: a basic example use. Note that the [matching_function] uses the
     same tags, payload conversions, and payload encoding as the [case list].

{[
type t = A of string | B of int * int | C
let encoding_t =
  (* Tags and payload encodings for each constructors *)
  let a_tag = 0 and a_encoding = string in
  let b_tag = 1 and b_encoding = obj2 (req "x" int) (req "y" int) in
  let c_tag = 2 and c_encoding = unit in
  matching
    (* optimised encoding function *)
    (function
       | A s -> matched a_tag a_encoding s
       | B (x, y) -> matched b_tag b_encoding (x, y)
       | C -> matched c_tag c_encoding ())
    (* decoding case list *)
    [
       case ~title:"A"
         (Tag a_tag)
         a_encoding
         (function A s -> Some s | _ -> None) (fun s -> A s);
       case ~title:"B"
         (Tag b_tag)
         b_encoding
         (function B (x, y) -> Some (x, y) | _ -> None) (fun (x, y) -> B (x, y));
       case ~title:"C"
         (Tag c_tag)
         c_encoding
         (function C -> Some () | _ -> None) (fun () -> C);
    ]
]}

     @raise [Invalid_argument] if it is given an empty [case list]

     @raise [Invalid_argument] if there are more than one [case] with the same
     [tag] in the [case list]

     @raise [Invalid_argument] if there are more cases in the [case list] than
     can fit in the [tag_size] *)
  val matching :
    ?tag_size:[`Uint8 | `Uint16] ->
    't matching_function ->
    't case list ->
    't encoding

  (** Same as matching except that the matching function is
      a linear traversal of the cases.

     @raise [Invalid_argument] if it is given an empty [case list]

     @raise [Invalid_argument] if there are more than one [case] with the same
     [tag] in the [case list]

     @raise [Invalid_argument] if there are more cases in the [case list] than
     can fit in the [tag_size] *)
  val union : ?tag_size:[`Uint8 | `Uint16] -> 't case list -> 't encoding

  (** [With_JSON_discriminant] is a subset of [Encoding] where the
      union/matching combinators (and associated functions) add discriminant for
      the JSON backend.

      The following restrictions apply:
      - The case encodings must be objects.
      - The case encoding objects must not include a "kind" field.
      - The case encoding objects must not have duplicate field names.
      - The JSON discriminants must all be distinct.

      {[
      let e =
        let open Data_encoding in
        let open Data_encoding.With_JSON_discriminant in
        …
      ]} *)
  module With_JSON_discriminant : sig
    (** [case_tag]'s only variant [Tag] includes both a numeric tag for the binary
      encoding and a string tag for the JSON encoding. *)
    type case_tag = Tag of (int * string)

    type 't case

    (** [case] is similar to [Encoding.case] but it takes a
      [SaferEncoding.case_tag] parameter. This includes both a numeric tag and a
      string tag.

      In Binary:
      This has no impact. The [case_tag] argument of [Encoding] already has a
      numeric tag.

      In JSON:
      The [SaferEncoding] adds a field for discriminating the different cases,
      making these encodings less likely to include accidental bugs. More
      specifically, when you use [case (Tag (_, s)) e _ _] then the underlying
      union uses an encoding based on [e] and [s]. Specifically, if [e] is an
      object encoding, then it adds the field [(req "kind" (constant s))] to
      [e].

      @raise Invalid_argument if [e] is not an object.

      @raise Invalid_argument if [e] is an object with a ["kind"] field (this
      field name is reserved for the discriminating field added by [case]). *)
    val case :
      title:string ->
      ?description:string ->
      case_tag ->
      'a encoding ->
      ('t -> 'a option) ->
      ('a -> 't) ->
      't case

    (** [union] and [matching] now check that there are no duplicate ["kind"]
      discriminating values. If there is, they raises [Invalid_argument]. *)

    (** Similarly to [case_tag], [matched] also takes an additional [string]
      parameter. This parameter is used in the same way as [case] (to add a ["kind"] field
      to the JSON encoding) and it fails in the same way as [case].

      @raise Invalid_argument if the encoding is not an object.

      @raise Invalid_argument if the encoding is an object with a ["kind"]
      field. *)
    val matched :
      ?tag_size:[`Uint8 | `Uint16] ->
      int * string ->
      'a encoding ->
      'a ->
      match_result

    val matching :
      ?tag_size:[`Uint8 | `Uint16] ->
      't matching_function ->
      't case list ->
      't encoding

    val union : ?tag_size:[`Uint8 | `Uint16] -> 't case list -> 't encoding
  end

  (** {3 Predicates over descriptors} *)

  (** Is the given encoding serialized as a JSON object? *)
  val is_obj : 'a encoding -> bool

  (** Does the given encoding encode a tuple? *)
  val is_tup : 'a encoding -> bool

  (** Classify the binary serialization of an encoding as explained in the
      preamble. *)
  val classify : 'a encoding -> [`Fixed of int | `Dynamic | `Variable]

  (** {3 Specialized descriptors} *)

  (** Encode enumeration via association list
      - represented as a string in JSON and
      - represented as an integer representing the element's position
        in the list in binary. The integer size depends on the list size.*)
  val string_enum : (string * 'a) list -> 'a encoding

  (** Create encodings that produce data of a fixed length when binary encoded.
      See the preamble for an explanation. *)
  module Fixed : sig
    (** @raise Invalid_argument if the argument is less or equal to zero. *)
    val string : int -> string encoding

    (** @raise Invalid_argument if the argument is less or equal to zero. *)
    val string' : string_json_repr -> int -> string encoding

    (** @raise Invalid_argument if the argument is less or equal to zero. *)
    val bytes : int -> Bytes.t encoding

    (** @raise Invalid_argument if the argument is less or equal to zero. *)
    val bigstring :
      ?string_json_repr:string_json_repr -> int -> bigstring encoding

    (** @raise Invalid_argument if the argument is less or equal to zero. *)
    val bytes' : string_json_repr -> int -> Bytes.t encoding

    (** [add_padding e n] is a padded version of the encoding [e]. In Binary,
        there are [n] null bytes ([\000]) added after the value encoded by [e].
        In JSON, padding is ignored.

        @raise Invalid_argument if [n <= 0]. *)
    val add_padding : 'a encoding -> int -> 'a encoding

    (** [list n e] is an encoding for lists of exactly [n] elements. If a list
        of more or fewer elements is provided, then the encoding fails with the
        [write_error List_invalid_length]. For decoding, it can fail with
        [read_error Not_enough_data] or [read_error Extra_bytes], or it may
        cause other failures further down the line when the AST traversal
        becomes out-of-sync with the underlying byte-stream traversal.

        The difference of the errors being used when encoding and decoding is
        because when encoding we have access to the list and we can check the
        actual length, whereas when decoding we only see bytes, sometimes too
        many, sometimes not enough.

        This encoding has a narrow set of possible applications because it is
        very restrictive. Still, it can to:
        - mirror static guarantees about the length of some lists,
        - special-case some common lengths of typical input in a union (see
          example below),
        - other ends.

{[
type expr =
  | Op of string * expr list (* most commonly 1 or 2 operands *)
  | Literal of string
let expr_encoding =
  mu "expr" (fun e ->
    union [
      case ~title:"op-nonary" (Tag 0)
        string
        (function Op (op, []) -> Some op | _ -> None)
        (fun op -> Op (op, []));
      case ~title:"op-unary" (Tag 1)
        (tup2 string (Fixed.list 1 e))
        (function Op (op, ([_]) as operand) -> Some (op, operand) | _ -> None)
        (fun (op, operand) -> Op (op, operand));
      case ~title:"op-binary" (Tag 2)
        (tup2 string (Fixed.list 2 e))
        (function Op (op, ([_;_]) as operand) -> Some (op, operand) | _ -> None)
        (fun (op, operand) -> Op (op, operand));
      case ~title:"op-moreary" (Tag 3)
        (tup2 string (list e))
        (function Op (op, operand) -> Some (op, operand) | _ -> None)
        (fun (op, operand) -> Op (op, operand));
      case ~title:"literal" (Tag 4)
        string
        (function Literal l -> Some l | _ -> None)
        (fun l -> Literal l);
        ]
  )
]}

        Interestingly, the cases for known lengths can be generated
        programmatically.

        @raise Invalid_argument if the argument [n] is less or equal to zero.

        @raise Invalid_argument if the argument [e] is a [`Variable]-size
        encoding or a zero-byte encoding. *)
    val list : int -> 'a encoding -> 'a list encoding

    (** See [list] above.

        @raise Invalid_argument if the argument [n] is less or equal to zero.

        @raise Invalid_argument if the argument [e] is a [`Variable]-size
        encoding or a zero-byte encoding. *)
    val array : int -> 'a encoding -> 'a array encoding
  end

  (** Create encodings that produce data of a variable length when binary encoded.
      See the preamble for an explanation. *)
  module Variable : sig
    val string : string encoding

    val string' : string_json_repr -> string encoding

    val bytes : Bytes.t encoding

    val bytes' : string_json_repr -> Bytes.t encoding

    val bigstring :
      ?string_json_repr:string_json_repr -> unit -> bigstring encoding

    (** @raise Invalid_argument if the encoding argument is variable length
        or may lead to zero-width representation in binary. *)
    val array : ?max_length:int -> 'a encoding -> 'a array encoding

    (** @raise Invalid_argument if the encoding argument is variable length
        or may lead to zero-width representation in binary. *)
    val list : ?max_length:int -> 'a encoding -> 'a list encoding
  end

  module Bounded : sig
    (** Encoding of a string whose length does not exceed the specified length.

        If [length_kind] is set, then it is used to encode the length of the
        string in a header. If [length_kind] is omitted then the length field
        uses the smallest fixed-width integer that can accommodate the
        maximum size - e.g., [`Uint8] for very short strings, [`Uint16] for
        longer strings, etc.

        Attempting to construct a string with a length that is too long causes
        an [Invalid_argument] exception.

        @raise Invalid_argument if [length_kind] is set but it cannot accommodate
        the specified bound. E.g.,
        [Bounded.string' ~length_kind:`Uint8 Hex 1000] raises.

        @raise Invalid_argument if specified bound is negative or null.

        @raise Invalid_argument if [length_kind] is unset and the specified
        bound is larger than 2^30. *)
    val string' :
      ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
      string_json_repr ->
      int ->
      string encoding

    (** Same as [string' Plain] *)
    val string : int -> string encoding

    (** See {!string'} above. *)
    val bytes' :
      ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
      string_json_repr ->
      int ->
      Bytes.t encoding

    (** Same as [bytes' Hex] *)
    val bytes : int -> Bytes.t encoding

    (** Same as [string] but for {!type-bigstring}.

        @param [?string_json_repr] defaults to [Hex].

        @raise Invalid_argument if [length_kind] is set but it cannot accommodate
        the specified bound. E.g.,
        [Bounded.string' ~length_kind:`Uint8 Hex 1000] raises.

        @raise Invalid_argument if specified bound is negative or null. *)
    val bigstring :
      ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
      ?string_json_repr:string_json_repr ->
      int ->
      bigstring encoding
  end

  (** Mark an encoding as being of dynamic size.
      Forces the size to be stored alongside content when needed.
      Typically used to combine two variable encodings in a same
      objects or tuple, or to use a variable encoding in an array or a list. *)
  val dynamic_size :
    ?kind:[`N | `Uint30 | `Uint16 | `Uint8] -> 'a encoding -> 'a encoding

  (** [check_size size encoding] ensures that the binary encoding
      of a value will not be allowed to exceed [size] bytes. The reader
      and the writer fails otherwise. This function do not modify
      the JSON encoding.

      @raise Invalid_argument if [size < 0]
  *)
  val check_size : int -> 'a encoding -> 'a encoding

  (** Recompute the encoding definition each time it is used.
      Useful for dynamically updating the encoding of values of an extensible
      type via a global reference (e.g., exceptions). *)
  val delayed : (unit -> 'a encoding) -> 'a encoding

  (** Define different encodings for JSON and binary serialization. *)
  val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

  (** Combinator for recursive encodings.

     Notice that the function passed to [mu] must be pure. Otherwise,
     the behavior is unspecified.

     A stateful recursive encoding can still be put under a [delayed]
     combinator to make sure that a new encoding is generated each
     time it is used. Caching the encoding generation when the state
     has not changed is then the responsability of the client.

  *)
  val mu :
    string ->
    ?title:string ->
    ?description:string ->
    ('a encoding -> 'a encoding) ->
    'a encoding

  (** {3 Documenting descriptors} *)

  (** Give a name to an encoding and optionally
      add documentation to an encoding. *)
  val def :
    string -> ?title:string -> ?description:string -> 't encoding -> 't encoding

  (** See {!lazy_encoding} below.*)
  type 'a lazy_t

  (** Combinator to have a part of the binary encoding lazily deserialized.
      This is transparent on the JSON side. *)
  val lazy_encoding : 'a encoding -> 'a lazy_t encoding

  (** Force the decoding (memoized for later calls), and return the
      value if successful. *)
  val force_decode : 'a lazy_t -> 'a option

  (** Obtain the bytes without actually deserializing.  Will serialize
      and memoize the result if the value is not the result of a lazy
      deserialization. *)
  val force_bytes : 'a lazy_t -> Bytes.t

  (** Make a lazy value from an immediate one. *)
  val make_lazy : 'a encoding -> 'a -> 'a lazy_t

  (** Apply on structure of lazy value, and combine results *)
  val apply_lazy :
    fun_value:('a -> 'b) ->
    fun_bytes:(Bytes.t -> 'b) ->
    fun_combine:('b -> 'b -> 'b) ->
    'a lazy_t ->
    'b

  module Compact : sig
    (** This module provides specialized encoding combinators that are
        implemented to reduce the size of the serialization result.

        The main trick this module relies on is the notion of “shared tags”.
        In [Data_encoding], the [union] combinator uses (at least) one byte
        every time it is used, to “tag” the output and distinguish between
        various disjunction cases. As a consequence, if [n] [union] are
        composed together to define one encoding, (at least) [n] bytes are
        being allocated. However, in practice, only few bits are used in
        each tags, which means the rest is “wasted.”

        As an example, consider this type:

        {[
        type t =
          | T1 of { f1 : int option; f2 : (int, bool) Either.t }
          | T2 of { f3: int }
        ]}

        A value of [t] using the constructor [T1] will be serialized into
        a binary array of this form:

        {v
        ┌────────┬─────────┬─────────────┬─────────┬─────────────┐
        │ tag(t) │ tag(f1) │ payload(f1) │ tag(f2) │ payload(f2) │
        └────────┴─────────┴─────────────┴─────────┴─────────────┘
          1 byte   1 byte    N bytes       1 byte    M bytes
        v}

        Where [tag(f)] is a value used by [Data_encoding] to distinguish
        between several encoding alternatives for [f], and [payload(f)] is
        the resulting binary array.

        For both [option] and [Either.t], the tag of the encoding only uses
        one bit in practice. Which means that for [T1], encoding the pair
        [(f1, f2)] needs two bits, but the default approach of
        [Data_encoding] uses two {i bytes} instead.  Similarly, to
        distinguish between [T1] and [T2] needs one bit, but the default
        approach is to use an additional tag (one additional {i byte}).

        This module provides an approach to tackle this issue, by
        allocating only one tag ({i i.e.}, one byte) that is used to store
        the useful bits to distinguish between the disjunction cases. We
        call this tag the “shared tag” of the encoding. The bits of the
        shared tag describes precisely the layout of the encoded data.

        For instance, considering a compact encoding for [t], the third
        bit of the tag can be used to distinguish between [T1] and [T2].
        In case the third bit is 0, the first bit of the tag determines
        the case of [option], and the second the case of [Either.t].

        As a consequence the resulting binary array for the constructor
        [T1] is, using
        - [_] to represent meaningless bits,
        - [0] and [1] to represent actual bit values,
        - [e] to represent the bit used to distinguish the [Either] case of [f2], and
        - [o] to represent the bit used to distinguish the [Option] case of [f1]:

        {v
        ┌──────────┬─────────────┬─────────────┐
        │ _____0eo │ payload(f1) │ payload(f2) │
        └──────────┴─────────────┴─────────────┘
          1 byte     N bytes       M bytes
        v}

        while the resulting binary array for the constructor [T2] is

        {v
        ┌──────────┬─────────────┐
        │ _____100 │ payload(f3) │
        └──────────┴─────────────┘
          1 byte     N bytes
        v} *)

    (** The description of a compact encoding. *)
    type 'a t

    (** Turn a compact encoding into a regular {!Data_encoding.t}.

        @param tag_size controls the size of the tag used to discriminate the
        values. Note that in data-encoding, all the writes and reads are byte
        aligned so the tag must fit in either 0 ([`Uint0]), 1 ([`Uint8]), or 2
        ([`Uint16]) bytes.

        The default is [`Uint0], i.e., no tag at all. This is can only represent
        values which use 0 bits of tags.

        It is recommended to set the [tag_size] explicitly.

        @raise Invalid_argument if the shared tags cannot fit in [tag_size]
        space. *)
    val make : ?tag_size:[`Uint0 | `Uint8 | `Uint16] -> 'a t -> 'a encoding

    (** [tag_bit_count c] is the number of bits of tag that a compact encoding
        uses. *)
    val tag_bit_count : 'a t -> int

    (** {1 Combinators} *)

    (** Similarly to [Data_encoding], we provide various combinators to
        compose compact encoding together. *)

    (** {2 Base types} *)

    (** A type with no inhabitant. *)
    type void

    (** A compact encoding used to denote an impossible case inside of
        conjunction operators such as [union].

        Uses 0 bit of tag. *)
    val void : void t

    (** [refute x] can be used to refute a branch of a [match] which
        exhibits a value of type [void]. *)
    val refute : void -> 'a

    (** A compact encoding of the singleton value [unit], which has zero
        memory footprint.

        Uses zero (0) bits of tag.

        In JSON it is represented as the empty object [{}]. *)
    val unit : unit t

    (** A compact encoding of the singleton value [unit], which has zero
        memory footprint.

        Uses zero (0) bits of tag.

        In JSON it is represented as [null]. *)
    val null : unit t

    (** Efficient encoding of boolean values. It uses one (1) bit in the
        shared tag, and zero bit in the payload. *)
    val bool : bool t

    (** [payload encoding] unconditionally uses [encoding] in the
        payload, and uses zero (0) bit in the shared tag. *)
    val payload : 'a encoding -> 'a t

    (** Uses one (1) bit in the tag to encode an option. *)
    val option : 'a t -> 'a option t

    (** {2 Conversion} *)

    (** [conv ?json f g e] reuses the encoding [e] for type [b] to encode
        a type [a] using the isomorphism [(f, g)]. The optional argument
        allows to overwrite the encoding used for JSON, in place of the
        one computed by default. *)
    val conv : ?json:'a encoding -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

    (** {2 Conjunctions} *)

    (** [tup1 e] wraps the underlying encoding of [e] in a [tup1] (from the
        parent module). This is only useful in that, provided you use
        [make ~tag_size:`Uint0] to translate the returned compact encoding, it
        allows you to call [merge_tups] on it.

        Uses as many bits of tag as [e]. *)
    val tup1 : 'a t -> 'a t

    (** [tup2 e1 e2] concatenates the shared tags and payloads of [e1] and
        [e2].

        Uses as many bits of tags as the sum of the tags used by its arguments. *)
    val tup2 : 'a t -> 'b t -> ('a * 'b) t

    (** [tup3 e1 e2 e3] concatenates the shared tags and payloads of [e1],
        [e2], and [e3].

        Uses as many bits of tags as the sum of the tags used by its arguments. *)
    val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

    (** [tup4 e1 e2 e3 e4] concatenates the shared tags and payloads of
        [e1], [e2], [e3] and [e4].

        Uses as many bits of tags as the sum of the tags used by its arguments. *)
    val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

    val tup5 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5) t

    val tup6 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      'f6 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) t

    val tup7 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      'f6 t ->
      'f7 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) t

    val tup8 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      'f6 t ->
      'f7 t ->
      'f8 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) t

    val tup9 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      'f6 t ->
      'f7 t ->
      'f8 t ->
      'f9 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) t

    val tup10 :
      'f1 t ->
      'f2 t ->
      'f3 t ->
      'f4 t ->
      'f5 t ->
      'f6 t ->
      'f7 t ->
      'f8 t ->
      'f9 t ->
      'f10 t ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) t

    type 'a field

    (** [req "f" compact] can be used in conjunction with [objN] to create
        compact encoding with more readable JSON encoding, as an
        alternative of [tupN]. The JSON output is a dictionary which
        contains the field [f] with a value encoded using [compact]. *)
    val req : string -> 'a t -> 'a field

    (** Same as {!req}, but the field is optional.

        An [objN] compact encoding uses as many bits of tags as its number of
        [opt] fields. *)
    val opt : string -> 'a t -> 'a option field

    (** [obj1] can be used in conjunction with [req] or [opt] to produce
        more readable JSON outputs.

        Uses as many bits of tags as there are [opt] fields in its arguments. *)
    val obj1 : 'a field -> 'a t

    (** An alternative to [tup2] which can be used in conjunction with
        {!req} and {!opt} to produce more readable JSON outputs based on
        dictionary.

        Uses as many bits of tags as there are [opt] fields in its arguments. *)
    val obj2 : 'a field -> 'b field -> ('a * 'b) t

    (** An alternative to [tup3] which can be used in conjunction with
        {!req} and {!opt} to produce more readable JSON outputs based on
        dictionary.

        Uses as many bits of tags as there are [opt] fields in its arguments. *)
    val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t

    (** An alternative to [tup4] which can be used in conjunction with
        {!req} and {!opt} to produce more readable JSON outputs based on
        dictionary.

        Uses as many bits of tags as there are [opt] fields in its arguments. *)
    val obj4 :
      'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

    val obj5 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5) t

    val obj6 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6) t

    val obj7 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7) t

    val obj8 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8) t

    val obj9 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      'f9 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9) t

    val obj10 :
      'f1 field ->
      'f2 field ->
      'f3 field ->
      'f4 field ->
      'f5 field ->
      'f6 field ->
      'f7 field ->
      'f8 field ->
      'f9 field ->
      'f10 field ->
      ('f1 * 'f2 * 'f3 * 'f4 * 'f5 * 'f6 * 'f7 * 'f8 * 'f9 * 'f10) t

    (** A compact encoding for [int32] values. It uses 2 bits in the
        shared tag, to determine how many bytes are used in the payload:

        {ul {li [00]: from 0 to 255, one byte.}
            {li [01]: from 256 to 65,535, two bytes.}
            {li [10]: from 65,536 to [Int32.max_int] and for negative values, four bytes.}}

        Note that by itself, this compact encoding is not necessarily more
        economical in space. However, in combination with other compact
        encodings (say, when you have two bits of tag to spare anyway) or given
        a very skewed distribution of values (say, when the vast majority of
        your values are in the 0–255 interval), then it can help you save some
        space.

        Uses two (2) bits of tag. *)
    val int32 : int32 t

    (** A compact encoding for [int64] values. It uses 2 bits in the
        shared tag, to determine how many bytes are used in the payload:

        {ul {li [00]: from 0 to 255, one byte.}
            {li [01]: from 256 to 65,535, two bytes.}
            {li [10]: from 65,536 to 4,294,967,295 four bytes.}
            {li [11]: from 4,294,967,295 and for negative values eight bytes.}}

        See {!int32} for usage recommendations.

        Uses two (2) bits of tag. *)
    val int64 : int64 t

    (** [list ~bits:n encoding] uses [n] bits in the shared tag to encode the
        size of small lists.

        For instance, [list ~bits:2 encoding],

        {ul {li [00]: the payload is empty, because it is the empty list}
            {li [01]: the singleton list, whose element is encoded using
                [encoding].}
            {li [10]: a list of two elements encoded with [encoding]}
            {li [11]: a list of more than two elements, prefixed with its
                encoded size (i.e., the number of bytes it takes to represent
                the whole value) (which uses 4 bytes)}}

        With [~bits:3], lists of 0 to 6 items are encoded with tags [000] to
        [110], and lists of 7 or more are encoded with tag [111] and the
        length.

        Uses [n] bits of tags. *)
    val list : bits:int -> 'a encoding -> 'a list t

    (** {2 Disjunctions} *)

    type 'a case

    (** Usage: [case name encode decode encoding]

        Intended to be used inside a [union]. *)
    val case :
      title:string ->
      ?description:string ->
      'b t ->
      ('a -> 'b option) ->
      ('b -> 'a) ->
      'a case

    (** [union cases] creates a new compact encoding to encompass a
        disjunction of cases.

        The value uses some tag bits to distinguish the different cases of the
        union (see discussion of parameter [union_tag_bits]) and some tag bits
        (potentially 0) to distinguish the values within a case (see discussion
        of parameter [cases_tag_bits]).

        E.g., Given [type t = A of bool | B of int option] and the encoding
        {v
        let c =
          union [
            case "A" (function A b -> Some b | _ -> None) (fun b -> A b) bool;
            case "B" (function B i -> Some i | _ -> None) (fun i -> B b) (option (payload int));
        in
        make ~tag_size:`Uint8 c
        v}
        then a value can have either of the following 4 tags:
        - 0b00000000: case [A], [false]
        - 0b00000001: case [A], [true]
        - 0b00000010: case [B], [Some] (a payload of 4 bytes follows)
        - 0b00000011: case [B], [None]

        In other words, the second bit of this tag is used to discriminate the
        cases of the union, whilst the first bit is used to discriminate within
        each case.

        Note that the compact union can be combined with more compact encoding
        before being passed to [make] in which case the two bits of tags will be
        combined with the tags of the other compact encodings. E.g.,
        [make ~tag_size:`Uint8 (tup2 c c)].

        @param union_tag_bits is the number of bits used to distinguish the
        different cases of the union. For example, if the union has 4 cases
        (i.e., if [List.length cases = 4]) then you can use [~union_tag_bits:2].

        If not provided explicitly, [union_tag_bits] is inferred: it is set to
        the smallest value which can accommodate the provided [cases].

        It is recommended to set [union_tag_bits] explicitly.

        You can over-provision the [union_tag_bits] if you expect the
        [cases] to grow in the future.

        @raise Invalid_argument if the value passed for [union_tag_bits] is not
        sufficient to distinguish between the [cases].

        @param cases_tag_bits is the number of bits that each of the [cases] can
        use. This is only useful if the cases use more than 0 bits of tag.

        It is recommended to set [cases_tag_bits] explicitly if you need the
        layout to be stable even if the [cases] or one of its element changes.

        You can over-provision the [cases_tag_bits] if you expect one of the
        cases to change to use more bits of tag or if you expect that a new case
        using more tag bits will be added in the future.

        E.g., passing [~cases_tag_bits:7] to the [union] in the example above
        will cause the values to be represented as follows:
        - 0b00000000: case [A], [false]
        - 0b00000001: case [A], [true]
        - 0b10000000: case [B], [Some] (a payload of 4 bytes follows)
        - 0b10000001: case [B], [None]

        @raise Invalid_argument if one of the elements of [cases] needs more
        than [cases_tag_bits] bits of tag.

        E.g., [union ~cases_tag_bits:0 [case "Bool" Option.some Fun.id bool]]
        raises [Invalid_argument] because {!bool} uses one bit of tag which is
        more than [0].

        @raise Invalid_argument if [cases] is empty. *)
    val union :
      ?union_tag_bits:int -> ?cases_tag_bits:int -> 'a case list -> 'a t

    (** [void_case ~title] is an impossible case. It is provided so you can add
        unused tags within a union. E.g.,
        [union [case _; void_case ~title:"reserved-for-v04-compatibility"; case _; case _]]
        uses two bits of tag for the discrimination of the union,
        but the tag [01] is unused (reserved for some version compatibility). *)
    val void_case : title:string -> 'a case

    (** [or_int32 ~i32_title ~alt_title ?alt_description c] creates a new
        compact encoding for the disjunction of
        any type [a] (see {!val-case}) with [int32]. It uses the same number
        of bits as {!int32}, that is 2, and uses the spare tag ([11]) within
        this size for values of type [a].

        @param i32_title is used as a prefix to each of the int32 cases' title.

        @param alt_title is used as the title of the alt case. (See {!case} and
        {!union} for details.)

        @param alt_description is used as the description of the alternate case.
        (See {!case} and {!union} for details.) *)
    val or_int32 :
      int32_title:string ->
      alt_title:string ->
      ?alt_description:string ->
      'a encoding ->
      (int32, 'a) Either.t t

    (** {2 JSON tweaking} *)

    (** [splitted ~json ~compact] is a compact encoding that is represented as
        [compact] in binary and as [json] in JSON. *)
    val splitted : json:'a encoding -> compact:'a t -> 'a t

    (** {1 Custom} *)

    (** This module can be used to write compact encoding for complex types
        without relying on the existing combinators. *)
    module Custom : sig
      type tag = int

      (** Combine multiple tags; will throw an error if the total length of
          the tags is more than 16. *)
      val join_tags : (tag * int) list -> tag

      module type S = sig
        (** The type of [input] this module allows to encode. *)
        type input

        (** The various ways to efficiently encode [input]. *)
        type layout

        (** The list of layouts available to encode [input]. *)
        val layouts : layout list

        (** The number of bits necessary to distinguish between the various
            layouts. *)
        val tag_len : int

        (** [tag layout] computes the tag of {!Data_encoding.union} to be
            used to encode values classified as [layout].

            {b Warning:} It is expected that [tag layout < 2^tag_len - 1]. *)
        val tag : layout -> tag

        (** [title layout] computes the title to be used in documentation for
            the given layout. The title is not always available (some elements
            don't have one) and [None] is used in that case. *)
        val title : layout -> string option

        (** [partial_encoding layout] returns the encoding to use for values
            classified as [layout].

            This encoding can be partial in the sense that it may fail (it
            will raise an [Invalid_argument]) for some values of [x].
            However, it is expected that [partial_encoding (classify x) x]
            will always succeed. *)
        val partial_encoding : layout -> input encoding

        (** [classify x] returns the layout to be used to encode [x]. *)
        val classify : input -> layout

        (** The encoding to use when targeting a JSON output. *)
        val json_encoding : input encoding
      end

      (** [make (module M)] is a compact encoding for the type of [M.input].

          The JSON representation is entirely determined by [M.json_encoding].

          The binary representation is determined as follows.
          - A value [v : M.input] is classified into a layout [l] by [M.classify v].
          - A tag [M.tag l] is used (which may be combined with the tags of other
            compact encodings as described before).
          - The payload is the same bytes as can be found in the string returned by
            [Data_encoding.Binary.to_string (M.partial_encoding l) v].

          In other words, the tag of a value is [M.(tag (layout v))] and the payload
          of a value is [M.(partial_encoding (layout v) v)].

          It is the user's responsibility to ensure that all the values of [M]
          follow the invariants documented in {!S}. *)
      val make : (module S with type input = 'a) -> 'a t
    end
  end

  type 'a compact = 'a Compact.t
end

include module type of struct
  include Encoding
end

(** Create a {!Data_encoding.t} value which records knowledge of
      older versions of a given encoding as long as one can "upgrade"
      from an older version to the next (if upgrade is impossible one
      should consider that the encoding is completely different).

      See the module [Documented_example] in ["./test/versioned.ml"]
      for a tutorial.
  *)
module With_version : sig
  (** An encapsulation of consecutive encoding versions. *)
  type _ t

  (** [first_version enc] records that [enc] is the first (known)
      version of the object. *)
  val first_version : 'a encoding -> 'a t

  (** [next_version enc upgrade prev] constructs a new version from
      the previous version [prev] and an [upgrade] function. *)
  val next_version : 'a encoding -> ('b -> 'a) -> 'b t -> 'a t

  (** Make an encoding from an encapsulation of versions; the
      argument [~name] is used to prefix the version "tag" in the
      encoding, it should not change from one version to the next. *)
  val encoding : name:string -> 'a t -> 'a encoding
end

module Json : sig
  (** In memory JSON data, compatible with [Ezjsonm]. *)
  type json =
    [ `O of (string * json) list
    | `Bool of bool
    | `Float of float
    | `A of json list
    | `Null
    | `String of string ]

  type t = json

  type schema = Json_schema.schema

  (** Encodes raw JSON data (BSON is used for binary). *)
  val encoding : json Encoding.t

  (** Encodes a JSON schema (BSON encoded for binary). *)
  val schema_encoding : schema Encoding.t

  (** Create a {!Json_encoding.encoding} from an {!type:encoding}. *)
  val convert : 'a Encoding.t -> 'a Json_encoding.encoding

  (** Generate a schema from an {!type:encoding}. *)
  val schema : ?definitions_path:string -> 'a Encoding.t -> schema

  (** Construct a JSON object from an encoding. *)
  val construct :
    ?include_default_fields:[`Always | `Auto | `Never] ->
    't Encoding.t ->
    't ->
    json

  type jsonm_lexeme =
    [ `Null
    | `Bool of bool
    | `String of string
    | `Float of float
    | `Name of string
    | `As
    | `Ae
    | `Os
    | `Oe ]

  (** [construct_seq enc t] is a representation of [t] as a sequence of json
      lexeme ([jsonm_lexeme Seq.t]). This sequence is lazy: lexemes are computed
      on-demand. *)
  val construct_seq : 't Encoding.t -> 't -> jsonm_lexeme Seq.t

  (** [string_seq_of_jsonm_lexeme_seq ~newline ~chunk_size_hint s] is a sequence
      of strings, the concatenation of which is a valid textual representation
      of the json value represented by [s]. Each string chunk is roughly
      [chunk_size_hint] long (except the last one that may be shorter).

      With the [newline] parameter set to [true], a single newline character is
      appended to the textual representation.

      Forcing one element of the resulting string sequence forces multiple
      elements of the underlying lexeme sequence. Once enough lexemes have been
      forced that roughly [chunk_size_hint] characters are needed to represent
      them, the representation is returned and the rest of the sequence is held
      lazily.

      Note that most chunks split at a lexeme boundary. This may not be true for
      string literals or float literals, the representation of which may be
      spread across multiple chunks. *)
  val string_seq_of_jsonm_lexeme_seq :
    newline:bool -> chunk_size_hint:int -> jsonm_lexeme Seq.t -> string Seq.t

  val small_string_seq_of_jsonm_lexeme_seq :
    newline:bool -> jsonm_lexeme Seq.t -> string Seq.t

  (** [blit_instructions_seq_of_jsonm_lexeme_seq ~newline ~buffer json]
      is a sequence of [(buff, offset, length)] such that the concatenation of the
      sub-strings thus designated represents the json value in text form.

      The intended use is to blit each of the substring onto whatever output the
      consumer decides. In most cases, the Sequence's [buff] is physically equal
      to [buffer]. This is not always true and one cannot rely on that fact. E.g.,
      when the json includes a long string literal, the function might instruct
      the consumer to blit from that literal directly.

      This function performs few allocations, especially of fresh strings.

      Note that once the next element of the sequence is forced, the blit
      instructions become invalid: the content of [buff] may have been rewritten
      by the side effect of forcing the next element.

      @raise [Invalid_argument] if [Bytes.length buffer] is less than 32. *)
  val blit_instructions_seq_of_jsonm_lexeme_seq :
    newline:bool ->
    buffer:bytes ->
    jsonm_lexeme Seq.t ->
    (Bytes.t * int * int) Seq.t

  (** Destruct a JSON object into a value.
      Fail with an exception if the JSON object and encoding do not match.

      @param [bson_relaxation] (default to [false]) works around a limitation of
      the BSON format. Specifically, in BSON, top-level arrays are represented as
      number-indexed objects. When [bson_relaxation] is [true], then the
      destructor attempts to automatically translate the indistinguishable
      values as guided by the encoding. *)
  val destruct : ?bson_relaxation:bool -> 't Encoding.t -> json -> 't

  (** JSON Error. *)

  type path = path_item list

  (** A set of accessors that point to a location in a JSON object. *)
  and path_item =
    [ `Field of string  (** A field in an object. *)
    | `Index of int  (** An index in an array. *)
    | `Star  (** Any / every field or index. *)
    | `Next  (** The next element after an array. *) ]

  (** Exception raised by destructors, with the location in the original
      JSON structure and the specific error. *)
  exception Cannot_destruct of (path * exn)

  (** Unexpected kind of data encountered, with the expectation. *)
  exception Unexpected of string * string

  (** Some {!val:union} couldn't be destructed, with the reasons for each {!val:case}. *)
  exception No_case_matched of exn list

  (** Array of unexpected size encountered, with the expectation. *)
  exception Bad_array_size of int * int

  (** Missing field in an object. *)
  exception Missing_field of string

  (** Supernumerary field in an object. *)
  exception Unexpected_field of string

  val print_error :
    ?print_unknown:(Format.formatter -> exn -> unit) ->
    Format.formatter ->
    exn ->
    unit

  (** Helpers for writing encoders. *)
  val cannot_destruct : ('a, Format.formatter, unit, 'b) format4 -> 'a

  val wrap_error : ('a -> 'b) -> 'a -> 'b

  (** Read a JSON document from a string. *)
  val from_string : string -> (json, string) result

  (** Write a JSON document to a string. This goes via an intermediate
      buffer and so may be slow on large documents. *)
  val to_string : ?newline:bool -> ?minify:bool -> json -> string

  val pp : Format.formatter -> json -> unit
end

module Bson : sig
  type bson = Json_repr_bson.bson

  type t = bson

  (** Construct a BSON object from an encoding. *)
  val construct : 't Encoding.t -> 't -> bson

  (** Destruct a BSON object into a value.
      Fail with an exception if the JSON object and encoding do not match.. *)
  val destruct : 't Encoding.t -> bson -> 't
end

module Binary_schema : sig
  type t

  val pp : Format.formatter -> t -> unit

  val encoding : t Encoding.t
end

module Binary_stream : sig
  (** The type for a stream, as used in the {!Binary.read_stream} function.

      You never need to initialise a stream: one is initialised for you when you
      use first {!Binary.read_stream} and you may pass a stream around to
      further calls to {!Binary.read_stream}.
   *)
  type t

  (** [is_empty s] is [true] iff the stream is not currently holding any
      yet-to-be-decoded bytes. Consequently, reading on that stream will always
      return [Await]. *)
  val is_empty : t -> bool
end

module Binary : sig
  (** All the errors that might be returned while reading a binary value *)
  type read_error =
    | Not_enough_data
        (** Decoding requires more bytes than are available in the
            source of the data. E.g., there are fewer bytes available in the
            reading buffer than is required by the length field of a
            dynamically-sized string. *)
    | Extra_bytes  (** Decoding requires fewer bytes than were provided. *)
    | No_case_matched  (** Unknown case in a {!string_enum}. *)
    | Unexpected_tag of int  (** Unknown case in a {!union} or {!matching}. *)
    | Invalid_int of {min : int; v : int; max : int}
        (** An integer is out of range. E.g., an integer is beyond a
            user-provided range. *)
    | Invalid_float of {min : float; v : float; max : float}
        (** A float is out of range. *)
    | Trailing_zero
        (** An arbitrary-precision number (N or Z) leads to a null byte. *)
    | Size_limit_exceeded
        (** A value is encoded with more bytes than is allowed by the encoding.
            E.g., the constraints of a {!check_size} are being broken. *)
    | List_too_long
        (** A list contains more elements than is specified in its [max_length]
            parameter. *)
    | Array_too_long
        (** An array contains more elements than is specified in its
            [max_length] parameter. *)
    | Exception_raised_in_user_function of string
        (** A function provided by the user raised an exception. E.g., a
            function in a {!conv} encoding or a {!delayed} encoding raised. *)
    | User_invariant_guard of string
        (** A user-provided guard returned an [Error _]. E.g., in a
            {!conv_with_guard}.*)

  (** [Read_error e] is an exception wrapping the {!read_error} [e]. It is used
      only by function suffixed by [_exn] where the suffix-less function would
      have returned [Error e]. *)
  exception Read_error of read_error

  val pp_read_error : Format.formatter -> read_error -> unit

  val read_error_encoding : read_error t

  (** All the errors that might be returned while writing a binary value *)
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

  val pp_write_error : Format.formatter -> write_error -> unit

  val write_error_encoding : write_error t

  exception Write_error of write_error

  (** Compute the expected length of the binary representation of a value.

      @raise Write_error in case some size/length invariants are broken.
   *)
  val length : 'a Encoding.t -> 'a -> int

  (** Returns the size of the binary representation that the given
      encoding might produce, only when this size does not depends of the value
      itself.

      E.g., [fixed_length (tup2 int64 (Fixed.string 2))] is [Some _]

      E.g., [fixed_length (result int64 (Fixed.string 2))] is [None]

      E.g., [fixed_length (list (tup2 int64 (Fixed.string 2)))] is [None] *)
  val fixed_length : 'a Encoding.t -> int option

  (** Returns the maximum size of the binary representation that the given
      encoding might produce, only when this maximum size does not depends of
      the value itself.

      E.g., [maximum_length (tup2 int64 (Fixed.string 2))] is [Some _]

      E.g., [maximum_length (result int64 (Fixed.string 2))] is [Some _]

      E.g., [maximum_length (list (tup2 int64 (Fixed.string 2)))] is [None]

      Note that the function assumes that recursive encodings (build using [mu])
      are used for recursive data types. As a result, [maximum_length] will
      return [None] if given a recursive encoding.

      If there are static guarantees about the maximum size of the
      representation for values of a given type, you can wrap your encoding in
      [check_size]. This will cause [maximum_length] to return [Some _]. *)
  val maximum_length : 'a Encoding.t -> int option

  (** [read enc buf ofs len] tries to reconstruct a value from the
      bytes in [buf] starting at offset [ofs] and reading at most
      [len] bytes. This function also returns the offset of the first
      unread bytes in the [buf].

      The function will fail (returning [Error _]) if
      - the bytes in [buf] designated by [ofs] and [len] are not compatible with
        the encoding [enc] (e.g., an integer is out the range specified in the
        encoding, or a union's tag is not recognised),
      - it needs to read more than [len] bytes to decode the value,
      - a user-provided function (such as that of a [conv] or [delayed]) raises
        an exception,
      - etc.
      In which case the returned error contains minimal diagnosis information
      about the discrepancy between the bytes and the encoding.

      Other reading functions ({!of_string}, {!of_bytes}) may fail for the same
      reasons.

      The returned value contains no pointer back to [buf] (as a whole or as
      sub-strings), even in the case where the encoding is or includes
      [Fixed.string] or [Fixed.bytes]. *)
  val read :
    'a Encoding.t -> string -> int -> int -> (int * 'a, read_error) result

  (** [read_opt] is like [read] but in case of failure, the error is ignored and
      [None] is returned instead. *)
  val read_opt : 'a Encoding.t -> string -> int -> int -> (int * 'a) option

  (** [read_exn] is like [read] but in case of failure, the error is wrapped in
      [Read_error] which is raised. *)
  val read_exn : 'a Encoding.t -> string -> int -> int -> int * 'a

  (** Return type for the function [read_stream]. *)
  type 'ret status =
    | Success of {result : 'ret; size : int; stream : Binary_stream.t}
        (** Fully decoded value, together with the total amount of bytes reads,
        and the remaining unread stream. *)
    | Await of (Bytes.t -> 'ret status)  (** Partially decoded value.*)
    | Error of read_error
        (** Failure. The stream is garbled and it should be dropped. *)

  (** Streamed equivalent of [read].

      @raise [Invalid_argument] if called with a variable-size encoding.

      [read_stream e] is [Await k] and you can use [k] to feed the first bytes
      to be decoded.

      If you feed invalid bytes (i.e., bytes that do not match [e]) to [k], it
      returns [Error].

      If you feed bytes that form a valid strict prefix for [e], then it
      returns [Await k], and you can use [k] to feed it more bytes.

      If you feed it sufficient bytes to decode a whole value described by
      [e], then it returns [Success s] where [s.result] is the decoded value,
      [s.size] is the number of bytes that were read to decode this value, and
      [s.stream] is the left-over stream.

      The leftover stream may contain more bytes which you may treat however
      you like depending on your application. You may ignore padding bytes
      reserved for extensions (in which case you can simply ignore the stream).
      You may use the leftover stream to call [read_stream] again to decode the
      rest of the value.

      [read_stream ~init e] is the same as
      [let (Await k) = read_stream e in k b] where [b] are the leftover bytes of
      [init].

      E.g., reading multiple values from a socket or some such source of bytes
      that becomes available as time goes by.
{[
let iter socket encoding f =
   let rec loop = function
      | Success {result; size; stream} ->
         log "read %d bytes" size;
         f result (* apply iterator function on each decoded value *);
         loop (read_stream ~init:stream e) (* continue with leftover *)
      | Await k ->
         loop (k (read_more_bytes_from socket))
      | Error e ->
         log "error: %a" pp_read_error e
   in
   loop (read_stream e)
]} *)
  val read_stream : ?init:Binary_stream.t -> 'a Encoding.t -> 'a status

  (** The internal state that writers handle. It is presented explicitly as an
      abstract type so that you must use the constructor to obtain it. The
      constructor ({!make_writer_state}) performs basic bound checks. *)
  type writer_state

  (** [make_writer_state buffer ~offset ~allowed_bytes] is
      [None] if [allowed_bytes < 0],
      [None] if [allowed_bytes > length buffer - offset],
      [Some _] otherwise. *)
  val make_writer_state :
    bytes -> offset:int -> allowed_bytes:int -> writer_state option

  (** [write enc v state]
      where [Some state] is [make_writer_state buffer ~offset ~allowed_bytes]
      writes the binary [enc] representation of [v] onto [buffer] starting at
      [offset]. The function will fail (returning [Error _]) if the encoding
      would use more than [allowed_bytes].

      The function returns the offset of first unwritten bytes, or returns
      [Error _] in case of failure.
      In the latter case, some data might have been written on the buffer. *)
  val write : 'a Encoding.t -> 'a -> writer_state -> (int, write_error) result

  val write_opt : 'a Encoding.t -> 'a -> writer_state -> int option

  val write_exn : 'a Encoding.t -> 'a -> writer_state -> int

  (** [of_bytes enc buf] is equivalent to [read enc buf 0 (length buf)].
      The function fails if the buffer is not fully read. *)
  val of_bytes : 'a Encoding.t -> Bytes.t -> ('a, read_error) result

  val of_bytes_opt : 'a Encoding.t -> Bytes.t -> 'a option

  (** [of_bytes_exn enc buf] is equivalent to [of_bytes], except
      @raise [Read_error] instead of returning [None] in case of error. *)
  val of_bytes_exn : 'a Encoding.t -> Bytes.t -> 'a

  (** [of_string enc buf] is like [of_bytes enc buf] but it reads bytes from a
      string. *)
  val of_string : 'a Encoding.t -> string -> ('a, read_error) result

  val of_string_opt : 'a Encoding.t -> string -> 'a option

  val of_string_exn : 'a Encoding.t -> string -> 'a

  (** [to_bytes enc v] is the equivalent of [write env buf 0 len]
      where [buf] is a newly allocated buffer.
      The parameter [buffer_size] controls the initial size of [buf].

      @raise [Invalid_argument] if [buffer_size < 0]. *)
  val to_bytes :
    ?buffer_size:int -> 'a Encoding.t -> 'a -> (Bytes.t, write_error) result

  val to_bytes_opt : ?buffer_size:int -> 'a Encoding.t -> 'a -> Bytes.t option

  (** [to_bytes_exn enc v] is equivalent to [to_bytes enc v], except

      @raise [Write_error] instead of returning [None] in case of error. *)
  val to_bytes_exn : ?buffer_size:int -> 'a Encoding.t -> 'a -> Bytes.t

  (** [to_string enc v] is like [to_bytes] but it returns a string.

      Note: [to_string enc v] is always equal to
      [Bytes.to_string (to_bytes enc v)] but more efficient.

      @raise [Invalid_argument] if [buffer_size < 0]. *)
  val to_string :
    ?buffer_size:int -> 'a Encoding.t -> 'a -> (string, write_error) result

  val to_string_opt : ?buffer_size:int -> 'a Encoding.t -> 'a -> string option

  (** @raise [Write_error] instead of returning [None] in case of error. *)
  val to_string_exn : ?buffer_size:int -> 'a Encoding.t -> 'a -> string

  val describe : 'a Encoding.t -> Binary_schema.t

  module Slicer : sig
    (** A [slice] is a part of a binary representation of some data. The
        concatenation of multiple slices represents the whole data. *)
    type slice = {name : string; value : string; pretty_printed : string}

    type slicer_state

    (** [None] if [offset] and [length] do not describe a valid substring. *)
    val make_slicer_state :
      string -> offset:int -> length:int -> slicer_state option

    (** [slice e state] slices the data represented by the substring described
        by [state].

        If [e] does not correctly describe the given bytes (i.e., if [read]
        would fail on equivalent parameters) then it returns [Error].

        Otherwise it returns [Ok sl] and
        [String.concat "" (List.map (fun s -> s.value) sl] is byte-for-byte
        identical to the substring described by [state]. *)
    val slice :
      _ Encoding.t ->
      slicer_state ->
      (slice list, Binary_error_types.read_error) result

    val slice_opt : _ Encoding.t -> slicer_state -> slice list option

    val slice_exn : _ Encoding.t -> slicer_state -> slice list

    (** [slice_string] slices the whole content of the string.

        In other words, [slice_string e s] is
        [slice e (Option.get @@ make_slicer_state s 0 (String.length s)]. *)
    val slice_string : _ Encoding.t -> string -> (slice list, read_error) result

    val slice_string_opt : _ Encoding.t -> string -> slice list option

    val slice_string_exn : _ Encoding.t -> string -> slice list

    val slice_bytes : _ Encoding.t -> bytes -> (slice list, read_error) result

    val slice_bytes_opt : _ Encoding.t -> bytes -> slice list option

    val slice_bytes_exn : _ Encoding.t -> bytes -> slice list
  end
end

type json = Json.t

(** [json] is an encoding for JSON values. It is mostly intended for internal
    use or for defining your own low-level combinators.

    WARNING! Due to a limitation of BSON, this encoding does not safely
    roundtrip. Specifically, [Json.destruct json (Json.construct json v)]
    is not guaranteed to be equal to [v]. More specifically, in BSON, top-level
    Arrays are represented as number-indexed Objects and this library has no way
    to distinguish between the two, doubly so for empty collections.

    See {!Json.destruct}'s [?bson_relaxation] optional parameter. *)
val json : json Encoding.t

type json_schema = Json.schema

val json_schema : json_schema Encoding.t

type bson = Bson.t

module Registration : sig
  type id = string

  (** A encoding that has been {!register}ed. It can be retrieved using either
      {!list} or {!find}.

      Note registration/retrieval erases the type information that is built by
      the combinator. In other words, [t] is a non-parametric type. As a result,
      you cannot recover an OCaml value of the type of the registered
      encoding. You can only perform operations where the type doesn't
      escape — e.g., converting between binary and json. *)
  type t

  (** Descriptions and schemas of registered encodings. *)
  val binary_schema : t -> Binary_schema.t

  val json_schema : t -> Json.schema

  val description : t -> string option

  (** Printers for the encodings. *)
  val json_pretty_printer : t -> Format.formatter -> Json.t -> unit

  val binary_pretty_printer : t -> Format.formatter -> Bytes.t -> unit

  (** [register (def id encoding)] registers the [encoding] with the [id]. It
      can later be found using {!find} and providing the matching [id]. It will
      also appear in the results of {!list}.

      @raise [Invalid_argument] if [encoding] is not of one of the following
      form:

      - [def id _] (see {!val:Encoding.def})
      - [splitted ~binary:(def id _)] (see {!val:Encoding.splitted})
      - [dynamic_size (def id _)] (see {!val:Encoding.dynamic_size})
      - [check_size _ (def id _)] (see {!val:Encoding.check_size}) *)
  val register : ?pp:(Format.formatter -> 'a -> unit) -> 'a Encoding.t -> unit

  (** [slice r b] attempts to slice a binary representation [b] of some data
      assuming it is correctly described by the registered encoding [r].
      If [r] does not correctly describe [b], then it returns [None].

      See {!Binary.Slicer.slice_string} for details about slicing. *)
  val slice :
    t -> string -> (Binary.Slicer.slice list, Binary.read_error) result

  (** [slice_all b] attempts to slice a binary representation [b] of some data
      for all of the registered encodings. It returns a list of the slicing for
      each of the registered encodings that correctly describe [b].

      See {!Binary.Slicer.slice_string} for details about slicing. *)
  val slice_all : string -> (string * Binary.Slicer.slice list) list

  (** [find id] is [Some r] if [register (def id e)] has been called, in which
      case [r] matches [e]. Otherwise, it is [None]. *)
  val find : id -> t option

  (** [list ()] is a list of pairs [(id, r)] where [r] is
      a registered encoding for the [id]. *)
  val list : unit -> (id * t) list

  (** Conversion functions from JSON to bytes. *)
  val bytes_of_json : t -> Json.t -> Bytes.t option

  (** Conversion functions from bytes to JSON. *)
  val json_of_bytes : t -> Bytes.t -> Json.t option

  (** {3 Introspection API}

      The introspection functions below expose some internal representations of
      the registered [encoding]s. These functions are not for the casual user
      and are subject to changes from one version of the library to another. *)

  (** A transparent, raw form of a {!register}ed encoding, allowing far greater
      introspection than a {!t} value. It can be retrieved using
      {!find_introspectable} or operated upon using {!iter}.

      Note that retrieval and iteration over [introspectable] preserve, but do not present, the
      type information of the original combinator. In other words,
      [introspectable] is a
      non-parametric abstraction over arbitrarily-typed [Encoding.t]. This means that,
      while it cannot be used in any way that would allow the underlying type parameter
      to escape, it can be operated upon (e.g. using {!iter}) using functions with
      locally abstract types ([fun (type a) -> ...]) or explicit polymorphism.


      The inclusion of this type is intended almost exclusively for use in
      specialized external tools rather than by casual users. As this type
      and its related functions permit introspection into a registered encoding
      that is otherwise opaque-by-design, it should only be relied
      upon as a last resort when no other options are available. *)
  type introspectable = Any : _ Encoding.t -> introspectable

  (** [find_introspectable id] is [Some e'] if [register (def id e)] has been called,
      in which case [e'] should be [Any e]. Otherwise, it is [None].

      As with the [introspectable] type itself, casual users are
      discouraged from calling this function; it is designed only for
      external tools to process raw encodings that cannot be brought
      into scope except through querying the [Registration] module.
  *)
  val find_introspectable : id -> introspectable option

  (** [iter ~id f] is equivalent to calling [Option.iter f (find_introspectable id)].
      It may be called wherever the result of [find_introspectable id] would otherwise
      be used only once, as an argument to a function call, and discarded
      subsequently.

      As with the [introspectable] type itself, casual users are
      discouraged from calling this function; it is designed only for
      external tools to process raw encodings that cannot be brought
      into scope except through querying the [Registration] module.
  *)
  val iter : id:string -> (introspectable -> unit) -> unit
end
