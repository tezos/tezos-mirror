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

(** In memory JSON data *)
type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

(* [tag_size] is not declared in the upstream library, instead, the expanded
   polymorphic-variant type-expression is used as is. We include it in the
   protocol environment to help coq-of-ocaml process the files. *)
type tag_size = [`Uint8 | `Uint16]

type json_schema

(** The type descriptors for values of type ['a]. *)
type 'a t

type 'a encoding = 'a t

type string_json_repr = Hex | Plain

val classify : 'a encoding -> [`Fixed of int | `Dynamic | `Variable]

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

(** Big number
      In JSON, data is encoded as a decimal string.
      In binary, data is encoded as a variable length sequence of
      bytes, with a running unary size bit: the most significant bit of
      each byte tells is this is the last byte in the sequence (0) or if
      there is more to read (1). The second most significant bit of the
      first byte is reserved for the sign (positive if zero). Binary_size and
      sign bits ignored, data is then the binary representation of the
      absolute value of the number in little-endian order. *)
val z : Z.t encoding

(** Positive big number, see [z]. *)
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
val string :
  ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
  string_json_repr ->
  string encoding

(** Encoding of arbitrary bytes. See [string] *)
val bytes :
  ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
  string_json_repr ->
  Bytes.t encoding

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

      @raise Invalid_argument if called within the body of a {!mu}. *)
val option : 'a encoding -> 'a option encoding

(** Combinator to make a {!result} value
    (represented as a 1-byte tag followed by the data of either type in binary,
    and either unwrapped value in JSON (the caller must ensure that both
    encodings do not collide)). *)
val result : 'a encoding -> 'b encoding -> ('a, 'b) result encoding

(** List combinator.
    - encoded as an array in JSON
    - encoded as the concatenation of all the element in binary
      prefixed its size in bytes

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
  ('a -> 'b) -> ('b -> 'a) -> ?schema:json_schema -> 'b encoding -> 'a encoding

(** [conv_with_guard] is similar to {!conv} but the function that takes in the value
    from the outside (untrusted) world has a chance to fail.

    Specifically, if the function returns [Error msg] then the decoding is
    interrupted with an error carrying the message [msg]. If the function
    returns [Ok _] then the decoding proceeds normally. *)
val conv_with_guard :
  ('a -> 'b) ->
  ('b -> ('a, string) result) ->
  ?schema:json_schema ->
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
     inhabited. *)
type 'a matching_function = 'a -> match_result

and match_result

(** [matched t e u] represents the fact that a value is tagged with [t] and
    carries the payload [u] which can be encoded with [e].

    The optional argument [tag_size] must match the one passed to the
    {!matching} function [matched] is called inside of.

    An example is given in the documentation of {!matching}.

    @raise [Invalid_argument] if [t < 0]

    @raise [Invalid_argument] if [t] does not fit in [tag_size] *)
val matched : ?tag_size:tag_size -> int -> 'a encoding -> 'a -> match_result

(** Encodes a variant constructor. Takes the encoding for the specific
    parameters, a recognizer function that will extract the parameters
    in case the expected case of the variant is being serialized, and
    a constructor function for deserialization.

    The tag must be less than the tag size of the union in which you use the case.
    An optional tag gives a name to a case and should be used to maintain
    compatibility.

    An optional name for the case can be provided, which is used in the binary
    documentation.

    @raise [Invalid_argument] if [case_tag] is [Tag t] with [t < 0] *)
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
  ?tag_size:tag_size -> 't matching_function -> 't case list -> 't encoding

(** Same as matching except that the matching function is
    a linear traversal of the cases.

    @raise [Invalid_argument] if it is given an empty [case list]

    @raise [Invalid_argument] if there are more than one [case] with the same
    [tag] in the [case list]

    @raise [Invalid_argument] if there are more cases in the [case list] than
    can fit in the [tag_size] *)
val union : ?tag_size:tag_size -> 't case list -> 't encoding

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
    ?tag_size:tag_size -> int * string -> 'a encoding -> 'a -> match_result

  val matching :
    ?tag_size:tag_size -> 't matching_function -> 't case list -> 't encoding

  val union : ?tag_size:tag_size -> 't case list -> 't encoding
end

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
  val string : string_json_repr -> int -> string encoding

  (** @raise Invalid_argument if the argument is less or equal to zero. *)
  val bytes : string_json_repr -> int -> Bytes.t encoding

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
  val string : string_json_repr -> string encoding

  val bytes : string_json_repr -> Bytes.t encoding

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
      [Bounded.string ~length_kind:`Uint8 Hex 1000] raises.

      @raise Invalid_argument if [length_kind] is unset and the specified
      bound is larger than 2^30. *)
  val string :
    ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
    string_json_repr ->
    int ->
    string encoding

  (** See {!string} above. *)
  val bytes :
    ?length_kind:[`N | `Uint30 | `Uint16 | `Uint8] ->
    string_json_repr ->
    int ->
    Bytes.t encoding
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

    @raise Invalid_argument if [size < 0] *)
val check_size : int -> 'a encoding -> 'a encoding

(** Define different encodings for JSON and binary serialization. *)
val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

(** Combinator for recursive encodings.

     Notice that the function passed to [mu] must be pure. Otherwise,
     the behavior is unspecified.

     A stateful recursive encoding can still be put under a [delayed]
     combinator to make sure that a new encoding is generated each
     time it is used. Caching the encoding generation when the state
     has not changed is then the responsibility of the client.

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
val force_bytes : 'a lazy_t -> bytes

(** Make a lazy value from an immediate one. *)
val make_lazy : 'a encoding -> 'a -> 'a lazy_t

(** Apply on structure of lazy value, and combine results *)
val apply_lazy :
  fun_value:('a -> 'b) ->
  fun_bytes:(bytes -> 'b) ->
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
    'f1 t -> 'f2 t -> 'f3 t -> 'f4 t -> 'f5 t -> ('f1 * 'f2 * 'f3 * 'f4 * 'f5) t

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
  val union : ?union_tag_bits:int -> ?cases_tag_bits:int -> 'a case list -> 'a t

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

      @param alt_title is used as the title of the alt case. (See {!val-case} and
      {!union} for details.)

      @param alt_description is used as the description of the alternate case.
      (See {!val-case} and {!union} for details.) *)
  val or_int32 :
    int32_title:string ->
    alt_title:string ->
    ?alt_description:string ->
    'a encoding ->
    (int32, 'a) Either.t t

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
        follow the invariants documented in {!module-type-S}. *)
    val make : (module S with type input = 'a) -> 'a t
  end
end

type 'a compact = 'a Compact.t

val json : json encoding

val json_schema : json_schema encoding

module Json : sig
  val schema : ?definitions_path:string -> 'a encoding -> json_schema

  (** Construct a JSON object from an encoding. *)
  val construct :
    ?include_default_fields:[`Always | `Auto | `Never] ->
    't encoding ->
    't ->
    json

  (** Destruct a JSON object into a value.
      Fail with an exception if the JSON object and encoding do not match.

      @param [bson_relaxation] (default to [false]) works around a limitation of
      the BSON format. Specifically, in BSON, top-level arrays are represented as
      number-indexed objects. When [bson_relaxation] is [true], then the
      destructor attempts to automatically translate the indistinguishable
      values as guided by the encoding. *)
  val destruct : ?bson_relaxation:bool -> 't encoding -> json -> 't

  (** JSON Error *)

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

  val pp : Format.formatter -> json -> unit
end

module Binary : sig
  (** Compute the expected length of the binary representation of a value.

      @raise Write_error in case some size/length invariants are broken.
   *)
  val length : 'a encoding -> 'a -> int

  (** Returns the size of the binary representation that the given
      encoding might produce, only when this size does not depends of the value
      itself.

      E.g., [fixed_length (tup2 int64 (Fixed.string 2))] is [Some _]

      E.g., [fixed_length (result int64 (Fixed.string 2))] is [None]

      E.g., [fixed_length (list (tup2 int64 (Fixed.string 2)))] is [None] *)
  val fixed_length : 'a encoding -> int option

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
  val maximum_length : 'a encoding -> int option

  val of_bytes_opt : 'a encoding -> bytes -> 'a option

  val of_string_opt : 'a encoding -> string -> 'a option

  val to_bytes_opt : ?buffer_size:int -> 'a encoding -> 'a -> bytes option

  (** [to_bytes_exn enc v] is equivalent to [to_bytes enc v], except

      @raise [Write_error] instead of returning [None] in case of error. *)
  val to_bytes_exn : ?buffer_size:int -> 'a encoding -> 'a -> bytes

  val to_string_opt : ?buffer_size:int -> 'a encoding -> 'a -> string option

  (** @raise [Write_error] instead of returning [None] in case of error. *)
  val to_string_exn : ?buffer_size:int -> 'a encoding -> 'a -> string
end
