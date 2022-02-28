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

(** In memory JSON data *)
type json =
  [ `O of (string * json) list
  | `Bool of bool
  | `Float of float
  | `A of json list
  | `Null
  | `String of string ]

type json_schema

type 'a t

type 'a encoding = 'a t

val classify : 'a encoding -> [`Fixed of int | `Dynamic | `Variable]

val splitted : json:'a encoding -> binary:'a encoding -> 'a encoding

val null : unit encoding

val empty : unit encoding

val unit : unit encoding

val constant : string -> unit encoding

val int8 : int encoding

val uint8 : int encoding

val int16 : int encoding

val uint16 : int encoding

val int31 : int encoding

val int32 : int32 encoding

val int64 : int64 encoding

val n : Z.t encoding

val z : Z.t encoding

val bool : bool encoding

val string : string encoding

val bytes : bytes encoding

val option : 'a encoding -> 'a option encoding

val string_enum : (string * 'a) list -> 'a encoding

module Fixed : sig
  val string : int -> string encoding

  val bytes : int -> bytes encoding

  val add_padding : 'a encoding -> int -> 'a encoding
end

module Variable : sig
  val string : string encoding

  val bytes : bytes encoding

  val array : ?max_length:int -> 'a encoding -> 'a array encoding

  val list : ?max_length:int -> 'a encoding -> 'a list encoding
end

module Bounded : sig
  val string : int -> string encoding

  val bytes : int -> bytes encoding
end

val dynamic_size :
  ?kind:[`Uint30 | `Uint16 | `Uint8] -> 'a encoding -> 'a encoding

val json : json encoding

val json_schema : json_schema encoding

type 'a field

val req :
  ?title:string -> ?description:string -> string -> 't encoding -> 't field

val opt :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't option field

val varopt :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't option field

val dft :
  ?title:string ->
  ?description:string ->
  string ->
  't encoding ->
  't ->
  't field

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

val merge_objs : 'o1 encoding -> 'o2 encoding -> ('o1 * 'o2) encoding

val merge_tups : 'a1 encoding -> 'a2 encoding -> ('a1 * 'a2) encoding

val array : ?max_length:int -> 'a encoding -> 'a array encoding

val list : ?max_length:int -> 'a encoding -> 'a list encoding

val assoc : 'a encoding -> (string * 'a) list encoding

type case_tag = Tag of int | Json_only

type 't case

val case :
  title:string ->
  ?description:string ->
  case_tag ->
  'a encoding ->
  ('t -> 'a option) ->
  ('a -> 't) ->
  't case

type match_result

type 'a matching_function = 'a -> match_result

(* [tag_size] is not declared in the upstream library, instead, the expanded
   polymorphic-variant type-expression is used as is. We include it in the
   protocol environment to help coq-of-ocaml process the files. *)
type tag_size = [`Uint8 | `Uint16]

val matched : ?tag_size:tag_size -> int -> 'a encoding -> 'a -> match_result

val matching :
  ?tag_size:tag_size -> 't matching_function -> 't case list -> 't encoding

val union : ?tag_size:tag_size -> 't case list -> 't encoding

val def :
  string -> ?title:string -> ?description:string -> 't encoding -> 't encoding

val conv :
  ('a -> 'b) -> ('b -> 'a) -> ?schema:json_schema -> 'b encoding -> 'a encoding

val conv_with_guard :
  ('a -> 'b) ->
  ('b -> ('a, string) result) ->
  ?schema:json_schema ->
  'b encoding ->
  'a encoding

val with_decoding_guard :
  ('a -> (unit, string) result) -> 'a encoding -> 'a encoding

val mu :
  string ->
  ?title:string ->
  ?description:string ->
  ('a encoding -> 'a encoding) ->
  'a encoding

type 'a lazy_t

val lazy_encoding : 'a encoding -> 'a lazy_t encoding

val force_decode : 'a lazy_t -> 'a option

val force_bytes : 'a lazy_t -> bytes

val make_lazy : 'a encoding -> 'a -> 'a lazy_t

val apply_lazy :
  fun_value:('a -> 'b) ->
  fun_bytes:(bytes -> 'b) ->
  fun_combine:('b -> 'b -> 'b) ->
  'a lazy_t ->
  'b

module Json : sig
  val schema : ?definitions_path:string -> 'a encoding -> json_schema

  val construct :
    ?include_default_fields:[`Always | `Auto | `Never] ->
    't encoding ->
    't ->
    json

  val destruct : ?bson_relaxation:bool -> 't encoding -> json -> 't

  (** JSON Error *)

  type path = path_item list

  and path_item =
    [ `Field of string  (** A field in an object. *)
    | `Index of int  (** An index in an array. *)
    | `Star  (** Any / every field or index. *)
    | `Next  (** The next element after an array. *) ]

  (** Exception raised by destructors, with the location in the original
      JSON structure and the specific error. *)
  exception Cannot_destruct of (path * exn)

  (** Unexpected kind of data encountered (w/ the expectation). *)
  exception Unexpected of string * string

  (** Some {!union} couldn't be destructed, w/ the reasons for each {!case}. *)
  exception No_case_matched of exn list

  (** Array of unexpected size encountered  (w/ the expectation). *)
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
  val fixed_length : 'a encoding -> int option

  val maximum_length : 'a encoding -> int option

  val length : 'a encoding -> 'a -> int

  val to_bytes_opt : ?buffer_size:int -> 'a encoding -> 'a -> bytes option

  val to_bytes_exn : ?buffer_size:int -> 'a encoding -> 'a -> bytes

  val of_bytes_opt : 'a encoding -> bytes -> 'a option

  val to_string_opt : ?buffer_size:int -> 'a encoding -> 'a -> string option

  val to_string_exn : ?buffer_size:int -> 'a encoding -> 'a -> string

  val of_string_opt : 'a encoding -> string -> 'a option
end

(** [check_size size encoding] ensures that the binary encoding
    of a value will not be allowed to exceed [size] bytes. The reader
    and the writer fails otherwise. This function do not modify
    the JSON encoding. *)
val check_size : int -> 'a encoding -> 'a encoding

module Compact : sig
  (** This module provides specialized encoding combinators that are
      implemented to reduce the size of the serialization result.

      NB: this module is somewhat experimental and any client code should
      be tested thouroughly. The interface exposed here may change,
      especially as it will be moved into the data_encoding library soon.
      Ultimately, any compelling use of this module should lead to the
      addition of a dedicated combinator upstream to make direct use of
      this module unnecessary.

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

      As a consequence, the resulting binary array for the constructor
      [T1] is

      {v
      ┌──────────┬─────────────┬─────────────┐
      │ 000000eo │ payload(f1) │ payload(f2) │
      └──────────┴─────────────┴─────────────┘
        1 byte     N bytes       M bytes
      v}

      while the resulting binary array for the constructor [T2] is

      {v
      ┌──────────┬─────────────┐
      │ 00000100 │ payload(f3) │
      └──────────┴─────────────┘
        1 byte     N bytes
      v} *)

  (** The description of a compact encoding. *)
  type 'a t

  (** Turn a compact encoding into a regular {!Data_encoding.t}.

      @raise Invalid_argument if the shared tags cannot fit in [tag_size]
      space. *)
  val make : ?tag_size:[`Uint8 | `Uint16] -> 'a t -> 'a encoding

  (** {1 Combinators} *)

  (** Similarly to [Data_encoding], we provide various combinators to
      compose compact encoding together. *)

  (** {2 Base types} *)

  (** A type with no inhabitant. *)
  type void

  (** A compact encoding used to denote an impossible case inside of
      conjunction operators such as [union]. *)
  val void : void t

  (** [refute x] can be used to refute a branch of a [match] which
      exhibits a value of type [void]. *)
  val refute : void -> 'a

  (** A compact encoding of the singleton value [empty], which has zero
      memory footprint.

      For instance, one can define a compact encoding of [bool] values
      with [union [empty; empty]]: this compact encoding uses one bit in the
      shared tag, and zero in the payload. *)
  val empty : unit t

  (** Efficient encoding of boolean values. It uses one bit in the
      shared tag, and zero bit in the payload. *)
  val bool : bool t

  (** [payload encoding] unconditionally uses [encoding] in the
      payload, and uses zero bit in the shared tag. *)
  val payload : 'a encoding -> 'a t

  (** Uses one bit in the tag to encode an option. *)
  val option : 'a t -> 'a option t

  (** {2 Conversion} *)

  (** [conv ?json f g e] reuses the encoding [e] for type [b] to encode
      an type [a] using the isomorphism [(f, g)]. The optional argument
      allows to overwrite the encoding used for JSON, in place of the
      one computed by default. *)
  val conv : ?json:'a encoding -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

  (** {2 Conjunctions} *)

  val tup1 : 'a t -> 'a t

  (** [tup2 e1 e2] concatenates the shared tags and payloads of [e1] and
      [e2]. *)
  val tup2 : 'a t -> 'b t -> ('a * 'b) t

  (** [tup3 e1 e2 e3] concatenates the shared tags and payloads of [e1],
      [e2], and [e3]. *)
  val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

  (** [tup4 e1 e2 e3 e4] concatenates the shared tags and payloads of
      [e1], [e2], [e3] and [e4]. *)
  val tup4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

  type 'a field

  (** [req "f" compact] can be used in conjunction with [optN] to create
      compact encoding with more readable JSON encoding, as an
      alternative of [tupN]. The JSON output is a dictionary which
      contains the field [f] with a value encoded using [compact]. *)
  val req : string -> 'a t -> 'a field

  (** Same as {!req}, but the field is optional. *)
  val opt : string -> 'a t -> 'a option field

  (** [obj1] can be used in conjunction with [req] or [opt] to produce
      more readable JSON outputs.  *)
  val obj1 : 'a field -> 'a t

  (** An alternative to [tup2] which can be used in conjunction with
      {!req} and {!opt} to produce more readable JSON outputs based on
      dictionary. *)
  val obj2 : 'a field -> 'b field -> ('a * 'b) t

  (** An alternative to [tup3] which can be used in conjunction with
      {!req} and {!opt} to produce more readable JSON outputs based on
      dictionary. *)
  val obj3 : 'a field -> 'b field -> 'c field -> ('a * 'b * 'c) t

  (** An alternative to [tup4] which can be used in conjunction with
      {!req} and {!opt} to produce more readable JSON outputs based on
      dictionary. *)
  val obj4 :
    'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

  (** A compact encoding for [int32] values. It uses 2 bits in the
      shared tag, to determine how many bytes are used in the payload:

      {ul {li [00]: from 0 to 255, one byte.}
          {li [01]: from 256 to 65,535, two bytes.}
          {li [10]: from 65,536 to [Int32.max_int] and for negative values, four bytes.}} *)
  val int32 : int32 t

  (** A compact encoding for [int64] values. It uses 2 bits in the
      shared tag, to determine how many bytes are used in the payload:

      {ul {li [00]: from 0 to 255, one byte.}
          {li [01]: from 256 to 65,535, two bytes.}
          {li [10]: from 65,536 to 4,294,967,295 four bytes.}
          {li [11]: from 4,294,967,295 and for negative values eight bytes.}} *)
  val int64 : int64 t

  (** [list ~bits:n encoding] uses [n] bits in the shared tag to encode the
      size of small lists.

      For instance, [list ~bits:2 encoding],

      {ul {li [00]: the payload is empty, because it is the empty list}
          {li [01]: the singleton list, whose element is encoded using
              [encoding].}
          {li [10]: a list of two elements encoded with [encoding]}
          {li [11]: a list of more than two elements, prefixed with its
              size (that uses 8 bytes)}}

      With [~bits:3], lists of 0 to 6 items are encoded with tags [000] to
      [110], and lists of 7 or more are encoded with tag [111] and the
      length.

      In the current implementation, performance may become bad for [~bits
      > 8], so don't do that. *)
  val list : bits:int -> 'a encoding -> 'a list t

  (** {2 Disjunctions} *)

  type 'a case

  (** Usage: [case name encode decode encoding]

      Intended to be used inside a [union]. *)
  val case : string -> ('a -> 'b option) -> ('b -> 'a) -> 'b t -> 'a case

  (** [union cases] creates a new compact encoding to encompass a
      disjunction of cases.

      On the one hand, the [tag_bits] optional label can be used to
      specify the number of bits used to distinguish between cases. On
      the other hand, the [inner_bits] optional label can be used to
      specify the number of bits used by the compact encoding of the
      disjunction cases.

      Note that, if omitted, [tag_bits] and [inner_bits] are inferred
      by [union] to be minimal. To avoid breaking backward compatibility
      unknowingly, it is recommended to set them. *)
  val union : ?tag_bits:int -> ?inner_bits:int -> 'a case list -> 'a t

  (** [or_int32 c] creates a new compact encoding for the disjunction of
      any type [a] (see {!case}) with [int32]. It uses the same number
      of bits as {!int32}, that is 2, and uses the tag [11] for values
      of type [a]. *)
  val or_int32 :
    int32_kind:string ->
    alt_kind:string ->
    'a encoding ->
    (int32, 'a) Either.t t

  (** {1 Custom} *)

  (** This module can be used to write compact encoding for complex type
      without using relying on the existing combinators. *)
  module Custom : sig
    type tag = int32

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

          {b Warning:} It is expected that [tag layout < 2^tag_len -
          1]. *)
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

        The binary representation is determined as follow.
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
