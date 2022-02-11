(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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
val make : ?tag_size:[`Uint8 | `Uint16] -> 'a t -> 'a Data_encoding.t

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
val payload : 'a Data_encoding.t -> 'a t

(** Uses one bit in the tag to encode an option. *)
val option : 'a t -> 'a option t

(** {2 Conversion} *)

(** [conv ?json f g e] reuses the encoding [e] for type [b] to encode
    an type [a] using the isomorphism [(f, g)]. The optional argument
    allows to overwrite the encoding used for JSON, in place of the
    one computed by default. *)
val conv : ?json:'a Data_encoding.t -> ('a -> 'b) -> ('b -> 'a) -> 'b t -> 'a t

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
val obj4 : 'a field -> 'b field -> 'c field -> 'd field -> ('a * 'b * 'c * 'd) t

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
val list : bits:int -> 'a Data_encoding.t -> 'a list t

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
  'a Data_encoding.t ->
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
    val partial_encoding : layout -> input Data_encoding.t

    (** [classify x] returns the layout to be used to encode [x]. *)
    val classify : input -> layout

    (** The encoding to use when targeting a JSON output. *)
    val json_encoding : input Data_encoding.t
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
