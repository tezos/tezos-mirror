(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

exception Incorrect_tree_type

exception Uninitialized_self_ref

(** A key in the tree is a list of string. *)
type key = string trace

exception Key_not_found of key

type tree_instance = Tree.tree_instance = ..

(** {2 Types}*)

(** Represents a partial encoder for a specific constructor of a sum-type. *)
type ('tag, 'a) case

(** Represents an encoder and a decoder. *)
type 'a t

(** {2 Functions }*)

(** [return x] is an encoder that does nothing on encoding. On decoding it
    ignores the tree and returns [x]. *)
val return : 'a -> 'a t

(** [conv f g enc] transforms from one encoding to a different one using
    [f] for mapping the results decoded using [enc], and [g] for mapping from
    the input.

    It is the user's responsibility to ensure that [f] and [g] are inverses,
    that is, that [f (g x) = g (f x)] for all [x]. *)
val conv : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** [conv_lwt f g enc] is the same as [conv] but where [f] and [g] are
    effectful (produce lwt promises).

    It is the user's responsibility to ensure that [f] and [g] are inverses,
    that is, that [f (g x) = g (f x)] for all [x]. *)
val conv_lwt : ('a -> 'b Lwt.t) -> ('b -> 'a Lwt.t) -> 'a t -> 'b t

(** [tup2 ~flatten e1 e2] combines [e1] and [e2] into an encoder for pairs.
    If [flatten] is true, the elements are encoded directly under the given
    tree, otherwise each element is wrapped under an index node to avoid
    colliding keys.

    Example:
      [encode
        (tup2
          ~flatten:false
           (value [] Data_encoding.string)
           (value [] Data_encoding.string))
        (("A", "B"))]

    Gives a tree:
      "A"
      "B"

    While
      [encode
          (tup2
              ~flatten:true
              (value [] Data_encoding.string)
              (value [] Data_encoding.string))
          (("A", "B"))]

    Gives a tree:
      [1] -> "A"
      [2] -> "B" *)
val tup2 : flatten:bool -> 'a t -> 'b t -> ('a * 'b) t

(** [tup3 ?flatten e1 e2 e3] combines the given encoders [e1 .. e3] into an
    encoder for a tuple of three elements. *)
val tup3 : flatten:bool -> 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [tup4 ?flatten  e1 e2 e3 e4] combines the given encoders [e1 .. e4] into an
    encoder for a tuple of four elements. *)
val tup4 : flatten:bool -> 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

(** [tup5 ?flatten e1 e2 e3 e4 e5] combines the given encoders [e1 .. e5] into
    an encoder for a tuple of five elements. *)
val tup5 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  ('a * 'b * 'c * 'd * 'e) t

(** [tup6 ?flatten e1 e2 e3 e4 e5 e6] combines the given encoders [e1 .. e6]
    into an encoder for a tuple of six elements. *)
val tup6 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  ('a * 'b * 'c * 'd * 'e * 'f) t

(** [tup7 ?flatten e1 e2 e3 e4 e5 e6 e7] combines the given encoders
    [e1 .. e7] into an encoder for a tuple of seven elements. *)
val tup7 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g) t

(** [tup8 ?flatten e1 e2 e3 e4 e5 e6 e7 e8] combines the given encoders
    [e1 .. e8] into an encoder for a tuple of eight elements. *)
val tup8 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  'h t ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) t

(** [tup9 ~flatten e1 e2 e3 e4 e5 e6 e7 e8 e9] combines the given encoders
    [e1 .. e9] into an encoder for a tuple of nine elements. *)
val tup9 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  'h t ->
  'i t ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) t

(** [tup10 ~flatten e1 e2 e3 e4 e5 e6 e7 e8 e9 e10] combines the given encoders
    [e1 .. e10] into an encoder for a tuple of ten elements. *)
val tup10 :
  flatten:bool ->
  'a t ->
  'b t ->
  'c t ->
  'd t ->
  'e t ->
  'f t ->
  'g t ->
  'h t ->
  'i t ->
  'j t ->
  ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i * 'j) t

(** [raw key] is an encoder for bytes under the given [key]. *)
val raw : key -> bytes t

(** [value_option key encoding] returns an encoder that uses [encoding]
    for encoding values, but does not fail if the [key] is
    absent. *)
val value_option : key -> 'a Data_encoding.t -> 'a option t

(** [value ?default key enc] creates an encoder under the given
    [key] using the provided data-encoding [enc] for
    encoding/decoding values, and using [default] as a fallback when
    decoding in case the [key] is absent from the tree. *)
val value : ?default:'a -> key -> 'a Data_encoding.t -> 'a t

(** [scope key enc] moves the given encoder [enc] to encode values under a
    branch [key]. *)
val scope : key -> 'a t -> 'a t

(** [case tag enc inj proj] returns a partial encoder that represents a case
    in a sum-type. The encoder hides the (existentially bound) type of the
    parameter to the specific case, provided converter functions [inj] and
    [proj] for the base encoder [enc]. *)
val case : 'tag -> 'b t -> ('a -> 'b option) -> ('b -> 'a) -> ('tag, 'a) case

(** [case_lwt tag enc inj proj] same as [case tag enc inj proj] but where
    [inj] and [proj] returns in lwt values. *)
val case_lwt :
  'tag -> 'b t -> ('a -> 'b Lwt.t option) -> ('b -> 'a Lwt.t) -> ('tag, 'a) case

(** [tagged_union tag_enc cases] returns an encoder that use [tag_enc] for
    encoding the value of a field [tag]. The encoder searches through the list
    of cases for a matching branch. When a matching branch is found, it uses
    its embedded encoder for the value. This function is used for constructing
    encoders for sum-types.

    The [default] labeled argument can be provided to have a
    fallback in case the value is missing from the tree. *)
val tagged_union :
  ?default:(unit -> 'a) -> 'tag t -> ('tag, 'a) case list -> 'a t

(** [option enc] lifts the given encoding [enc] to one that can encode
    optional values. *)
val option : 'a t -> 'a option t

(** [delayed f] produces a tree encoder/decoder that delays evaluation of
    [f ()] until the encoder or decoder is actually needed. This is required
    to allow for directly recursive encoders/decoders. *)
val delayed : (unit -> 'a t) -> 'a t

(** [either enc_a enc_b] returns an encoder where [enc_a] is used for
    the left case of [Either.t], and [enc_b] for the [Right] case. *)
val either : 'a t -> 'b t -> ('a, 'b) Either.t t

module type TREE = sig
  type tree

  type key := string list

  type value := bytes

  (** @raise Incorrect_tree_type *)
  val select : Tree.tree_instance -> tree

  val wrap : tree -> Tree.tree_instance

  val remove : tree -> key -> tree Lwt.t

  val add : tree -> key -> value -> tree Lwt.t

  val add_tree : tree -> key -> tree -> tree Lwt.t

  val find : tree -> key -> value option Lwt.t

  val find_tree : tree -> key -> tree option Lwt.t

  val hash : tree -> Context_hash.t

  val length : tree -> key -> int Lwt.t

  val list :
    tree -> ?offset:int -> ?length:int -> key -> (string * tree) list Lwt.t
end

type wrapped_tree

module Wrapped : TREE with type tree = wrapped_tree

(** [wrapped_tree] is a tree encoding for wrapped tree. *)
val wrapped_tree : wrapped_tree t

module Runner : sig
  module type S = sig
    type tree

    (** [encode enc x tree] encodes a value [x] using the encoder [enc]
    into the provided [tree]. *)
    val encode : 'a t -> 'a -> tree -> tree Lwt.t

    (** [decode enc x tree] decodes a value using the encoder [enc] from
    the provided [tree]. *)
    val decode : 'a t -> tree -> 'a Lwt.t
  end

  (** Builds a new runner for encoders using a specific tree. *)
  module Make (T : TREE) : S with type tree = T.tree
end

module Encodings_util : sig
  module type Bare_tezos_context_sig = sig
    type t

    type tree

    type index

    module Tree :
      Tezos_context_sigs.Context.TREE
        with type t := t
         and type key := string list
         and type value := bytes
         and type tree := tree

    val init :
      ?patch_context:(t -> t tzresult Lwt.t) ->
      ?readonly:bool ->
      ?index_log_size:int ->
      string ->
      index Lwt.t

    val empty : index -> t
  end

  module Make (Ctx : Bare_tezos_context_sig) : sig
    module Tree : sig
      include module type of Ctx.Tree

      type tree = Ctx.tree

      val select : Tree.tree_instance -> tree

      val wrap : tree -> Tree.tree_instance
    end

    module Tree_encoding_runner : Runner.S with type tree = Ctx.tree

    (* Create an empty tree *)
    val empty_tree : unit -> Ctx.tree Lwt.t
  end
end

module Lazy_map_encoding : sig
  module type Lazy_map_sig = sig
    type key

    type 'a t

    type 'a producer = key -> 'a Lwt.t

    module Map : Stdlib.Map.S with type key = key

    val origin : 'a t -> wrapped_tree option

    val string_of_key : key -> string

    val loaded_bindings : 'a t -> (key * 'a option) list

    val create :
      ?values:'a Map.t ->
      ?produce_value:'a producer ->
      ?origin:wrapped_tree ->
      unit ->
      'a t
  end

  module type S = sig
    type 'a map

    val lazy_map : 'a t -> 'a map t
  end

  (** [Make (YouMap)] creates a module with the [lazy_map]
      combinator which can be used to decode [YouMap] specifically. *)
  module Make (Map : Lazy_map_sig) : S with type 'a map := 'a Map.t
end

module Lazy_vector_encoding : sig
  module type Lazy_vector_sig = sig
    type 'a t

    type key

    type 'a producer = key -> 'a Lwt.t

    module Map : Lazy_map_encoding.Lazy_map_sig with type key = key

    val origin : 'a t -> wrapped_tree option

    val string_of_key : key -> string

    val loaded_bindings : 'a t -> (key * 'a option) list

    val create :
      ?first_key:key ->
      ?values:'a Map.Map.t ->
      ?produce_value:'a producer ->
      ?origin:wrapped_tree ->
      key ->
      'a t

    val num_elements : 'a t -> key

    val first_key : 'a t -> key
  end

  module type S = sig
    type 'a vector

    type key

    val lazy_vector : key t -> 'a t -> 'a vector t
  end

  module Make (Vector : Lazy_vector_sig) :
    S with type 'a vector := 'a Vector.t and type key := Vector.key
end

module CBV_encoding : sig
  module type CBV_sig = sig
    type t

    type chunk

    val origin : t -> wrapped_tree option

    val loaded_chunks : t -> (int64 * chunk option) list

    val length : t -> int64

    val create :
      ?origin:wrapped_tree -> ?get_chunk:(int64 -> chunk Lwt.t) -> int64 -> t
  end

  module type S = sig
    type cbv

    type chunk

    val cbv : chunk t -> cbv t
  end

  module Make (CBV : CBV_sig) :
    S with type cbv := CBV.t and type chunk := CBV.chunk
end
