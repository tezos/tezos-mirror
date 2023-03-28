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

type key = string list

(** Raised when an encoder produced by [tagged_union] does not contain a
    matching branch. *)
exception No_tag_matched_on_encoding

(** Tree encoder type. *)
type -'a t

(** Represents a partial encoder for specific constructor of a sum-type. *)
type ('tag, 'a) case

(** [delayed f] produces a tree encoder that delays evaluation of [f ()] until
    the encoder is actually needed. This is required to allow for directly
    recursive encoders. *)
val delayed : (unit -> 'a t) -> 'a t

(** [contramap f e] is contravariant map operation that creates a new decoder
    that maps its input using [f] before feeding it to [e]. *)
val contramap : ('a -> 'b) -> 'b t -> 'a t

(** [contramap_lwt f e]. Same as [contramap] except that [f] als produces
    an Lwt effect. *)
val contramap_lwt : ('a -> 'b Lwt.t) -> 'b t -> 'a t

(** [ignore] is an encoder that ignores its value. *)
val ignore : 'a t

(** [run enc x tree] encodes the given value [x] using the encoder [enc] and
    writes it to the tree [tree]. May raise a [Key_not_found] or a
    [No_tag_matched] exception. *)
val run : 'tree Tree.backend -> 'a t -> 'a -> 'tree -> 'tree Lwt.t

(** [raw key] returns an encoder that encodes raw bytes at the given key. *)
val raw : key -> bytes t

(** [value_option key enc] encodes the value at a given [key] using the
    provided [enc] encoder for the value, or remove any previous
    value stored at [key] if [None] is provided. *)
val value_option : key -> 'a Data_encoding.t -> 'a option t

(** [value key enc] encodes the value at a given [key] using the provided
    [enc] encoder for the value. *)
val value : key -> 'a Data_encoding.t -> 'a t

(** [scope key enc] moves the given encoder [enc] to encode values under a
    branch [key]. *)
val scope : key -> 'a t -> 'a t

(** [lazy_mapping to_key enc] returns a subtree plus key-value list
    encoder that encodes values from a given key-value list using the
    key-mapping function [to_key] and the provided encoder [enc] for
    the values.

    During the encoding process, the subtree is added to the target tree
    under the prefix, before the key-value list is processed. *)
val lazy_mapping :
  ('k -> key) -> 'v t -> (Tree.wrapped_tree option * ('k * 'v option) list) t

(** [case tag enc f] return a partial encoder that represents a case in a
    sum-type. The encoder hides the (existentially bound) type of the
    parameter to the specific case, provided a converter function [f] and
    base encoder [enc]. *)
val case : 'tag -> 'b t -> ('a -> 'b option) -> ('tag, 'a) case

(** [case_lwt tag enc f] same as [case tag enc f] except that [f] produces
    an [Lwt] on a successful match. *)
val case_lwt : 'tag -> 'b t -> ('a -> 'b Lwt.t option) -> ('tag, 'a) case

(** [tagged_union tag_enc cases] returns an encoder that uses [tag_enc] for
    encoding the value of a field [tag]. The encoder searches through the list
    of cases for a matching branch. When a matching branch is found, it
    applies its embedded encoder for the value. This function is used for
    constructing encoders for sum-types.

    If an insufficient list of cases are provided, the resulting encoder may
    fail with a [No_tag_matched] error when [run].  *)
val tagged_union : 'tag t -> ('tag, 'a) case list -> 'a t

(** [lwt enc] promotes the given encoder [enc] to one that can handle lwt
    values. *)
val lwt : 'a t -> 'a Lwt.t t

(** [tup2 e1 e2] creates an encoder that encodes a tuple of elements using
    [e1] and [e2]. *)
val tup2 : 'a t -> 'b t -> ('a * 'b) t

(** [tup3 e1 e2] creates an encoder that encodes a triple of elements using
    [e1], [e2], and [e3]. *)
val tup3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t

(** [wrapped_tree] adds the [Tree.wrapped_tree] to the tree under the prefix
    at which it is called. *)
val wrapped_tree : Tree.wrapped_tree t
