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

(** Raised when a requested key is not present. *)
exception Key_not_found of key

(** Raised when an encoder produced by [tagged_union] does not contain a
    matching branch. *)
exception No_tag_matched_on_decoding

(** Raised when data-encoding fails to decode a certain value. *)
exception Decode_error of {key : key; error : Data_encoding.Binary.read_error}

(** Tree decoder type *)
type 'a t

(** Represents a partial encoder for a specific constructor of a sum-type. *)
type ('tag, 'a) case

(** [run decoder tree] runs the tree decoder against the tree. May raise a
    [Key_not_found] or a [No_tag_matched] exception. *)
val run : 'tree Tree.backend -> 'a t -> 'tree -> 'a Lwt.t

(** [raw key] retrieves the raw value at the given [key].

    @raises Key_not_found when the requested key is not presented *)
val raw : key -> bytes t

(** [value_option key data_encoding] tries to retrieve the value at a
    given [key] by decoding its raw value using the provided
    [data_encoding], or return [None] if [key] is missing.

    @raises Decode_error when decoding of the value fails *)
val value_option : key -> 'a Data_encoding.t -> 'a option t

(** [value ?default key data_encoding] retrieves the value at a
    given [key] by decoding its raw value using the provided
    [data_encoding].

    The [default] labeled argument can be provided to specify a
    fallback value for when the key is absent from the tree.

    @raises Key_not_found when the requested key is not presented
    and the [default] argument is omitted.
    @raises Decode_error when decoding of the value fails *)
val value : ?default:'a -> key -> 'a Data_encoding.t -> 'a t

(** [scope key decoder] applies a tree decoder for a provided [key].

    @raises Key_not_found when the requested key is not presented *)
val scope : key -> 'a t -> 'a t

(** [lazy_mapping to_key decoder] decodes to a function [f] that can
    be called to look up keyed values in the current tree and the
    subtree from where the values are decoded.

    For example, the expression [f k] is virtually equivalent to
    running a decoder like [value (to_key k) decoder] against the tree.

    This function is primarily useful when providing a [~produce_value]
    function to [Lazy_map.create]. *)
val lazy_mapping :
  ('i -> key) -> 'a t -> (Tree.wrapped_tree option * ('i -> 'a Lwt.t)) t

(** [delayed f] produces a tree decoder that delays evaluation of [f ()] until
    the decoder is actually needed. This is required to allow for directly
    recursive decoders. *)
val delayed : (unit -> 'a t) -> 'a t

(** [of_lwt p] lifts the promise [p] into a decoding value. *)
val of_lwt : 'a Lwt.t -> 'a t

(** [map f d] maps over the result of the decoder [d] with function [f]. *)
val map : ('a -> 'b) -> 'a t -> 'b t

(** [map_lwt f d] maps over the result of the decoder [d] with the effectful
      function [f]. *)
val map_lwt : ('a -> 'b Lwt.t) -> 'a t -> 'b t

(** [case tag dec f] returns a partial decoder that represents a case in a
    variant type. The decoder hides the (existentially bound) type of the
    parameter to the specific case, provided a converter function [f] and
    base decoder [dec]. *)
val case : 'tag -> 'b t -> ('b -> 'a) -> ('tag, 'a) case

(** [case_lwt tag enc f] same as [case tag enc f] except that [f] produces
    an [Lwt] value. *)
val case_lwt : 'tag -> 'b t -> ('b -> 'a Lwt.t) -> ('tag, 'a) case

(** [tagged_union ?default tag_dec cases] returns a decoder that use
    [tag_dec] for decoding the value of a field [tag]. The decoder
    searches through the list of cases for a matching branch. When a
    matching branch is found, it uses its embedded decoder for the
    value. This function is used for constructing decoders for
    sum-types.

    [default] is an optional labeled argument that can be provided
    in order to have a fallback to use in case the tag is absent
    from the tree (which means, the value has not yet been
    initialized in the tree).

    If an insufficient list of cases are provided, the resulting encoder may
    fail with a [No_tag_matched] error when [run].  *)
val tagged_union :
  ?default:(unit -> 'a) -> 'tag t -> ('tag, 'a) case list -> 'a t

(** [wrapped_tree] returns the [Tree.wrapped_tree] located at the prefix tree
    under which it is called.

    @raises Key_not_found when the requested key is not present. *)
val wrapped_tree : Tree.wrapped_tree t

(** Syntax module for the {!Tree_decoding}. This is intended to be opened
    locally in functions. Within the scope of this module, the code can
    include binding operators, leading to a [let]-style syntax. Similar to
    {!Lwt_result_syntax} and other syntax modules. *)
module Syntax : sig
  (** [return x] returns a value in the decoding monad. *)
  val return : 'a -> 'a t

  (** [bind m f] monadic composition that decodes using [m] and passes the
      result to [f]. *)
  val bind : 'a t -> ('a -> 'b t) -> 'b t

  (** [both d1 d2] returns a decoder that decodes using [d1] followed by [d2]
      and combines their results. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** [let*] is a binding operator alias for {!bind}. *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

  (** [let+] is a binding operator alias for {!map}. *)
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

  (** [and*] is a binding operator alias for {!both}. *)
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t

  (** [and*] is a binding operator alias for {!both}. *)
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end
