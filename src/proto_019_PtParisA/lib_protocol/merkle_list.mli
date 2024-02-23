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

type error += Merkle_list_invalid_position

(** Given a list of size [count_limit], returns the maximum depth of
    its merklisation. *)
val max_depth : count_limit:int -> int

module type T = sig
  (** The type of a Merkle list *)
  type t

  (** The type of a hash *)
  type h

  (** The type of an element *)
  type elt

  (** A path, together with an element's position, is the proof of inclusion
      of an element in the Merkle list. *)
  type path

  (** A dummy path that can be used as a placeholder when no path is
      actually required. *)
  val dummy_path : path

  val pp_path : Format.formatter -> path -> unit

  (** The empty Merkle list *)
  val nil : t

  (** The empty hash *)
  val empty : h

  (** [root t] returns the root hash of a Merkle list. *)
  val root : t -> h

  (** [snoc t el] adds element [el] to a Merkle list [t] and returns
      the new list. *)
  val snoc : t -> elt -> t

  (** Tail recursive variant of [snoc]. *)
  val snoc_tr : t -> elt -> t

  (** [of_list elems] returns the Merkle list constructed with [elems]. *)
  val of_list : elt list -> t

  (** [compute elems] returns the root hash of the Merkle list constructed with
      [elems]. *)
  val compute : elt list -> h

  (** Encoding of a path. *)
  val path_encoding : path Data_encoding.t

  (** Encoding of a path, with optional bound [max_length]. *)
  val bounded_path_encoding : ?max_length:int -> unit -> path Data_encoding.t

  (** [compute_path t pos] computes the path of the element in position [pos].

      Can fail with [Merkle_list_invalid_position] if [pos] is negative or
      if it is greater than the number of elements in the list. *)
  val compute_path : t -> int -> path tzresult

  (** [check_path path pos elt expected_root] checks that an [elt] with path
      [path] at position [pos] has the [expected_root].

      Can fail with [Merkle_list_invalid_position] if [pos] is negative or
      if it is greater than the number of elements in the list. *)
  val check_path : path -> int -> elt -> h -> bool tzresult

  (** [path_depth path] returns the depth of the tree [path] is
      related to. *)
  val path_depth : path -> int

  val elt_bytes : elt -> Bytes.t

  (**/**)

  module Internal_for_tests : sig
    val path_to_list : path -> h list

    (** Checks equality between Merkle lists. Outside of testing, clients should
        use [root] for comparison. *)
    val equal : t -> t -> bool

    val to_list : t -> h list
  end
end

module Make (El : sig
  type t

  val to_bytes : t -> bytes
end)
(H : S.HASH) : T with type elt = El.t and type h = H.t
