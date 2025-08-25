(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
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

module type S = sig
  (** The type of primitives. *)
  type head

  (** The type of paths. *)
  type path

  (** The type of patterns. *)
  type t

  (** The type of a pattern on a list of terms. *)
  type plist

  (** Micheline nodes. *)
  type node

  (** Returns true iff a pattern matches a node. *)
  val pattern_matches : t -> node -> bool

  (** Returns all matches of a pattern in a node. *)
  val all_matches : t -> node -> path list

  (** [focus_matches patt matches] converts a list of matches for a contextual
      pattern [patt] into the list of matches for the focused sub-pattern.
      Returns the empty list if there is no focused subpattern. *)
  val focus_matches : t -> path list -> path list

  (** Matches an integer, with an optional predicate. *)
  val int : (Z.t -> bool) option -> t

  (** Matches a string, with an optional predicate. *)
  val string : (string -> bool) option -> t

  (** Matches a bytes, with an optional predicate. *)
  val bytes : (Bytes.t -> bool) option -> t

  (** [prim hd subpatts] constructs a pattern matching a primitive application
      with primitive equal to head and the specified subterms pattern [subpatts]. *)
  val prim : head -> plist -> t

  (** More general version of [prim] where an arbitrary predicate on primitives
      can be used. *)
  val prim_pred : (head -> bool) -> plist -> t

  (** Matches a [seq] node with a specified subterms pattern. *)
  val seq : plist -> t

  (** Matches anything. *)
  val any : t

  (** Flags a pattern as being a focus of the pattern. There can be several foci per
      pattern but they cannot be nested (an error is raised in this case). *)
  val focus : t -> t

  (** Matches any list of patterns. *)
  val list_any : plist

  (** Matches the empty list of pattern. *)
  val list_empty : plist

  (** Matches a nonempty list of pattern. *)
  val list_cons : t -> plist -> plist

  (** Alias for [list_cons] *)
  val ( @. ) : t -> plist -> plist

  (** Pretty-printing. *)
  val pp : Format.formatter -> t -> unit

  (** Returns a unique id for patterns. Two patterns with the same uid are
      physically equal, but not necessarily the other way around (except
      in hash-consing mode). *)
  val uid : t -> int
end

module Make
    (X : Algebraic_signature.S)
    (Micheline : Micheline_sig.S with type head = X.t)
    (Path : Path.S) :
  S with type head = X.t and type path = Path.t and type node = Micheline.node

module Make_with_hash_consing
    (X : Algebraic_signature.S)
    (Micheline :
      Micheline_sig.S
        with type head = X.t
         and type label = Micheline_with_hash_consing.hcons_info)
    (Path : Path.S) : sig
  include
    S with type head = X.t and type path = Path.t and type node = Micheline.node

  val all_matches_with_hash_consing : t -> node -> path list
end
