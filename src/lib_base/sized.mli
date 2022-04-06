(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module type SizedSet = sig
  (** This module implements a sized-set data-structure.

      This module has the same interface as {!Stdlib.Set} but with a different
      performance trade-off:

      The cardinal function is much more efficient, but all other functions are
      a little bit slower. You should use this module if you need to call the
      [cardinal] function often. You should stick to {!Stdlib.Set} otherwise.*)

  (** All the types and functions from the underlying Sets are supported.

      The functions [union] and [inter] are less efficient than {!TzLwtreslib.Set.S.union} and {!TzLwtreslib.Set.S.inter} which should be considered instead, especially in case there are called several times.
      {!to_set} and {!of_set} can be used for this purpose.*)
  include TzLwtreslib.Set.S

  (** The type of corresponding Stdlib sets. *)
  type set

  (** [to_set t] returns the set of the sizedset [t] *)
  val to_set : t -> set

  (** [of_set s] returns the sizedset of the set [s] *)
  val of_set : set -> t
end

module MakeSizedSet (Set : TzLwtreslib.Set.S) :
  SizedSet with type elt = Set.elt and type set := Set.t

module type SizedMap = sig
  (** This module implements a sized-map data-structure.

      This module has the same interface as [Stdlib.Map] but with a different
      performance trade-off:

      The cardinal function is much more efficient, but all other functions are
      a little bit slower. You should use this module if you need to call the
      [cardinal] function often. You should stick to [Stdlib.Map] otherwise. *)

  (** All the types and functions from the underlying Maps are supported.

      The functions [merge] and [union] are less efficient than {!TzLwtreslib.Map.S.merge} and {!TzLwtreslib.Map.S.union} which should be considered instead, especially in case there are called several times.
      {!to_map} and {!of_map} can be used for this purpose.*)
  include TzLwtreslib.Map.S

  (** The type of corresponding Stdlib maps. *)
  type 'a map

  (** [to_map t] returns the map of the sizedmap [t] *)
  val to_map : 'a t -> 'a map

  (** [of_map m] returns the sizedmap of the map [m] *)
  val of_map : 'a map -> 'a t
end

module MakeSizedMap (Map : TzLwtreslib.Map.S) :
  SizedMap with type key = Map.key and type 'a map := 'a Map.t
