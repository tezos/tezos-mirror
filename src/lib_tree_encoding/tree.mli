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

(** Exposes a module type {!S} representing trees. *)

type key = string list

type value = bytes

exception Incorrect_tree_type

(** An immutable tree API. *)
module type S = sig
  type tree

  (** @raise Incorrect_tree_type *)
  val select : Lazy_containers.Lazy_map.tree -> tree

  val wrap : tree -> Lazy_containers.Lazy_map.tree

  val remove : tree -> key -> tree Lwt.t

  val add : tree -> key -> value -> tree Lwt.t

  val add_tree : tree -> key -> tree -> tree Lwt.t

  val find : tree -> key -> value option Lwt.t

  val find_tree : tree -> key -> tree option Lwt.t

  val length : tree -> key -> int Lwt.t
end

type 'tree backend = (module S with type tree = 'tree)

val select : 'tree backend -> Lazy_containers.Lazy_map.tree -> 'tree

val wrap : 'tree backend -> 'tree -> Lazy_containers.Lazy_map.tree

val remove : 'tree backend -> 'tree -> key -> 'tree Lwt.t

val add : 'tree backend -> 'tree -> key -> value -> 'tree Lwt.t

val add_tree : 'tree backend -> 'tree -> key -> 'tree -> 'tree Lwt.t

val find : 'tree backend -> 'tree -> key -> value option Lwt.t

val find_tree : 'tree backend -> 'tree -> key -> 'tree option Lwt.t

val length : 'tree backend -> 'tree -> key -> int Lwt.t

(** A [wrapped_tree] allows modifications to the underlying tree, without
    affecting the tree that it was decoded from. *)
type wrapped_tree = Wrapped_tree : 'tree * 'tree backend -> wrapped_tree

module Wrapped : S with type tree = wrapped_tree
