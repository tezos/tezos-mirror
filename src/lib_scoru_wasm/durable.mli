(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 TriliTech <contact@trili.tech>                         *)
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

(** [t] allows a [wrapped_tree] to be manipulated as a tree of
    [chunked_byte_vector] *)
type t

(** [key] was too long, or contained invalid steps. *)
exception Invalid_key of string

(** A value was not found in the durable store. *)
exception Not_found

(** [Durable_storage.t] was empty. *)
exception Durable_empty

(** [encoding] is a [Tree_encoding] for [t]. *)
val encoding : t Tree_encoding.t

val of_storage :
  default:t -> Tezos_webassembly_interpreter.Durable_storage.t -> t

(** @raise Durable_empty *)
val of_storage_exn : Tezos_webassembly_interpreter.Durable_storage.t -> t

val to_storage : t -> Tezos_webassembly_interpreter.Durable_storage.t

(** [key] is the type that indexes [t]. It enforces several constraints:
    - a key's length is bounded.
    - a key is a series of non-empty steps, where
    - a step is preceded by '/'
    - a step only contains alphanumeric ascii, or dots ('.') *)
type key

(** [max_key_length] is the maximum length of a key in bytes. *)
val max_key_length : int

(** raise @Invalid_key *)
val key_of_string_exn : string -> key

val to_value_key : key -> string list

(** [find_value durable key] optionally looks for the value encoded at [key]
    in [durable]. *)
val find_value : t -> key -> Lazy_containers.Chunked_byte_vector.t option Lwt.t

(** raise @Not_found *)
val find_value_exn : t -> key -> Lazy_containers.Chunked_byte_vector.t Lwt.t

(** [copy_tree_exn tree from_key to_key] produces a new tree in which a copy of 
    the entire subtree at from_key is copied at  to_key.*)
val copy_tree_exn : t -> key -> key -> t Lwt.t

(** [count_subtrees durable key] returns the number of subtrees under [key]. *)
val count_subtrees : t -> key -> int Lwt.t

(** [delete durable key] deletes the value at and/or subtrees of [key]. *)
val delete : t -> key -> t Lwt.t

(** [hash_exn durable key] retrieves the tree hash of the value at the given [key].
    This is not the same as the hash of the value.

    @raise Not_found when [key] is not found
*)
val hash_exn : t -> key -> Context_hash.t Lwt.t
