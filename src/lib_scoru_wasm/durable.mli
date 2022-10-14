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

(** Attempted to write/read to/from a value at [offset],
    beyond the [limit]. *)
exception Out_of_bounds of (int64 * int64)

(** [Durable_storage.t] was empty. *)
exception Durable_empty

(** [encoding] is a [Tezos_tree_encoding] for [t]. *)
val encoding : t Tezos_tree_encoding.t

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

val key_of_string_opt : string -> key option

(** [find_value durable key] optionally looks for the value encoded at [key]
    in [durable]. *)
val find_value :
  t -> key -> Tezos_lazy_containers.Chunked_byte_vector.t option Lwt.t

(** raise @Not_found *)
val find_value_exn :
  t -> key -> Tezos_lazy_containers.Chunked_byte_vector.t Lwt.t

(** [copy_tree_exn tree from_key to_key] produces a new tree in which a copy of
    the entire subtree at from_key is copied to to_key.*)
val copy_tree_exn : t -> key -> key -> t Lwt.t

(** [move_tree_exn tree from_key to_key] produces a new tree in which
    the entire subtree at from_key is moved to to_key.*)
val move_tree_exn : t -> key -> key -> t Lwt.t

(** [count_subtrees durable key] returns the number of subtrees under [key]. *)
val count_subtrees : t -> key -> int Lwt.t

(** [subtree_name_at durable key n] returns the name of the n_th subtree
    under [key]. *)
val subtree_name_at : t -> key -> int -> string Lwt.t

(** [delete durable key] deletes the value at and/or subtrees of [key]. *)
val delete : t -> key -> t Lwt.t

(** [hash_exn durable key] retrieves the tree hash of the value at the given [key].
    This is not the same as the hash of the value.

    @raise Not_found when [key] is not found
*)
val hash_exn : t -> key -> Context_hash.t Lwt.t

(** [write_value durable key offset bytes] writes [bytes] to [key],
    starting at the given [offset].

    If no value at [key] exists, it is created.

    @raise Out_of_bounds
*)
val write_value_exn : t -> key -> int64 -> string -> t Lwt.t

(** [read_value durable key offset max_bytes] reads up to [max_bytes]
    bytes from the value at [key], starting at the given [offset].

    @raise Not_found when [key] is not found.
    @raise Out_of_bounds when [offset] is larger than the value.
*)
val read_value_exn : t -> key -> int64 -> int64 -> string Lwt.t
