(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022-2023 TriliTech <contact@trili.tech>                    *)
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

(** Invalid index for a subkey *)
exception Index_too_large of int

(** A value was not found in the durable store. *)
exception Value_not_found

(** A tree does not exists under key in the durable store. *)
exception Tree_not_found

(** Attempted to write/read to/from a value at [offset],
    beyond the [limit]. *)
exception Out_of_bounds of (int64 * int64)

(** [Durable_storage.t] was empty. *)
exception Durable_empty

(** Cannot modify a readonly value. *)
exception Readonly_value

(** Cannot read from or write to more than 2,048 bytes *)
exception IO_too_large

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

(** @raise Invalid_key *)
val key_of_string_exn : string -> key

val key_of_string_opt : string -> key option

(** [find_value durable key] optionally looks for the value encoded at [key]
    in [durable]. *)
val find_value :
  t -> key -> Tezos_lazy_containers.Chunked_byte_vector.t option Lwt.t

(** @raise Value_not_found *)
val find_value_exn :
  t -> key -> Tezos_lazy_containers.Chunked_byte_vector.t Lwt.t

(** [copy_tree_exn tree ?edit_readonly from_key to_key] produces a new tree in which a copy of
    the entire subtree at from_key is copied to to_key.

    [~edit_readonly:true] allows a a tree to be copied into a readonly location.

    @raise Readonly_value
*)
val copy_tree_exn : t -> ?edit_readonly:bool -> key -> key -> t Lwt.t

(** [move_tree_exn tree from_key to_key] produces a new tree in which
    the entire subtree at from_key is moved to to_key.

    @raise Readonly_value
*)
val move_tree_exn : t -> key -> key -> t Lwt.t

(** [list durable key] returns the subkeys under [key]. *)
val list : t -> key -> string list Lwt.t

(** [count_subtrees durable key] returns the number of subtrees under [key]. *)
val count_subtrees : t -> key -> int Lwt.t

(** [subtree_name_at durable key n] returns the name of the n_th subtree
    under [key]. *)
val subtree_name_at : t -> key -> int -> string Lwt.t

(** [delete ?edit_readonly durable key] deletes the value at and/or
    subtrees of [key].

    @raise Readonly_value when [edit_readonly] is not set while trying
    to edit the readonly section.
*)
val delete : ?edit_readonly:bool -> t -> key -> t Lwt.t

(** [hash ~kind durable key] retrieves the tree hash of the value (if
    [kind = `Value]) or the subtree ([kind = `Subtree]) at the given
    [key].  This is not the same as the hash of the value. *)
val hash : kind:[`Value | `Subtree] -> t -> key -> Context_hash.t option Lwt.t

(** [hash_exn ~kind durable key] retrieves the tree hash of the value
    (if [kind = `Value]) or the subtree ([kind = `Subtree]) at the
    given [key]. This is not the same as the hash of the value.

    @raise Value_not_found when [key] is not found and [kind = `Subtree]
    @raise Tree_not_found when [key] is not found and [kind = `Value]. *)
val hash_exn : kind:[`Value | `Subtree] -> t -> key -> Context_hash.t Lwt.t

(** [set_value_exn durable key str] installs the value [str] in
    [durable] under [key], replacing any previous contents under this
    key without fetching it. *)
val set_value_exn : t -> ?edit_readonly:bool -> key -> string -> t Lwt.t

(** [write_value_exn ?edit_readonly durable key offset bytes] writes
    [bytes] to [key], starting at the given [offset].

    If no value at [key] exists, it is created.

    [~edit_readonly:true] allows a value to be written into a readonly location.

    @raise Out_of_bounds
    @raise Readonly_value iff [edit_readonly] is not set to [true]
    when attempting to write in the [readonly] section.
*)
val write_value_exn :
  t -> ?edit_readonly:bool -> key -> int64 -> string -> t Lwt.t

(** [read_value durable key offset max_bytes] reads up to [max_bytes]
    bytes from the value at [key], starting at the given [offset].

    @raise Value_not_found when [key] is not found.
    @raise Out_of_bounds when [offset] is larger than the value.
*)
val read_value_exn : t -> key -> int64 -> int64 -> string Lwt.t

module Internal_for_tests : sig
  val key_is_readonly : key -> bool

  val key_to_list : key -> string list
end
