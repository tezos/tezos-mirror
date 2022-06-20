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

type error +=
  | Cannot_write_file of string
  | Cannot_create_dir of string
  | Cannot_read_file of string
  | Io_error of [`Close | `Open] Lwt_utils_unix.io_error
  | Unix_error of Unix.error
  | Decoding_error of Data_encoding.Binary.read_error

(** Signature for hash tables with additional information *)
module type H = sig
  include Hashtbl.SeededS

  (** Type of values  *)
  type value

  (** Name used to derive a path (relative to [data_dir] in [load_from_disk]) of
      where to store the persistent information for this hash table. *)
  val name : string

  (** String version of key (used for filenames). *)
  val string_of_key : key -> string

  (** Parse a key. We must have [key_of_string (string_of_key k) = k]. *)
  val key_of_string : string -> key option

  (** Encoding for values (only the binary encoding is used *)
  val value_encoding : value Data_encoding.t
end

(** Create an on-disk persistent version of {!Hashtbl}. *)
module Make_table (H : H) : sig
  type key = H.key

  type value = H.value

  (** Type of persistent hash tables *)
  type t

  (** Persistent version of {!H.replace} *)
  val replace : t -> key -> value -> unit tzresult Lwt.t

  (** Persistent version of {!H.remove} *)
  val remove : t -> key -> unit tzresult Lwt.t

  (** Same as {!H.find} *)
  val find : t -> key -> value option

  (** Same as {!H.iter_s} *)
  val iter_s : (key -> value -> unit Lwt.t) -> t -> unit Lwt.t

  (** Same as {!H.iter_es} *)
  val iter_es :
    (key -> value -> unit tzresult Lwt.t) -> t -> unit tzresult Lwt.t

  (** Persistent version of {!H.replace_seq} *)
  val replace_seq : t -> (key * value) Seq.t -> unit tzresult Lwt.t

  (** [load_from_disk ~initial_size ~data_dir] creates a hash table of size
      [initial_size]. The hash table is populated by persistent elements present
      in [data_dir/H.name] (the directory is created if it does not exist). *)
  val load_from_disk : initial_size:int -> data_dir:string -> t tzresult Lwt.t
end

(** Create an on-disk persistent version of the {!Hash_queue} data structure. *)
module Make_queue (N : sig
  (** Name used to derive a path (relative to [data_dir] in [load_from_disk]) of where
      to store the persistent information for this queue. *)
  val name : string
end)
(K : S.HASH) (V : sig
  type t

  val encoding : t Data_encoding.t
end) : sig
  type t

  (** [remove q k] removes the binding from [k] in [q]. If [k] is not bound in
      [c], it does nothing. The removal is persisted on disk. *)
  val remove : t -> K.t -> unit tzresult Lwt.t

  (** [replace q k v] binds the key [k] to the value [v] in the queue [q]. This
      may or may not cause another binding to be removed, depending on the
      number of bindings already present in [q]. The addition (or replacement)
      is persisted on disk. *)
  val replace : t -> K.t -> V.t -> unit tzresult Lwt.t

  (** [fold f q init] folds the function [f] over the bindings
      of [q] (in memory). The elements are iterated from oldest to newest. *)
  val fold : (K.t -> V.t -> 'a -> 'a) -> t -> 'a -> 'a

  (** [load_from_disk ~capacity ~data_dir] creates a bounded hash queue of
      capacity [capacity]. The queue is populated by persistent elements present
      in [data_dir/N.name] (the directory is created if it does not exist). *)
  val load_from_disk : capacity:int -> data_dir:string -> t tzresult Lwt.t
end
