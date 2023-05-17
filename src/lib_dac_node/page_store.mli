(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Marigold <contact@marigold.dev>                        *)
(* Copyright (c) 2023 Trili Tech <contact@trili.tech>                        *)
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
  | Reveal_data_path_not_a_directory of string
  | Cannot_create_reveal_data_dir of string
  | Cannot_write_page_to_page_storage of {hash : string; content : bytes}
  | Cannot_read_page_from_page_storage of string
  | Incorrect_page_hash of {
      expected : Dac_plugin.raw_hash;
      actual : Dac_plugin.raw_hash;
    }

(** [S] is the module type defining the backend required for
    persisting DAC pages data onto the page storage. *)
module type S = sig
  (** [t] represents an instance of a storage backened. *)
  type t

  (** [configuration] is the type of the configuration which can
      be used to initialize the store.*)
  type configuration

  (** [init configuration] initializes a page store using the information
      provided in [configuration]. *)
  val init : configuration -> t

  (** [save dac_plugin t ~hash ~content] writes [content] of page onto storage
      backend [t] under given [key].

      When writing fails it returns a [Cannot_write_page_to_page_storage]
      error.

      We require [save] to be atomic. *)
  val save :
    Dac_plugin.t ->
    t ->
    hash:Dac_plugin.hash ->
    content:bytes ->
    unit tzresult Lwt.t

  (** [mem dac_plugin t hash] Checks whether there is an entry for [hash] in
      the page storage.
      It can fail with error [Cannot_read_from_page_storage hash_as_string],
      if it was not possible to check that the page exists for any reason
      related to the page storage backend implementation. *)
  val mem : Dac_plugin.t -> t -> Dac_plugin.hash -> bool tzresult Lwt.t

  (** [load dac_plugin t hash] returns [content] of the storage backend [t]
      represented by a key. When reading fails it returns a
      [Cannot_read_page_from_page_storage] error. *)
  val load : Dac_plugin.t -> t -> Dac_plugin.hash -> bytes tzresult Lwt.t
end

(** [Filesystem] is an implementation of the page store backed up by
    the local filesystem. The configuration is a string denoting the
    path where files will be saved. A page is saved as a file whose
    name is the hex encoded hash of its content. *)
module Filesystem : S with type configuration = string

(** The configuration type for page stores that fetch pages remotely, when they
    are not available on the local page store. The parameter [cctxt]
    corresponds to the client context used to connect to the remote host for
    fetching pages. *)
type remote_configuration = {
  cctxt : Dac_node_client.cctxt;
  page_store : Filesystem.t;
}

(** A [Page_store] implementation backed by the local filesystem, which
   uses a connection to a Dac node to retrieve pages that are not
   saved locally. *)
module Remote : S with type configuration = remote_configuration

(**/**)

module Internal_for_tests : sig
  (** [With_data_integrity_check] tweaks a module [P] of type [Page_store]
      so that the [content] of a page is verified against the [hash] supplied,
      before the page is saved in the underlying [Page_store]. If the
      verification of the hash fails, the
      page is not saved and the error [Hash_of_page_is_invalid] is returned. *)
  module With_data_integrity_check (P : S) :
    S with type configuration = P.configuration and type t = P.t

  (** [With_remote_context(R)(P)] tweaks a module [P] of type [Page_store]
      so that it will fetch pages using a function [R.fetch] to load pages
      remotely and saved them in the page store of type [P], when a page
      is not present in said store. *)
  module With_remote_fetch (R : sig
    type remote_context

    val fetch :
      Dac_plugin.t -> remote_context -> Dac_plugin.hash -> bytes tzresult Lwt.t
  end)
  (P : S) :
    S
      with type configuration = R.remote_context * P.t
       and type t = R.remote_context * P.t
end

(** [ensure_reveal_data_dir_exists reveal_data_dir] checks that the
      path at [reveal_data_dir] exists and is a directory. If
      the path does not exist, it is created as a directory.
      Parent directories are recursively created when they do not
      exist.

      This function may fail with
      {ul
        {li [Reveal_data_path_not_a_directory reveal_data_dir] if the
          path exists and is not a directory,}

        {li [Cannot_create_reveal_data_dir reveal_data_dir] If the
            creation of the directory fails.}
      }
  *)
val ensure_reveal_data_dir_exists : string -> unit tzresult Lwt.t
