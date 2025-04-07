(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** Type that represents a protocol. *)
type protocol

(** Type that represents the state of the protocol manager. *)
type t

(** [eq_source_target state] is true if source and target protocols designate
    the same protocol. *)
val eq_source_target : t -> bool

(** [get_git_dir state] returns the absolute path of the tezos git repository
    directory where the protocol manager operates. *)
val get_git_dir : t -> string

(** [incr_commit_count state] increments by one the count of commits made by
    the protocol manager. *)
val incr_commit_count : t -> t

(** [make_absolute path state] returns the absolute path of [path] considering
    that it is relative to the git repository directory. *)
val make_absolute : string -> t -> string

(** Module type that contains functions to get protocol related directories. *)
module type Dir_sig = sig
  (** [get_src_proto state] returns the protocol source directory. *)
  val get_src_proto : t -> string

  (** [get_lib_protocol state] returns the lib_protocol directory of the
      protocol. *)
  val get_lib_protocol : t -> string

  (** [get_doc state] returns the doc directory of the protocol. *)
  val get_doc : t -> string
end

(** Module type that contains functions to get informations of a protocol. *)
module type Protocol_sig = sig
  (** Module that contains functions to get the directories of the protocol. *)
  module Dir : sig
    (** Module that contains functions to get the paths relative to the git
        repository directory. *)
    module Relative : Dir_sig

    (** Module that contains the functions to get the absolute paths. *)
    module Absolute : Dir_sig
  end

  (** [get_name state] returns the name of the protocol in lowercase. *)
  val get_name : t -> string

  (** [get_version_value state] returns the protocol version_value string. It
      is of the form quebec_021 or alpha. *)
  val get_version_value : t -> string

  (** [get_constructor state] returns a string representing an Ocaml
      constructor of the protocol. *)
  val get_constructor : t -> string

  (** [get_hash ~__LOC__ ?short state] returns the hash of the protocol if
      exists or fails. If [short] is provided returns the 8 first characters of
      the hash. *)
  val get_hash : __LOC__:string -> ?short:unit -> t -> string

  (** [get_bin_suffix ~__LOC__ state] returns the suffix used for protocol
      binaries. If the protocol has a version but not a hash it fails. *)
  val get_bin_suffix : __LOC__:string -> t -> string

  (** [get_module_name_root ~__LOC__ state] returns the root of protocol module
      names.  If the protocol has a version but not a hash it fails. *)
  val get_module_name_root : __LOC__:string -> t -> string

  (** [get_dune_name ~__LOC__ state] returns the name of the protocol used in
      dune files. If the protocol has a version but not a hash it fails. *)
  val get_dune_name : __LOC__:string -> t -> string

  (** [get_version ~__LOC__ state] returns ther version of the protocol. If the
      protocol has not a version it fails. *)
  val get_version : __LOC__:string -> t -> int
end

(** Module to get information about the source protocol. *)
module Source : Protocol_sig

(** Module to get information about the target protocol. *)
module Target : Protocol_sig

(** [set_target_hash ~hash state] returns [state] but with [hash] as hash of
    the target protocol. *)
val set_target_hash : hash:string option -> t -> t

(** Log an error message and how to clean up created files and exits the
    process. *)
val exit : __LOC__:string -> t -> 'a

(** Creates a state. *)
val create :
  git_dir:string -> source:string -> target:string -> target_pattern:string -> t
