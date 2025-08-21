(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2021 Nomadic Labs, <contact@nomadic-labs.com>          *)
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

(** Errors related to checks related to the data dir. *)
type error += Invalid_data_dir of {data_dir : string; msg : string option}

type error += Could_not_read_data_dir_version of string

(** Default file names to store the information about the node's network
    identity, peers and configuration. *)

val default_identity_file_name : string

val default_config_file_name : string

val default_peers_file_name : string

(** Defines the properties that must be satisfied by the call to
   [ensure_data_dir].

    Depending of the given mode, it will:
    - Exists: ensures that the directory exists, if not, creates a
      fresh directory by creating it and writing a version file.
    - Is_bare: ensures that, if the directory exists, it contains no other
      file than:
            - the version file;
            - the identity file;
            - the configuration file;
            - the peer list file;
      Otherwise, creates an fresh directory.
    - Is_compatible: similar to [Exists] but, additionally, will
      check the version compatibility.
*)
type ensure_mode =
  | Exists
  | Is_bare of {clean_if_needed : bool}
  | Is_compatible

(** [ensure_data_dir ~mode genesis data_dir] ensures that a directory
    is valid with regards to the given [mode]. See [ensure_mode] for
    the mode's properties. By default, the mode is set to
    [Is_compatible]. *)
val ensure_data_dir :
  ?mode:ensure_mode -> Genesis.t -> string -> unit tzresult Lwt.t

(** Upgrade data directory from an older version.

    [upgrade_data_dir dir genesis ~chain_name ~sandbox_parameters]
    checks if an upgrade of the given data directory [dir] is
    available. If the data directory is upgradable then the upgrade is
    performed. Otherwise, nothing is done. *)
val upgrade_data_dir :
  data_dir:string ->
  Genesis.t ->
  chain_name:Distributed_db_version.Name.t ->
  sandbox_parameters:(string * Data_encoding.json) option ->
  unit tzresult Lwt.t

(** [store_dir dir] is a directory within [dir] that the node uses for
    its store. In order for [store_dir dir] to be valid, [dir] must be
    a valid directory name. *)
val store_dir : string -> string

(** [protocol_dir dir] is a directory within [dir] that the node uses
    for its protocol. In order for [protocol_dir dir] to be valid,
    [dir] must be a valid directory name.*)
val protocol_dir : string -> string

(** [lock_file dir] is a file within [dir] that the node uses for its
    lock. In order for [lock_file dir] to be valid, [dir] must be a
    valid directory name. *)
val lock_file : string -> string

(** [upgrade_status ~data_dir] returns whether or not an upgrade is
    available. In addition to that, it prints a user friendly log. *)
val upgrade_status : data_dir:string -> bool tzresult Lwt.t
