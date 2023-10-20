(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type t = {rollup_address : Address.t; context_version : Context.Version.t}

(** Read the metadata file from [dir]. *)
val read_metadata_file : dir:string -> t option tzresult Lwt.t

(** Write a metadata to the metadata file in [dir]. *)
val write_metadata_file : dir:string -> t -> unit tzresult Lwt.t
