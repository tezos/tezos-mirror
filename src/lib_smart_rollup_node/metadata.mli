(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

type genesis_info = {level : int32; commitment_hash : Commitment.Hash.t}

module V0 : sig
  type t = {rollup_address : Address.t; context_version : Context.Version.t}

  (** Read the metadata file from [dir]. *)
  val read_metadata_file : dir:string -> t option tzresult Lwt.t

  (** Write a metadata to the metadata file in [dir]. *)
  val write_metadata_file : dir:string -> t -> unit tzresult Lwt.t
end

module V1 : sig
  type t = {
    rollup_address : Address.t;
    context_version : Context.Version.t;
    kind : Kind.t;
    genesis_info : genesis_info;
  }

  (** Read the metadata file from [dir]. *)
  val read_metadata_file : dir:string -> t option tzresult Lwt.t

  (** Write a metadata to the metadata file in [dir]. *)
  val write_metadata_file : dir:string -> t -> unit tzresult Lwt.t
end

module Versioned : sig
  type t = V0 of V0.t | V1 of V1.t

  (** Read the metadata file from [dir]. *)
  val read_metadata_file : dir:string -> t option tzresult Lwt.t

  (** Write a metadata to the metadata file in [dir]. *)
  val write_metadata_file : dir:string -> t -> unit tzresult Lwt.t
end

include module type of V1
