(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** The type of individual unsafe patch content. *)
type unsafe_patch =
  | Increase_max_nb_ticks of int64
      (** Increase the maximum number of ticks.  *)
  | Patch_durable_storage of {key : string; value : string}
  | Patch_PVM_version of {version : string}

(** Where the patch comes from  *)
type kind =
  | Hardcoded  (** Hardcoded by the rollup node *)
  | User_provided  (** Provided by the user in the configuration file *)

(** The type of registered patches for the PVM. *)
type t = private (unsafe_patch * kind) list

(** Encoding for unsafe patches. *)
val unsafe_patch_encoding : unsafe_patch Data_encoding.t

(** Pretty printer for unsafe patches. *)
val pp_unsafe_patch : Format.formatter -> unsafe_patch -> unit

(** [make kind address patches] builds the patches from the user provided list
    [patches] and adds the hardcoded PVM patches for the rollup [address]. *)
val make : Kind.t -> Address.t -> unsafe_patch list -> t tzresult
