(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

(** This module contains values that depend on environment
    variables. Those values are lazy so that they are evaluated within
    a test and fail if one environment variable is missing. *)

(** Value of the environment variable [TF_WORKSPACE]. *)
val workspace : string Lazy.t

(** Path to the ssh private key that will be used with the docker
    image. Depends on [workspace]. *)
val ssh_private_key : string Lazy.t

(** Path to the ssh public key that will be used with the docker
    image. Depends on [workspace]. *)
val ssh_public_key : string Lazy.t

(** Path to the docker image that will be used when running
    VMs. Depends on [workspace]. *)
val dockerfile : string Lazy.t
