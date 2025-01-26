(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

open TzMonad

type download_request

val defaults : download_request list

val dal_trusted_setup_folder : string

val output_files : string * string

val download_list : download_request list -> unit tzresult Lwt.t
