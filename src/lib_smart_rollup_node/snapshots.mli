(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2023 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(** [export ~data_dir ~dest] creates a tar gzipped archive in [dest] (or the
    current directory) containing a snapshot of the data of the rollup node with
    data directory [data_dir]. The path of the snapshot archive is returned. *)
val export : data_dir:string -> dest:string option -> string tzresult Lwt.t
