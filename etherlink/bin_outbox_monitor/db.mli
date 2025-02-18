(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Functori, <contact@functori.com>                       *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Data is stored in an SQLite database. *)
type t = Sqlite.t

(** Initialize the database by creating it if it doesn't exist and applying
    migrations when needed. *)
val init : data_dir:string -> [`Read_only | `Read_write] -> t tzresult Lwt.t
