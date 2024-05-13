(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Functori, <contact@functori.com>             *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs, <contact@nomadic-labs.com>     *)
(*                                                                           *)
(*****************************************************************************)

(** Helper functions to load/store the level of the last processed L1 block by
    the crawler from/to disk. *)

type t

(** Initializes a KVS store at the given location to remember the last processed
    level by the crawler on disk. *)
val init : root_dir:string -> t tzresult Lwt.t

(** [load_last_processed_level t ] loads and returns the content of
    [data_dir/last_processed_level.json] as an int32. If the file doesn't
    exists, the result is [None]. The function returns
    [Failed_to_load_last_processed_level] in the error monad in case of
    failure. *)
val load_last_processed_level : t -> int32 option tzresult Lwt.t

(** [save_last_processed_level t ~level] saves the value
    of [last_processed_level] into [data_dir/last_processed_level.json],
    overriding any previous value. The function returns
    [Failed_to_save_last_processed_level] in the error monad in case of
    failure. *)
val save_last_processed_level : t -> level:int32 -> unit tzresult Lwt.t
