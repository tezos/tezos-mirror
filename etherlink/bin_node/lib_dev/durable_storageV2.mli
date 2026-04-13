(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2026 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(*****************************************************************************)

(** Typed access layer for the EVM node durable storage.

    All durable storage access MUST go through this module.
    The [path] GADT ensures type-safe read/write/delete operations.
    [Raw_path] is provided as an escape hatch for paths not yet
    modeled in the GADT. New code should add typed constructors
    instead of using [Raw_path]. *)

(** {2 Typed path GADT} *)

type _ path = Raw_path : string -> bytes path

(** {2 Typed operations} *)

val storage_version : Pvm.State.t -> int tzresult Lwt.t

val read : 'a path -> Pvm.State.t -> 'a tzresult Lwt.t

val read_opt : 'a path -> Pvm.State.t -> 'a option tzresult Lwt.t

val write : 'a path -> 'a -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val delete : 'a path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val delete_dir : 'a path -> Pvm.State.t -> Pvm.State.t tzresult Lwt.t

val exists : 'a path -> Pvm.State.t -> bool tzresult Lwt.t

val subkeys : 'a path -> Pvm.State.t -> string trace tzresult Lwt.t
