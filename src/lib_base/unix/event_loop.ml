(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs. <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** The [Event_loop] module provides an abstraction layer on top of promises
    for use in the Octez codebase.

    For now, it is just an alias for [Lwt_main.run], but [Lwt] use is about to
    change / be replaced in the near future, following the recent adoption of
    OCaml 5 and the opportunity to use parallelism.

    Some components will use [eio*] libs, and if your library or binary uses
    these components, you *MUST* use this module instead of the traditionnal
    [Lwt_main.run].
*)

(** Alias of [Lwt_main.run] *)
let main_run = Lwt_main.run
