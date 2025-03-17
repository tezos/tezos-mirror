(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2025 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Brassaia-eio-tezos
    Invocation:   dune exec brassaia-eio/test/irmin-tezos/main.exe
    Subject: This file is the entrypoint of Brassaia-eio-tezos Tezt tests. It
    dispatches to other files.
*)

let () = Generate.register ()
