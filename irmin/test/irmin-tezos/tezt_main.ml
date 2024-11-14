(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Irmin-tezos
    Invocation:   dune exec irmin/test/irmin-tezos/main.exe
    Subject: This file is the entrypoint of Irmin-tezos Tezt tests. It
    dispatches to other files.
*)

let () = Generate.register ()
