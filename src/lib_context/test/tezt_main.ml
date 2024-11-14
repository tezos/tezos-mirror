(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Irmin
    Invocation:   dune exec src/lib_context/test/main.exe
    Subject:      This file is the entrypoint of all Lib_context Tezt tests.
                  It dispatches to other files.
*)

let () = Test_context.register ()
