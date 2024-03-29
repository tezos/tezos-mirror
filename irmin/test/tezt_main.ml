(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

(** Testing
    -------
    Component:    Irmin
    Invocation:   dune exec irmin/test/main.exe
    Subject:      This file is the entrypoint of all Irmin Tezt tests. It dispatches to
            other files.
*)

let () = Test_lib_irmin_store.register ()
