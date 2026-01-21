(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let checks state _current_level = Lwt.return state

let () = Tezt_migration_registry.Register.register ~title:"Alpha: checks" checks
