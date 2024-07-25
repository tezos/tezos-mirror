(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Functori <contact@functori.com>                        *)
(*                                                                           *)
(*****************************************************************************)

(* This module defines how tezt tests will interact with cast client.
   Cast is an ethereum client to interact with the node provided by Foundry
   https://book.getfoundry.sh/cast *)

let spawn arguments = Process.spawn "cast" arguments

let spawn_command_and_read_string ?expect_failure arguments =
  let process = spawn arguments in
  let* data = Process.check_and_read_stdout ?expect_failure process in
  return (String.trim data)

let version () = spawn_command_and_read_string ["--version"]
