(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* Copyright (c) 2024 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(*****************************************************************************)

let write_file ?runner ~contents filename =
  match runner with
  | None -> write_file filename ~contents |> Lwt.return
  | Some runner ->
      let cmd =
        Runner.Shell.(redirect_stdout (cmd [] "echo" [contents]) filename)
      in
      let cmd, args = Runner.wrap_with_ssh runner cmd in
      Process.run cmd args
