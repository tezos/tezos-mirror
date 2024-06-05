(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let generate_key () =
  let path = Format.asprintf "%s" (Lazy.force Env.ssh_private_key) in
  (* -C is for comment;
     -N is for the passphrase (no passphrase here)
  *)
  Process.run "ssh-keygen" ["-t"; "rsa"; "-f"; path; "-C"; path; "-N"; ""]
