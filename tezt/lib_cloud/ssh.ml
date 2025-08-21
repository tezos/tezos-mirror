(*****************************************************************************)
(*                                                                           *)
(* SPDX-License-Identifier: MIT                                              *)
(* SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>      *)
(*                                                                           *)
(*****************************************************************************)

let generate_key () =
  let path = Format.asprintf "%s" (Env.ssh_private_key_filename ()) in
  (* -C is for comment;
     -N is for the passphrase (no passphrase here)
  *)
  Process.run "ssh-keygen" ["-t"; "rsa"; "-f"; path; "-C"; path; "-N"; ""]

let public_key () =
  let ssh_public_key_filename = Env.ssh_public_key_filename () in
  let* () =
    if not (Sys.file_exists ssh_public_key_filename) then (
      Log.info "SSH public key not found, creating it..." ;
      generate_key ())
    else Lwt.return_unit
  in
  let* content =
    Process.run_and_read_stdout ~name:"cat" "cat" [ssh_public_key_filename]
  in
  Lwt.return content
