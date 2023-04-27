(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019 Nomadic Labs, <contact@nomadic-labs.com>               *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

open Filename.Infix

let get_config data_dir config_file expected_pow =
  let open Lwt_result_syntax in
  let* cfg =
    match (data_dir, config_file) with
    | None, None ->
        let default_config =
          Config_file.default_data_dir // Data_version.default_config_file_name
        in
        let*! config_file_exists = Lwt_unix.file_exists default_config in
        if config_file_exists then Config_file.read default_config
        else return Config_file.default_config
    | None, Some config_file -> Config_file.read config_file
    | Some data_dir, None ->
        let* cfg =
          Config_file.read (data_dir // Data_version.default_config_file_name)
        in
        return {cfg with data_dir}
    | Some data_dir, Some config_file ->
        let* cfg = Config_file.read config_file in
        return {cfg with data_dir}
  in
  Config_file.update ?expected_pow cfg

(** Commands *)

let identity_file data_dir = data_dir // Data_version.default_identity_file_name

let show data_dir config_file expected_pow =
  Shared_arg.process_command
    (let open Lwt_result_syntax in
    let* {Config_file.data_dir; _} =
      get_config data_dir config_file expected_pow
    in
    let* id = Node_identity_file.read (identity_file data_dir) in
    Format.printf "Peer_id: %a.@." P2p_peer.Id.pp id.peer_id ;
    return_unit)

let check data_dir config_file expected_pow =
  Shared_arg.process_command
    (let open Lwt_result_syntax in
    let* {Config_file.data_dir; p2p = {expected_pow; _}; _} =
      get_config data_dir config_file expected_pow
    in
    let* id = Node_identity_file.read ~expected_pow (identity_file data_dir) in
    Format.printf
      "Peer_id: %a. Proof of work is higher than %.2f.@."
      P2p_peer.Id.pp
      id.peer_id
      expected_pow ;
    return_unit)

let generate data_dir config_file expected_pow =
  Shared_arg.process_command
    (let open Lwt_result_syntax in
    let* {Config_file.data_dir; p2p = {expected_pow; _}; _} =
      get_config data_dir config_file expected_pow
    in
    let check_data_dir ~data_dir =
      let dummy_genesis =
        {
          Genesis.time = Time.Protocol.epoch;
          block = Block_hash.zero;
          protocol = Protocol_hash.zero;
        }
      in
      Data_version.ensure_data_dir ~mode:Exists dummy_genesis data_dir
    in
    let* _id =
      Node_identity_file.generate
        ~check_data_dir
        (identity_file data_dir)
        expected_pow
    in
    return_unit)

(** Main *)

module Term = struct
  let expected_pow =
    let open Cmdliner in
    let doc =
      "Expected amount of proof-of-work for the node identity. The optional \
       parameter should be a float between 0 and 256, where\n\
      \       0 disables the proof-of-work mechanism."
    in
    Arg.(value & pos 0 (some float) None & info [] ~docv:"DIFFICULTY" ~doc)

  let cmds =
    [
      Cmdliner.Cmd.v
        (Cmdliner.Cmd.info
           ~doc:
             "reads, parses and displays the current identity of the node. Use \
              this command to see what identity will be used by Tezos. This is \
              the default operation"
           "show")
        Cmdliner.Term.(
          ret
            (const show $ Shared_arg.Term.data_dir $ Shared_arg.Term.config_file
           $ expected_pow));
      Cmdliner.Cmd.v
        (Cmdliner.Cmd.info
           ~doc:
             "generates an identity whose proof of work stamp difficulty is at \
              least equal to $(i,difficulty). The value provided must be a \
              floating point number between 0 and 256. It roughly reflects the \
              numbers of expected leading zeroes in the hash of the identity \
              data-structure. Therefore, a value of 0 means no proof-of-work, \
              and the difficulty doubles for each increment of 1 in the \
              difficulty value"
           "generate")
        Cmdliner.Term.(
          ret
            (const generate $ Shared_arg.Term.data_dir
           $ Shared_arg.Term.config_file $ expected_pow));
      Cmdliner.Cmd.v
        (Cmdliner.Cmd.info
           ~doc:
             "checks that an identity is valid and that its proof of work \
              stamp difficulty is at least equal to $(i,difficulty)"
           "check")
        Cmdliner.Term.(
          ret
            (const check $ Shared_arg.Term.data_dir
           $ Shared_arg.Term.config_file $ expected_pow));
    ]
end

module Manpage = struct
  let command_description =
    "The $(b,identity) command is meant to create and manage node identities. \
     An $(i,identity) uniquely identifies a peer on the network and consists \
     of a cryptographic key pair as well as a proof-of-work stamp that \
     certifies that enough CPU time has been dedicated to produce the \
     identity, to avoid sybil attacks. An identity with enough proof-of-work \
     is required to participate in the Tezos network, therefore this command \
     is necessary to launch Tezos the first time."

  let man =
    (* [ `S misc_docs ] @ *)
    Shared_arg.Manpage.bugs

  let info = Cmdliner.Cmd.info ~doc:"Manage node identities" ~man "identity"
end

let cmd = Cmdliner.Cmd.group Manpage.info Term.cmds
