(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2023 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Types = Tezos_dal_node_services.Types

module Term = struct
  let p2p_point_arg ~default_port =
    let open Cmdliner in
    let decoder str =
      match P2p_point.Id.of_string ~default_port str with
      | Ok x -> Ok x
      | Error msg -> Error (`Msg msg)
    in
    let printer = P2p_point.Id.pp in
    Arg.conv (decoder, printer)

  let docs = "OPTIONS"

  let data_dir =
    let open Cmdliner in
    let doc =
      Format.sprintf
        "The directory where the octez DAL node will store all its data. \
         Parent directories are created if necessary."
    in
    Arg.(
      value
      & opt (some file) None
      & info ~docs ~docv:"DIR" ~doc ["data-dir"; "d"])

  let rpc_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.rpc_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP socket point at which this RPC server of this instance can be \
         reached."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let expected_pow =
    let open Cmdliner in
    let doc = "Expected level of proof-of-work for peers identity." in
    Arg.(
      value
      & opt (some float) None
      & info ~docs ~doc ~docv:"FLOAT" ["expected-pow"])

  let net_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.listen_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP address and port at which the socket is bound. If \
         [public_addr] is not set, this is also the address and port at which \
         this instance can be reached by other P2P nodes."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["net-addr"])

  let public_addr =
    let open Cmdliner in
    let default_port = Configuration_file.default.public_addr |> snd in
    let doc =
      Format.asprintf
        "The TCP address and port at which this instance can be reached by \
         other P2P nodes."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["public-addr"])

  let endpoint_arg =
    let open Cmdliner in
    let decoder string =
      try Uri.of_string string |> Result.ok
      with _ -> Error (`Msg "The string '%s' is not a valid URI")
    in
    let printer = Uri.pp_hum in
    Arg.conv (decoder, printer)

  let endpoint =
    let open Cmdliner in
    let doc = "The Tezos node that the DAL node should connect to." in
    Arg.(
      value
      & opt (some endpoint_arg) None
      & info ~docs ~doc ~docv:"[ADDR:PORT]" ["endpoint"])

  let operator_profile_printer fmt = function
    | Types.Attester pkh ->
        Format.fprintf fmt "%a" Signature.Public_key_hash.pp pkh
    | Producer {slot_index} -> Format.fprintf fmt "%d" slot_index

  let attester_profile_arg =
    let open Cmdliner in
    let decoder string =
      match Signature.Public_key_hash.of_b58check_opt string with
      | None -> Error (`Msg "Unrecognized profile")
      | Some pkh -> Types.Attester pkh |> Result.ok
    in
    Arg.conv (decoder, operator_profile_printer)

  let producer_profile_arg =
    let open Cmdliner in
    let decoder string =
      let error () =
        Format.kasprintf
          (fun s -> Error (`Msg s))
          "Unrecognized profile for producer (expected nonnegative integer, \
           got %s)"
          string
      in
      match int_of_string_opt string with
      | None -> error ()
      | Some i when i < 0 -> error ()
      | Some slot_index -> Types.Producer {slot_index} |> Result.ok
    in
    Arg.conv (decoder, operator_profile_printer)

  let attester_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node attester profiles for given public key hashes."
    in
    Arg.(
      value
      & opt (list attester_profile_arg) []
      & info ~docs ~doc ~docv:"[PKH]" ["attester-profiles"])

  let producer_profile =
    let open Cmdliner in
    let doc = "The Octez DAL node producer profiles for given slot indexes." in
    Arg.(
      value
      & opt (list producer_profile_arg) []
      & info ~docs ~doc ~docv:"[slot index]" ["producer-profiles"])

  let bootstrap_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node bootstrap node profile. Note that a bootstrap node \
       cannot also be an attester/slot producer"
    in
    Arg.(value & flag & info ~docs ~doc ["bootstrap-profile"])

  let peers =
    let open Cmdliner in
    let default_list = Configuration_file.default.peers in
    let doc =
      "An additional peer list to expand the bootstrap peers from \
       dal_config.bootstrap_peers."
    in
    Arg.(
      value
      & opt (list string) default_list
      & info ~docs ~doc ~docv:"ADDR:PORT,..." ["peers"])

  let metrics_addr =
    let open Cmdliner in
    let doc = "Address on which to provide metrics over HTTP." in
    let default_port = Configuration_file.default.metrics_addr |> snd in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info
          ~docs
          ~doc
          ~docv:
            "ADDR:PORT or :PORT (by default ADDR is localhost and PORT is \
             11733)"
          ["metrics-addr"])

  let term process =
    Cmdliner.Term.(
      ret
        (const process $ data_dir $ rpc_addr $ expected_pow $ net_addr
       $ public_addr $ endpoint $ metrics_addr $ attester_profile
       $ producer_profile $ bootstrap_profile $ peers))
end

module Run = struct
  let description =
    [`S "DESCRIPTION"; `P "This command allows to run a DAL node."]

  let man = description

  let info =
    let version = Tezos_version_value.Bin_version.version_string in
    Cmdliner.Cmd.info ~doc:"The Octez DAL node" ~man ~version "run"

  let cmd run = Cmdliner.Cmd.v info (Term.term run)
end

module Config = struct
  let description =
    [
      `S "CONFIG DESCRIPTION";
      `P
        "Entry point for initializing, configuring and running an Octez DAL \
         node.";
    ]

  let man = description

  module Init = struct
    let man =
      [
        `S "DESCRIPTION";
        `P
          "This commands creates a configuration file with the default \
           parameters and the one provided on the command-line. This \
           configuration is then used by the run command.";
      ]

    let info =
      let version = Tezos_version_value.Bin_version.version_string in
      Cmdliner.Cmd.info ~doc:"Configuration initialisation" ~man ~version "init"

    let cmd run = Cmdliner.Cmd.v info (Term.term run)
  end

  let cmd run =
    let default = Cmdliner.Term.(ret (const (`Help (`Pager, None)))) in
    let info =
      let version = Tezos_version_value.Bin_version.version_string in
      Cmdliner.Cmd.info ~doc:"The Octez DAL node" ~man ~version "config"
    in
    Cmdliner.Cmd.group ~default info [Init.cmd run]
end

type options = {
  data_dir : string option;
  rpc_addr : P2p_point.Id.t option;
  expected_pow : float option;
  listen_addr : P2p_point.Id.t option;
  public_addr : P2p_point.Id.t option;
  endpoint : Uri.t option;
  profiles : Types.profiles option;
  metrics_addr : P2p_point.Id.t option;
  peers : string list;
}

type t = Run | Config_init

let make ~run =
  let run subcommand data_dir rpc_addr expected_pow listen_addr public_addr
      endpoint metrics_addr attesters producers bootstrap_flag peers =
    let run profiles =
      run
        subcommand
        {
          data_dir;
          rpc_addr;
          expected_pow;
          listen_addr;
          public_addr;
          endpoint;
          profiles;
          metrics_addr;
          peers;
        }
    in
    match (bootstrap_flag, attesters @ producers) with
    | false, [] -> run None
    | true, [] -> run @@ Some Types.Bootstrap
    | false, operator_profiles -> run @@ Some (Operator operator_profiles)
    | true, _ :: _ ->
        `Error
          (false, "A bootstrap node cannot also be an attester/slot producer.")
  in
  let default =
    Cmdliner.Term.(
      ret
        (const (fun _ _ _ _ _ -> `Help (`Pager, None))
        $ Term.data_dir $ Term.rpc_addr $ Term.expected_pow $ Term.net_addr
        $ Term.endpoint))
  in
  let info =
    let version = Tezos_version_value.Bin_version.version_string in
    Cmdliner.Cmd.info ~doc:"The Octez DAL node" ~version "octez-dal-node"
  in
  Cmdliner.Cmd.group
    ~default
    info
    [Run.cmd (run Run); Config.cmd (run Config_init)]
