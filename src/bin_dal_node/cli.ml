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
        "The TCP address and port at which this instance can be reached by \
         other P2P nodes."
    in
    Arg.(
      value
      & opt (some (p2p_point_arg ~default_port)) None
      & info ~docs ~doc ~docv:"ADDR:PORT" ["net-addr"])

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

  let attestor_profile_arg =
    let open Cmdliner in
    let decoder string =
      match Signature.Public_key_hash.of_b58check_opt string with
      | None -> Error (`Msg "Unrecognized profile")
      | Some pkh -> `Attestor pkh |> Result.ok
    in
    let printer fmt profile =
      match profile with `Attestor pkh -> Signature.Public_key_hash.pp fmt pkh
    in
    Arg.conv (decoder, printer)

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
      | Some slot_index -> Result.ok (`Producer slot_index)
    in
    let printer fmt profile =
      match profile with
      | `Producer slot_index -> Format.fprintf fmt "%d" slot_index
    in
    Arg.conv (decoder, printer)

  let attestor_profile =
    let open Cmdliner in
    let doc =
      "The Octez DAL node attestor profile for a given public key hash."
    in
    Arg.(
      value
      & opt (some attestor_profile_arg) None
      & info ~docs ~doc ~docv:"[PKH]" ["attestor-profile"])

  let producer_profile =
    let open Cmdliner in
    let doc = "The Octez DAL node producer profile for a given slot index." in
    Arg.(
      value
      & opt (some producer_profile_arg) None
      & info ~docs ~doc ~docv:"[slot index]" ["producer-profile"])

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
            "ADDR:PORT or :PORT (by default ADDR is localhost and PORT is 9932)"
          ["metrics-addr"])

  let term process =
    Cmdliner.Term.(
      ret
        (const process $ data_dir $ rpc_addr $ expected_pow $ net_addr
       $ endpoint $ metrics_addr $ attestor_profile $ producer_profile $ peers))
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
  endpoint : Uri.t option;
  profile : Services.Types.profile option;
  metrics_addr : P2p_point.Id.t option;
  peers : string list;
}

type t = Run | Config_init

let make ~run =
  let run subcommand data_dir rpc_addr expected_pow listen_addr endpoint
      metrics_addr attestor_opt producer_opt peers =
    let profile =
      match (attestor_opt, producer_opt) with
      | None, None -> None
      | None, Some (`Producer slot_index) ->
          Some (Services.Types.Producer {slot_index})
      | Some (`Attestor pkh), _ ->
          (* If both attestor and producer are present on the commandline, we
             prioritize the attestor argument.

             TODO: https://gitlab.com/tezos/tezos/-/issues/5967
             The DAL node is able to handle several profiles concurrently. The commandline
             should allow that.
          *)
          Some (Services.Types.Attestor pkh)
    in
    run
      subcommand
      {
        data_dir;
        rpc_addr;
        expected_pow;
        listen_addr;
        endpoint;
        profile;
        metrics_addr;
        peers;
      }
    |> function
    | Ok v -> `Ok v
    | Error error ->
        let str = Format.asprintf "%a" pp_print_trace error in
        `Error (false, str)
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
