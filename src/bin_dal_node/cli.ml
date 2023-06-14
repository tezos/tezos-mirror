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
    let default = Configuration.default_data_dir in
    let doc =
      Format.sprintf
        "The directory where the octez DAL node will store all its data. \
         Parent directories are created if necessary."
    in
    Arg.(
      value & opt file default & info ~docs ~docv:"DIR" ~doc ["data-dir"; "d"])

  let rpc_addr =
    let open Cmdliner in
    let default = Configuration.default_rpc_addr in
    let doc =
      Format.asprintf
        "The TCP socket point at which this RPC server of this instance can be \
         reached. Default value is %a."
        P2p_point.Id.pp
        default
    in
    Arg.(
      value
      & opt (p2p_point_arg ~default_port:(snd default)) default
      & info ~docs ~doc ~docv:"ADDR:PORT" ["rpc-addr"])

  let expected_pow =
    let open Cmdliner in
    let default = Configuration.default_expected_pow in
    let doc = "Expected level of proof-of-work for peers identity." in
    Arg.(
      value & opt float default & info ~docs ~doc ~docv:"FLOAT" ["expected-pow"])

  let net_addr =
    let open Cmdliner in
    let default = Configuration.default_listen_addr in
    let doc =
      Format.asprintf
        "The TCP address and port at which this instance can be reached by \
         other P2P nodes. Default value is %a"
        P2p_point.Id.pp
        default
    in
    Arg.(
      value
      & opt (p2p_point_arg ~default_port:(snd default)) default
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
    let doc = "The octez-node that the DAL node should connect to." in
    Arg.(
      value
      & opt endpoint_arg (Uri.of_string "http://localhost:9732")
      & info ~docs ~doc ~docv:"[ADDR:PORT]" ["endpoint"])

  (* This is provided as a command line option for facility but
     eventually it should not appear to the user. *)
  let use_unsafe_srs_for_tests =
    let open Cmdliner in
    Arg.(value & flag & info ["use-unsafe-srs-for-tests"])

  let term process =
    Cmdliner.Term.(
      ret
        (const process $ data_dir $ rpc_addr $ expected_pow $ net_addr
       $ endpoint $ use_unsafe_srs_for_tests))
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
  data_dir : string;
  rpc_addr : P2p_point.Id.t;
  expected_pow : float;
  net_addr : P2p_point.Id.t;
  octez_node : Uri.t;
  use_unsafe_srs_for_tests : bool;
}

type t = Run | Config_init

let make ~run =
  let run subcommand data_dir rpc_addr expected_pow net_addr octez_node
      use_unsafe_srs_for_tests =
    run
      subcommand
      {
        data_dir;
        rpc_addr;
        expected_pow;
        net_addr;
        octez_node;
        use_unsafe_srs_for_tests;
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
