(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let merge
    Cli.
      {
        data_dir;
        rpc_addr;
        expected_pow;
        listen_addr;
        endpoint;
        metrics_addr;
        profile;
        peers;
      } configuration =
  Configuration_file.
    {
      configuration with
      data_dir = Option.value ~default:configuration.data_dir data_dir;
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      listen_addr = Option.value ~default:configuration.listen_addr listen_addr;
      expected_pow =
        Option.value ~default:configuration.expected_pow expected_pow;
      endpoint = Option.value ~default:configuration.endpoint endpoint;
      profile = Option.either profile configuration.profile;
      metrics_addr =
        Option.value ~default:configuration.metrics_addr metrics_addr;
      peers = peers @ configuration.peers;
    }

let run subcommand cli_options =
  match subcommand with
  | Cli.Run ->
      let data_dir =
        Option.value
          ~default:Configuration_file.default.data_dir
          cli_options.Cli.data_dir
      in
      Lwt_main.run @@ Daemon.run ~data_dir (merge cli_options)
  | Config_init ->
      Lwt_main.run
      @@ Configuration_file.save (merge cli_options Configuration_file.default)

let _ =
  let commands = Cli.make ~run in
  Cmdliner.Cmd.eval commands
