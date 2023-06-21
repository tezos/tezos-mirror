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

let run subcommand
    Cli.
      {
        data_dir;
        endpoint;
        rpc_addr;
        expected_pow;
        listen_addr;
        profile;
        use_unsafe_srs_for_tests;
        peers;
      } =
  let default_configuration =
    Configuration_file.
      {
        data_dir =
          Option.value ~default:Configuration_file.default.data_dir data_dir;
        rpc_addr =
          Option.value ~default:Configuration_file.default.rpc_addr rpc_addr;
        use_unsafe_srs = use_unsafe_srs_for_tests;
        neighbors = [];
        peers;
        listen_addr;
        expected_pow;
        network_name = default.network_name;
        endpoint = default.endpoint;
        profile;
      }
  in
  match subcommand with
  | Cli.Run ->
      let rpc_context = Rpc_context.make endpoint in
      Lwt_main.run @@ Daemon.run default_configuration rpc_context
  | Config_init -> Lwt_main.run @@ Configuration_file.save default_configuration

let _ =
  let commands = Cli.make ~run in
  Cmdliner.Cmd.eval commands
