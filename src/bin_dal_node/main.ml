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

(* TODO: https://gitlab.com/tezos/tezos/-/issues/6110
   Improve profile configuration UX for when we have conflicting CLI and config file. *)
let merge_profiles ~from_config_file ~from_cli =
  let open Types in
  match from_cli with
  | None -> from_config_file
  | Some from_cli -> (
      (* Note that the profile from the CLI is prioritized over
         the profile provided from the config file. *)
      match (from_config_file, from_cli) with
      | Bootstrap, Bootstrap -> Bootstrap
      | Operator _, Bootstrap -> Bootstrap
      | Bootstrap, Operator op -> Operator op
      | Operator op1, Operator op2 -> Operator (op1 @ op2))

let merge
    Cli.
      {
        data_dir;
        rpc_addr;
        expected_pow;
        listen_addr;
        public_addr;
        endpoint;
        metrics_addr;
        profiles;
        peers;
      } configuration =
  Configuration_file.
    {
      configuration with
      data_dir = Option.value ~default:configuration.data_dir data_dir;
      rpc_addr = Option.value ~default:configuration.rpc_addr rpc_addr;
      listen_addr = Option.value ~default:configuration.listen_addr listen_addr;
      public_addr = Option.value ~default:configuration.public_addr public_addr;
      expected_pow =
        Option.value ~default:configuration.expected_pow expected_pow;
      endpoint = Option.value ~default:configuration.endpoint endpoint;
      profiles =
        merge_profiles
          ~from_cli:profiles
          ~from_config_file:configuration.profiles;
      metrics_addr =
        Option.value ~default:configuration.metrics_addr metrics_addr;
      peers = peers @ configuration.peers;
    }

let wrap_with_error main_promise =
  let open Lwt_syntax in
  let* r = Lwt_exit.wrap_and_exit main_promise in
  match r with
  | Ok () ->
      let* _ = Lwt_exit.exit_and_wait 0 in
      Lwt.return (`Ok ())
  | Error err ->
      let* _ = Lwt_exit.exit_and_wait 1 in
      Lwt.return @@ `Error (false, Format.asprintf "%a" pp_print_trace err)

let run subcommand cli_options =
  match subcommand with
  | Cli.Run ->
      let data_dir =
        Option.value
          ~default:Configuration_file.default.data_dir
          cli_options.Cli.data_dir
      in
      Lwt.Exception_filter.(set handle_all_except_runtime) ;
      Lwt_main.run @@ wrap_with_error
      @@ Daemon.run ~data_dir (merge cli_options)
  | Config_init ->
      Lwt.Exception_filter.(set handle_all_except_runtime) ;
      Lwt_main.run @@ wrap_with_error
      @@ Configuration_file.save (merge cli_options Configuration_file.default)

let _ =
  let commands = Cli.make ~run in
  exit @@ Cmdliner.Cmd.eval commands
