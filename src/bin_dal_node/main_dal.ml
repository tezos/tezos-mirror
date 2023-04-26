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

let group =
  {Tezos_clic.name = "dal-node"; title = "Commands related to the DAL node"}

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Tezos_clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the DAL node data directory. Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let rpc_addr_arg =
  let default = Configuration.default_rpc_addr in
  Tezos_clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"rpc-address|ip"
    ~doc:
      (Format.sprintf
         "The address the DAL node listens to. Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let int_parameter =
  let open Tezos_clic in
  parameter (fun _ p ->
      let open Lwt_result_syntax in
      let* i =
        try Lwt.return_ok (int_of_string p)
        with _ -> failwith "Cannot read int"
      in
      if i < 0 then failwith "Parameter must be non-negative" else return i)

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Tezos_clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"rpc-port"
    ~doc:
      (Format.sprintf
         "The port the DAL node listens to. Default value is %s"
         default)
    ~default
    int_parameter

let listen_addr_arg =
  let default = P2p_point.Id.to_string Configuration.default_listen_addr in
  Tezos_clic.default_arg
    ~long:"net-addr"
    ~placeholder:"ADDR:PORT"
    ~doc:
      (Format.sprintf
         "The TCP address and port at which this instance can be reached. \
          Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let use_unsafe_srs_for_tests_arg =
  Tezos_clic.switch
    ~long:"use-unsafe-srs-for-tests"
    ~doc:
      (Format.sprintf
         "Run dal-node in test mode with an unsafe SRS (Trusted setup)")
    ()

let config_init_command =
  let open Lwt_result_syntax in
  let open Tezos_clic in
  command
    ~group
    ~desc:"Configure DAL node."
    (args5
       data_dir_arg
       rpc_addr_arg
       rpc_port_arg
       listen_addr_arg
       use_unsafe_srs_for_tests_arg)
    (prefixes ["init-config"] stop)
    (fun (data_dir, rpc_addr, rpc_port, listen_addr, use_unsafe_srs) cctxt ->
      let open Configuration in
      let listen_addr = P2p_point.Id.of_string_exn listen_addr in
      let config =
        {
          data_dir;
          rpc_addr;
          rpc_port;
          use_unsafe_srs;
          neighbors = [];
          peers = [];
          listen_addr;
          expected_pow = default_expected_pow;
          network_name = default_network_name;
        }
      in
      let* () = save config in
      let*! _ =
        cctxt#message "DAL node configuration written in %s" (filename config)
      in
      return ())

let run_command =
  let open Tezos_clic in
  command
    ~group
    ~desc:"Run the DAL node."
    (args1 data_dir_arg)
    (prefixes ["run"] @@ stop)
    (fun data_dir cctxt -> Daemon.run ~data_dir cctxt)

let commands () = [run_command; config_init_command]

let select_commands _ _ =
  let open Lwt_result_syntax in
  return (commands ())

let () = Client_main_run.run (module Client_config) ~select_commands
