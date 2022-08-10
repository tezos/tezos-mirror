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

let group = {Clic.name = "dal-node"; title = "Commands related to the DAL node"}

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Clic.default_arg
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
  Clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"rpc-address|ip"
    ~doc:
      (Format.sprintf
         "The address the DAL node listens to. Default value is %s"
         default)
    ~default
    (Client_config.string_parameter ())

let int_parameter =
  let open Clic in
  parameter (fun _ p ->
      try Lwt.return_ok (int_of_string p) with _ -> failwith "Cannot read int")

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"rpc-port"
    ~doc:
      (Format.sprintf
         "The port the DAL node listens to. Default value is %s"
         default)
    ~default
    int_parameter

let unsafe_srs_for_tests_arg =
  Clic.switch
    ~long:"unsafe-srs-for-tests"
    ~doc:
      (Format.sprintf
         "Run dal-node in test mode with an unsafe SRS (Trusted setup)")
    ()

let config_init_command =
  let open Lwt_result_syntax in
  let open Clic in
  command
    ~group
    ~desc:"Configure DAL node."
    (args4 data_dir_arg rpc_addr_arg rpc_port_arg unsafe_srs_for_tests_arg)
    (prefixes ["init-config"] stop)
    (fun (data_dir, rpc_addr, rpc_port, unsafe_srs) cctxt ->
      let open Configuration in
      let config = {data_dir; rpc_addr; rpc_port; unsafe_srs} in
      let* () = save config in
      let*! _ =
        cctxt#message "DAL node configuration written in %s" (filename config)
      in
      return ())

let run_command =
  let open Clic in
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
