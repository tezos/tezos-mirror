(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

let sc_rollup_address_param =
  Clic.param
    ~name:"sc-rollup-address"
    ~desc:"The smart-contract rollup address"
    (Clic.parameter (fun _ s ->
         match Protocol.Alpha_context.Sc_rollup.Address.of_b58check_opt s with
         | None -> failwith "Invalid smart-contract rollup address"
         | Some addr -> return addr))

let rpc_addr_arg =
  let default = Configuration.default_rpc_addr in
  Clic.default_arg
    ~long:"rpc-addr"
    ~placeholder:"address|ip"
    ~doc:
      (Format.sprintf
         "The address the smart-contract rollup node listens to. Default value \
          is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let rpc_port_arg =
  let default = Configuration.default_rpc_port |> string_of_int in
  Clic.default_arg
    ~long:"rpc-port"
    ~placeholder:"port"
    ~doc:
      (Format.sprintf
         "The port the smart-contract rollup node listens to. Default value is \
          %s"
         default)
    ~default
    Client_proto_args.int_parameter

let data_dir_arg =
  let default = Configuration.default_data_dir in
  Clic.default_arg
    ~long:"data-dir"
    ~placeholder:"data-dir"
    ~doc:
      (Format.sprintf
         "The path to the smart-contract rollup node data directory. Default \
          value is %s"
         default)
    ~default
    Client_proto_args.string_parameter

let sc_rollup_commands () =
  let open Clic in
  let group =
    {
      Clic.name = "sc_rollup.node";
      title = "Commands related to the smart-contract rollup node.";
    }
  in
  [
    command
      ~group
      ~desc:"Configure the smart-contract rollup node."
      (args3 data_dir_arg rpc_addr_arg rpc_port_arg)
      (prefixes ["config"; "init"; "on"] @@ sc_rollup_address_param @@ stop)
      (fun (data_dir, rpc_addr, rpc_port) sc_rollup_address cctxt ->
        let open Configuration in
        let config = {data_dir; sc_rollup_address; rpc_addr; rpc_port} in
        save config >>=? fun () ->
        cctxt#message
          "Smart-contract rollup node configuration written in %s"
          (filename config)
        >>= fun _ -> return ());
  ]

let select_commands _ _ =
  return
    (List.map
       (Clic.map_command (new Protocol_client_context.wrap_full))
       (sc_rollup_commands ()))

let () = Client_main_run.run (module Client_config) ~select_commands
